//! Run a pass over the HIR to collect all references to each definition.
use std::collections::HashMap;

use pomelo_hir as hir;
use pomelo_hir::{arena::Idx, DefLoc, FileArena, LongTyCon, LongVId};

use crate::HirVisitor;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValUsage {
    Pat(Idx<hir::Pat>),
    Expr(Idx<hir::Expr>),
    Dec(Idx<hir::Dec>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyUsage {
    Ty(Idx<hir::Ty>),
    Dec(Idx<hir::Dec>),
    Pat(Idx<hir::Pat>),
}

/// Tracks variable, type constructor, and exception usages.
///
/// TODO: track tyvars?
/// TODO: `exception_map` is not being used right now, everything is being treated as a VId
#[derive(Debug, Clone)]
pub struct UsageCtxt<'a> {
    hir: &'a hir::File,
    val_map: HashMap<(LongVId, DefLoc), Vec<ValUsage>>,
    ty_map: HashMap<(LongTyCon, DefLoc), Vec<TyUsage>>,
    exception_map: HashMap<Idx<hir::Dec>, Vec<Idx<hir::Expr>>>,
}

impl<'a> UsageCtxt<'a> {
    pub fn new(hir: &'a hir::File) -> Self {
        let mut out = Self {
            hir,
            val_map: HashMap::new(),
            ty_map: HashMap::new(),
            exception_map: HashMap::new(),
        };

        for dec in hir.topdecs() {
            out.visit_dec(*dec);
        }

        out
    }

    pub fn register_value(&mut self, usage: ValUsage, def: (LongVId, DefLoc)) {
        self.val_map.entry(def).or_insert(Vec::new()).push(usage);
    }

    pub fn register_tycon(&mut self, usage: TyUsage, def: (LongTyCon, DefLoc)) {
        self.ty_map.entry(def).or_insert(Vec::new()).push(usage);
    }

    pub fn register_exception(&mut self, usage: Idx<hir::Expr>, def: Idx<hir::Dec>) {
        self.exception_map
            .entry(def)
            .or_insert(Vec::new())
            .push(usage);
    }

    pub fn lookup_value(&self, def: &(LongVId, DefLoc)) -> Option<&[ValUsage]> {
        self.val_map.get(def).map(Vec::as_slice)
    }

    pub fn lookup_tycon(&self, def: &(LongTyCon, DefLoc)) -> Option<&[TyUsage]> {
        self.ty_map.get(def).map(Vec::as_slice)
    }

    pub fn lookup_exception(&self, def: Idx<hir::Dec>) -> Option<&[Idx<hir::Expr>]> {
        self.exception_map.get(&def).map(Vec::as_slice)
    }

    pub fn arenas(&self) -> &impl FileArena {
        self.hir.arenas()
    }
}

impl<'a> HirVisitor for UsageCtxt<'a> {
    fn visit_dec(&mut self, dec: Idx<hir::Dec>) {
        let d = self.hir.arenas().get_dec(dec);

        use hir::DecKind::*;
        match &d.kind {
            Seq { decs } => decs.iter().for_each(|d| self.visit_dec(*d)),
            Val { bindings, .. } => bindings.iter().for_each(|b| {
                self.visit_pat(b.pat);
                self.visit_expr(b.expr);
            }),
            Ty { bindings } => bindings.iter().for_each(|b| {
                self.visit_ty(b.ty);
            }),
            Datatype { databinds } => databinds
                .iter()
                .for_each(|databind| self.visit_databind(databind)),
            Replication { rhs, .. } => self.register_tycon(TyUsage::Dec(dec), rhs.clone()),
            Abstype {
                databinds,
                dec: abstype_dec,
            } => {
                databinds
                    .iter()
                    .for_each(|databind| self.visit_databind(databind));
                self.visit_dec(*abstype_dec);
            }
            Exception { exbind } => match exbind {
                hir::ExBind::Name { ty, .. } => {
                    if let Some(ty) = ty {
                        self.visit_ty(*ty)
                    }
                }
                hir::ExBind::Assignment { rhs, .. } => {
                    self.register_value(ValUsage::Dec(dec), rhs.clone())
                }
            },
            Local { inner, outer } => {
                self.visit_dec(*inner);
                self.visit_dec(*outer);
            }
            Fixity { vids, .. } => vids.iter().for_each(|(vid, loc)| {
                self.register_value(ValUsage::Dec(dec), (LongVId::from(*vid), *loc))
            }),
            Missing | Open { .. } => {}
        }
    }

    fn visit_expr(&mut self, expr: Idx<hir::Expr>) {
        let e = self.hir.arenas().get_expr(expr);

        use hir::ExprKind::*;
        match &e.kind {
            Missing | Scon(_) => {}
            Seq { exprs } => exprs.iter().for_each(|e| self.visit_expr(*e)),
            VId { longvid, .. } => self.register_value(ValUsage::Expr(expr), longvid.clone()),
            Record { rows } => rows.iter().for_each(|r| self.visit_expr(r.expr)),
            Let { dec, expr } => {
                self.visit_dec(*dec);
                self.visit_expr(*expr);
            }
            Application { expr, param } => {
                self.visit_expr(*expr);
                self.visit_expr(*param);
            }
            Infix {
                lhs,
                vid: (vid, loc),
                rhs,
            } => {
                self.visit_expr(*lhs);
                self.register_value(ValUsage::Expr(expr), (LongVId::from(*vid), *loc));
                self.visit_expr(*rhs);
            }
            Typed { expr, ty } => {
                self.visit_expr(*expr);
                self.visit_ty(*ty);
            }
            Handle { expr, match_ } => {
                self.visit_expr(*expr);
                match_.iter().for_each(|mrule| self.visit_mrule(mrule));
            }
            Fn { match_ } => match_.iter().for_each(|mrule| self.visit_mrule(mrule)),
            Raise { expr } => self.visit_expr(*expr),
        }
    }

    fn visit_pat(&mut self, pat: Idx<hir::Pat>) {
        let p = self.hir.arenas().get_pat(pat);

        use hir::PatKind::*;
        match &p.kind {
            Missing | Wildcard | Scon(_) => {}
            VId {
                longvid: (vid, loc),
                ..
            } => {
                // Note that this is a data constructor, not a type constructor!
                if loc.is_some() {
                    self.register_value(ValUsage::Pat(pat), (vid.clone(), loc.unwrap()));
                }
            }
            Record { rows } => rows.iter().for_each(|row| {
                if let hir::PatRow::Pattern { pat, .. } = row {
                    self.visit_pat(*pat);
                }
            }),
            Constructed {
                longvid,
                pat: cons_pat,
                ..
            } => {
                self.register_value(ValUsage::Pat(pat), longvid.clone());
                self.visit_pat(*cons_pat);
            }
            Infix {
                lhs,
                vid: (vid, loc),
                rhs,
            } => {
                self.visit_pat(*lhs);
                self.register_value(ValUsage::Pat(pat), (LongVId::from(*vid), *loc));
                self.visit_pat(*rhs);
            }
            Typed { pat, ty } => {
                self.visit_pat(*pat);
                self.visit_ty(*ty);
            }
            Layered { ty, pat, .. } => {
                if let Some(ty) = ty {
                    self.visit_ty(*ty);
                }
                self.visit_pat(*pat);
            }
        }
    }

    fn visit_ty(&mut self, ty: Idx<hir::Ty>) {
        let t = self.hir.arenas().get_ty(ty);

        use hir::TyKind::*;
        match &t.kind {
            Missing => {}
            Var(_) => {} // TODO: make a `TyVar` also refer to it's `DefLoc`?
            Record { tyrows } => tyrows.iter().for_each(|row| self.visit_ty(row.ty)),
            Constructed { tyseq, longtycon } => {
                tyseq.iter().for_each(|ty| self.visit_ty(*ty));
                self.register_tycon(TyUsage::Ty(ty), longtycon.clone());
            }
            Function { domain, range } => {
                self.visit_ty(*domain);
                self.visit_ty(*range);
            }
        }
    }
}

// Helper methods for visitor impl
impl<'a> UsageCtxt<'a> {
    fn visit_databind(&mut self, databind: &hir::DataBind) {
        databind
            .conbinds
            .iter()
            .filter_map(|conbind| conbind.ty)
            .for_each(|ty| self.visit_ty(ty))
    }

    fn visit_mrule(&mut self, mrule: &hir::MRule) {
        self.visit_pat(mrule.pat);
        self.visit_expr(mrule.expr);
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn value_refs() {
        use crate::usages::{UsageCtxt, ValUsage};
        use pomelo_hir::{DefLoc, FileArena};
        use pomelo_parse::Parser;

        let src = r#"
        val a = 1;
        val b = let val a = a in a end;
        val c = a;
    "#;

        let ast = Parser::new(src).parse();
        let (hir, errs) = pomelo_hir::lower_ast_to_hir(ast);

        for e in errs.iter() {
            println!("{e}");
        }

        assert!(errs.is_empty());
        let topdecs = hir.topdecs();

        // References to the outer scope `a`
        let (_, bind) = hir.get_dec(topdecs[0]).val().unwrap();
        let a_outer_key = (
            hir.get_pat(bind[0].pat).vid().unwrap().1 .0.clone(),
            DefLoc::Dec(topdecs[0]),
        );
        let mut a_outer_refs = vec![];

        // References to the `a` scoped to the `let .. in .. end` expr
        let mut a_inner_refs = vec![];
        let (_, valbinds) = hir.get_dec(topdecs[1]).val().unwrap();
        let (dec, expr) = hir.get_expr(valbinds[0].expr).let_expr().unwrap();
        a_inner_refs.push(ValUsage::Expr(expr));
        let a_inner_key = (a_outer_key.0.clone(), DefLoc::Dec(dec));
        let (_, valbinds) = hir.get_dec(dec).val().unwrap();
        a_outer_refs.push(ValUsage::Expr(valbinds[0].expr));

        let (_, valbinds) = hir.get_dec(topdecs[2]).val().unwrap();
        a_outer_refs.push(ValUsage::Expr(valbinds[0].expr));

        let a_outer_key = (a_outer_key.0.clone(), a_outer_key.1);

        let ctx = UsageCtxt::new(&hir);
        assert_eq!(
            ctx.lookup_value(&a_outer_key),
            Some(a_outer_refs.as_slice())
        );
        assert_eq!(
            ctx.lookup_value(&a_inner_key),
            Some(a_inner_refs.as_slice())
        );
    }

    #[test]
    fn tycon_refs() {
        use crate::usages::{TyUsage, UsageCtxt};
        use pomelo_hir::{DefLoc, FileArena, LongTyCon};
        use pomelo_parse::Parser;

        let src = r#"
        type a = int;
        type b = int list;
        datatype 'a option = None | Some of 'a;

        type optint = a option;
        type optlistint = b option;

        val c: a = 0: a;
    "#;

        let ast = Parser::new(src).parse();
        let (hir, errs) = pomelo_hir::lower_ast_to_hir(ast);

        for e in errs.iter() {
            println!("{e}");
        }

        assert!(errs.is_empty());
        let topdecs = hir.topdecs();

        let a_tycon = hir.get_dec(topdecs[0]).ty().unwrap()[0].tycon;
        let a_dec = (LongTyCon::from(a_tycon), DefLoc::Dec(topdecs[0]));
        let mut a_refs = vec![];
        let b_tycon = hir.get_dec(topdecs[1]).ty().unwrap()[0].tycon;
        let b_dec = (LongTyCon::from(b_tycon), DefLoc::Dec(topdecs[1]));
        let mut b_refs = vec![];
        let option_tycon = hir.get_dec(topdecs[2]).datatype().unwrap()[0].tycon;
        let option_dec = (LongTyCon::from(option_tycon), DefLoc::Dec(topdecs[2]));
        let mut option_refs = vec![];

        let a_option_bind = &hir.get_dec(topdecs[3]).ty().unwrap()[0];
        let (constys, _) = hir.get_ty(a_option_bind.ty).cons().unwrap();
        a_refs.push(TyUsage::Ty(constys[0]));
        option_refs.push(TyUsage::Ty(a_option_bind.ty));

        let b_option_bind = &hir.get_dec(topdecs[4]).ty().unwrap()[0];
        let (constys, _) = hir.get_ty(b_option_bind.ty).cons().unwrap();
        b_refs.push(TyUsage::Ty(constys[0]));
        option_refs.push(TyUsage::Ty(b_option_bind.ty));

        let (_, c_valbind) = hir.get_dec(topdecs[5]).val().unwrap();
        let a_ref = hir.get_pat(c_valbind[0].pat).typed().unwrap().1;
        a_refs.push(TyUsage::Ty(a_ref));
        let a_ref = hir.get_expr(c_valbind[0].expr).typed().unwrap().1;
        a_refs.push(TyUsage::Ty(a_ref));

        let ctx = UsageCtxt::new(&hir);
        assert_eq!(ctx.lookup_tycon(&a_dec), Some(a_refs.as_slice()));
        assert_eq!(ctx.lookup_tycon(&b_dec), Some(b_refs.as_slice()));
        assert_eq!(ctx.lookup_tycon(&option_dec), Some(option_refs.as_slice()));
    }

    #[test]
    fn data_constructor_refs() {
        use crate::usages::{UsageCtxt, ValUsage};
        use pomelo_hir::{DefLoc, FileArena};
        use pomelo_parse::Parser;

        let src = r#"
        datatype 'a option = None | Some of 'a;
        val one_if_some = fn None => None
                    | Some _ => Some 1;
    "#;

        let ast = Parser::new(src).parse();
        let (hir, errs) = pomelo_hir::lower_ast_to_hir(ast);

        for e in errs.iter() {
            println!("{e}");
        }

        assert!(errs.is_empty());
        let topdecs = hir.topdecs();

        let option_dec = DefLoc::Dec(topdecs[0]);
        let none_dec = hir.get_dec(topdecs[0]).datatype().unwrap()[0].conbinds[0].vid;
        let some_dec = hir.get_dec(topdecs[0]).datatype().unwrap()[0].conbinds[1].vid;

        let mut none_refs = vec![];
        let mut some_refs = vec![];

        let (_, map_bind) = hir.get_dec(topdecs[1]).val().unwrap();
        let fn_expr = hir.get_expr(map_bind[0].expr).fn_expr().unwrap();
        none_refs.push(ValUsage::Pat(fn_expr[0].pat));
        none_refs.push(ValUsage::Expr(fn_expr[0].expr));
        some_refs.push(ValUsage::Pat(fn_expr[1].pat));

        let (expr, _) = hir.get_expr(fn_expr[1].expr).application().unwrap();
        some_refs.push(ValUsage::Expr(expr));

        let ctx = UsageCtxt::new(&hir);
        assert_eq!(
            ctx.lookup_value(&(none_dec.into(), option_dec)),
            Some(none_refs.as_slice())
        );
        assert_eq!(
            ctx.lookup_value(&(some_dec.into(), option_dec)),
            Some(some_refs.as_slice())
        );
    }
}
