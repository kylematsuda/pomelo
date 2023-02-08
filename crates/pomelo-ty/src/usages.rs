//! Run a pass over the HIR to collect all references to each definition.

use std::collections::HashMap;

use pomelo_hir as hir;
use pomelo_hir::{arena::Idx, DefLoc, FileArena};

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
#[derive(Debug, Clone)]
pub struct UsageCtxt<'a> {
    hir: &'a hir::File,
    val_map: HashMap<DefLoc, Vec<ValUsage>>,
    ty_map: HashMap<DefLoc, Vec<TyUsage>>,
    exception_map: HashMap<Idx<hir::Dec>, Vec<Idx<hir::Expr>>>,
}

impl<'a> UsageCtxt<'a> {
    pub fn new(hir: &'a hir::File) -> Self {
        Self {
            hir,
            val_map: HashMap::new(),
            ty_map: HashMap::new(),
            exception_map: HashMap::new(),
        }
    }

    pub fn register_value(&mut self, usage: ValUsage, def: DefLoc) {
        self.val_map.entry(def).or_insert(Vec::new()).push(usage);
    }

    pub fn register_tycon(&mut self, usage: TyUsage, def: DefLoc) {
        self.ty_map.entry(def).or_insert(Vec::new()).push(usage);
    }

    pub fn register_exception(&mut self, usage: Idx<hir::Expr>, def: Idx<hir::Dec>) {
        self.exception_map
            .entry(def)
            .or_insert(Vec::new())
            .push(usage);
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
            Replication { rhs: (_, loc), .. } => self.register_tycon(TyUsage::Dec(dec), *loc),
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
                hir::ExBind::Assignment { rhs: (_, loc), .. } => {
                    self.register_value(ValUsage::Dec(dec), *loc);
                }
            },
            Local { inner, outer } => {
                self.visit_dec(*inner);
                self.visit_dec(*outer);
            }
            Fixity { vids, .. } => vids
                .iter()
                .for_each(|(_, loc)| self.register_value(ValUsage::Dec(dec), *loc)),
            Missing | Open { .. } => {}
        }
    }

    fn visit_expr(&mut self, expr: Idx<hir::Expr>) {
        let e = self.hir.arenas().get_expr(expr);

        use hir::ExprKind::*;
        match &e.kind {
            Missing | Scon(_) => {}
            Seq { exprs } => exprs.iter().for_each(|e| self.visit_expr(*e)),
            VId {
                longvid: (_, loc), ..
            } => self.register_value(ValUsage::Expr(expr), *loc),
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
                vid: (_, loc),
                rhs,
            } => {
                self.visit_expr(*lhs);
                self.register_value(ValUsage::Expr(expr), *loc);
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
                longvid: (_, loc), ..
            } => {
                if let Some(loc) = loc {
                    self.register_tycon(TyUsage::Pat(pat), *loc);
                }
            }
            Record { rows } => rows.iter().for_each(|row| {
                if let hir::PatRow::Pattern { pat, .. } = row {
                    self.visit_pat(*pat);
                }
            }),
            Constructed {
                longvid: (_, loc),
                pat: cons_pat,
                ..
            } => {
                self.register_tycon(TyUsage::Pat(pat), *loc);
                self.visit_pat(*cons_pat);
            }
            Infix {
                lhs,
                vid: (_, loc),
                rhs,
            } => {
                self.visit_pat(*lhs);
                self.register_value(ValUsage::Pat(pat), *loc);
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
            Constructed {
                tyseq,
                longtycon: (_, loc),
            } => {
                tyseq.iter().for_each(|ty| self.visit_ty(*ty));
                self.register_tycon(TyUsage::Ty(ty), *loc);
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
