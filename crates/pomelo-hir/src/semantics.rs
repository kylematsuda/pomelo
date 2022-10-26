use crate::arena::Idx;
use crate::core::pretty::HirPrettyPrint;
use crate::core::BodyArena;
use crate::core::{Dec, DecKind, Expr, ExprKind, MRule, Pat, PatKind, PatRow, Type};
use crate::identifiers::LongVId;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BodySymbols<'hir, A> {
    map: HashMap<LongVId, BodyRefs>,
    arena: &'hir A,
}

impl<'hir, A: BodyArena> BodySymbols<'hir, A> {
    pub fn new(arena: &'hir A) -> Self {
        Self {
            map: HashMap::new(),
            arena,
        }
    }

    pub fn add_def(&mut self, name: LongVId, kind: NameKind, dec: Idx<Dec>) -> Result<(), ()> {
        let bodyrefs = self
            .map
            .entry(name)
            .or_insert(BodyRefs::default_from_kind(kind));

        if bodyrefs.def.is_some() {
            Err(())
        } else {
            bodyrefs.def = Some(dec);
            Ok(())
        }
    }

    pub fn add_ref(&mut self, name: LongVId, kind: NameKind, hir_ref: HirRef) -> Result<(), ()> {
        self.map
            .entry(name)
            .or_insert(BodyRefs::default_from_kind(kind))
            .refs
            .push(hir_ref);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BodyRefs {
    def: Option<Idx<Dec>>,
    kind: NameKind,
    refs: Vec<HirRef>,
}

impl BodyRefs {
    pub fn default_from_kind(kind: NameKind) -> Self {
        Self {
            def: None,
            kind,
            refs: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameKind {
    Value,
    TyVar,
    TyCon,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirRef {
    Pat(Idx<Pat>),
    Expr(Idx<Expr>),
    Dec(Idx<Dec>),
    Ty(Idx<Type>),
}

fn pretty_span(span: Option<(usize, usize)>) -> String {
    span.map(|(start, end)| format!("({},{})", start, end))
        .unwrap_or("Missing".to_owned())
}

impl HirPrettyPrint for HirRef {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        match self {
            Self::Pat(p) => {
                let ast = &arena.get_pat(*p).ast_id;
                format!(
                    "Pat({})@{}",
                    p.pretty(arena),
                    pretty_span(ast.as_span(arena))
                )
            }
            Self::Expr(e) => {
                let ast = &arena.get_expr(*e).ast_id;
                format!(
                    "Expr({})@{}",
                    e.pretty(arena),
                    pretty_span(ast.as_span(arena))
                )
            }
            Self::Dec(d) => {
                let ast = &arena.get_dec(*d).ast_id;
                format!(
                    "Dec({})@{}",
                    d.pretty(arena),
                    pretty_span(ast.as_span(arena))
                )
            }
            Self::Ty(t) => {
                let ast = &arena.get_ty(*t).ast_id;
                format!(
                    "Ty({})@{}",
                    t.pretty(arena),
                    pretty_span(ast.as_span(arena))
                )
            }
        }
    }
}

impl<'hir, A: BodyArena> std::fmt::Display for BodySymbols<'hir, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "BodySymbols:")?;
        for (k, v) in self.map.iter() {
            writeln!(f, "\t{}", k.pretty(self.arena))?;
            writeln!(
                f,
                "\t\tdec: {}",
                v.def
                    .map(|d| {
                        let ast = &self.arena.get_dec(d).ast_id;
                        format!(
                            "Dec@{}",
                            pretty_span(ast.as_span(self.arena))
                        )
                    })
                    .unwrap_or("None".to_owned())
            )?;
            writeln!(f, "\t\trefs:")?;

            for r in &v.refs {
                writeln!(f, "\t\t\t{}", r.pretty(self.arena))?;
            }
        }
        Ok(())
    }
}

impl<'hir, A: BodyArena> BodySymbols<'hir, A> {
    // Only handle value names right now, don't worry about types yet
    pub fn add_dec(&mut self, index: Idx<Dec>) -> Result<(), ()> {
        let dec = self.arena.get_dec(index);
        match &dec.kind {
            DecKind::Seq { decs } => {
                for dec in decs.into_iter() {
                    self.add_dec(*dec)?;
                }
            }
            DecKind::Val { pat, expr, .. } => {
                self.add_pat(*pat, Some(index))?;
                self.add_expr(*expr)?;
            }
            DecKind::Abstype { dec, .. } => {
                self.add_dec(*dec)?;
            }
            DecKind::Local { inner, outer } => {
                self.add_dec(*inner)?;
                self.add_dec(*outer)?;
            }
            DecKind::Fixity { vids, .. } => {
                for vid in vids.into_iter() {
                    self.add_ref(LongVId::from_vid(*vid), NameKind::Value, HirRef::Dec(index))?;
                }
            }
            DecKind::Missing
            | DecKind::Ty { .. }
            | DecKind::Exception { .. }
            | DecKind::Open { .. }
            | DecKind::Datatype { .. }
            | DecKind::Replication { .. } => {}
        }
        Ok(())
    }

    pub fn add_expr(&mut self, index: Idx<Expr>) -> Result<(), ()> {
        let expr = self.arena.get_expr(index);
        match &expr.kind {
            ExprKind::Seq { exprs } => {
                for e in exprs.into_iter() {
                    self.add_expr(*e)?;
                }
            }
            ExprKind::VId { longvid, .. } => {
                self.add_ref(longvid.clone(), NameKind::Value, HirRef::Expr(index))?;
            }
            ExprKind::Record { rows } => {
                for row in rows.into_iter() {
                    self.add_expr(row.expr)?;
                }
            }
            ExprKind::Let { dec, expr } => {
                self.add_dec(*dec)?;
                self.add_expr(*expr)?;
            }
            ExprKind::Application { expr, param } => {
                self.add_expr(*expr)?;
                self.add_expr(*param)?;
            }
            ExprKind::Infix { lhs, vid, rhs } => {
                self.add_expr(*lhs)?;
                self.add_ref(
                    LongVId::from_vid(*vid),
                    NameKind::Value,
                    HirRef::Expr(index),
                )?;
                self.add_expr(*rhs)?;
            }
            ExprKind::Typed { expr, .. } => {
                self.add_expr(*expr)?;
            }
            ExprKind::Handle { expr, match_ } => {
                self.add_expr(*expr)?;
                self.add_match(match_)?;
            }
            ExprKind::Raise { expr } => {
                self.add_expr(*expr)?;
            }
            ExprKind::Fn { match_ } => {
                self.add_match(match_)?;
            }
            ExprKind::Missing | ExprKind::Scon(_) => {}
        }
        Ok(())
    }

    pub fn add_pat(&mut self, index: Idx<Pat>, def: Option<Idx<Dec>>) -> Result<(), ()> {
        let pat = self.arena.get_pat(index);
        match &pat.kind {
            PatKind::Missing | PatKind::Scon(_) | PatKind::Wildcard => {}
            PatKind::VId { longvid, .. } => {
                if let Some(def) = def {
                    self.add_def(longvid.clone(), NameKind::Value, def)?;
                }
                self.add_ref(longvid.clone(), NameKind::Value, HirRef::Pat(index))?;
            }
            PatKind::Typed { pat, .. } => {
                self.add_pat(*pat, def)?;
            }
            PatKind::Record { rows } => {
                for row in rows.into_iter() {
                    if let PatRow::Pattern { pat, .. } = row {
                        self.add_pat(*pat, def)?;
                    }
                }
            }
            PatKind::Infix { lhs, vid, rhs } => {
                self.add_pat(*lhs, def)?;
                self.add_ref(LongVId::from_vid(*vid), NameKind::Value, HirRef::Pat(index))?;
                self.add_pat(*rhs, def)?;
            }
            PatKind::Layered { vid, pat, .. } => {
                self.add_ref(LongVId::from_vid(*vid), NameKind::Value, HirRef::Pat(index))?;
                self.add_pat(*pat, def)?;
            }
            PatKind::Constructed { longvid, pat, .. } => {
                self.add_ref(longvid.clone(), NameKind::Value, HirRef::Pat(index))?;
                self.add_pat(*pat, def)?;
            }
        }
        Ok(())
    }

    pub fn add_match(&mut self, match_: &[MRule]) -> Result<(), ()> {
        for m in match_ {
            self.add_pat(m.pat, None)?;
            self.add_expr(m.expr)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::core::TopDecBody;
    use crate::semantics::BodySymbols;
    use pomelo_parse::{ast, passes::apply_passes, AstNode, Parser};

    fn check(src: &str) {
        let parser = Parser::new(src);
        let tree = apply_passes(parser.parse_dec());

        for e in tree.errors() {
            eprintln!("{:?}", e);
        }
        eprintln!("{}", tree);

        let node = ast::Dec::cast(tree.syntax()).unwrap();
        let body = TopDecBody::from_syntax(node);
        let mut symbols = BodySymbols::new(body.arena());
        symbols.add_dec(body.dec()).unwrap();

        eprintln!("{}", symbols);
    }

    #[test]
    fn test_symbol_map_basic() {
        check(
            "val x =
                let 
                    val a = 1
                    val b = 2
                in 
                    a + b 
                end 
            ",
        )
    }
}
