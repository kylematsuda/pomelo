//! Pretty print HIR nodes.
use crate::arena::Idx;
use crate::{
    ConBind, DataBind, Dec, DecKind, ExBind, ExpRow, Expr, ExprKind, FileArena, Fixity, Label,
    LongStrId, LongTyCon, LongVId, MRule, Name, NameInterner, Pat, PatKind, PatRow, Scon, StrId,
    Ty, TyCon, TyKind, TyRow, TyVar, TypBind, VId, ValBind,
};

const MISSING: &str = "*missing*";

/// Interface for printing HIR nodes.
pub(crate) trait HirPrettyPrint {
    fn pretty<A: FileArena>(&self, arena: &A) -> String;
}

fn op_str(op: bool) -> &'static str {
    if op {
        "op"
    } else {
        ""
    }
}

fn boxed_seq<'a, N: HirPrettyPrint + 'a, A: FileArena>(
    nodes: impl Iterator<Item = &'a N>,
    arena: &A,
) -> Vec<String> {
    nodes.map(|n| n.pretty(arena)).collect()
}

impl HirPrettyPrint for Name {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match self {
            Self::String(index) => <A as NameInterner>::get(arena, *index).to_owned(),
            Self::Generated(n) => format!("_temp{n}"),
            Self::BuiltIn(b) => b.as_str().to_owned(),
        }
    }
}

impl HirPrettyPrint for Dec {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match &self.kind {
            DecKind::Missing => MISSING.to_owned(),
            DecKind::Seq { decs } => boxed_seq(decs.iter(), arena).join("; "),
            DecKind::Val { tyvarseq, bindings } => {
                format!(
                    "val {}{}",
                    boxed_seq(tyvarseq.iter(), arena).join(" "),
                    boxed_seq(bindings.iter(), arena).join(" and "),
                )
            }
            DecKind::Ty { bindings } => {
                format!("type {}", boxed_seq(bindings.iter(), arena).join(" and "),)
            }
            DecKind::Datatype { databinds } => {
                format!(
                    "datatype {}",
                    boxed_seq(databinds.iter(), arena).join(" and ")
                )
            }
            DecKind::Replication { lhs, rhs } => {
                format!(
                    "datatype {} = datatype {}",
                    lhs.pretty(arena),
                    rhs.0.pretty(arena)
                )
            }
            DecKind::Abstype { databinds, dec } => {
                format!(
                    "abstype {} with {} end",
                    boxed_seq(databinds.iter(), arena).join(" and "),
                    dec.pretty(arena),
                )
            }
            DecKind::Local { inner, outer } => {
                format!(
                    "local {} in {} end",
                    inner.pretty(arena),
                    outer.pretty(arena),
                )
            }
            DecKind::Exception { exbind } => {
                format!("exception {}", exbind.pretty(arena))
            }
            DecKind::Open { longstrids } => {
                format!("open {}", boxed_seq(longstrids.iter(), arena).join(" "))
            }
            DecKind::Fixity { fixity, vids } => {
                let (dec_str, val_str) = match fixity {
                    Fixity::Left(v) => {
                        let s = v.unwrap_or(0).to_string();
                        ("infix", s)
                    }
                    Fixity::Right(v) => {
                        let s = v.unwrap_or(0).to_string();
                        ("infixr", s)
                    }
                    Fixity::Nonfix => ("nonfix", "".to_owned()),
                };
                format!(
                    "{} {}{}{}",
                    dec_str,
                    val_str,
                    if val_str.is_empty() { "" } else { " " },
                    boxed_seq(vids.iter().map(|v| &v.0), arena).join(" ")
                )
            }
        }
    }
}

impl HirPrettyPrint for Idx<Dec> {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        arena.get_dec(*self).pretty(arena)
    }
}

impl HirPrettyPrint for ValBind {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        format!(
            "{}{} = {}",
            if self.rec { "rec " } else { "" },
            self.pat.pretty(arena),
            self.expr.pretty(arena)
        )
    }
}

impl HirPrettyPrint for TypBind {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        format!(
            "{} {} = {}",
            boxed_seq(self.tyvarseq.iter(), arena).join(" "),
            self.tycon.pretty(arena),
            self.ty.pretty(arena)
        )
    }
}

impl HirPrettyPrint for DataBind {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        let mut tyvars = boxed_seq(self.tyvarseq.iter(), arena).join(" ");
        format!(
            "{}{} = {}",
            if !tyvars.is_empty() {
                tyvars.push(' ');
                &tyvars
            } else {
                ""
            },
            self.tycon.pretty(arena),
            boxed_seq(self.conbinds.iter(), arena).join(" | ")
        )
    }
}

impl HirPrettyPrint for ConBind {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        let ty = self
            .ty
            .map(|t| t.pretty(arena))
            .map(|ty| format!(" of {ty}"))
            .unwrap_or_else(String::new);

        format!("{}{}{ty}", op_str(self.op), self.vid.pretty(arena))
    }
}

impl HirPrettyPrint for ExBind {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match self {
            ExBind::Name { op, vid, ty } => {
                let ty = ty
                    .map(|t| t.0.pretty(arena))
                    .map(|ty| format!("of {ty}"))
                    .unwrap_or_else(String::new);
                format!("{}{} {ty}", op_str(*op), vid.pretty(arena))
            }
            ExBind::Assignment {
                op_lhs,
                lhs,
                op_rhs,
                rhs,
            } => {
                format!(
                    "{}{} = {}{}",
                    op_str(*op_lhs),
                    lhs.pretty(arena),
                    op_str(*op_rhs),
                    rhs.0.pretty(arena)
                )
            }
        }
    }
}

impl HirPrettyPrint for Pat {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match &self.kind {
            PatKind::Missing => MISSING.to_owned(),
            PatKind::Wildcard => "_".to_owned(),
            PatKind::Scon(s) => s.pretty(arena),
            PatKind::VId { op, longvid } => {
                format!("{}{}", op_str(*op), longvid.0.pretty(arena))
            }
            PatKind::Record { rows } => {
                format!("{{ {} }}", boxed_seq(rows.iter(), arena).join(", "))
            }
            PatKind::Typed { pat, ty } => {
                format!("{} : {}", pat.pretty(arena), ty.pretty(arena))
            }
            PatKind::Constructed { op, longvid, pat } => {
                format!(
                    "{}{} {}",
                    op_str(*op),
                    longvid.0.pretty(arena),
                    pat.pretty(arena)
                )
            }
            PatKind::Infix { lhs, vid, rhs } => {
                format!(
                    "({} {} {})",
                    lhs.pretty(arena),
                    vid.0.pretty(arena),
                    rhs.pretty(arena)
                )
            }
            PatKind::Layered { op, vid, ty, pat } => {
                let ty = ty
                    .map(|t| format!(" : {}", t.pretty(arena)))
                    .unwrap_or_else(|| "".to_owned());

                format!(
                    "{}{}{} as {}",
                    op_str(*op),
                    vid.pretty(arena),
                    ty,
                    pat.pretty(arena)
                )
            }
        }
    }
}

impl HirPrettyPrint for Idx<Pat> {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        arena.get_pat(*self).pretty(arena)
    }
}

impl HirPrettyPrint for PatRow {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match &self {
            PatRow::Wildcard => "..".to_owned(),
            PatRow::Pattern { label, pat } => {
                format!("{}={}", label.pretty(arena), pat.pretty(arena))
            }
        }
    }
}

impl HirPrettyPrint for Ty {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match &self.kind {
            TyKind::Missing => MISSING.to_owned(),
            TyKind::Var(v) => v.pretty(arena),
            TyKind::Record { tyrows } => {
                format!("{{ {} }}", boxed_seq(tyrows.iter(), arena).join(", "))
            }
            TyKind::Function { domain, range } => {
                format!("{} -> {}", domain.pretty(arena), range.pretty(arena))
            }
            TyKind::Constructed { tyseq, longtycon } => {
                let mut tys = boxed_seq(tyseq.iter(), arena);
                tys.push(longtycon.0.pretty(arena));
                tys.join(" ")
            }
        }
    }
}

impl HirPrettyPrint for Idx<Ty> {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        arena.get_ty(*self).pretty(arena)
    }
}

impl HirPrettyPrint for TyRow {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        format!("{}:{}", self.label.pretty(arena), self.ty.pretty(arena))
    }
}

impl HirPrettyPrint for Expr {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match &self.kind {
            ExprKind::Missing => MISSING.to_owned(),
            ExprKind::Seq { exprs } => format!("({})", boxed_seq(exprs.iter(), arena).join("; ")),
            ExprKind::Scon(s) => s.pretty(arena),
            ExprKind::VId { op, longvid } => {
                format!("{}{}", op_str(*op), longvid.0.pretty(arena))
            }
            ExprKind::Record { rows } => {
                format!("{{ {} }}", boxed_seq(rows.iter(), arena).join(", "))
            }
            ExprKind::Fn { match_ } => {
                let matches = boxed_seq(match_.iter(), arena).join(" | ");
                format!("(fn {matches})")
            }
            ExprKind::Let { dec, expr } => {
                format!("let {} in {} end", dec.pretty(arena), expr.pretty(arena))
            }
            ExprKind::InfixOrApp { exprs } => boxed_seq(exprs.iter(), arena).join(" "),
            ExprKind::Infix { lhs, vid, rhs } => {
                format!(
                    "({} {} {})",
                    lhs.pretty(arena),
                    vid.0.pretty(arena),
                    rhs.pretty(arena)
                )
            }
            ExprKind::Application { expr, param } => {
                format!("{} {}", expr.pretty(arena), param.pretty(arena))
            }
            ExprKind::Typed { expr, ty } => {
                format!("{} : {}", expr.pretty(arena), ty.pretty(arena))
            }
            ExprKind::Handle { expr, match_ } => {
                format!(
                    "{} handle {}",
                    expr.pretty(arena),
                    boxed_seq(match_.iter(), arena).join(" | ")
                )
            }
            ExprKind::Raise { expr } => {
                format!("raise {}", expr.pretty(arena))
            }
        }
    }
}

impl HirPrettyPrint for Idx<Expr> {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        arena.get_expr(*self).pretty(arena)
    }
}

impl HirPrettyPrint for ExpRow {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        format!("{}={}", self.label.pretty(arena), self.expr.pretty(arena))
    }
}

impl HirPrettyPrint for MRule {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        format!("{} => {}", self.pat.pretty(arena), self.expr.pretty(arena))
    }
}

impl HirPrettyPrint for Scon {
    fn pretty<A: FileArena>(&self, _arena: &A) -> String {
        match self {
            Scon::Missing => MISSING.to_owned(),
            Scon::Int(i) => i.to_string(),
            Scon::Word(w) => w.to_string(),
            Scon::Real(f) => f.to_string(),
            Scon::String(s) => s.clone(),
            Scon::Char(c) => c.to_string(),
        }
    }
}

impl HirPrettyPrint for Label {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match self {
            Label::Missing => MISSING.to_owned(),
            Label::Numeric(i) => i.to_string(),
            Label::Named(n) => n.pretty(arena),
        }
    }
}

impl HirPrettyPrint for VId {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match self {
            VId::Missing => MISSING.to_owned(),
            VId::Name(n) => n.pretty(arena),
        }
    }
}

impl HirPrettyPrint for LongVId {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        let mut s = boxed_seq(self.strids.iter(), arena);
        s.push(self.vid.pretty(arena));
        s.join(".")
    }
}

impl HirPrettyPrint for StrId {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match self {
            StrId::Missing => MISSING.to_owned(),
            StrId::Name(n) => n.pretty(arena),
        }
    }
}

impl HirPrettyPrint for LongStrId {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        let mut s = boxed_seq(self.strid_path.iter(), arena);
        s.push(self.strid.pretty(arena));
        s.join(".")
    }
}

impl HirPrettyPrint for TyVar {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match self {
            TyVar::Missing => MISSING.to_owned(),
            TyVar::Name(n) => n.pretty(arena),
        }
    }
}

impl HirPrettyPrint for TyCon {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        match self {
            TyCon::Missing => MISSING.to_owned(),
            TyCon::Name(n) => n.pretty(arena),
        }
    }
}

impl HirPrettyPrint for LongTyCon {
    fn pretty<A: FileArena>(&self, arena: &A) -> String {
        let mut s = boxed_seq(self.strids.iter(), arena);
        s.push(self.tycon.pretty(arena));
        s.join(".")
    }
}
