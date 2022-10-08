use crate::arena::Idx;
use crate::core::{
    BodyArena, ConBind, DataBind, Dec, DecKind, ExBind, ExpRow, Expr, ExprKind, Fixity, MRule, Pat,
    PatKind, PatRow, Scon, TyKind, TyRow, Type,
};
use crate::identifiers::{Label, LongStrId, LongTyCon, LongVId, StrId, TyCon, TyVar, VId};

const MISSING: &str = "*missing*";

fn op_str(op: bool) -> &'static str {
    if op {
        "op"
    } else {
        ""
    }
}

fn boxed_seq<N: HirPrettyPrint, A: BodyArena>(nodes: &Box<[N]>, arena: &A, joiner: &str) -> String {
    nodes
        .into_iter()
        .map(|n| n.pretty(arena))
        .collect::<Vec<_>>()
        .join(joiner)
}

pub(crate) trait HirPrettyPrint {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String;
}

impl HirPrettyPrint for Dec {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        match &self.kind {
            DecKind::Missing => MISSING.to_owned(),
            DecKind::Val {
                rec,
                tyvarseq,
                pat,
                expr,
            } => {
                let rec = if *rec { "rec " } else { "" };
                format!(
                    "val {} {}{} = {}",
                    boxed_seq(tyvarseq, arena, " "),
                    rec,
                    pat.pretty(arena),
                    expr.pretty(arena)
                )
            }
            DecKind::Ty {
                tyvarseq,
                tycon,
                ty,
            } => {
                format!(
                    "type {} {} = {}",
                    boxed_seq(tyvarseq, arena, " "),
                    tycon.pretty(arena),
                    ty.pretty(arena),
                )
            }
            DecKind::Datatype { databind } => {
                format!("datatype {}", databind.pretty(arena))
            }
            DecKind::Replication { lhs, rhs } => {
                format!(
                    "datatype {} = datatype {}",
                    lhs.pretty(arena),
                    rhs.pretty(arena)
                )
            }
            DecKind::Abstype { databinds, decs } => {
                format!(
                    "abstype {} with {} end",
                    boxed_seq(databinds, arena, " and "),
                    boxed_seq(decs, arena, "; "),
                )
            }
            DecKind::Local {
                inner_decs,
                outer_decs,
            } => {
                format!(
                    "local {} in {} end",
                    boxed_seq(inner_decs, arena, "; "),
                    boxed_seq(outer_decs, arena, "; ")
                )
            }
            DecKind::Exception { exbind } => {
                format!("exception {}", exbind.pretty(arena))
            }
            DecKind::Open { longstrids } => {
                format!("open {}", boxed_seq(longstrids, arena, " "))
            }
            DecKind::Fixity { fixity, vids } => {
                let (dec_str, val_str) = match fixity {
                    Fixity::Left(v) => {
                        let mut s = v.unwrap_or(0).to_string();
                        s.push(' ');
                        ("infix", s)
                    }
                    Fixity::Right(v) => {
                        let mut s = v.unwrap_or(0).to_string();
                        s.push(' ');
                        ("infixr", s)
                    }
                    Fixity::Nonfix => ("nonfix", "".to_owned()),
                };
                format!("{} {} {}", dec_str, val_str, boxed_seq(vids, arena, " "))
            }
        }
    }
}

impl HirPrettyPrint for Idx<Dec> {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        arena
            .get_dec(*self)
            .map(|d| d.pretty(arena))
            .expect("index is valid")
    }
}

impl HirPrettyPrint for DataBind {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        format!(
            "{} {} = {}",
            boxed_seq(&self.tyvarseq, arena, " "),
            self.tycon.pretty(arena),
            boxed_seq(&self.conbinds, arena, " | ")
        )
    }
}

impl HirPrettyPrint for ConBind {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        let ty = self
            .ty
            .map(|t| t.pretty(arena))
            .map(|ty| format!("of {}", ty))
            .unwrap_or_else(|| "".to_owned());

        format!("{}{} {}", op_str(self.op), self.vid.pretty(arena), ty)
    }
}

impl HirPrettyPrint for ExBind {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        match self {
            ExBind::Name { op, vid, ty } => {
                let ty = ty
                    .map(|t| t.pretty(arena))
                    .map(|ty| format!("of {}", ty))
                    .unwrap_or_else(|| "".to_string());
                format!("{}{} {}", op_str(*op), vid.pretty(arena), ty)
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
                    rhs.pretty(arena)
                )
            }
        }
    }
}

impl HirPrettyPrint for Pat {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        match &self.kind {
            PatKind::Missing => MISSING.to_owned(),
            PatKind::Wildcard => "_".to_owned(),
            PatKind::Nil => "nil".to_owned(),
            PatKind::Scon(s) => s.pretty(arena),
            PatKind::VId { op, longvid } => {
                format!("{}{}", op_str(*op), longvid.pretty(arena))
            }
            PatKind::Record { rows } => {
                let mut s = String::new();
                s.push_str("{ ");
                s.push_str(&boxed_seq(rows, arena, ", "));
                s.push_str(" }");
                s
            }
            PatKind::Typed { pat, ty } => {
                format!("{} : {}", pat.pretty(arena), ty.pretty(arena))
            }
            PatKind::Constructed { op, longvid, pat } => {
                format!(
                    "{}{} {}",
                    op_str(*op),
                    longvid.pretty(arena),
                    pat.pretty(arena)
                )
            }
            PatKind::Infix { lhs, vid, rhs } => {
                format!(
                    "{} {} {}",
                    lhs.pretty(arena),
                    vid.pretty(arena),
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
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        arena
            .get_pat(*self)
            .map(|p| p.pretty(arena))
            .expect("index is valid")
    }
}

impl HirPrettyPrint for PatRow {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        match &self {
            PatRow::Wildcard => "..".to_owned(),
            PatRow::Pattern { label, pat } => {
                format!("{}: {}", label.pretty(arena), pat.pretty(arena))
            }
        }
    }
}

impl HirPrettyPrint for Type {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        match &self.kind {
            TyKind::Missing => MISSING.to_owned(),
            TyKind::Var(v) => v.pretty(arena),
            TyKind::Record { tyrows } => {
                let mut s = String::new();
                s.push_str("{ ");
                s.push_str(&boxed_seq(tyrows, arena, ", "));
                s.push_str(" }");
                s
            }
            TyKind::Function { domain, range } => {
                format!("{} -> {}", domain.pretty(arena), range.pretty(arena))
            }
            TyKind::Constructed { tyseq, longtycon } => {
                let mut s = String::new();
                s.push_str(&boxed_seq(tyseq, arena, " "));
                s.push(' ');
                s.push_str(&longtycon.pretty(arena));
                s
            }
        }
    }
}

impl HirPrettyPrint for Idx<Type> {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        arena
            .get_ty(*self)
            .map(|t| t.pretty(arena))
            .expect("index is valid")
    }
}

impl HirPrettyPrint for TyRow {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        format!("{}: {}", self.label.pretty(arena), self.ty.pretty(arena))
    }
}

impl HirPrettyPrint for Expr {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        match &self.kind {
            ExprKind::Missing => MISSING.to_owned(),
            ExprKind::Nil => "nil".to_owned(),
            ExprKind::Scon(s) => s.pretty(arena),
            ExprKind::VId { op, longvid } => {
                format!("{}{}", op_str(*op), longvid.pretty(arena))
            }
            ExprKind::Record { rows } => {
                let mut s = String::new();
                s.push_str("{ ");
                s.push_str(&boxed_seq(rows, arena, ", "));
                s.push_str(" }");
                s
            }
            ExprKind::Fn { match_ } => {
                let matches = boxed_seq(match_, arena, " | ");
                format!("fn {}", matches)
            }
            ExprKind::Let { decs, expr } => {
                format!(
                    "let {} in {} end",
                    boxed_seq(decs, arena, "; "),
                    expr.pretty(arena)
                )
            }
            ExprKind::Infix { lhs, vid, rhs } => {
                format!(
                    "{} {} {}",
                    lhs.pretty(arena),
                    vid.pretty(arena),
                    rhs.pretty(arena)
                )
            }
            ExprKind::Application { expr, param } => {
                format!("{} {}", expr.pretty(arena), param.pretty(arena))
            }
            ExprKind::Typed { expr, ty } => {
                format!("{} : {}", expr.pretty(arena), ty.pretty(arena))
            }
            ExprKind::Handle { expr } => {
                format!("handle {}", expr.pretty(arena))
            }
            ExprKind::Raise { expr } => {
                format!("raise {}", expr.pretty(arena))
            }
        }
    }
}

impl HirPrettyPrint for Idx<Expr> {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        arena
            .get_expr(*self)
            .map(|e| e.pretty(arena))
            .expect("index is valid")
    }
}

impl HirPrettyPrint for ExpRow {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        format!("{}: {}", self.lab.pretty(arena), self.expr.pretty(arena))
    }
}

impl HirPrettyPrint for MRule {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        format!("{} => {}", self.pat.pretty(arena), self.expr.pretty(arena))
    }
}

impl HirPrettyPrint for Scon {
    fn pretty<A: BodyArena>(&self, _arena: &A) -> String {
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
    fn pretty<A: BodyArena>(&self, _arena: &A) -> String {
        match self {
            Label::Missing => MISSING.to_owned(),
            Label::Numeric(i) => i.to_string(),
            Label::Named(n) => n.to_string(),
        }
    }
}

impl HirPrettyPrint for Idx<Label> {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        arena
            .get_label(*self)
            .map(|l| l.pretty(arena))
            .expect("index is valid")
    }
}

impl HirPrettyPrint for VId {
    fn pretty<A: BodyArena>(&self, _arena: &A) -> String {
        match self {
            VId::Missing => MISSING.to_owned(),
            VId::Name(n) => n.to_string(),
        }
    }
}

impl HirPrettyPrint for Idx<VId> {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        arena
            .get_vid(*self)
            .map(|v| v.pretty(arena))
            .expect("index is valid")
    }
}

impl HirPrettyPrint for LongVId {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        let mut s = boxed_seq(&self.strids, arena, ".");
        s.push('.');
        s.push_str(&self.vid.pretty(arena));
        s
    }
}

impl HirPrettyPrint for StrId {
    fn pretty<A: BodyArena>(&self, _arena: &A) -> String {
        match self {
            StrId::Missing => MISSING.to_owned(),
            StrId::Name(n) => n.to_string(),
        }
    }
}

impl HirPrettyPrint for Idx<StrId> {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        arena
            .get_strid(*self)
            .map(|s| s.pretty(arena))
            .expect("index is valid")
    }
}

impl HirPrettyPrint for LongStrId {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        let mut s = boxed_seq(&self.strid_path, arena, ".");
        s.push('.');
        s.push_str(&self.strid.pretty(arena));
        s
    }
}

impl HirPrettyPrint for TyVar {
    fn pretty<A: BodyArena>(&self, _arena: &A) -> String {
        match self {
            TyVar::Missing => MISSING.to_owned(),
            TyVar::Name(n) => n.to_string(),
        }
    }
}

impl HirPrettyPrint for Idx<TyVar> {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        arena
            .get_tyvar(*self)
            .map(|t| t.pretty(arena))
            .expect("index is valid")
    }
}

impl HirPrettyPrint for TyCon {
    fn pretty<A: BodyArena>(&self, _arena: &A) -> String {
        match self {
            TyCon::Missing => MISSING.to_owned(),
            TyCon::Name(n) => n.to_string(),
        }
    }
}

impl HirPrettyPrint for Idx<TyCon> {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        arena
            .get_tycon(*self)
            .map(|t| t.pretty(arena))
            .expect("index is valid")
    }
}

impl HirPrettyPrint for LongTyCon {
    fn pretty<A: BodyArena>(&self, arena: &A) -> String {
        let mut s = boxed_seq(&self.strids, arena, ".");
        s.push('.');
        s.push_str(&self.tycon.pretty(arena));
        s
    }
}
