//! Definition of the hir.
//!
//! This module's contents are exported at the crate root, this is just separated for some internal
//! organization.
use pomelo_parse::ast;

use crate::arena::Idx;
use crate::identifiers::{Label, LongStrId, LongTyCon, LongVId, TyCon, TyVar, VId};
use crate::{AstId, FileArena};

/// Location where an identifier is bound.
///
/// The `Pat` variant is only used for bindings inside of the pattern in a match statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefLoc {
    Dec(Idx<Dec>),
    Pat(Idx<Pat>),
    Builtin,
    Missing,
}

/// HIR declaration node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dec {
    pub kind: DecKind,
    pub ast_id: AstId<ast::Dec>,
}

impl Dec {
    pub fn bound_vids<A: FileArena>(&self, arena: &A) -> Vec<LongVId> {
        self.kind.bound_vids(arena)
    }

    pub fn bound_tycons<A: FileArena>(&self, arena: &A) -> Vec<LongTyCon> {
        self.kind.bound_tycons(arena)
    }
}

/// Kinds of HIR declarations.
///
/// These correspond to the basic forms in Chapter 2 of the Definition, after
/// desugaring the derived forms from Appendix A.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecKind {
    Missing,
    Seq {
        decs: Box<[Idx<Dec>]>,
    },
    Val {
        tyvarseq: Box<[TyVar]>,
        bindings: Box<[ValBind]>,
    },
    Ty {
        bindings: Box<[TypBind]>,
    },
    Datatype {
        databinds: Box<[DataBind]>,
    },
    Replication {
        lhs: TyCon,
        rhs: (LongTyCon, DefLoc),
    },
    Abstype {
        databinds: Box<[DataBind]>,
        dec: Idx<Dec>,
    },
    Exception {
        exbind: ExBind,
    },
    Local {
        inner: Idx<Dec>,
        outer: Idx<Dec>,
    },
    Open {
        longstrids: Box<[LongStrId]>,
    },
    Fixity {
        fixity: Fixity,
        vids: Box<[(VId, DefLoc)]>,
    },
}

impl DecKind {
    pub fn bound_vids<A: FileArena>(&self, arena: &A) -> Vec<LongVId> {
        match self {
            DecKind::Missing
            | DecKind::Ty { .. }
            | DecKind::Replication { .. }
                // Fixity is a weird one, need to figure out how to treat it
            | DecKind::Fixity { .. } => vec![],
            DecKind::Seq { decs } => {
                let mut names = vec![];

                for d in decs.iter() {
                    let d = arena.get_dec(*d).bound_vids(arena);
                    names.extend(d);
                }

                names
            }
            DecKind::Val { bindings, .. } => bindings
                .iter()
                .flat_map(|b| b.bound_vids(arena).into_iter())
                .collect(),
            DecKind::Datatype { databinds } => databinds.iter().flat_map(|d| d.bound_vids().into_iter()).collect(),
            DecKind::Abstype { databinds, dec } => {
                let mut names = databinds
                    .iter()
                    .flat_map(|d| d.bound_vids().into_iter())
                    .collect::<Vec<_>>();
                names.extend(arena.get_dec(*dec).bound_vids(arena));
                names
            }
            DecKind::Exception { exbind } => vec![exbind.bound_vid()],
            DecKind::Local { outer, .. } => arena.get_dec(*outer).bound_vids(arena),
            DecKind::Open { .. } => todo!(),
        }
    }

    pub fn bound_tycons<A: FileArena>(&self, arena: &A) -> Vec<LongTyCon> {
        match self {
            DecKind::Missing
            | DecKind::Val {  .. }
            | DecKind::Exception { .. }
                // Fixity is a weird one, need to figure out how to treat it
            | DecKind::Fixity { .. } => vec![],
            DecKind::Seq { decs } => {
                let mut tycons = vec![];

                for d in decs.iter() {
                    let d = arena.get_dec(*d).bound_tycons(arena);
                    tycons.extend(d);
                }

                tycons
            },
            DecKind::Ty { bindings } => bindings.iter().map(TypBind::bound_tycon).collect(),
            DecKind::Datatype { databinds } => databinds.iter().map(DataBind::bound_tycon).collect(),
            DecKind::Replication { lhs, .. } => vec![LongTyCon::from(*lhs)],
            DecKind::Abstype { databinds, dec } => {
                let mut tycons = databinds
                    .iter()
                    .map(DataBind::bound_tycon)
                    .collect::<Vec<_>>();
                tycons.extend(arena.get_dec(*dec).bound_tycons(arena));
                tycons
            }
            DecKind::Local { outer, .. } => arena.get_dec(*outer).bound_tycons(arena),
            DecKind::Open { .. } => todo!(),
        }
    }
}

/// Binding of the names in a pattern to an expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValBind {
    pub rec: bool,
    pub pat: Idx<Pat>,
    pub expr: Idx<Expr>,
}

impl ValBind {
    pub fn bound_vids<A: FileArena>(&self, arena: &A) -> Vec<LongVId> {
        arena.get_pat(self.pat).bound_vids(arena)
    }
}

/// Binding of a type constructor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypBind {
    pub tyvarseq: Box<[TyVar]>,
    pub tycon: TyCon,
    pub ty: Idx<Ty>,
}

impl TypBind {
    pub fn bound_tycon(&self) -> LongTyCon {
        LongTyCon::from(self.tycon)
    }
}

/// Binding of a new datatype.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataBind {
    pub tyvarseq: Box<[TyVar]>,
    pub tycon: TyCon,
    pub conbinds: Box<[ConBind]>,
}

impl DataBind {
    pub fn bound_vids(&self) -> Vec<LongVId> {
        self.conbinds
            .iter()
            .map(|b| LongVId::from_vid(b.vid))
            .collect()
    }

    pub fn bound_tycon(&self) -> LongTyCon {
        LongTyCon::from(self.tycon)
    }
}

/// Binding of a datatype constructor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConBind {
    pub op: bool,
    pub vid: VId,
    pub ty: Option<Idx<Ty>>,
}

/// Binding an exception.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExBind {
    Name {
        op: bool,
        vid: VId,
        ty: Option<(Idx<Ty>, DefLoc)>,
    },
    Assignment {
        op_lhs: bool,
        lhs: VId,
        op_rhs: bool,
        rhs: (LongVId, DefLoc),
    },
}

impl ExBind {
    pub fn bound_vid(&self) -> LongVId {
        match self {
            Self::Name { vid, .. } => LongVId::from_vid(*vid),
            Self::Assignment { lhs, .. } => LongVId::from_vid(*lhs),
        }
    }
}

/// Type of fixity, including whether it is left- or right-associative and its operator precedence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fixity {
    Left(Option<u8>),
    Right(Option<u8>),
    Nonfix,
}

/// HIR expression node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ast_id: AstId<ast::Expr>,
}

/// Kinds of HIR expressions.
///
/// These correspond to the basic forms in Chapter 2 of the Definition, after
/// desugaring the derived forms from Appendix A.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Missing,
    Scon(Scon),
    Seq {
        exprs: Box<[Idx<Expr>]>,
    },
    VId {
        op: bool,
        longvid: (LongVId, DefLoc),
    },
    Record {
        rows: Box<[ExpRow]>,
    },
    Let {
        dec: Idx<Dec>,
        expr: Idx<Expr>,
    },
    InfixOrApp {
        exprs: Box<[Idx<Expr>]>,
    },
    Application {
        expr: Idx<Expr>,
        param: Idx<Expr>,
    },
    Infix {
        lhs: Idx<Expr>,
        vid: (VId, DefLoc),
        rhs: Idx<Expr>,
    },
    Typed {
        expr: Idx<Expr>,
        ty: Idx<Ty>,
    },
    Handle {
        expr: Idx<Expr>,
        match_: Box<[MRule]>,
    },
    Raise {
        expr: Idx<Expr>,
    },
    Fn {
        match_: Box<[MRule]>,
    },
}

/// HIR constant (literal).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scon {
    Missing,
    Int(i128),
    Word(u128),
    Real(FloatWrapper),
    String(String),
    Char(char),
}

/// Wrapper so we can derive `Eq` for [`Scon`].
///
/// See
/// [`FloatTypeWrapper`](https://github.com/rust-lang/rust-analyzer/blob/master/crates/hir-def/src/expr.rs)
/// from `rust-analyzer`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FloatWrapper(u64);

impl FloatWrapper {
    pub fn new(value: f64) -> Self {
        Self(value.to_bits())
    }
}

impl std::fmt::Display for FloatWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", f64::from_bits(self.0))
    }
}

/// Record entry in an expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpRow {
    pub label: Label,
    pub expr: Idx<Expr>,
}

/// HIR pattern node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pat {
    pub kind: PatKind,
    pub ast_id: AstId<ast::Pat>,
}

impl Pat {
    /// Names bound by this pattern
    pub fn bound_vids<A: FileArena>(&self, arena: &A) -> Vec<LongVId> {
        self.kind.bound_vids(arena)
    }
}

/// Kinds of HIR expressions.
///
/// These correspond to the basic forms in Chapter 2 of the Definition, after
/// desugaring the derived forms from Appendix A.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatKind {
    Missing,
    Wildcard,
    Scon(Scon),
    VId {
        op: bool,
        // Note that `longvid` can bind a new variable, or it could refer to a variant of a
        // datatype! If it's the latter, then we need to know where the variant was defined.
        longvid: (LongVId, Option<DefLoc>),
    },
    Record {
        rows: Box<[PatRow]>,
    },
    Constructed {
        op: bool,
        longvid: (LongVId, DefLoc),
        pat: Idx<Pat>,
    },
    Infix {
        lhs: Idx<Pat>,
        vid: (VId, DefLoc),
        rhs: Idx<Pat>,
    },
    Typed {
        pat: Idx<Pat>,
        ty: Idx<Ty>,
    },
    Layered {
        op: bool,
        vid: VId,
        ty: Option<Idx<Ty>>,
        pat: Idx<Pat>,
    },
}

impl PatKind {
    /// Names bound by this pattern
    pub fn bound_vids<A: FileArena>(&self, arena: &A) -> Vec<LongVId> {
        match &self {
            PatKind::Missing | PatKind::Wildcard | PatKind::Scon(_) => vec![],
            PatKind::VId {
                longvid: (name, def),
                ..
            } => match def {
                None => vec![name.clone()],
                Some(_) => vec![],
            },
            PatKind::Record { rows } => {
                let mut names = vec![];

                for r in rows.iter() {
                    if let PatRow::Pattern { pat, .. } = r {
                        names.extend(arena.get_pat(*pat).bound_vids(arena));
                    }
                }
                names
            }
            PatKind::Constructed { pat, .. } => arena.get_pat(*pat).bound_vids(arena),
            PatKind::Infix { lhs, rhs, .. } => {
                let mut lhs = arena.get_pat(*lhs).bound_vids(arena);
                let rhs = arena.get_pat(*rhs).bound_vids(arena);
                lhs.extend(rhs);
                lhs
            }
            PatKind::Typed { pat, .. } => arena.get_pat(*pat).bound_vids(arena),
            PatKind::Layered { vid, pat, .. } => {
                let mut names = vec![LongVId::from_vid(*vid)];
                let pat_names = arena.get_pat(*pat).bound_vids(arena);
                names.extend(pat_names);
                names
            }
        }
    }
}

/// Record entry in a pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatRow {
    Wildcard,
    Pattern { label: Label, pat: Idx<Pat> },
}

/// HIR type node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {
    pub kind: TyKind,
    // None only if TyKind::Missing
    pub ast_id: AstId<ast::Ty>,
}

/// Kinds of HIR types.
///
/// These correspond to the basic forms in Chapter 2 of the Definition, after
/// desugaring the derived forms from Appendix A.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    Missing,
    Var(TyVar),
    Record {
        tyrows: Box<[TyRow]>,
    },
    Constructed {
        tyseq: Box<[Idx<Ty>]>,
        longtycon: (LongTyCon, DefLoc),
    },
    Function {
        domain: Idx<Ty>,
        range: Idx<Ty>,
    },
}

/// Record entry in a type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyRow {
    pub label: Label,
    pub ty: Idx<Ty>,
}

/// A rule in a match statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MRule {
    pub pat: Idx<Pat>,
    pub expr: Idx<Expr>,
}
