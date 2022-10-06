use crate::arena::Idx;
use crate::identifiers::{Label, LongStrId, LongTyCon, LongVId, TyCon, TyVar, VId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Dec {
    Val {
        pat: Idx<Pat>,
        expr: Idx<Expr>,
    },
    Ty {
        tyvarseq: Box<[Idx<TyVar>]>,
        tycon: Idx<TyCon>,
    },
    Datatype {
        databind: DataBind,
    },
    Replication {
        lhs: Idx<TyCon>,
        rhs: LongTyCon,
    },
    Abstype {
        databinds: Box<[DataBind]>, 
        decs: Box<[Idx<Dec>]>,
    },
    Exception {
        exbind: ExBind,
    },
    Local {
        inner_decs: Box<[Idx<Dec>]>,
        outer_decs: Box<[Idx<Dec>]>,
    },
    Open {
        strids: Box<[LongStrId]>,
    },
    Fixity {
        fixity: Fixity,
        vids: Box<[Idx<VId>]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataBind {
    tyvarseq: Box<[Idx<TyVar>]>,
    tycon: Idx<TyCon>,
    conbinds: Box<[ConBind]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConBind {
    op: bool,
    vid: Idx<VId>,
    ty: Option<Idx<TyRef>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExBind {
    Name {
        op: bool,
        vid: Idx<VId>,
        ty: Option<Idx<TyRef>>,
    },
    Assignment {
        op_lhs: bool,
        lhs: Idx<VId>,
        op_rhs: bool,
        rhs: LongVId,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fixity {
    Left(Option<u8>),
    Right(Option<u8>),
    Nonfix,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Missing,
    Scon(Scon),
    VId {
        op: bool,
        longvid: LongVId,
    },
    Record {
        rows: Box<[ExpRow]>,
    },
    Let {
        decs: Box<[Idx<Dec>]>,
        expr: ExprIdx,
    },
    Application {
        expr: ExprIdx,
        param: ExprIdx,
    },
    Infix {
        lhs: ExprIdx,
        op: Idx<VId>,
        rhs: ExprIdx,
    },
    Typed {
        expr: ExprIdx,
        ty: TyRefIdx,
    },
    Handle {
        expr: ExprIdx,
    },
    Raise {
        expr: ExprIdx,
    },
    Fn {
        match_: Box<[MRule]>,
    },
}

pub type ExprIdx = Idx<Expr>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scon {
    Int(i128),
    Word(u128),
    Real(FloatWrapper),
    String(String),
    Char(char),
}

/// See `FloatTypeWrapper` in r-a/hir-def/src/expr.rs
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpRow {
    lab: LabelIdx,
    expr: ExprIdx,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Missing,
    Wildcard,
    Scon(Scon),
    VId {
        op: bool,
        longvid: LongVId,
    },
    Record {
        rows: Box<[PatRow]>,
    },
    Constructed {
        op: bool,
        longvid: LongVId,
        pat: PatIdx,
    },
    Infix {
        lhs: PatIdx,
        vid: Idx<VId>,
        rhs: PatIdx,
    },
    Typed {
        pat: PatIdx,
        ty: TyRefIdx,
    },
    Layered {
        op: bool,
        vid: Idx<VId>,
        ty: Option<TyRefIdx>,
        pat: PatIdx,
    },
}

pub type PatIdx = Idx<Pat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatRow {
    Wildcard,
    Pattern { lab: LabelIdx, expr: ExprIdx },
}

pub type LabelIdx = Idx<Label>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyRef {
    Missing,
    Var(Idx<TyVar>),
    Record {
        tyrows: Box<[TyRow]>,
    },
    Constructed {
        tyseq: Box<[Idx<TyRef>]>,
        longtycon: LongTyCon,
    },
    Function {
        domain: Idx<TyRef>,
        range: Idx<TyRef>,
    },
}

pub type TyRefIdx = Idx<TyRef>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyRow {
    lab: LabelIdx,
    ty: Idx<TyRef>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MRule {
    pat: PatIdx,
    expr: ExprIdx,
}
