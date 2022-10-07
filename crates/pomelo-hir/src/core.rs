use crate::arena::{Arena, Idx};
use crate::identifiers::{Label, LongStrId, LongTyCon, LongVId, StrId, TyCon, TyVar, VId};
use crate::topdecs::{AstIdMap, FileArena, FileAstIdx};
use pomelo_parse::{ast, language::SML, AstNode, AstPtr};

pub mod lower;
pub mod pretty;
mod test;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TopDecBody {
    arenas: BodyArenaImpl,

    // The actual outermost dec(s)
    //
    // `TopDec` maps to each real (syntactic) topdec.
    // This is similar to `ItemTree` in r-a.
    // However, when lowing to HIR (`Dec`, etc.), it makes sense to split multiple
    // semantic declarations into their own `Dec` instances.
    // For example, "val a = b and c = d" represents a single `TopDec`, but two
    // `Dec`s.
    dec: Box<[Idx<Dec>]>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct BodyArenaImpl {
    pub(crate) pats: Arena<Pat>,
    pub(crate) exprs: Arena<Expr>,

    // Inner decs, as in a "let ... in ... end" expr
    pub(crate) decs: Arena<Dec>,
    pub(crate) tys: Arena<Type>,

    pub(crate) vids: Arena<VId>,
    pub(crate) strids: Arena<StrId>,
    pub(crate) tycons: Arena<TyCon>,
    pub(crate) labels: Arena<Label>,
    pub(crate) tyvars: Arena<TyVar>,

    pub(crate) ast_map: AstIdMap,
}

pub trait BodyArena: FileArena {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat>;
    fn get_pat(&self, index: Idx<Pat>) -> Option<&Pat>;

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr>;
    fn get_expr(&self, index: Idx<Expr>) -> Option<&Expr>;

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec>;
    fn get_dec(&self, index: Idx<Dec>) -> Option<&Dec>;

    fn alloc_ty(&mut self, ty: Type) -> Idx<Type>;
    fn get_ty(&self, index: Idx<Type>) -> Option<&Type>;

    fn alloc_label(&mut self, label: Label) -> Idx<Label>;
    fn get_label(&self, index: Idx<Label>) -> Option<&Label>;

    fn alloc_tyvar(&mut self, tyvar: TyVar) -> Idx<TyVar>;
    fn get_tyvar(&self, index: Idx<TyVar>) -> Option<&TyVar>;
}

impl FileArena for BodyArenaImpl {
    fn alloc_vid(&mut self, vid: VId) -> Idx<VId> {
        self.vids.alloc(vid)
    }

    fn get_vid(&self, index: Idx<VId>) -> Option<&VId> {
        self.vids.get(index)
    }

    fn alloc_strid(&mut self, strid: StrId) -> Idx<StrId> {
        self.strids.alloc(strid)
    }

    fn get_strid(&self, index: Idx<StrId>) -> Option<&StrId> {
        self.strids.get(index)
    }

    fn alloc_tycon(&mut self, tycon: TyCon) -> Idx<TyCon> {
        self.tycons.alloc(tycon)
    }

    fn get_tycon(&self, index: Idx<TyCon>) -> Option<&TyCon> {
        self.tycons.get(index)
    }

    fn alloc_ast_id<N>(&mut self, ast: &AstPtr<N>) -> FileAstIdx<N>
    where
        N: AstNode<Language = SML>,
    {
        self.ast_map.alloc(ast)
    }

    fn get_ast_id<N>(&mut self, index: FileAstIdx<N>) -> Option<AstPtr<N>>
    where
        N: AstNode<Language = SML>,
    {
        self.ast_map.get(index)
    }
}

impl BodyArena for BodyArenaImpl {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat> {
        self.pats.alloc(pat)
    }

    fn get_pat(&self, index: Idx<Pat>) -> Option<&Pat> {
        self.pats.get(index)
    }

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.exprs.alloc(expr)
    }

    fn get_expr(&self, index: Idx<Expr>) -> Option<&Expr> {
        self.exprs.get(index)
    }

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec> {
        self.decs.alloc(dec)
    }

    fn get_dec(&self, index: Idx<Dec>) -> Option<&Dec> {
        self.decs.get(index)
    }

    fn alloc_label(&mut self, label: Label) -> Idx<Label> {
        self.labels.alloc(label)
    }

    fn get_label(&self, index: Idx<Label>) -> Option<&Label> {
        self.labels.get(index)
    }

    fn alloc_ty(&mut self, ty: Type) -> Idx<Type> {
        self.tys.alloc(ty)
    }

    fn get_ty(&self, index: Idx<Type>) -> Option<&Type> {
        self.tys.get(index)
    }

    fn alloc_tyvar(&mut self, tyvar: TyVar) -> Idx<TyVar> {
        self.tyvars.alloc(tyvar)
    }

    fn get_tyvar(&self, index: Idx<TyVar>) -> Option<&TyVar> {
        self.tyvars.get(index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dec {
    pub kind: DecKind,
    // None only if DecKind::Missing
    pub ast_id: Option<FileAstIdx<ast::Dec>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecKind {
    Missing,
    Val {
        rec: bool,
        tyvarseq: Box<[Idx<TyVar>]>,
        pat: Idx<Pat>,
        expr: Idx<Expr>,
    },
    Ty {
        tyvarseq: Box<[Idx<TyVar>]>,
        tycon: Idx<TyCon>,
        ty: Idx<Type>,
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
        longstrids: Box<[LongStrId]>,
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
    ty: Option<Idx<Type>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExBind {
    Name {
        op: bool,
        vid: Idx<VId>,
        ty: Option<Idx<Type>>,
    },
    Assignment {
        op_lhs: bool,
        lhs: Idx<VId>,
        op_rhs: bool,
        rhs: LongVId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fixity {
    Left(Option<u8>),
    Right(Option<u8>),
    Nonfix,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    // None only if ExprKind::Missing
    pub ast_id: Option<FileAstIdx<ast::Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Missing,
    Scon(Scon),
    Nil,
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
        vid: Idx<VId>,
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
    Missing,
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
pub struct Pat {
    pub kind: PatKind,
    // None only if PatKind::Missing
    pub ast_id: Option<FileAstIdx<ast::Pat>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatKind {
    Missing,
    Wildcard,
    Nil,
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
    Pattern { label: LabelIdx, pat: PatIdx },
}

pub type LabelIdx = Idx<Label>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub kind: TyKind,
    // None only if TyKind::Missing
    pub ast_id: Option<FileAstIdx<ast::Ty>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    Missing,
    Var(Idx<TyVar>),
    Record {
        tyrows: Box<[TyRow]>,
    },
    Constructed {
        tyseq: Box<[Idx<Type>]>,
        longtycon: LongTyCon,
    },
    Function {
        domain: Idx<Type>,
        range: Idx<Type>,
    },
}

pub type TyRefIdx = Idx<Type>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyRow {
    lab: LabelIdx,
    ty: Idx<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MRule {
    pat: PatIdx,
    expr: ExprIdx,
}
