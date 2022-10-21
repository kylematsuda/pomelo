use crate::arena::{Arena, Idx};
use crate::identifiers::{
    Label, LongStrId, LongTyCon, LongVId, NameInterner, NameInternerImpl, TyCon, TyVar, VId,
};
use crate::topdecs::{AstIdMap, FileArena, FileAstIdx};
use pomelo_parse::{ast, language::SML, AstNode, AstPtr};

pub mod lower;
pub mod pretty;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TopDecBody {
    arenas: BodyArenaImpl<NameInternerImpl>,

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
pub(crate) struct BodyArenaImpl<NameInterner> {
    pub(crate) pats: Arena<Pat>,
    pub(crate) exprs: Arena<Expr>,

    // Inner decs, as in a "let ... in ... end" expr
    pub(crate) decs: Arena<Dec>,
    pub(crate) tys: Arena<Type>,

    pub(crate) name_interner: NameInterner,
    pub(crate) ast_map: AstIdMap,
}

pub trait BodyArena: FileArena {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat>;
    fn get_pat(&self, index: Idx<Pat>) -> &Pat;

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr>;
    fn get_expr(&self, index: Idx<Expr>) -> &Expr;

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec>;
    fn get_dec(&self, index: Idx<Dec>) -> &Dec;

    fn alloc_ty(&mut self, ty: Type) -> Idx<Type>;
    fn get_ty(&self, index: Idx<Type>) -> &Type;
}

impl<I: NameInterner> NameInterner for BodyArenaImpl<I> {
    fn fresh(&mut self) -> u32 {
        self.name_interner.fresh()
    }

    fn alloc(&mut self, s: &str) -> Idx<String> {
        self.name_interner.alloc(s)
    }

    fn get(&self, index: Idx<String>) -> &str {
        self.name_interner.get(index)
    }
}

impl<I: NameInterner> FileArena for BodyArenaImpl<I> {
    fn alloc_ast_id<N>(&mut self, ast: &N) -> FileAstIdx<N>
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

impl<I: NameInterner> BodyArena for BodyArenaImpl<I> {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat> {
        self.pats.alloc(pat)
    }

    fn get_pat(&self, index: Idx<Pat>) -> &Pat {
        self.pats.get(index)
    }

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.exprs.alloc(expr)
    }

    fn get_expr(&self, index: Idx<Expr>) -> &Expr {
        self.exprs.get(index)
    }

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec> {
        self.decs.alloc(dec)
    }

    fn get_dec(&self, index: Idx<Dec>) -> &Dec {
        self.decs.get(index)
    }

    fn alloc_ty(&mut self, ty: Type) -> Idx<Type> {
        self.tys.alloc(ty)
    }

    fn get_ty(&self, index: Idx<Type>) -> &Type {
        self.tys.get(index)
    }
}

/// Used as a pointer from the HIR node back to its corresponding AST node.
///
/// This node may be missing, an exact 1-1 translation of the AST node,
/// or a node that was generated during lowering (modeled by [`NodeParent`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstId<N> {
    Missing,
    Node(FileAstIdx<N>),
    // Generated during AST lowering
    Generated(NodeParent),
}

/// Used to find the parent span of nodes that were generated during lowering.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeParent {
    Dec(FileAstIdx<ast::Dec>),
    Expr(FileAstIdx<ast::Expr>),
    Pat(FileAstIdx<ast::Pat>),
}

impl NodeParent {
    pub fn from_expr<A: BodyArena>(expr: &ast::Expr, arena: &mut A) -> Self {
        let id = arena.alloc_ast_id(expr);
        Self::Expr(id)
    }

    pub fn from_pat<A: BodyArena>(pat: &ast::Pat, arena: &mut A) -> Self {
        let id = arena.alloc_ast_id(pat);
        Self::Pat(id)
    }

    pub fn from_dec<A: BodyArena>(dec: &ast::Dec, arena: &mut A) -> Self {
        let id = arena.alloc_ast_id(dec);
        Self::Dec(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dec {
    pub kind: DecKind,
    pub ast_id: AstId<ast::Dec>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecKind {
    Missing,
    Seq {
        decs: Box<[Idx<Dec>]>,
    },
    Val {
        rec: bool,
        tyvarseq: Box<[TyVar]>,
        pat: Idx<Pat>,
        expr: Idx<Expr>,
    },
    Ty {
        tyvarseq: Box<[TyVar]>,
        tycon: TyCon,
        ty: Idx<Type>,
    },
    Datatype {
        databind: DataBind,
    },
    Replication {
        lhs: TyCon,
        rhs: LongTyCon,
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
        vids: Box<[VId]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataBind {
    tyvarseq: Box<[TyVar]>,
    tycon: TyCon,
    conbinds: Box<[ConBind]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConBind {
    op: bool,
    vid: VId,
    ty: Option<Idx<Type>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExBind {
    Name {
        op: bool,
        vid: VId,
        ty: Option<Idx<Type>>,
    },
    Assignment {
        op_lhs: bool,
        lhs: VId,
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
    pub ast_id: AstId<ast::Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Missing,
    Scon(Scon),
    Seq {
        exprs: Box<[Idx<Expr>]>,
    },
    VId {
        op: bool,
        longvid: LongVId,
    },
    Record {
        rows: Box<[ExpRow]>,
    },
    Let {
        dec: Idx<Dec>,
        expr: ExprIdx,
    },
    Application {
        expr: ExprIdx,
        param: ExprIdx,
    },
    Infix {
        lhs: ExprIdx,
        vid: VId,
        rhs: ExprIdx,
    },
    Typed {
        expr: ExprIdx,
        ty: TyRefIdx,
    },
    Handle {
        expr: ExprIdx,
        match_: Box<[MRule]>,
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
    label: Label,
    expr: ExprIdx,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pat {
    pub kind: PatKind,
    pub ast_id: AstId<ast::Pat>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatKind {
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
        vid: VId,
        rhs: PatIdx,
    },
    Typed {
        pat: PatIdx,
        ty: TyRefIdx,
    },
    Layered {
        op: bool,
        vid: VId,
        ty: Option<Idx<Type>>,
        pat: PatIdx,
    },
}

pub type PatIdx = Idx<Pat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatRow {
    Wildcard,
    Pattern { label: Label, pat: PatIdx },
}

pub type LabelIdx = Idx<Label>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub kind: TyKind,
    // None only if TyKind::Missing
    pub ast_id: AstId<ast::Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    Missing,
    Var(TyVar),
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
    label: Label,
    ty: Idx<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MRule {
    pat: PatIdx,
    expr: ExprIdx,
}
