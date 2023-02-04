//! High-level intermediate representation (HIR) for `pomelo`.
//!
//! Similar to the [HIR in Rust](https://rustc-dev-guide.rust-lang.org/hir.html), the HIR mainly
//! serves to desugar some derived language constructs to simplify semantic analysis.
//! Our HIR is also a simple graph in an index-based arena, which hopefully is a little more
//! transparent to work with than the [`rowan`](https://docs.rs/rowan/latest/rowan/)-based AST that
//! is outputted by the parsing stage.
//!
//! # High-level notes
//!
//! Let's start by thinking in analogy to Rust, so we can take some inspiration
//! from `rustc` and `rust-analyzer`.
//! Both of these make a strong conceptual division between `Item`s and `Body`s.
//! An [`Item`](https://doc.rust-lang.org/reference/items.html) is anything that can appear at the
//! top level of a module (`fn`, `struct`, `enum`, `const`, `mod`, etc.), while a `Body` is the
//! stuff inside of the `Item` that might need to be type checked (i.e., expressions live inside of bodies).
//!
//! For example, we might have the following Rust function declaration in a module,
//! ```ignore
//! pub fn foo(x: Bar) -> Baz {
//!     let y = 0;
//!     let z = "something";
//!     Baz::new()
//! }
//! ```
//! The `Item` here is the `fn` declaration, which we can lookup to resolve the name `foo`,
//! as well as lookup the functions signature, etc.
//!
//! Meanwhile, the `Body` is all the stuff inside.
//! Note that `Item`s define names that are visible within the entire module,
//! while a declaration in a `Body`s only has scope within that `Body`.
//! In addition, we need to run type inference and checking on the contents of the `Body`,
//! but not on `Item`'s signature.
//! Importantly, changing the `Body` of one `Item` cannot impact the type-checking of the `Body` of
//! a different `Item`.
//!
//! This all means that there is a pretty natural split between item declarations and expressions
//! in Rust.
//! We try to emulate that here in constructing our IR for SML.
//! An SML [`File`](crate::topdecs::File) consists of a list of [`Dec`](crate::core::Dec)s.
//! Some of these `Dec`s may contain a [`Body`](crate::core::Body), which contains patterns or
//! expressions.
//! Lowering a file from the AST to the HIR should consist of lowering each `Dec` and
//! storing its file-scope names (variable names it binds, etc.) in some sort of `LoweringCtxt`,
//! to be used later in name resolution.
//!
//! After this is done, we can do name resolution (scoping) on the `Body`s and hopefully be in a
//! good position to start working out type inference.
//!
//! ## Small wrinkle: infix vs function applications
//!
//! A fun fact that I learned about SML (which is maybe obvious to anyone who has actually used
//! an ML before) is that we actually need some semantic analysis to distinguish expressions or
//! patterns that contain a bunch of names in a row.
//! In that situation, we can't tell just from the expression itself whether it's supposed to be a
//! sequence of function applications or if it might also contain infix operators
//! -- any function `foo` that takes a 2-tuple of arguments, `foo (x, y)`, can be
//! turned into an infix operator `x foo y` with an `infix` declaration.
//! Currently, parsing just gives up and outputs an `InfixOrAppExpr` node, but of
//! course we will need to know how to interpret these expressions when doing type inference.
//!
//! Currently, I think that the nicest way to resolve this is during the lowering stage.
//! We can make the `LoweringCtxt` keep track of infix declarations as we lower the `File`.
//! We could also do a pass to fix up these after the initial lowering stage, but then we'll end up
//! creating a bunch of extra HIR nodes that will be useless -- not a huge problem, but currently
//! the `Arena` does not have any way to deallocate nodes.
pub mod arena;
pub mod body;
pub mod identifiers;
pub mod lower;
pub mod scope;
pub mod semantics;

use std::marker::PhantomData;

use pomelo_parse::{
    ast::{self, AstNode},
    language::{SyntaxNodePtr, SML},
};

use crate::arena::Idx;
use crate::body::BodyArena;
use crate::identifiers::{Label, LongStrId, LongTyCon, LongVId, TyCon, TyVar, VId};

/// A pointer from the HIR node back to its corresponding AST node.
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

impl<N: AstNode<Language = SML>> AstId<N> {
    pub fn is_generated(&self) -> bool {
        match self {
            Self::Generated(_) => true,
            Self::Missing | Self::Node(_) => false,
        }
    }

    pub fn get_node(&self) -> Option<FileAstIdx<N>> {
        match self {
            Self::Node(n) => Some(*n),
            Self::Missing | Self::Generated(_) => None,
        }
    }

    pub fn as_span<A: BodyArena>(&self, arena: &A) -> Option<(usize, usize)> {
        match self {
            Self::Missing => None,
            Self::Node(n) => arena.get_ast_span(*n),
            Self::Generated(parent) => parent.as_span(arena),
        }
    }
}

/// A pointer to an AST node.
///
/// Currently, this is needed because we only hold references to `SyntaxNodePtr`,
/// which know nothing about the type of the pointed-to AST node.
/// This nice thing is that this allows us to allocate all of the `SyntaxNodePtr`s in the same
/// arena (unlike if we used the typed `AstPtr`s).
#[derive(Debug, PartialEq, Eq)]
pub struct FileAstIdx<N> {
    index: Idx<SyntaxNodePtr>,
    _ph: PhantomData<fn() -> N>,
}

impl<N> Clone for FileAstIdx<N> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            _ph: self._ph,
        }
    }
}

impl<N> Copy for FileAstIdx<N> {}

/// Used to lookup the parent span of nodes that were generated during lowering.
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

    pub fn as_span<A: BodyArena>(&self, arena: &A) -> Option<(usize, usize)> {
        match self {
            Self::Dec(d) => arena.get_ast_span(*d),
            Self::Expr(e) => arena.get_ast_span(*e),
            Self::Pat(p) => arena.get_ast_span(*p),
        }
    }
}

/// A single source file or an input from the REPL.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct File {
    pub(crate) decs: Vec<TopDec>,
}

impl File {
    pub fn get_dec(&self, index: usize) -> Option<&TopDec> {
        self.decs.get(index)
    }

    pub fn add_dec(&mut self, dec: TopDec) {
        self.decs.push(dec);
    }
}

/// Fundamental reuse unit is the TopDec
///
/// Why?
///     Typing in body of one TopDec should not invalidate others
///     Plays nice with repl (sequence of TopDecs)
///     Plays nice with "use" directive
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TopDec {
    body: body::Body,
}

impl TopDec {
    pub fn new(body: body::Body) -> Self {
        Self { body }
    }
}

/// HIR declaration node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dec {
    pub kind: DecKind,
    pub ast_id: AstId<ast::Dec>,
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
        longvid: LongVId,
    },
    Record {
        rows: Box<[ExpRow]>,
    },
    Let {
        dec: Idx<Dec>,
        expr: ExprIdx,
    },
    InfixOrApp {
        exprs: Box<[Idx<Expr>]>,
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

/// Wrapper so we can derive `Eq`.
///
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
    pub label: Label,
    pub expr: ExprIdx,
}

/// HIR pattern node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pat {
    pub kind: PatKind,
    pub ast_id: AstId<ast::Pat>,
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

/// HIR type node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
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
    pub pat: PatIdx,
    pub expr: ExprIdx,
}
