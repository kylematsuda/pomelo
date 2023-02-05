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

pub mod hir;
pub use hir::*;

pub mod identifiers;
pub use identifiers::*;

pub mod lower;
pub mod pretty;

use std::collections::HashMap;
use std::marker::PhantomData;

use pomelo_parse::{
    ast::{self, AstNode, AstPtr},
    language::{SyntaxNodePtr, SML},
    Error, SyntaxTree,
};

use crate::arena::{Arena, Idx};
use crate::identifiers::{
    Label, LongStrId, LongTyCon, LongVId, NameInternerImpl, TyCon, TyVar, VId,
};

// TODO: define a new hir-error type.
//
// Figure out an error handling strategy during lowering.
// This can probably be pretty simple to start, just accumulating errors in the `ctx`?

pub fn lower_ast_to_hir(ast: SyntaxTree) -> (File, Vec<Error>) {
    let errors = ast.errors().cloned();
    let node = ast::File::cast(ast.syntax()).unwrap();
    let ctx = lower::LoweringCtxt::new();
    let (file, lowering_errs) = ctx.lower_file(&node);
    (file, errors.chain(lowering_errs.into_iter()).collect())
}

/// Represents a desugared top-level declaration and its contents.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct File {
    arenas: FileArenaImpl<NameInternerImpl>,

    // The actual outermost dec(s)
    //
    // `TopDec` maps to each real (syntactic) topdec.
    // This is similar to `ItemTree` in r-a.
    // However, when lowing to HIR (`Dec`, etc.), it makes sense to split multiple
    // semantic declarations into their own `Dec` instances.
    // For example, "val a = b and c = d" represents a single `TopDec`, but two
    // `Dec`s.
    topdecs: Vec<Idx<Dec>>,
}

impl File {
    pub fn arenas(&self) -> &impl FileArena {
        &self.arenas
    }

    pub fn arenas_mut(&mut self) -> &mut impl FileArena {
        &mut self.arenas
    }

    pub fn topdecs(&self) -> &[Idx<Dec>] {
        &self.topdecs
    }

    pub fn topdecs_mut(&mut self) -> &mut Vec<Idx<Dec>> {
        &mut self.topdecs
    }
}

pub trait FileArena: NameInterner {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat>;
    fn get_pat(&self, index: Idx<Pat>) -> &Pat;
    fn get_pat_mut(&mut self, index: Idx<Pat>) -> &mut Pat;

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr>;
    fn get_expr(&self, index: Idx<Expr>) -> &Expr;
    fn get_expr_mut(&mut self, index: Idx<Expr>) -> &mut Expr;

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec>;
    fn get_dec(&self, index: Idx<Dec>) -> &Dec;
    fn get_dec_mut(&mut self, index: Idx<Dec>) -> &mut Dec;

    fn alloc_ty(&mut self, ty: Ty) -> Idx<Ty>;
    fn get_ty(&self, index: Idx<Ty>) -> &Ty;
    fn get_ty_mut(&mut self, index: Idx<Ty>) -> &mut Ty;

    fn alloc_ast_id<N>(&mut self, ast: &N) -> FileAstIdx<N>
    where
        N: AstNode<Language = SML>;

    fn get_ast_id<N>(&self, index: FileAstIdx<N>) -> Option<AstPtr<N>>
    where
        N: AstNode<Language = SML>;

    fn get_ast_span<N>(&self, index: FileAstIdx<N>) -> Option<(usize, usize)>
    where
        N: AstNode<Language = SML>;
}

pub trait NameInterner {
    fn fresh(&mut self) -> u32;
    fn alloc(&mut self, s: &str) -> Idx<String>;
    fn get(&self, index: Idx<String>) -> &str;

    fn fresh_vid(&mut self) -> VId {
        VId::Name(Name::Generated(self.fresh()))
    }

    fn fresh_strid(&mut self) -> StrId {
        StrId::Name(Name::Generated(self.fresh()))
    }

    fn fresh_tyvar(&mut self) -> TyVar {
        TyVar::Name(Name::Generated(self.fresh()))
    }

    fn fresh_tycon(&mut self) -> TyCon {
        TyCon::Name(Name::Generated(self.fresh()))
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct FileArenaImpl<NameInterner> {
    pub(crate) pats: Arena<Pat>,
    pub(crate) exprs: Arena<Expr>,

    // Inner decs, as in a "let ... in ... end" expr
    pub(crate) decs: Arena<Dec>,
    pub(crate) tys: Arena<Ty>,

    pub(crate) name_interner: NameInterner,
    pub(crate) ast_map: AstIdMap,
}

impl<I: NameInterner> NameInterner for FileArenaImpl<I> {
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

impl<I: NameInterner> FileArena for FileArenaImpl<I> {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat> {
        self.pats.alloc(pat)
    }

    fn get_pat(&self, index: Idx<Pat>) -> &Pat {
        self.pats.get(index)
    }

    fn get_pat_mut(&mut self, index: Idx<Pat>) -> &mut Pat {
        self.pats.get_mut(index)
    }

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.exprs.alloc(expr)
    }

    fn get_expr(&self, index: Idx<Expr>) -> &Expr {
        self.exprs.get(index)
    }

    fn get_expr_mut(&mut self, index: Idx<Expr>) -> &mut Expr {
        self.exprs.get_mut(index)
    }

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec> {
        self.decs.alloc(dec)
    }

    fn get_dec(&self, index: Idx<Dec>) -> &Dec {
        self.decs.get(index)
    }

    fn get_dec_mut(&mut self, index: Idx<Dec>) -> &mut Dec {
        self.decs.get_mut(index)
    }

    fn alloc_ty(&mut self, ty: Ty) -> Idx<Ty> {
        self.tys.alloc(ty)
    }

    fn get_ty(&self, index: Idx<Ty>) -> &Ty {
        self.tys.get(index)
    }

    fn get_ty_mut(&mut self, index: Idx<Ty>) -> &mut Ty {
        self.tys.get_mut(index)
    }

    fn alloc_ast_id<N>(&mut self, ast: &N) -> FileAstIdx<N>
    where
        N: AstNode<Language = SML>,
    {
        self.ast_map.alloc(ast)
    }

    fn get_ast_id<N>(&self, index: FileAstIdx<N>) -> Option<AstPtr<N>>
    where
        N: AstNode<Language = SML>,
    {
        self.ast_map.get(index)
    }

    fn get_ast_span<N>(&self, index: FileAstIdx<N>) -> Option<(usize, usize)>
    where
        N: AstNode<Language = SML>,
    {
        self.ast_map.get_span(index)
    }
}

// See r-a hir_expand
#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct AstIdMap {
    arena: Arena<SyntaxNodePtr>,
    backmap: HashMap<SyntaxNodePtr, Idx<SyntaxNodePtr>>,
}

impl AstIdMap {
    pub fn alloc<N: AstNode<Language = SML>>(&mut self, ast: &N) -> FileAstIdx<N> {
        let astptr = AstPtr::new(ast);
        let syntax = astptr.syntax_node_ptr();

        let index = self.arena.alloc(syntax.clone());
        self.backmap.insert(syntax, index);

        FileAstIdx {
            index,
            _ph: PhantomData,
        }
    }

    pub fn get<N: AstNode<Language = SML>>(&self, index: FileAstIdx<N>) -> Option<AstPtr<N>> {
        let ptr = self.arena.get(index.index).clone();
        SyntaxNodePtr::cast(ptr)
    }

    /// FIXME: figure out how to properly handle text spans; this is duct tape for now
    pub fn get_span<N>(&self, index: FileAstIdx<N>) -> Option<(usize, usize)>
    where
        N: AstNode<Language = SML>,
    {
        self.get(index)
            .map(|id| id.syntax_node_ptr().text_range())
            .map(|r| (r.start().into(), r.end().into()))
    }
}

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
