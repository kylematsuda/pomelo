//! High-level intermediate representation (HIR) for `pomelo`.
//!
//! Similar to the [HIR in Rust](https://rustc-dev-guide.rust-lang.org/hir.html), the HIR mainly
//! serves to desugar some derived language constructs to simplify semantic analysis.
//! The lowering from the AST to HIR is implemented in [`lower`].
//!
//! # Structure of the HIR
//!
//! The main entry point to the HIR is [`File`], which contains the list of top level
//! declarations ([`Dec`]) in the file.
//! Internally, the HIR is represented as a tree in an index-based arena -- the idea is that this is
//! hopefully a little simpler to work with than the
//! [`rowan`](https://docs.rs/rowan/latest/rowan/)-based AST that is outputted at the parsing stage.
//! HIR nodes (notably [`Dec`], [`Expr`], [`Pat`], and [`Ty`]) are stored by a [`FileArena`].
//! The syntactic kinds of nodes are represented by [`DecKind`], [`ExprKind`], [`PatKind`], and
//! [`TyKind`].
//! References between nodes are represented as indexes ([`Idx<T>`](crate::arena::Idx)) to nodes in
//! the arena, which conveniently means that we don't have to worry about ownership problems (see
//! later notes about the tradeoff here).
//!
//! Basic semantic information is tracked and embedded in the HIR during lowering from the AST.
//! For example, variable expressions are represented by an `ExprKind::Vid { longvid: (LongVId,
//! DefLoc), .. }`, where [`LongVId`] represents an identifier and [`DefLoc`] is a reference to the
//! declaration or pattern where the identifier is declared.
//! Type constructors ([`TyCon`]) similarly come with a reference to their declaration.
//! (Note: this needs to be tested a lot more!)
//!
//! The symbol table is held in the [`LoweringCtxt`](crate::lower::LoweringCtxt), accessible through calling
//! [`LoweringCtxt::resolver`](crate::lower::LoweringCtxt::resolver).
//! Besides being used to annotate the HIR with name resolution data, we also use this to distinguish
//! between infix expressions vs function applications.
//! This is not done in the parsing stage because it requires tracking which identifiers have been
//! declared `infix`/`infixr`/`nonfix`.
//! The implementation uses Pratt parsing and is located in the `lower::infix` module
//! (also done to distinguish infix vs constructed patterns).
//!
//! The HIR does not currently have a way to go from a declaration to all of its references, which
//! is something that will be important for common IDE actions.
//! This could be baked in at the lowering stage as well by annotating variable declarations with
//! a list of usages.
//! For now, this is implemented separately in [`pomelo_ty`](../pomelo_ty/index.html).
//!
//! # Lowering strategy and tradeoffs
//!
//! Currently, the HIR is lowered in one pass and is contained all in a single graph.
//! This is nice and simple, and also makes sense because the declarations in an SML
//! file are sequential (in contrast to, e.g., a Rust module where the items may appear in any
//! order).
//!
//! ### Comparison to `rustc` and `rust-analyzer`
//!
//! I was confused about this for a while, so it might be worth writing a little more (at least
//! to help me remember why I did it this way).
//! The monolithic structure of the HIR here is very different to how things are done in
//! `rustc` and `rust-analyzer`, the two compilers I've spent the most time looking at for inspiration.
//! Both `rustc` and `rust-analyzer` make a strong conceptual division between `Item`s and `Body`s.
//! An [`Item`](https://doc.rust-lang.org/reference/items.html) is anything that can appear at the
//! top level of a module (`fn`, `struct`, `enum`, `const`, `mod`, etc.), while a `Body` is the
//! stuff inside of the `Item` that might need to be type checked (i.e., expressions live inside of bodies).
//! As mentioned previously, the order of `Items` in a file doesn't affect the meaning of a
//! Rust program (unlike in SML).
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
//! If separate `Item`s are stored independently, then we take advantage of the fact that typing
//! within one `Body` doesn't effect another `Body`, and we don't have to completely reanalyze the file
//! every time it is edited.
//!
//! However, this actually doesn't make too much sense for SML, since changing the order of top-level
//! declarations in a file can substantially change the program's meaning.
//! At the very least, we probably have to redo type-checking on the entire file below the
//! point where it was edited, so it's probably not too much extra cost to redo the parsing and lowering of
//! the entire file to HIR.
//!
//! ### A note about immutability
//!
//! Currently, this only covers the pure functional subset of Core SML.
//! This is convenient because immutability makes it easy to have the HIR be static single assignment (SSA).
//! Each variable usage (syntactically a [`VId`]) is annotated with a [`DefLoc`] that points to its
//! declaration.
//! The combination `(VId, DefLoc)` is a unique identifier that points to only one place in the
//! HIR.
//! If we ever try to extend this to include imperative features, it will be interesting to see how
//! challenging it is to adapt the HIR.
//!
//! # TODO:
//!
//! Handle errors during HIR lowering.
//! Figure out a good way to surface them.
//!
//! Figure out if validation passes should be run on HIR or AST.
//!
//! Figure out if each HIR node needs to hold the index of its parent node.
//!
//! Figure out logging for this stage and probably parsing too.
pub mod arena;

pub mod builtins;
pub use builtins::*;

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

// TODO: define a new hir-error type.
//
// Figure out an error handling strategy during lowering.
// This can probably be pretty simple to start, just accumulating errors in the `ctx`?

/// Obtain the HIR from the AST.
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

    pub fn arenas_mut(&mut self) -> &mut impl FileArenaExt {
        &mut self.arenas
    }

    pub fn topdecs(&self) -> &[Idx<Dec>] {
        &self.topdecs
    }

    pub fn topdecs_mut(&mut self) -> &mut Vec<Idx<Dec>> {
        &mut self.topdecs
    }
}

impl FileArena for File {
    fn get_pat(&self, index: Idx<Pat>) -> &Pat {
        self.arenas.get_pat(index)
    }
    fn get_expr(&self, index: Idx<Expr>) -> &Expr {
        self.arenas.get_expr(index)
    }
    fn get_dec(&self, index: Idx<Dec>) -> &Dec {
        self.arenas.get_dec(index)
    }
    fn get_ty(&self, index: Idx<Ty>) -> &Ty {
        self.arenas.get_ty(index)
    }
    fn get_name(&self, index: Idx<String>) -> &str {
        self.arenas.get_name(index)
    }
}

/// Interface for navigating the HIR.
pub trait FileArena {
    fn get_pat(&self, index: Idx<Pat>) -> &Pat;
    fn get_expr(&self, index: Idx<Expr>) -> &Expr;
    fn get_dec(&self, index: Idx<Dec>) -> &Dec;
    fn get_ty(&self, index: Idx<Ty>) -> &Ty;
    fn get_name(&self, index: Idx<String>) -> &str;
}

/// Interface for building the HIR.
pub trait FileArenaExt: FileArena + NameInterner {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat>;
    fn get_pat_mut(&mut self, index: Idx<Pat>) -> &mut Pat;

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr>;
    fn get_expr_mut(&mut self, index: Idx<Expr>) -> &mut Expr;

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec>;
    fn get_dec_mut(&mut self, index: Idx<Dec>) -> &mut Dec;

    fn alloc_ty(&mut self, ty: Ty) -> Idx<Ty>;
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

/// Interface for interning and generating identifiers.
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
    fn get_pat(&self, index: Idx<Pat>) -> &Pat {
        self.pats.get(index)
    }

    fn get_expr(&self, index: Idx<Expr>) -> &Expr {
        self.exprs.get(index)
    }

    fn get_dec(&self, index: Idx<Dec>) -> &Dec {
        self.decs.get(index)
    }

    fn get_ty(&self, index: Idx<Ty>) -> &Ty {
        self.tys.get(index)
    }

    fn get_name(&self, index: Idx<String>) -> &str {
        self.name_interner.get(index)
    }
}

impl<I: NameInterner> FileArenaExt for FileArenaImpl<I> {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat> {
        self.pats.alloc(pat)
    }

    fn get_pat_mut(&mut self, index: Idx<Pat>) -> &mut Pat {
        self.pats.get_mut(index)
    }

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.exprs.alloc(expr)
    }

    fn get_expr_mut(&mut self, index: Idx<Expr>) -> &mut Expr {
        self.exprs.get_mut(index)
    }

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec> {
        self.decs.alloc(dec)
    }

    fn get_dec_mut(&mut self, index: Idx<Dec>) -> &mut Dec {
        self.decs.get_mut(index)
    }

    fn alloc_ty(&mut self, ty: Ty) -> Idx<Ty> {
        self.tys.alloc(ty)
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

/// Storage for links from the HIR back to the AST.
///
/// This will be particularly important for error messages, as we will want to refer to an error's
/// original span in the AST (as opposed to whatever it is after lowering).
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
/// Currently, this is needed because we only hold references to `SyntaxNodePtr`s,
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
