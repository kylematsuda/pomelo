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

use std::marker::PhantomData;

use pomelo_parse::{
    ast::{self, AstNode},
    language::{SyntaxNodePtr, SML},
};

use crate::arena::Idx;
use crate::body::{FileArena, FileArenaImpl};
use crate::identifiers::{
    Label, LongStrId, LongTyCon, LongVId, NameInternerImpl, TyCon, TyVar, VId,
};
use crate::lower::LoweringCtxt;

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

    pub fn as_span<A: FileArena>(&self, arena: &A) -> Option<(usize, usize)> {
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
    pub fn from_expr<A: FileArena>(expr: &ast::Expr, arena: &mut A) -> Self {
        let id = arena.alloc_ast_id(expr);
        Self::Expr(id)
    }

    pub fn from_pat<A: FileArena>(pat: &ast::Pat, arena: &mut A) -> Self {
        let id = arena.alloc_ast_id(pat);
        Self::Pat(id)
    }

    pub fn from_dec<A: FileArena>(dec: &ast::Dec, arena: &mut A) -> Self {
        let id = arena.alloc_ast_id(dec);
        Self::Dec(id)
    }

    pub fn as_span<A: FileArena>(&self, arena: &A) -> Option<(usize, usize)> {
        match self {
            Self::Dec(d) => arena.get_ast_span(*d),
            Self::Expr(e) => arena.get_ast_span(*e),
            Self::Pat(p) => arena.get_ast_span(*p),
        }
    }
}

/// Location where an identifier is bound.
///
/// The `Pat` variant should only be used inside of match statements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefLoc {
    Dec(Idx<Dec>),
    Pat(Idx<Pat>),
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

/// HIR declaration node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dec {
    pub kind: DecKind,
    pub ast_id: AstId<ast::Dec>,
}

impl Dec {
    pub fn bound_vids(&self, ctx: &LoweringCtxt) -> Vec<LongVId> {
        match &self.kind {
            DecKind::Missing
            | DecKind::Ty { .. }
            | DecKind::Replication { .. }
                // Fixity is a weird one, need to figure out how to treat it
            | DecKind::Fixity { .. } => vec![],
            DecKind::Seq { decs } => {
                let mut names = vec![];

                for d in decs.iter() {
                    let d = ctx.arenas().get_dec(*d).bound_vids(ctx);
                    names.extend(d);
                }

                names
            }
            DecKind::Val { bindings, .. } => bindings
                .iter()
                .flat_map(|b| b.bound_vids(ctx).into_iter())
                .collect(),
            DecKind::Datatype { databind } => databind.bound_vids(),
            DecKind::Abstype { databinds, dec } => {
                let mut names = databinds
                    .iter()
                    .flat_map(|d| d.bound_vids().into_iter())
                    .collect::<Vec<_>>();
                names.extend(ctx.arenas().get_dec(*dec).bound_vids(ctx));
                names
            }
            DecKind::Exception { exbind } => vec![exbind.bound_vid()],
            DecKind::Local { outer, .. } => ctx.arenas().get_dec(*outer).bound_vids(ctx),
            DecKind::Open { .. } => todo!(),
        }
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
        tyvarseq: Box<[TyVar]>,
        tycon: TyCon,
        ty: (Idx<Type>, DefLoc),
    },
    Datatype {
        databind: DataBind,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValBind {
    pub rec: bool,
    pub pat: Idx<Pat>,
    pub expr: Idx<Expr>,
}

impl ValBind {
    pub fn bound_vids(&self, ctx: &LoweringCtxt) -> Vec<LongVId> {
        ctx.arenas().get_pat(self.pat).bound_vids(ctx)
    }
}

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConBind {
    op: bool,
    vid: VId,
    ty: Option<(Idx<Type>, DefLoc)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExBind {
    Name {
        op: bool,
        vid: VId,
        ty: Option<(Idx<Type>, DefLoc)>,
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
        ty: Idx<Type>,
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
    pub fn bound_vids(&self, ctx: &LoweringCtxt) -> Vec<LongVId> {
        match &self.kind {
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
                        names.extend(ctx.arenas().get_pat(*pat).bound_vids(ctx));
                    }
                }
                names
            }
            PatKind::Constructed { pat, .. } => ctx.arenas().get_pat(*pat).bound_vids(ctx),
            PatKind::Infix { lhs, rhs, .. } => {
                let arena = ctx.arenas();
                let mut lhs = arena.get_pat(*lhs).bound_vids(ctx);
                let rhs = arena.get_pat(*rhs).bound_vids(ctx);
                lhs.extend(rhs);
                lhs
            }
            PatKind::Typed { pat, .. } => ctx.arenas().get_pat(*pat).bound_vids(ctx),
            PatKind::Layered { vid, pat, .. } => {
                let mut names = vec![LongVId::from_vid(*vid)];
                let pat_names = ctx.arenas().get_pat(*pat).bound_vids(ctx);
                names.extend(pat_names);
                names
            }
        }
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
        ty: (Idx<Type>, DefLoc),
    },
    Layered {
        op: bool,
        vid: VId,
        ty: Option<(Idx<Type>, DefLoc)>,
        pat: Idx<Pat>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatRow {
    Wildcard,
    Pattern { label: Label, pat: Idx<Pat> },
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
    Var(TyVar, DefLoc),
    Record {
        tyrows: Box<[TyRow]>,
    },
    Constructed {
        tyseq: Box<[Idx<Type>]>,
        longtycon: (LongTyCon, DefLoc),
    },
    Function {
        domain: Idx<Type>,
        range: Idx<Type>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyRow {
    label: Label,
    ty: Idx<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MRule {
    pub pat: Idx<Pat>,
    pub expr: Idx<Expr>,
}
