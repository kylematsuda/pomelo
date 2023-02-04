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
pub mod core;
pub mod identifiers;
pub mod scope;
pub mod semantics;
pub mod topdecs;
