# pomelo

[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://kylematsuda.github.io/pomelo/pomelo/)
[![](https://github.com/kylematsuda/pomelo/workflows/Test/badge.svg)](https://github.com/kylematsuda/pomelo/actions?query=workflow:Test)
[![](https://img.shields.io/badge/license-MIT-green)](https://github.com/kylematsuda/pomelo/blob/main/LICENSE.md)

`pomelo` is a (hobby, WIP) lexer and parser for Standard ML (SML) '97 implemented in Rust.
My goal is to eventually extend this to implement a Language Server and interpreter.
This is extremely rough and incomplete and I'm learning a lot as I go!

## Name

A pomelo is a fun (big!) citrus fruit, which also happens to have "ML" in its name.

## Why SML?

[SML](https://en.wikipedia.org/wiki/Standard_ML) is a statically-typed functional language (with some imperative constructs) and was a precursor to later ML-family languages like OCaml and Haskell.

The ML family has a lot of really cool stuff:
- Hindley-Milner type inference, polymorphism, etc
- Pattern matching
- Module system

so lots of opportunities for me to learn (/get really confused)!

For now, I don't plan to touch modules or imperative stuff (see [Scope](https://github.com/kylematsuda/pomelo#scope) below).
However, just trying to implement the Core semantics is plenty for me at this point.

### Some SML resources

The language definition and standard library:
- [The Definition of Standard ML (Revised '97)](https://smlfamily.github.io/sml97-defn.pdf)
- [SML Basis Library](https://smlfamily.github.io/Basis/index.html) 

SML compilers:
- [SML of New Jersey](https://github.com/smlnj/smlnj)
- [MLton](https://github.com/MLton/mlton)

Here are some similar (but much more complete) projects by others:
- [SOSML (online SML interpreter)](https://github.com/SOSML/SOSML)
- [Millet (SML language server)](https://github.com/azdavis/millet)
- [SomewhatML (SML compiler)](https://github.com/SomewhatML/sml-compiler)

## Scope

Core SML language (so no modules). Also, no imperative stuff except for maybe basic I/O (so no arrays or references).

## General design

At this stage, the design is heavily influenced by [`rust-analyzer`](https://github.com/rust-lang/rust-analyzer). This is mainly because:
1. I'd like to eventually turn this into a language server, and
2. Rust is probably the language that I read best, and the code in `rust-analyzer` seemed a little more approachable than `rustc` initially.

### Lexer

[`pomelo-lex`](https://github.com/kylematsuda/pomelo/tree/main/crates/pomelo-lex) contains a very basic lexer, very influenced by [`rustc_lexer`](https://github.com/rust-lang/rust/blob/master/compiler/rustc_lexer).

### Parser 

[`pomelo-parse`](https://github.com/kylematsuda/pomelo/tree/main/crates/pomelo-parse) creates a concrete syntax tree.
This is modeled off of [`rust-analyzer`'s parser](https://github.com/rust-lang/rust-analyzer/tree/master/crates/parser) and also uses [`rowan`](https://docs.rs/rowan/latest/rowan/) to create the concrete and abstract/typed syntax tree.

### HIR

[`pomelo-hir`](https://github.com/kylematsuda/pomelo/tree/main/crates/pomelo-hir) defines the high-level intermediate representation (HIR).
The HIR is very similar to the AST, except all of the derived forms (see Appendix A of the Definition) are desugared to their more basic equivalent form (similar to how loops, etc. are desugared away in `rustc`'s [HIR](https://rustc-dev-guide.rust-lang.org/hir.html)).
Pomelo's HIR is represented as a graph stored in an arena (essentially a wrapper around a `Vec`, see `pomelo-hir::arena` or [`la_arena`](https://docs.rs/la-arena/latest/la_arena/)).

This module also contains the code for lowering from the AST.
Eventually, this will also contain some semantic analysis, probably something like `rustc`'s [`TyCtxt`](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/struct.TyCtxt.html) but way more basic.
Currently, I do not plan to use [`salsa`](https://github.com/salsa-rs/salsa) or any other kind of fancy system for caching completed queries, but this could be added later for more fun/learning/perf.
