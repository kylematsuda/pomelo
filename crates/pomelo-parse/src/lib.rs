//! Parser for SML '97.
//!
//! Currently, this only covers the Core language (no support for modules).
//!
//! The design of this module is heavily influenced by
//! [`rust-analyzer/crates/parser`](https://github.com/rust-lang/rust-analyzer/tree/master/crates/parser),
//! and uses [`rowan`](https://docs.rs/rowan/latest/rowan/) to construct the syntax tree.
//! It is also influenced by
//! [`apollo_parser`](https://docs.rs/apollo-parser/latest/apollo_parser/), a GraphQL parser in
//! Rust that also draws heavily from `rust-analyzer`.
//!
//! TODO: Maybe add some examples here?

pub mod grammar;

pub mod syntax;
pub use syntax::SyntaxKind;

pub mod language;
pub use language::{
    SyntaxElement, SyntaxElementChildren, SyntaxNode, SyntaxNodeChildren, SyntaxNodePtr,
    SyntaxToken,
};

pub mod parser;
pub use parser::{Checkpoint, NodeGuard, Parser, SyntaxTree};

pub mod ast;
pub use ast::{AstNode, AstPtr, AstToken};

pub mod passes;

#[cfg(test)]
pub(crate) mod tests;

use thiserror::Error;

/// An parsing error.
#[derive(Debug, Clone, Error, PartialEq, Eq)]
#[error("Error@{pos}: {msg}")]
pub struct Error {
    msg: String,
    text: String,
    pos: usize,
}

impl Error {
    pub fn new(msg: impl Into<String>, text: impl Into<String>, pos: usize) -> Self {
        Self {
            msg: msg.into(),
            text: text.into(),
            pos,
        }
    }
}

/// Lex and parse an input string.
///
/// Eventually, this can also apply validation passes
/// (currently, `passes::apply_passes` does nothing).
pub fn parse(input: &str) -> SyntaxTree {
    let tree = Parser::new(input).parse();
    passes::apply_passes(&tree);
    tree
}
