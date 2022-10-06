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

pub fn parse<'a>(input: &'a str) -> SyntaxTree {
    passes::apply_passes(Parser::new(input).parse())
}
