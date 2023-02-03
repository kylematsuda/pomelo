//! Boilerplate for making the [`rowan`](https://docs.rs/rowan/latest/rowan/) stuff work.
//!
//! This module is public because it is needed by consumers of the AST
//! (i.e., `pomelo-hir`).
use crate::SyntaxKind;

pub type SyntaxNode = rowan::SyntaxNode<SML>;
pub type SyntaxToken = rowan::SyntaxToken<SML>;
pub type SyntaxElement = rowan::SyntaxElement<SML>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<SML>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<SML>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<SML>;

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SML {}

impl rowan::Language for SML {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::FILE as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}
