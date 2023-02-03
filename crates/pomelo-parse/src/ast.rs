//! Representation of the AST.
//!
//! This follows [`rowan::ast`]. The basic idea is to try to "cast" the nodes
//! of the concrete syntax tree (CST) to AST nodes based on their `SyntaxKind`.
//! This is achieved with the [`AstNode`] and [`AstToken`] traits in this module.
pub mod constants;
pub use constants::*;

pub mod bindings;
pub use bindings::*;

pub mod declarations;
pub use declarations::*;

pub mod expressions;
pub use expressions::*;

pub mod identifiers;
pub use identifiers::*;

pub mod matches;
pub use matches::*;

pub mod patterns;
pub use patterns::*;

pub mod type_expressions;
pub use type_expressions::*;

use crate::{SyntaxNode, SyntaxToken};
pub use rowan::ast::AstNode;

pub trait AstToken {
    fn cast(token: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;
}

pub type AstPtr<N> = rowan::ast::AstPtr<N>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct File {
    syntax: SyntaxNode,
}

crate::impl_ast_node!(File, FILE);

impl File {
    pub fn declarations(&self) -> impl Iterator<Item = crate::ast::Dec> {
        rowan::ast::support::children(self.syntax())
    }
}

#[macro_export]
macro_rules! impl_ast_node {
    ($target:ty, $kind:ident) => {
        impl $crate::AstNode for $target {
            type Language = $crate::language::SML;

            fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool
            where
                Self: Sized,
            {
                use $crate::SyntaxKind::*;
                kind == $kind
            }

            fn cast(node: $crate::SyntaxNode) -> Option<Self>
            where
                Self: Sized,
            {
                use $crate::SyntaxKind::*;
                match node.kind() {
                    $kind => Some(Self { syntax: node }),
                    _ => None,
                }
            }

            fn syntax(&self) -> &$crate::SyntaxNode {
                &self.syntax
            }
        }

        impl std::fmt::Display for $target {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", <Self as $crate::AstNode>::syntax(self))
            }
        }
    };
}

#[macro_export]
macro_rules! impl_ast_token {
    ($target:ty, $kind:ident) => {
        impl $crate::AstToken for $target {
            fn cast(node: $crate::SyntaxToken) -> Option<Self>
            where
                Self: Sized,
            {
                use $crate::SyntaxKind::*;
                match node.kind() {
                    $kind => Some(Self { syntax: node }),
                    _ => None,
                }
            }

            fn syntax(&self) -> &$crate::SyntaxToken {
                &self.syntax
            }
        }

        impl std::fmt::Display for $target {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", <Self as $crate::AstToken>::syntax(self))
            }
        }
    };
}

pub mod support {
    use crate::{language::SML, AstNode, AstToken, SyntaxElement, SyntaxNode};

    pub use rowan::ast::support::{child, token};

    pub fn tokens<N: AstToken>(parent: &SyntaxNode) -> impl Iterator<Item = N> {
        parent.children_with_tokens().filter_map(|c| match c {
            SyntaxElement::Token(token) => N::cast(token),
            _ => None,
        })
    }

    pub fn children<N: AstNode<Language = SML>>(parent: &SyntaxNode) -> impl Iterator<Item = N> {
        rowan::ast::support::children(parent)
    }
}

mod util {
    #[macro_export]
    macro_rules! impl_from {
        ($target:ty, $x:ident, $t:ty) => {
            impl From<$t> for $target {
                fn from(x: $t) -> Self {
                    Self::$x(x)
                }
            }
        };
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    fn check(should_error: bool, src: &str, expect: Expect) {
        use crate::{ast::File, AstNode, Parser};
        let parser = Parser::new(src);
        let tree = parser.parse();
        let file = File::cast(tree.syntax()).unwrap();

        let actual: String = file.declarations().map(|d| format!("{}", d)).collect();
        expect.assert_eq(&actual);

        assert_eq!(tree.has_errors(), should_error);
    }

    #[test]
    fn file_test() {
        check(
            false,
            "val a = b; fun f a = g a; type int = d",
            expect!["val a = b; fun f a = g a; type int = d"],
        )
    }
}
