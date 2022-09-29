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

// pub trait AstNode {
//     fn cast(node: SyntaxNode) -> Option<Self>
//     where
//         Self: Sized;
//
//     fn syntax(&self) -> &SyntaxNode;
//
//     fn token(&self, kind: crate::SyntaxKind) -> Option<SyntaxToken> {
//         self.syntax()
//             .children_with_tokens()
//             .filter_map(|it| it.into_token())
//             .find(|it| it.kind() == kind)
//     }
//
//     fn get_nodes<N: AstNode>(&self) -> AstChildren<N> {
//         AstChildren::new(&self.syntax())
//     }
//
//     fn get_node<N: AstNode>(&self) -> Option<N> {
//         self.get_nodes().next()
//     }
//
//     fn get_tokens<T: AstToken>(&self) -> AstChildrenTokens<T> {
//         AstChildrenTokens::new(&self.syntax())
//     }
//
//     fn get_token<T: AstToken>(&self) -> Option<T> {
//         self.get_tokens().next()
//     }
// }

pub trait AstToken {
    fn cast(token: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;
}

// /// c.f. rust-analyzer/crates/syntax/src/ast.rs
// #[derive(Debug, Clone)]
// pub struct AstChildren<N> {
//     inner: SyntaxNodeChildren,
//     ph: PhantomData<N>,
// }
//
// impl<N: AstNode> AstChildren<N> {
//     fn new(parent: &SyntaxNode) -> Self {
//         AstChildren {
//             inner: parent.children(),
//             ph: PhantomData,
//         }
//     }
// }
//
// impl<N: AstNode> Iterator for AstChildren<N> {
//     type Item = N;
//     fn next(&mut self) -> Option<Self::Item> {
//         self.inner.find_map(N::cast)
//     }
// }
//

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
