pub mod bindings;

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

use crate::{SyntaxNode, SyntaxToken, SyntaxNodeChildren};
use std::marker::PhantomData;

pub trait AstNode {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

pub trait AstToken {
    fn cast(token: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;
}

/// c.f. rust-analyzer/crates/syntax/src/ast.rs
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstChildren { inner: parent.children(), ph: PhantomData }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.find_map(N::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct File {
    syntax: SyntaxNode,
}

crate::impl_ast_node!(File, FILE);

impl File {
    pub fn declarations(&self) -> AstChildren<crate::ast::Dec> {
        AstChildren::new(&self.syntax) 
    }
}

#[macro_export]
macro_rules! impl_ast_node {
    ($target:ty, $kind:ident) => {
        impl $crate::AstNode for $target {
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

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    fn check(should_error: bool, src: &str, expect: Expect) {
        use crate::{AstNode, Parser, ast::File};
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
            expect!["val a = b; fun f a = g a; type int = d"]
        )
    }
}
