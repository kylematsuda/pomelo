use crate::{SyntaxNode, SyntaxToken};

pub mod bindings;
pub mod declarations;
pub mod expressions;
pub mod identifiers;
pub mod matches;
pub mod patterns;
pub mod type_expressions;

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

#[macro_export]
macro_rules! impl_ast_node {
    ($target:ty, $kind:ident) => {
        impl crate::AstNode for $target {
            fn cast(node: crate::SyntaxNode) -> Option<Self>
            where
                Self: Sized,
            {
                use crate::SyntaxKind::*;
                match node.kind() {
                    $kind => Some(Self { syntax: node }),
                    _ => None,
                }
            }

            fn syntax(&self) -> &crate::SyntaxNode {
                &self.syntax
            }
        }
    };
}
