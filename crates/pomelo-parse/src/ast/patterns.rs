use crate::{impl_ast_node, AstNode, SyntaxKind, SyntaxNode};
use SyntaxKind::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Layered(LayeredPat),
    Typed(TypedPat),
    ConsInfix(ConsInfixPat),
    Cons(ConsPat),
    Atomic(AtomicPat),
}

impl AstNode for Pat {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        let out = match node.kind() {
            LAYERED_PAT => Self::Layered(LayeredPat { syntax: node }),
            TY_PAT => Self::Typed(TypedPat { syntax: node }),
            INFIX_CONS_PAT => Self::ConsInfix(ConsInfixPat { syntax: node }),
            CONS_PAT => Self::Cons(ConsPat { syntax: node }),
            AT_PAT => Self::Atomic(AtomicPat { syntax: node }),
            _ => return None,
        };
        Some(out)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Layered(inner) => inner.syntax(),
            Self::Typed(inner) => inner.syntax(),
            Self::ConsInfix(inner) => inner.syntax(),
            Self::Cons(inner) => inner.syntax(),
            Self::Atomic(inner) => inner.syntax(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LayeredPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(LayeredPat, LAYERED_PAT);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(TypedPat, TY_PAT);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConsInfixPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(ConsInfixPat, INFIX_CONS_PAT);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConsPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(ConsPat, CONS_PAT);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AtomicPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(AtomicPat, AT_PAT);
