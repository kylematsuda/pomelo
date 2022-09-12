use crate::{impl_ast_node, SyntaxNode};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match {
    pub syntax: SyntaxNode,
}

impl_ast_node!(Match, MATCH);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Mrule {
    pub syntax: SyntaxNode,
}

impl_ast_node!(Mrule, MRULE);
