use crate::{ast, impl_ast_node, AstChildren, AstNode, SyntaxNode};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match {
    pub syntax: SyntaxNode,
}

impl_ast_node!(Match, MATCH);

impl Match {
    pub fn mrules(&self) -> AstChildren<ast::Mrule> {
        self.get_nodes()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Mrule {
    pub syntax: SyntaxNode,
}

impl_ast_node!(Mrule, MRULE);

impl Mrule {
    pub fn pat(&self) -> Option<ast::Pat> {
        self.get_node()
    }

    pub fn expr(&self) -> Option<ast::Expr> {
        self.get_node()
    }
}
