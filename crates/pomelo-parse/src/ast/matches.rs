//! AST nodes for matches.
use crate::{ast, ast::support, impl_ast_node, AstNode, SyntaxNode};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match {
    pub syntax: SyntaxNode,
}

impl_ast_node!(Match, MATCH);

impl Match {
    pub fn mrules(&self) -> impl Iterator<Item = ast::Mrule> {
        support::children(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Mrule {
    pub syntax: SyntaxNode,
}

impl_ast_node!(Mrule, MRULE);

impl Mrule {
    pub fn pat(&self) -> Option<ast::Pat> {
        support::child(self.syntax())
    }

    pub fn expr(&self) -> Option<ast::Expr> {
        support::child(self.syntax())
    }
}
