use crate::{impl_ast_node, SyntaxNode, AstNode, AstChildren, ast};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match {
    pub syntax: SyntaxNode,
}

impl_ast_node!(Match, MATCH);

impl Match {
    pub fn mrules(&self) -> AstChildren<ast::Mrule> {
        AstChildren::new(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Mrule {
    pub syntax: SyntaxNode,
}

impl_ast_node!(Mrule, MRULE);

impl Mrule {
    pub fn pat(&self) -> Option<ast::Pat> {
        self.syntax.children().find_map(ast::Pat::cast)
    }

    pub fn expr(&self) -> Option<ast::Expr> {
        self.syntax.children().find_map(ast::Expr::cast)
    }
}
