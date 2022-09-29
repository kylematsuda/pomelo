use crate::{ast, ast::support, impl_ast_node, AstNode, SyntaxNode};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValBind {
    syntax: SyntaxNode,
}

impl_ast_node!(ValBind, VAL_BIND);

impl ValBind {
    pub fn pat(&self) -> Option<ast::Pat> {
        support::child(self.syntax())
    }

    pub fn expr(&self) -> Option<ast::Expr> {
        support::child(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FvalBind {
    syntax: SyntaxNode,
}

impl_ast_node!(FvalBind, FVAL_BIND);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyBind {
    syntax: SyntaxNode,
}

impl_ast_node!(TyBind, TY_BIND);
