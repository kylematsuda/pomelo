use crate::{ast, ast::support, impl_ast_node, AstNode, SyntaxKind, SyntaxNode};

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

    pub fn rec(&self) -> bool {
        support::token(self.syntax(), SyntaxKind::REC_KW).is_some()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FvalBind {
    syntax: SyntaxNode,
}

impl_ast_node!(FvalBind, FVAL_BIND);

impl FvalBind {
    pub fn rows(&self) -> impl Iterator<Item = FvalBindRow> {
        support::children(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FvalBindRow {
    syntax: SyntaxNode,
}

impl_ast_node!(FvalBindRow, FVAL_BIND_ROW);

impl FvalBindRow {
    pub fn op(&self) -> bool {
        support::token(self.syntax(), SyntaxKind::OP_KW).is_some()
    }

    pub fn atpats(&self) -> impl Iterator<Item = ast::AtomicPat> {
        support::children(self.syntax())
    }

    pub fn ty(&self) -> Option<ast::Ty> {
        support::child(self.syntax())
    }

    pub fn expr(&self) -> Option<ast::Expr> {
        support::child(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyBind {
    syntax: SyntaxNode,
}

impl_ast_node!(TyBind, TY_BIND);

impl TyBind {
    pub fn tyvarseq(&self) -> impl Iterator<Item = ast::TyVar> {
        support::tokens(&self.syntax())
    }

    pub fn tycon(&self) -> Option<ast::TyCon> {
        support::tokens(&self.syntax()).next()
    }

    pub fn ty(&self) -> Option<ast::Ty> {
        support::child(&self.syntax())
    }
}
