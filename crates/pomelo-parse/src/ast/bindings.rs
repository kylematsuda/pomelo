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
        support::tokens(self.syntax())
    }

    pub fn tycon(&self) -> Option<ast::TyCon> {
        support::tokens(self.syntax()).next()
    }

    pub fn ty(&self) -> Option<ast::Ty> {
        support::child(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DataBind {
    syntax: SyntaxNode,
}

impl_ast_node!(DataBind, DATA_BIND);

impl DataBind {
    pub fn tyvarseq(&self) -> impl Iterator<Item = ast::TyVar> {
        support::tokens(self.syntax())
    }

    pub fn tycon(&self) -> Option<ast::TyCon> {
        support::tokens(self.syntax()).next()
    }

    pub fn conbinds(&self) -> impl Iterator<Item = ast::ConBind> {
        support::children(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConBind {
    syntax: SyntaxNode,
}

impl_ast_node!(ConBind, CON_BIND);

impl ConBind {
    pub fn op(&self) -> bool {
        support::token(self.syntax(), SyntaxKind::OP_KW).is_some()
    }

    pub fn vid(&self) -> Option<ast::VId> {
        support::tokens(self.syntax()).next()
    }

    pub fn ty(&self) -> Option<ast::Ty> {
        support::child(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExBind {
    syntax: SyntaxNode,
}

impl_ast_node!(ExBind, EX_BIND);

// FIXME: Figure out how to do <op> correctly
impl ExBind {
    pub fn vid(&self) -> Option<ast::VId> {
        support::tokens(self.syntax()).next()
    }

    pub fn ty(&self) -> Option<ast::Ty> {
        support::child(self.syntax())
    }

    pub fn eq(&self) -> bool {
        support::token(self.syntax(), SyntaxKind::EQ).is_some()
    }

    pub fn longvid(&self) -> Option<ast::LongVId> {
        support::child(self.syntax())
    }
}
