use crate::{impl_ast_node, SyntaxNode};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValBind {
    syntax: SyntaxNode,
}

impl_ast_node!(ValBind, VAL_BIND);

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
