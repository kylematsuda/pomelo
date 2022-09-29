use crate::{ast::support, impl_ast_node, impl_ast_token, AstNode, SyntaxNode, SyntaxToken};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LongVId {
    pub syntax: SyntaxNode,
}

impl_ast_node!(LongVId, LONG_VID);

impl LongVId {
    pub fn vid(&self) -> Option<VId> {
        support::tokens(self.syntax()).next()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VId {
    pub syntax: SyntaxToken,
}

impl_ast_token!(VId, VID);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LongTyCon {
    pub syntax: SyntaxNode,
}

impl_ast_node!(LongTyCon, LONG_TY_CON);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LongStrId {
    pub syntax: SyntaxNode,
}

impl_ast_node!(LongStrId, LONG_STRID);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StrId {
    pub syntax: SyntaxToken,
}

impl_ast_token!(StrId, STRID);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label {
    pub syntax: SyntaxNode,
}

impl_ast_node!(Label, LAB);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyVar {
    syntax: SyntaxToken,
}

impl_ast_token!(TyVar, TYVAR);
