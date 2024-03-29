//! AST nodes for identifiers.
use crate::{
    ast::support,
    impl_ast_node, impl_ast_token,
    language::{SyntaxNode, SyntaxToken},
    AstNode,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LongVId {
    pub syntax: SyntaxNode,
}

impl_ast_node!(LongVId, LONG_VID);

impl LongVId {
    pub fn strids(&self) -> impl Iterator<Item = StrId> {
        support::tokens(self.syntax())
    }

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

impl LongTyCon {
    pub fn strids(&self) -> impl Iterator<Item = StrId> {
        support::tokens(self.syntax())
    }

    pub fn tycon(&self) -> Option<TyCon> {
        support::tokens(self.syntax()).next()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyCon {
    pub syntax: SyntaxToken,
}

impl_ast_token!(TyCon, TY_CON);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LongStrId {
    pub syntax: SyntaxNode,
}

impl_ast_node!(LongStrId, LONG_STRID);

impl LongStrId {
    pub fn strids(&self) -> impl Iterator<Item = StrId> {
        support::tokens(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StrId {
    pub syntax: SyntaxToken,
}

impl_ast_token!(StrId, STRID);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label {
    pub syntax: SyntaxToken,
}

impl_ast_token!(Label, LAB);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyVar {
    syntax: SyntaxToken,
}

impl_ast_token!(TyVar, TYVAR);
