use crate::{ast, impl_ast_node, AstChildren, AstNode, SyntaxKind, SyntaxNode};
use SyntaxKind::*;

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Fun(FunTy),
    Tuple(TupleTy),
    Cons(ConsTy),
    Record(RecordTy),
    TyVar(TyVarTy),
}

impl AstNode for Ty {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        let out = match node.kind() {
            FUN_TY => Self::Fun(FunTy { syntax: node }),
            TUPLE_TY_EXP => Self::Tuple(TupleTy { syntax: node }),
            TY_CON => Self::Cons(ConsTy { syntax: node }),
            RECORD_TY => Self::Record(RecordTy { syntax: node }),
            TYVAR_TY => Self::TyVar(TyVarTy { syntax: node }),
            _ => return None,
        };
        Some(out)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Fun(inner) => inner.syntax(),
            Self::Tuple(inner) => inner.syntax(),
            Self::Cons(inner) => inner.syntax(),
            Self::Record(inner) => inner.syntax(),
            Self::TyVar(inner) => inner.syntax(),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunTy {
    syntax: SyntaxNode,
}

impl_ast_node!(FunTy, FUN_TY);

impl FunTy {
    pub fn ty_1(&self) -> Option<ast::Ty> {
        self.get_node()
    }

    pub fn ty_2(&self) -> Option<ast::Ty> {
        self.get_nodes().skip(1).next()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleTy {
    syntax: SyntaxNode,
}

impl_ast_node!(TupleTy, TUPLE_TY_EXP);

impl TupleTy {
    pub fn tys(&self) -> AstChildren<ast::Ty> {
        self.get_nodes()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConsTy {
    syntax: SyntaxNode,
}

impl_ast_node!(ConsTy, TY_CON);

impl ConsTy {
    pub fn tys(&self) -> AstChildren<ast::Ty> {
        self.get_nodes()
    }

    pub fn longtycon(&self) -> Option<ast::LongTyCon> {
        self.get_node()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordTy {
    syntax: SyntaxNode,
}

impl_ast_node!(RecordTy, RECORD_TY);

impl RecordTy {
    pub fn tyrows(&self) -> AstChildren<ast::TyRow> {
        self.get_nodes()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyRow {
    syntax: SyntaxNode,
}

impl_ast_node!(TyRow, TY_ROW);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyVarTy {
    syntax: SyntaxNode,
}

impl_ast_node!(TyVarTy, TYVAR_TY);

impl TyVarTy {
    pub fn tyvar(&self) -> Option<ast::TyVar> {
        self.get_token()
    }
}
