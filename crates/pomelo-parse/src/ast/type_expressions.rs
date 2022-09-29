use crate::{ast, ast::support, impl_ast_node, AstNode, SyntaxKind, SyntaxNode};
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
    type Language = crate::language::SML;

    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, FUN_TY | TUPLE_TY_EXP | TY_CON | RECORD_TY | TYVAR_TY)
    }
    
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
        support::child(self.syntax())
    }

    pub fn ty_2(&self) -> Option<ast::Ty> {
        support::children(self.syntax()).skip(1).next()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleTy {
    syntax: SyntaxNode,
}

impl_ast_node!(TupleTy, TUPLE_TY_EXP);

impl TupleTy {
    pub fn tys(&self) -> impl Iterator<Item = ast::Ty> {
        support::children(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConsTy {
    syntax: SyntaxNode,
}

impl_ast_node!(ConsTy, TY_CON);

impl ConsTy {
    pub fn tys(&self) -> impl Iterator<Item = ast::Ty> {
        support::children(self.syntax())
    }

    pub fn longtycon(&self) -> Option<ast::LongTyCon> {
        support::child(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordTy {
    syntax: SyntaxNode,
}

impl_ast_node!(RecordTy, RECORD_TY);

impl RecordTy {
    pub fn tyrows(&self) -> impl Iterator<Item = ast::TyRow> {
        support::children(self.syntax())
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
        support::tokens(self.syntax()).next()
    }
}
