use crate::{impl_ast_node, AstNode, SyntaxKind, SyntaxNode};
use SyntaxKind::*;

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
            FUN_TY_EXP => Self::Fun(FunTy { syntax: node }),
            TUPLE_TY_EXP => Self::Tuple(TupleTy { syntax: node }),
            TY_CON_EXP => Self::Cons(ConsTy { syntax: node }),
            RECORD_TY_EXP => Self::Record(RecordTy { syntax: node }),
            TY_VAR => Self::TyVar(TyVarTy { syntax: node }),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunTy {
    syntax: SyntaxNode,
}

impl_ast_node!(FunTy, FUN_TY_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleTy {
    syntax: SyntaxNode,
}

impl_ast_node!(TupleTy, TUPLE_TY_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConsTy {
    syntax: SyntaxNode,
}

impl_ast_node!(ConsTy, TY_CON_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordTy {
    syntax: SyntaxNode,
}

impl_ast_node!(RecordTy, RECORD_TY_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyVarTy {
    syntax: SyntaxNode,
}

impl_ast_node!(TyVarTy, TY_VAR);
