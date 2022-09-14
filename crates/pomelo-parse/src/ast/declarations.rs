use crate::{impl_ast_node, AstNode, SyntaxKind, SyntaxNode};
use SyntaxKind::*;

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Dec {
    Val(ValDec),
    Fun(FunDec),
    Type(TypeDec),
    Datatype(DatatypeDec),
    DatatypeRep(DatatypeRepDec),
    Abstype(AbstypeDec),
    Exception(ExceptionDec),
    Local(LocalDec),
    Open(OpenDec),
    Seq(SeqDec),
    Infix(InfixDec),
    Infixr(InfixrDec),
    Nonfix(NonfixDec),
}

impl AstNode for Dec {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        let out = match node.kind() {
            VAL_DEC => Self::Val(ValDec::cast(node)?),
            FUN_DEC => Self::Fun(FunDec::cast(node)?),
            TY_DEC => Self::Type(TypeDec::cast(node)?),
            DATATYPE_DEC => Self::Datatype(DatatypeDec::cast(node)?),
            DATATYPE_REP => Self::DatatypeRep(DatatypeRepDec::cast(node)?),
            ABSTYPE_DEC => Self::Abstype(AbstypeDec::cast(node)?),
            EXCEPT_DEC => Self::Exception(ExceptionDec::cast(node)?),
            LOCAL_DEC => Self::Local(LocalDec::cast(node)?),
            OPEN_DEC => Self::Open(OpenDec::cast(node)?),
            SEQ_DEC => Self::Seq(SeqDec::cast(node)?),
            INFIX_DEC => Self::Infix(InfixDec::cast(node)?),
            INFIXR_DEC => Self::Infixr(InfixrDec::cast(node)?),
            NONFIX_DEC => Self::Nonfix(NonfixDec::cast(node)?),
            _ => return None,
        };
        Some(out)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Val(inner) => inner.syntax(),
            Self::Fun(inner) => inner.syntax(),
            Self::Type(inner) => inner.syntax(),
            Self::Datatype(inner) => inner.syntax(),
            Self::DatatypeRep(inner) => inner.syntax(),
            Self::Abstype(inner) => inner.syntax(),
            Self::Exception(inner) => inner.syntax(),
            Self::Local(inner) => inner.syntax(),
            Self::Open(inner) => inner.syntax(),
            Self::Seq(inner) => inner.syntax(),
            Self::Infix(inner) => inner.syntax(),
            Self::Infixr(inner) => inner.syntax(),
            Self::Nonfix(inner) => inner.syntax(),
        }
    }
}

impl fmt::Display for Dec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValDec {
    syntax: SyntaxNode,
}

impl_ast_node!(ValDec, VAL_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunDec {
    syntax: SyntaxNode,
}

impl_ast_node!(FunDec, FUN_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDec {
    syntax: SyntaxNode,
}

impl_ast_node!(TypeDec, TY_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DatatypeDec {
    syntax: SyntaxNode,
}

impl_ast_node!(DatatypeDec, DATATYPE_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DatatypeRepDec {
    syntax: SyntaxNode,
}

impl_ast_node!(DatatypeRepDec, DATATYPE_REP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbstypeDec {
    syntax: SyntaxNode,
}

impl_ast_node!(AbstypeDec, ABSTYPE_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExceptionDec {
    syntax: SyntaxNode,
}

impl_ast_node!(ExceptionDec, EXCEPT_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalDec {
    syntax: SyntaxNode,
}

impl_ast_node!(LocalDec, LOCAL_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OpenDec {
    syntax: SyntaxNode,
}

impl_ast_node!(OpenDec, OPEN_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SeqDec {
    syntax: SyntaxNode,
}

impl_ast_node!(SeqDec, SEQ_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InfixDec {
    syntax: SyntaxNode,
}

impl_ast_node!(InfixDec, INFIX_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InfixrDec {
    syntax: SyntaxNode,
}

impl_ast_node!(InfixrDec, INFIXR_DEC);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NonfixDec {
    syntax: SyntaxNode,
}

impl_ast_node!(NonfixDec, NONFIX_DEC);
