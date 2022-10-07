use crate::{ast, ast::support, impl_ast_node, AstNode, SyntaxKind, SyntaxNode, SyntaxToken};
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
    type Language = crate::language::SML;

    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            VAL_DEC
                | FUN_DEC
                | TY_DEC
                | DATATYPE_DEC
                | DATATYPE_REP
                | ABSTYPE_DEC
                | EXCEPT_DEC
                | LOCAL_DEC
                | OPEN_DEC
                | SEQ_DEC
                | INFIX_DEC
                | INFIXR_DEC
                | NONFIX_DEC
        )
    }

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

impl ValDec {
    pub fn tyvarseq(&self) -> impl Iterator<Item = ast::TyVar> {
        support::tokens(self.syntax())
    }

    pub fn bindings(&self) -> impl Iterator<Item = ast::ValBind> {
        support::children(self.syntax())
    }

    pub fn rec(&self) -> bool {
        support::token(self.syntax(), REC_KW).is_some()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunDec {
    syntax: SyntaxNode,
}

impl_ast_node!(FunDec, FUN_DEC);

impl FunDec {
    pub fn tyvarseq(&self) -> impl Iterator<Item = ast::TyVar> {
        support::tokens(self.syntax())
    }

    pub fn bindings(&self) -> impl Iterator<Item = ast::FvalBind> {
        support::children(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDec {
    syntax: SyntaxNode,
}

impl_ast_node!(TypeDec, TY_DEC);

impl TypeDec {
    pub fn bindings(&self) -> impl Iterator<Item = ast::TyBind> {
        support::children(self.syntax())
    }
}

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

impl SeqDec {
    pub fn declarations(&self) -> impl Iterator<Item = ast::Dec> {
        support::children(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InfixDec {
    syntax: SyntaxNode,
}

impl_ast_node!(InfixDec, INFIX_DEC);

impl InfixDec {
    pub fn fixity(&self) -> Option<ast::Fixity> {
        support::child(self.syntax())
    }

    pub fn vids(&self) -> impl Iterator<Item = ast::VId> {
        support::tokens(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InfixrDec {
    syntax: SyntaxNode,
}

impl_ast_node!(InfixrDec, INFIXR_DEC);

impl InfixrDec {
    pub fn fixity(&self) -> Option<ast::Fixity> {
        support::child(self.syntax())
    }

    pub fn vids(&self) -> impl Iterator<Item = ast::VId> {
        support::tokens(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fixity {
    syntax: SyntaxNode,
}

impl_ast_node!(Fixity, FIXITY);

impl Fixity {
    pub fn value(&self) -> u8 {
        support::token(self.syntax(), SyntaxKind::INT)
            .and_then(|s| s.text().parse().ok())
            .unwrap_or(0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NonfixDec {
    syntax: SyntaxNode,
}

impl_ast_node!(NonfixDec, NONFIX_DEC);

impl NonfixDec {
    pub fn vids(&self) -> impl Iterator<Item = ast::VId> {
        support::tokens(self.syntax())
    }
}
