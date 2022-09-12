use crate::{impl_ast_node, AstNode, SyntaxKind, SyntaxNode};
use SyntaxKind::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Fn(FnExpr),
    Case(CaseExpr),
    While(WhileExpr),
    If(IfExpr),
    Raise(RaiseExpr),
    Handle(HandleExpr),
    OrElse(OrElseExpr),
    AndAlso(AndAlsoExpr),
    Typed(TypedExpr),
    Infix(InfixExpr),
    Application(ApplicationExpr),
    Atomic(AtomicExpr),
}

impl AstNode for Expr {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        let out = match node.kind() {
            FN_EXP => Self::Fn(FnExpr { syntax: node }),
            CASE_MATCH_EXP => Self::Case(CaseExpr { syntax: node }),
            WHILE_EXP => Self::While(WhileExpr { syntax: node }),
            IF_EXP => Self::If(IfExpr { syntax: node }),
            RAISE_EXP => Self::Raise(RaiseExpr { syntax: node }),
            HANDLE_EXP => Self::Handle(HandleExpr { syntax: node }),
            ORELSE_EXP => Self::OrElse(OrElseExpr { syntax: node }),
            ANDALSO_EXP => Self::AndAlso(AndAlsoExpr { syntax: node }),
            TY_EXP => Self::Typed(TypedExpr { syntax: node }),
            INFIX_EXP => Self::Infix(InfixExpr { syntax: node }),
            APP_EXP => Self::Application(ApplicationExpr { syntax: node }),
            AT_EXP => Self::Atomic(AtomicExpr { syntax: node }),
            _ => return None,
        };
        Some(out)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Fn(inner) => inner.syntax(),
            Self::Case(inner) => inner.syntax(),
            Self::While(inner) => inner.syntax(),
            Self::If(inner) => inner.syntax(),
            Self::Raise(inner) => inner.syntax(),
            Self::Handle(inner) => inner.syntax(),
            Self::OrElse(inner) => inner.syntax(),
            Self::AndAlso(inner) => inner.syntax(),
            Self::Typed(inner) => inner.syntax(),
            Self::Infix(inner) => inner.syntax(),
            Self::Application(inner) => inner.syntax(),
            Self::Atomic(inner) => inner.syntax(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(FnExpr, FN_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CaseExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(CaseExpr, CASE_MATCH_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhileExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(WhileExpr, WHILE_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(IfExpr, IF_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RaiseExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(RaiseExpr, RAISE_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HandleExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(HandleExpr, HANDLE_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OrElseExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(OrElseExpr, ORELSE_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AndAlsoExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(AndAlsoExpr, ANDALSO_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(TypedExpr, TY_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InfixExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(InfixExpr, INFIX_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ApplicationExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(ApplicationExpr, APP_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AtomicExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(AtomicExpr, AT_EXP);
