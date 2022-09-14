use crate::{impl_ast_node, AstNode, SyntaxKind, SyntaxNode};
use SyntaxKind::*;

use std::fmt;

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
            FN_EXP => Self::Fn(FnExpr::cast(node)?),
            CASE_MATCH_EXP => Self::Case(CaseExpr::cast(node)?),
            WHILE_EXP => Self::While(WhileExpr::cast(node)?),
            IF_EXP => Self::If(IfExpr::cast(node)?),
            RAISE_EXP => Self::Raise(RaiseExpr::cast(node)?),
            HANDLE_EXP => Self::Handle(HandleExpr::cast(node)?),
            ORELSE_EXP => Self::OrElse(OrElseExpr::cast(node)?),
            ANDALSO_EXP => Self::AndAlso(AndAlsoExpr::cast(node)?),
            TY_EXP => Self::Typed(TypedExpr::cast(node)?),
            INFIX_EXP => Self::Infix(InfixExpr::cast(node)?),
            APP_EXP => Self::Application(ApplicationExpr::cast(node)?),
            _ => Self::Atomic(AtomicExpr::cast(node)?),
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.syntax())
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
pub enum AtomicExpr {
    SCon(SConExpr),
    VId(VIdExpr),
    Record(RecordExpr),
    RecSel(RecSelExpr),
    Unit(UnitExpr),
    Tuple(TupleExpr),
    List(ListExpr),
    Seq(SeqExpr),
    Let(LetExpr),
}

impl AstNode for AtomicExpr {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        let out = match node.kind() {
            SCON_EXP => Self::SCon(SConExpr::cast(node)?),
            VID_EXP => Self::VId(VIdExpr::cast(node)?),
            RECORD_EXP => Self::Record(RecordExpr::cast(node)?),
            RECORD_SEL_EXP => Self::RecSel(RecSelExpr::cast(node)?),
            UNIT_EXP => Self::Unit(UnitExpr::cast(node)?),
            TUPLE_EXP => Self::Tuple(TupleExpr::cast(node)?),
            LIST_EXP => Self::List(ListExpr::cast(node)?),
            SEQ_EXP => Self::Seq(SeqExpr::cast(node)?),
            LET_EXP => Self::Let(LetExpr::cast(node)?),
            _ => return None,
        };
        Some(out)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::SCon(inner) => inner.syntax(),
            Self::VId(inner) => inner.syntax(),
            Self::Record(inner) => inner.syntax(),
            Self::RecSel(inner) => inner.syntax(),
            Self::Unit(inner) => inner.syntax(),
            Self::Tuple(inner) => inner.syntax(),
            Self::List(inner) => inner.syntax(),
            Self::Seq(inner) => inner.syntax(),
            Self::Let(inner) => inner.syntax(),
        }
    }
}

impl fmt::Display for AtomicExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SConExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(SConExpr, SCON_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VIdExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(VIdExpr, VID_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(RecordExpr, RECORD_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecSelExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(RecSelExpr, RECORD_SEL_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnitExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(UnitExpr, UNIT_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(TupleExpr, TUPLE_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ListExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(ListExpr, LIST_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SeqExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(SeqExpr, SEQ_EXP);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetExpr {
    syntax: SyntaxNode,
}

impl_ast_node!(LetExpr, LET_EXP);