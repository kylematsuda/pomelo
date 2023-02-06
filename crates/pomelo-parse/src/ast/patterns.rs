//! AST nodes for patterns.
use crate::{
    ast, ast::support, impl_ast_node, impl_from, language::SyntaxNode, AstNode, SyntaxKind,
};
use SyntaxKind::*;

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Layered(LayeredPat),
    Typed(TypedPat),
    ConsOrInfix(ConsOrInfixPat),
    Atomic(AtomicPat),
}

impl AstNode for Pat {
    type Language = crate::language::SML;

    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, LAYERED_PAT | TY_PAT | INFIX_OR_APP_PAT) || AtomicPat::can_cast(kind)
    }

    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        let out = match node.kind() {
            LAYERED_PAT => Self::Layered(LayeredPat::cast(node)?),
            TY_PAT => Self::Typed(TypedPat::cast(node)?),
            INFIX_OR_APP_PAT => Self::ConsOrInfix(ConsOrInfixPat::cast(node)?),
            _ => Self::Atomic(AtomicPat::cast(node)?),
        };
        Some(out)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Layered(inner) => inner.syntax(),
            Self::Typed(inner) => inner.syntax(),
            Self::ConsOrInfix(inner) => inner.syntax(),
            Self::Atomic(inner) => inner.syntax(),
        }
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.syntax())
    }
}

impl_from!(Pat, Layered, LayeredPat);
impl_from!(Pat, Typed, TypedPat);
impl_from!(Pat, ConsOrInfix, ConsOrInfixPat);
impl_from!(Pat, Atomic, AtomicPat);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LayeredPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(LayeredPat, LAYERED_PAT);

impl LayeredPat {
    pub fn pats(&self) -> impl Iterator<Item = ast::Pat> {
        support::children(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(TypedPat, TY_PAT);

impl TypedPat {
    pub fn pat(&self) -> Option<ast::Pat> {
        support::child(self.syntax())
    }

    pub fn ty(&self) -> Option<ast::Ty> {
        support::child(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConsOrInfixPat {
    syntax: SyntaxNode,
}

impl_ast_node!(ConsOrInfixPat, INFIX_OR_APP_PAT);

impl ConsOrInfixPat {
    pub fn pats(&self) -> impl Iterator<Item = ast::Pat> {
        support::children(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AtomicPat {
    Wildcard(WildcardPat),
    SCon(SConPat),
    VId(VIdPat),
    Record(RecordPat),
    Unit(UnitPat),
    Tuple(TuplePat),
    List(ListPat),
}

impl AstNode for AtomicPat {
    type Language = crate::language::SML;

    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            WILDCARD_PAT | SCON_PAT | VID_PAT | RECORD_PAT | UNIT_PAT | TUPLE_PAT | LIST_PAT
        )
    }

    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        let out = match node.kind() {
            WILDCARD_PAT => Self::Wildcard(WildcardPat::cast(node)?),
            SCON_PAT => Self::SCon(SConPat::cast(node)?),
            VID_PAT => Self::VId(VIdPat::cast(node)?),
            RECORD_PAT => Self::Record(RecordPat::cast(node)?),
            UNIT_PAT => Self::Unit(UnitPat::cast(node)?),
            TUPLE_PAT => Self::Tuple(TuplePat::cast(node)?),
            LIST_PAT => Self::List(ListPat::cast(node)?),
            _ => return None,
        };
        Some(out)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Wildcard(inner) => inner.syntax(),
            Self::SCon(inner) => inner.syntax(),
            Self::VId(inner) => inner.syntax(),
            Self::Record(inner) => inner.syntax(),
            Self::Unit(inner) => inner.syntax(),
            Self::Tuple(inner) => inner.syntax(),
            Self::List(inner) => inner.syntax(),
        }
    }
}

impl fmt::Display for AtomicPat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.syntax())
    }
}

impl_from!(AtomicPat, Wildcard, WildcardPat);
impl_from!(AtomicPat, SCon, SConPat);
impl_from!(AtomicPat, VId, VIdPat);
impl_from!(AtomicPat, Record, RecordPat);
impl_from!(AtomicPat, Unit, UnitPat);
impl_from!(AtomicPat, Tuple, TuplePat);
impl_from!(AtomicPat, List, ListPat);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WildcardPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(WildcardPat, WILDCARD_PAT);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SConPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(SConPat, SCON_PAT);

impl SConPat {
    pub fn scon(&self) -> Option<ast::Scon> {
        support::tokens(self.syntax()).next()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VIdPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(VIdPat, VID_PAT);

impl VIdPat {
    pub fn op(&self) -> bool {
        support::token(self.syntax(), OP_KW).is_some()
    }

    pub fn longvid(&self) -> Option<ast::LongVId> {
        support::child(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(RecordPat, RECORD_PAT);

impl RecordPat {
    // Need to figure out what's actually happening with the various
    // patrow forms
    pub fn patrows(&self) -> impl Iterator<Item = ast::PatRow> {
        support::children(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatRow {
    syntax: SyntaxNode,
}

impl_ast_node!(PatRow, PAT_ROW);

impl PatRow {
    pub fn pat(&self) -> Option<ast::Pat> {
        support::child(self.syntax())
    }

    pub fn label(&self) -> Option<ast::Label> {
        support::tokens(self.syntax()).next()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnitPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(UnitPat, UNIT_PAT);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TuplePat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(TuplePat, TUPLE_PAT);

impl TuplePat {
    pub fn pats(&self) -> impl Iterator<Item = ast::Pat> {
        support::children(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ListPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(ListPat, LIST_PAT);

impl ListPat {
    pub fn pats(&self) -> impl Iterator<Item = ast::Pat> {
        support::children(self.syntax())
    }
}
