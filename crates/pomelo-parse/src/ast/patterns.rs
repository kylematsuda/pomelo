use crate::{impl_ast_node, AstNode, AstToken, AstChildren, SyntaxKind, SyntaxNode, ast};
use SyntaxKind::*;

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Layered(LayeredPat),
    Typed(TypedPat),
    ConsInfix(ConsInfixPat),
    Cons(ConsPat),
    Atomic(AtomicPat),
}

impl AstNode for Pat {
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        let out = match node.kind() {
            LAYERED_PAT => Self::Layered(LayeredPat::cast(node)?),
            TY_PAT => Self::Typed(TypedPat::cast(node)?),
            INFIX_CONS_PAT => Self::ConsInfix(ConsInfixPat::cast(node)?),
            CONS_PAT => Self::Cons(ConsPat::cast(node)?),
            _ => Self::Atomic(AtomicPat::cast(node)?),
        };
        Some(out)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Layered(inner) => inner.syntax(),
            Self::Typed(inner) => inner.syntax(),
            Self::ConsInfix(inner) => inner.syntax(),
            Self::Cons(inner) => inner.syntax(),
            Self::Atomic(inner) => inner.syntax(),
        }
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LayeredPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(LayeredPat, LAYERED_PAT);

impl LayeredPat {
    pub fn op(&self) -> bool {
        self.token(OP_KW).is_some()
    }

    pub fn vid(&self) -> Option<ast::VId> {
        self.token(VID).and_then(ast::VId::cast)
    }

    pub fn ty(&self) -> Option<ast::Ty> {
        self.syntax.children().find_map(ast::Ty::cast)
    }

    pub fn pat(&self) -> Option<ast::Pat> {
        self.syntax.children().find_map(ast::Pat::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(TypedPat, TY_PAT);

impl TypedPat {
    pub fn pat(&self) -> Option<ast::Pat> {
        self.syntax.children().find_map(ast::Pat::cast)
    }

    pub fn ty(&self) -> Option<ast::Ty> {
        self.syntax.children().find_map(ast::Ty::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConsInfixPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(ConsInfixPat, INFIX_CONS_PAT);

impl ConsInfixPat {
    pub fn pat_1(&self) -> Option<ast::Pat> {
        self.syntax.children().find_map(ast::Pat::cast)
    }

    pub fn vid(&self) -> Option<ast::VId> {
        self.token(VID).and_then(ast::VId::cast)
    }

    pub fn pat_2(&self) -> Option<ast::Pat> {
        self.syntax.children().filter_map(ast::Pat::cast).skip(1).next()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConsPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(ConsPat, CONS_PAT);

impl ConsPat {
    pub fn op(&self) -> bool {
        self.token(OP_KW).is_some()
    }

    pub fn longvid(&self) -> Option<ast::LongVId> {
        self.syntax.children().find_map(ast::LongVId::cast)
    }

    pub fn atpat(&self) -> Option<ast::AtomicPat> {
        self.syntax.children().find_map(ast::AtomicPat::cast)
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
    pub fn int(&self) -> Option<ast::Int> {
        self.token(INT).and_then(ast::Int::cast)
    }

    pub fn real(&self) -> Option<ast::Real> {
        self.token(REAL).and_then(ast::Real::cast)
    }

    pub fn word(&self) -> Option<ast::Word> {
        self.token(WORD).and_then(ast::Word::cast)
    }

    pub fn char(&self) -> Option<ast::Char> {
        self.token(CHAR).and_then(ast::Char::cast)
    }

    pub fn string(&self) -> Option<ast::String> {
        self.token(STRING).and_then(ast::String::cast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VIdPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(VIdPat, VID_PAT);

impl VIdPat {
    pub fn op(&self) -> bool {
        self.token(OP_KW).is_some()
    }

    pub fn vid(&self) -> Option<ast::VId> {
        self.token(VID).and_then(ast::VId::cast)
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
    pub fn patrows(&self) -> AstChildren<ast::PatRow> {
        AstChildren::new(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatRow {
    syntax: SyntaxNode,
}

impl_ast_node!(PatRow, PAT_ROW);

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
    pub fn pats(&self) -> AstChildren<ast::Pat> {
        AstChildren::new(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ListPat {
    pub syntax: SyntaxNode,
}

impl_ast_node!(ListPat, LIST_PAT);

impl ListPat {
    pub fn pats(&self) -> AstChildren<ast::Pat> {
        AstChildren::new(&self.syntax)
    }
}
