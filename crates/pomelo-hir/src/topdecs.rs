//! Items visible from outside the file (e.g., via a "use" directive)
//!
//! Is this even necessary...?
use crate::arena::{Arena, Idx};
use crate::core::{Dec, Expr, ExprIdx, Pat, TyRef};
use crate::identifiers::{NameData, Label, LongStrId, LongVId, TyCon, TyVar, VId};
use pomelo_parse::{ast, AstNode, SyntaxNodePtr};
use std::marker::PhantomData;
use std::collections::HashMap;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct File {
    pub(crate) decs: Vec<TopDec>,
    pub(crate) data: Box<FileData>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct FileData {
    names: NameData,
    sources: AstIdMap, 
}

// See r-a hir_expand
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct AstIdMap {
    arena: Arena<SyntaxNodePtr>,
    backmap: HashMap<SyntaxNodePtr, Idx<SyntaxNodePtr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileAstIdx<N: AstNode> {
    index: Idx<SyntaxNodePtr>,
    _ph: PhantomData<fn() -> N>,
}

/// Fundamental reuse unit is the TopDec
///
/// Why?
///     Typing in body of one TopDec should not invalidate others
///     Plays nice with repl (sequence of TopDecs)
///     Plays nice with "use" directive
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TopDec {
    Core(CoreDec),
    Module(ModuleDec),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CoreDec {
    Val {
        names: Box<[LongVId]>,
        ast_id: FileAstIdx<ast::ValDec>,
    },
    Ty {
        tycons: Box<[Idx<TyCon>]>,
        ast_id: FileAstIdx<ast::TypeDec>,
    },
    Datatype {
        databinds: Box<[DataBind]>,
        ast_id: FileAstIdx<ast::DatatypeDec>,
    },
    Replication {
        tycon: Idx<TyCon>,
        ast_id: FileAstIdx<ast::DatatypeRepDec>,
    },
    Abstype {
        databind: DataBind,
        decnames: Box<[CoreDec]>,
        ast_id: FileAstIdx<ast::AbstypeDec>,
    },
    Exception {
        exbinds: Box<[ExBind]>,
        ast_id: FileAstIdx<ast::ExceptionDec>,
    },
    Local {
        decnames: Box<[CoreDec]>,
        ast_id: FileAstIdx<ast::LocalDec>,
    },
    Open {
        strids: Box<[LongStrId]>,
        ast_id: FileAstIdx<ast::OpenDec>,
    },
    Infix {
        vids: Box<[Idx<VId>]>,
        prec: Option<u8>,
        ast_id: FileAstIdx<ast::InfixDec>,
    },
    Infixr {
        vids: Box<[Idx<VId>]>,
        prec: Option<u8>,
        ast_id: FileAstIdx<ast::InfixrDec>,
    },
    Nonfix {
        vids: Box<[Idx<VId>]>,
        prec: Option<u8>,
        ast_id: FileAstIdx<ast::NonfixDec>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataBind {
    pub tycon: Idx<TyCon>,
    pub cons: Box<[Idx<VId>]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExBind {
    Name { vid: Idx<VId> },
    Rebind { lhs: Idx<VId>, rhs: LongVId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleDec {
    Structure {},
    Signature {},
    Functor {},
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TopDecBody {
    pats: Arena<Pat>,
    exprs: Arena<Expr>,
    // Inner decs, as in a "let ... in ... end" expr
    decs: Arena<Dec>,
    tys: Arena<TyRef>,

    ids: NameData,
    labels: Arena<Label>,
    tyvars: Arena<TyVar>,

    // Visible results of the dec
    output: DecOutput,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecOutput {
    Val { expr: ExprIdx },
    Abstype { decs: Box<[Idx<Dec>]> }, // TODO
}
