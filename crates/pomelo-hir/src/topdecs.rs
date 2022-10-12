//! Items visible from outside the file (e.g., via a "use" directive)
//!
//! Is this even necessary...?
use crate::arena::{Arena, Idx};
use crate::identifiers::{LongStrId, LongVId, StrId, TyCon, VId};
use pomelo_parse::{ast, language::SML, AstNode, AstPtr, SyntaxNodePtr};
use std::collections::HashMap;
use std::marker::PhantomData;

pub mod lower;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct File {
    pub(crate) decs: Vec<TopDec>,
    pub(crate) data: Box<FileData>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct FileData {
    pub vids: Arena<VId>,
    pub strids: Arena<StrId>,
    pub tycons: Arena<TyCon>,
    pub sources: AstIdMap,
}

// See r-a hir_expand
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct AstIdMap {
    arena: Arena<SyntaxNodePtr>,
    backmap: HashMap<SyntaxNodePtr, Idx<SyntaxNodePtr>>,
}

impl AstIdMap {
    pub fn alloc<N: AstNode<Language = SML>>(&mut self, ast: &N) -> FileAstIdx<N> {
        let astptr = AstPtr::new(ast);
        let syntax = astptr.syntax_node_ptr();

        let index = self.arena.alloc(syntax.clone());
        self.backmap.insert(syntax, index.clone());

        FileAstIdx {
            index,
            _ph: PhantomData,
        }
    }

    pub fn get<N: AstNode<Language = SML>>(&self, index: FileAstIdx<N>) -> Option<AstPtr<N>> {
        self.arena
            .get(index.index)
            .map(Clone::clone)
            .and_then(SyntaxNodePtr::cast)
    }
}

pub trait FileArena {
    fn alloc_vid(&mut self, vid: VId) -> Idx<VId>;
    fn get_vid(&self, index: Idx<VId>) -> Option<&VId>;

    fn alloc_strid(&mut self, strid: StrId) -> Idx<StrId>;
    fn get_strid(&self, index: Idx<StrId>) -> Option<&StrId>;

    fn alloc_tycon(&mut self, tycon: TyCon) -> Idx<TyCon>;
    fn get_tycon(&self, index: Idx<TyCon>) -> Option<&TyCon>;

    fn alloc_ast_id<N>(&mut self, ast: &N) -> FileAstIdx<N>
    where
        N: AstNode<Language = SML>;
    fn get_ast_id<N>(&mut self, index: FileAstIdx<N>) -> Option<AstPtr<N>>
    where
        N: AstNode<Language = SML>;
}

impl FileArena for FileData {
    fn alloc_vid(&mut self, vid: VId) -> Idx<VId> {
        self.vids.alloc(vid)
    }

    fn get_vid(&self, index: Idx<VId>) -> Option<&VId> {
        self.vids.get(index)
    }

    fn alloc_strid(&mut self, strid: StrId) -> Idx<StrId> {
        self.strids.alloc(strid)
    }

    fn get_strid(&self, index: Idx<StrId>) -> Option<&StrId> {
        self.strids.get(index)
    }

    fn alloc_tycon(&mut self, tycon: TyCon) -> Idx<TyCon> {
        self.tycons.alloc(tycon)
    }

    fn get_tycon(&self, index: Idx<TyCon>) -> Option<&TyCon> {
        self.tycons.get(index)
    }

    fn alloc_ast_id<N>(&mut self, ast: &N) -> FileAstIdx<N>
    where
        N: AstNode<Language = SML>,
    {
        self.sources.alloc(ast)
    }

    fn get_ast_id<N>(&mut self, index: FileAstIdx<N>) -> Option<AstPtr<N>>
    where
        N: AstNode<Language = SML>,
    {
        self.sources.get(index)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FileAstIdx<N> {
    index: Idx<SyntaxNodePtr>,
    _ph: PhantomData<fn() -> N>,
}

impl<N> Clone for FileAstIdx<N> {
    fn clone(&self) -> Self {
        Self { index: self.index, _ph: self._ph }
    }
}

impl<N> Copy for FileAstIdx<N> {}

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
