//! Items visible from outside the file (e.g., via a "use" directive)
//!
//! Is this even necessary...?
use crate::arena::{Arena, Idx};
use crate::core::TopDecBody;
use crate::identifiers::{LongStrId, LongVId, NameInterner, NameInternerImpl, TyCon, VId};
use pomelo_parse::{ast, language::SML, AstNode, AstPtr, SyntaxNodePtr};
use std::collections::HashMap;
use std::marker::PhantomData;

pub mod lower;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct File {
    pub(crate) decs: Vec<TopDec>,
    pub(crate) data: Box<FileData<NameInternerImpl>>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct FileData<I> {
    pub interner: I,
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
        let ptr = self.arena.get(index.index).clone();
        SyntaxNodePtr::cast(ptr)
    }
}

pub trait FileArena: NameInterner {
    fn alloc_ast_id<N>(&mut self, ast: &N) -> FileAstIdx<N>
    where
        N: AstNode<Language = SML>;
    fn get_ast_id<N>(&self, index: FileAstIdx<N>) -> Option<AstPtr<N>>
    where
        N: AstNode<Language = SML>;

    /// FIXME: figure out how to properly handle text spans; this is duct tape for now
    fn get_ast_span<N>(&self, index: FileAstIdx<N>) -> Option<(usize, usize)>
    where
        N: AstNode<Language = SML>,
    {
        self.get_ast_id(index)
            .map(|id| id.syntax_node_ptr().text_range())
            .map(|r| (r.start().into(), r.end().into()))
    }
}

impl<I: NameInterner> NameInterner for FileData<I> {
    fn fresh(&mut self) -> u32 {
        self.interner.fresh()
    }

    fn alloc(&mut self, s: &str) -> Idx<String> {
        self.interner.alloc(s)
    }

    fn get(&self, index: Idx<String>) -> &str {
        self.interner.get(index)
    }
}

impl<I: NameInterner> FileArena for FileData<I> {
    fn alloc_ast_id<N>(&mut self, ast: &N) -> FileAstIdx<N>
    where
        N: AstNode<Language = SML>,
    {
        self.sources.alloc(ast)
    }

    fn get_ast_id<N>(&self, index: FileAstIdx<N>) -> Option<AstPtr<N>>
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
        Self {
            index: self.index,
            _ph: self._ph,
        }
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
        body: Option<TopDecBody>,
    },
    Ty {
        tycons: Box<[TyCon]>,
        ast_id: FileAstIdx<ast::TypeDec>,
        body: Option<TopDecBody>,
    },
    Datatype {
        databinds: Box<[DataBind]>,
        ast_id: FileAstIdx<ast::DatatypeDec>,
        body: Option<TopDecBody>,
    },
    Replication {
        tycon: TyCon,
        ast_id: FileAstIdx<ast::DatatypeRepDec>,
        body: Option<TopDecBody>,
    },
    Abstype {
        databind: DataBind,
        decnames: Box<[CoreDec]>,
        ast_id: FileAstIdx<ast::AbstypeDec>,
        body: Option<TopDecBody>,
    },
    Exception {
        exbinds: Box<[ExBind]>,
        ast_id: FileAstIdx<ast::ExceptionDec>,
        body: Option<TopDecBody>,
    },
    Local {
        decnames: Box<[CoreDec]>,
        ast_id: FileAstIdx<ast::LocalDec>,
        body: Option<TopDecBody>,
    },
    Open {
        strids: Box<[LongStrId]>,
        ast_id: FileAstIdx<ast::OpenDec>,
        body: Option<TopDecBody>,
    },
    Infix {
        vids: Box<[VId]>,
        prec: Option<u8>,
        ast_id: FileAstIdx<ast::InfixDec>,
        body: Option<TopDecBody>,
    },
    Infixr {
        vids: Box<[VId]>,
        prec: Option<u8>,
        ast_id: FileAstIdx<ast::InfixrDec>,
        body: Option<TopDecBody>,
    },
    Nonfix {
        vids: Box<[VId]>,
        ast_id: FileAstIdx<ast::NonfixDec>,
        body: Option<TopDecBody>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataBind {
    pub tycon: TyCon,
    pub cons: Box<[VId]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExBind {
    Name { vid: VId },
    Rebind { lhs: VId, rhs: LongVId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleDec {
    Structure {},
    Signature {},
    Functor {},
}
