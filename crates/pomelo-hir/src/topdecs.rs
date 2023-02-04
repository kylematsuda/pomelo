//! Items visible from outside the file (e.g., via a "use" directive)
//!
//! Is this even necessary...?
use crate::arena::{Arena, Idx};
use crate::identifiers::NameInterner;
use crate::{FileAstIdx, FileData};
use pomelo_parse::{
    language::{SyntaxNodePtr, SML},
    AstNode, AstPtr,
};
use std::collections::HashMap;
use std::marker::PhantomData;

// pub mod lower;

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
        self.backmap.insert(syntax, index);

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
