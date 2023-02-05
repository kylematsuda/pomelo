//! HIR definitions for the Core language constructs
//!
//! Should [`Body`] be specialized to only [`Expr`]s?

// pub mod lower;
pub mod pretty;

use std::collections::HashMap;
use std::marker::PhantomData;

use pomelo_parse::{
    language::{SyntaxNodePtr, SML},
    AstNode, AstPtr,
};

use crate::arena::{Arena, Idx};
use crate::identifiers::NameInterner;
use crate::{Dec, Expr, FileAstIdx, Pat, Type};

// #[cfg(test)]
// mod tests;

pub trait FileArena: NameInterner {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat>;
    fn get_pat(&self, index: Idx<Pat>) -> &Pat;
    fn get_pat_mut(&mut self, index: Idx<Pat>) -> &mut Pat;

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr>;
    fn get_expr(&self, index: Idx<Expr>) -> &Expr;
    fn get_expr_mut(&mut self, index: Idx<Expr>) -> &mut Expr;

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec>;
    fn get_dec(&self, index: Idx<Dec>) -> &Dec;
    fn get_dec_mut(&mut self, index: Idx<Dec>) -> &mut Dec;

    fn alloc_ty(&mut self, ty: Type) -> Idx<Type>;
    fn get_ty(&self, index: Idx<Type>) -> &Type;
    fn get_ty_mut(&mut self, index: Idx<Type>) -> &mut Type;

    fn alloc_ast_id<N>(&mut self, ast: &N) -> FileAstIdx<N>
    where
        N: AstNode<Language = SML>;

    fn get_ast_id<N>(&self, index: FileAstIdx<N>) -> Option<AstPtr<N>>
    where
        N: AstNode<Language = SML>;

    fn get_ast_span<N>(&self, index: FileAstIdx<N>) -> Option<(usize, usize)>
    where
        N: AstNode<Language = SML>;
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct FileArenaImpl<NameInterner> {
    pub(crate) pats: Arena<Pat>,
    pub(crate) exprs: Arena<Expr>,

    // Inner decs, as in a "let ... in ... end" expr
    pub(crate) decs: Arena<Dec>,
    pub(crate) tys: Arena<Type>,

    pub(crate) name_interner: NameInterner,
    pub(crate) ast_map: AstIdMap,
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

    /// FIXME: figure out how to properly handle text spans; this is duct tape for now
    pub fn get_span<N>(&self, index: FileAstIdx<N>) -> Option<(usize, usize)>
    where
        N: AstNode<Language = SML>,
    {
        self.get(index)
            .map(|id| id.syntax_node_ptr().text_range())
            .map(|r| (r.start().into(), r.end().into()))
    }
}

impl<I: NameInterner> NameInterner for FileArenaImpl<I> {
    fn fresh(&mut self) -> u32 {
        self.name_interner.fresh()
    }

    fn alloc(&mut self, s: &str) -> Idx<String> {
        self.name_interner.alloc(s)
    }

    fn get(&self, index: Idx<String>) -> &str {
        self.name_interner.get(index)
    }
}

impl<I: NameInterner> FileArena for FileArenaImpl<I> {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat> {
        self.pats.alloc(pat)
    }

    fn get_pat(&self, index: Idx<Pat>) -> &Pat {
        self.pats.get(index)
    }

    fn get_pat_mut(&mut self, index: Idx<Pat>) -> &mut Pat {
        self.pats.get_mut(index)
    }

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.exprs.alloc(expr)
    }

    fn get_expr(&self, index: Idx<Expr>) -> &Expr {
        self.exprs.get(index)
    }

    fn get_expr_mut(&mut self, index: Idx<Expr>) -> &mut Expr {
        self.exprs.get_mut(index)
    }

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec> {
        self.decs.alloc(dec)
    }

    fn get_dec(&self, index: Idx<Dec>) -> &Dec {
        self.decs.get(index)
    }

    fn get_dec_mut(&mut self, index: Idx<Dec>) -> &mut Dec {
        self.decs.get_mut(index)
    }

    fn alloc_ty(&mut self, ty: Type) -> Idx<Type> {
        self.tys.alloc(ty)
    }

    fn get_ty(&self, index: Idx<Type>) -> &Type {
        self.tys.get(index)
    }

    fn get_ty_mut(&mut self, index: Idx<Type>) -> &mut Type {
        self.tys.get_mut(index)
    }

    fn alloc_ast_id<N>(&mut self, ast: &N) -> FileAstIdx<N>
    where
        N: AstNode<Language = SML>,
    {
        self.ast_map.alloc(ast)
    }

    fn get_ast_id<N>(&self, index: FileAstIdx<N>) -> Option<AstPtr<N>>
    where
        N: AstNode<Language = SML>,
    {
        self.ast_map.get(index)
    }

    fn get_ast_span<N>(&self, index: FileAstIdx<N>) -> Option<(usize, usize)>
    where
        N: AstNode<Language = SML>,
    {
        self.ast_map.get_span(index)
    }
}
