//! HIR definitions for the Core language constructs
//!
//! Should [`Body`] be specialized to only [`Expr`]s?

use crate::arena::{Arena, Idx};
use crate::identifiers::{NameInterner, NameInternerImpl};
use crate::topdecs::{AstIdMap, FileArena};
use pomelo_parse::{ast, language::SML, AstNode, AstPtr};

pub mod lower;
pub mod pretty;

use crate::body::lower::HirLower;
use crate::{Dec, Expr, FileAstIdx, Pat, Type};

#[cfg(test)]
mod tests;

/// Analog of r-a's hir_def::Body
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    arenas: BodyArenaImpl<NameInternerImpl>,

    // The actual outermost dec(s)
    //
    // `TopDec` maps to each real (syntactic) topdec.
    // This is similar to `ItemTree` in r-a.
    // However, when lowing to HIR (`Dec`, etc.), it makes sense to split multiple
    // semantic declarations into their own `Dec` instances.
    // For example, "val a = b and c = d" represents a single `TopDec`, but two
    // `Dec`s.
    dec: Idx<Dec>,
}

impl Body {
    pub fn from_syntax(dec: ast::Dec) -> Self {
        let mut arenas = BodyArenaImpl::default();
        let dec = Dec::lower(dec, &mut arenas);
        Self { arenas, dec }
    }

    pub fn arena(&self) -> &impl BodyArena {
        &self.arenas
    }

    pub fn dec(&self) -> Idx<Dec> {
        self.dec
    }

    pub fn topdec(&self) -> &Dec {
        self.arenas.get_dec(self.dec)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct BodyArenaImpl<NameInterner> {
    pub(crate) pats: Arena<Pat>,
    pub(crate) exprs: Arena<Expr>,

    // Inner decs, as in a "let ... in ... end" expr
    pub(crate) decs: Arena<Dec>,
    pub(crate) tys: Arena<Type>,

    pub(crate) name_interner: NameInterner,
    pub(crate) ast_map: AstIdMap,
}

pub trait BodyArena: FileArena {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat>;
    fn get_pat(&self, index: Idx<Pat>) -> &Pat;

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr>;
    fn get_expr(&self, index: Idx<Expr>) -> &Expr;

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec>;
    fn get_dec(&self, index: Idx<Dec>) -> &Dec;

    fn alloc_ty(&mut self, ty: Type) -> Idx<Type>;
    fn get_ty(&self, index: Idx<Type>) -> &Type;
}

impl<I: NameInterner> NameInterner for BodyArenaImpl<I> {
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

impl<I: NameInterner> FileArena for BodyArenaImpl<I> {
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
}

impl<I: NameInterner> BodyArena for BodyArenaImpl<I> {
    fn alloc_pat(&mut self, pat: Pat) -> Idx<Pat> {
        self.pats.alloc(pat)
    }

    fn get_pat(&self, index: Idx<Pat>) -> &Pat {
        self.pats.get(index)
    }

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.exprs.alloc(expr)
    }

    fn get_expr(&self, index: Idx<Expr>) -> &Expr {
        self.exprs.get(index)
    }

    fn alloc_dec(&mut self, dec: Dec) -> Idx<Dec> {
        self.decs.alloc(dec)
    }

    fn get_dec(&self, index: Idx<Dec>) -> &Dec {
        self.decs.get(index)
    }

    fn alloc_ty(&mut self, ty: Type) -> Idx<Type> {
        self.tys.alloc(ty)
    }

    fn get_ty(&self, index: Idx<Type>) -> &Type {
        self.tys.get(index)
    }
}
