//! Lowering context.
use std::collections::HashMap;

use pomelo_parse::{ast, language::SML, AstNode, AstPtr};

use crate::arena::Idx;
use crate::body::FileArena;
use crate::identifiers::{LongTyCon, LongVId, NameInterner};
use crate::{AstId, Dec, DecKind, DefLoc, Expr, File, FileAstIdx, Pat};

/// Context needed while lowering.
///
/// This is used to do name resolution on the topdecs during lowering.
///
/// Maybe need to also have a `BodyLoweringCtxt` as well, just to keep track of weird nested infix stuff?
/// That seems like an annoying corner case...
#[derive(Debug, Default, Clone)]
pub struct LoweringCtxt {
    res: Resolver,
    file: crate::File,
}

impl LoweringCtxt {
    pub fn resolver(&self) -> &Resolver {
        &self.res
    }

    pub fn resolver_mut(&mut self) -> &mut Resolver {
        &mut self.res
    }

    pub fn enter_scope(&mut self, mut f: impl FnMut(&mut Self)) {
        // This is probably super inefficient...
        let saved_resolver = self.res.clone();
        f(self);
        self.res = saved_resolver;
    }

    pub fn file(&self) -> &File {
        &self.file
    }

    pub fn file_mut(&mut self) -> &mut File {
        &mut self.file
    }

    pub fn arenas(&self) -> &impl FileArena {
        &self.file.arenas
    }

    pub fn alloc_ast_id<N>(&mut self, ast: &N) -> FileAstIdx<N>
    where
        N: AstNode<Language = SML>,
    {
        self.arenas_mut().alloc_ast_id(ast)
    }

    pub fn get_ast_id<N>(&self, index: FileAstIdx<N>) -> Option<AstPtr<N>>
    where
        N: AstNode<Language = SML>,
    {
        self.arenas().get_ast_id(index)
    }

    pub fn get_ast_span<N>(&self, index: FileAstIdx<N>) -> Option<(usize, usize)>
    where
        N: AstNode<Language = SML>,
    {
        self.arenas().get_ast_span(index)
    }

    pub fn interner_mut(&mut self) -> &mut impl NameInterner {
        &mut self.file.arenas.name_interner
    }

    pub fn bound_names(&self, pat: Idx<Pat>) -> Vec<LongVId> {
        self.arenas().get_pat(pat).bound_vids(self)
    }

    /// This is needed because some of the dec can have recursive bindings.
    ///
    /// For these, we need to preallocate the Dec node so we can give its index out before
    /// lowering interior pats, exprs, etc.
    pub fn make_rec_dec(
        &mut self,
        ast_id: AstId<ast::Dec>,
        f: impl FnOnce(&mut Self, Idx<Dec>) -> DecKind,
    ) -> Idx<Dec> {
        let dec = Dec {
            kind: DecKind::Missing,
            ast_id,
        };
        let index = self.arenas_mut().alloc_dec(dec);
        let mut kind = f(self, index);

        let dec = self.arenas_mut().get_dec_mut(index);
        std::mem::swap(&mut dec.kind, &mut kind);
        self.register_bound_vids_dec(index);

        index
    }

    pub fn push_dec(&mut self, dec: Dec) -> Idx<Dec> {
        let index = self.arenas_mut().alloc_dec(dec);
        self.register_bound_vids_dec(index);
        index
    }

    pub fn register_rec_pat(&mut self, pat: Idx<Pat>, dec: Idx<Dec>) {
        let bound_vids = self.arenas().get_pat(pat).bound_vids(self);
        for v in bound_vids {
            self.resolver_mut().def_value(v, DefLoc::Dec(dec));
        }
    }

    pub fn register_bound_vids_dec(&mut self, index: Idx<Dec>) {
        let dec = self.arenas().get_dec(index);
        let bound_vids = dec.bound_vids(self);
        for v in bound_vids {
            self.resolver_mut().def_value(v, DefLoc::Dec(index));
        }
    }

    pub fn push_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.arenas_mut().alloc_expr(expr)
    }

    pub fn push_pat(&mut self, pat: Pat) -> Idx<Pat> {
        self.arenas_mut().alloc_pat(pat)
    }

    fn arenas_mut(&mut self) -> &mut impl FileArena {
        &mut self.file.arenas
    }
}

/// Holds the results of early name resolution
#[derive(Debug, Default, Clone)]
pub struct Resolver {
    values: HashMap<LongVId, DefLoc>,
    tys: HashMap<LongTyCon, DefLoc>,
}

impl Resolver {
    pub fn def_value(&mut self, vid: LongVId, loc: DefLoc) {
        self.values.insert(vid, loc);
    }

    pub fn def_ty(&mut self, ty: LongTyCon, loc: DefLoc) {
        self.tys.insert(ty, loc);
    }

    pub fn lookup_value(&self, vid: &LongVId) -> Option<DefLoc> {
        self.values.get(vid).copied()
    }

    pub fn lookup_ty(&self, ty: &LongTyCon) -> Option<DefLoc> {
        self.tys.get(ty).copied()
    }
}
