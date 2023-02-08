//! Lowering context.
use std::collections::HashMap;

use pomelo_parse::{ast, language::SML, AstNode, Error};

use crate::arena::Idx;
use crate::builtins;
use crate::lower::HirLower;
use crate::{
    AstId, Dec, DecKind, DefLoc, Expr, ExprKind, File, FileArena, FileArenaExt, FileAstIdx, Fixity,
    LongTyCon, LongVId, NameInterner, Pat, Ty, TyCon, VId,
};

/// Context needed while lowering.
///
/// This is used to do name resolution on the topdecs during lowering.
///
/// Maybe need to also have a `BodyLoweringCtxt` as well, just to keep track of weird nested infix stuff?
/// That seems like an annoying corner case...
#[derive(Debug, Clone)]
pub(crate) struct LoweringCtxt {
    res: Resolver,
    file: crate::File,
    errors: Vec<Error>, // TODO: define a new error type...
}

impl LoweringCtxt {
    pub fn new() -> Self {
        let res = Resolver::new();
        let file = crate::File::default();
        let errors = Vec::new();
        Self { res, file, errors }
    }

    pub(crate) fn lower_file(mut self, file: &ast::File) -> (crate::File, Vec<Error>) {
        for dec in file.declarations() {
            let index = Dec::lower(&mut self, dec);
            self.file_mut().topdecs_mut().push(index);
        }
        (self.file, self.errors)
    }

    pub(super) fn resolver(&self) -> &Resolver {
        &self.res
    }

    fn resolver_mut(&mut self) -> &mut Resolver {
        &mut self.res
    }

    pub(super) fn in_inner_scope<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        self.resolver_mut().enter_scope();
        let out = f(self);
        self.resolver_mut().exit_scope();
        out
    }

    fn file_mut(&mut self) -> &mut File {
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

    pub fn interner_mut(&mut self) -> &mut impl NameInterner {
        &mut self.file.arenas.name_interner
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

        // Updating the resolver
        self.register_bound_vids_dec(index);
        self.register_bound_tycons_dec(index);
        self.register_fixities(index);

        index
    }

    /// Register patterns in `val rec` declarations.
    ///
    /// This is needed because the expression in the binding can refer to the names in `pat`.
    pub fn register_rec_pat(&mut self, pat: Idx<Pat>, dec: Idx<Dec>) {
        let bound_vids = self.arenas().get_pat(pat).bound_vids(self.arenas());
        for v in bound_vids {
            // TODO: surface an error
            assert!(!v.is_builtin());
            self.resolver_mut().def_vid(v, DefLoc::Dec(dec));
        }
    }

    /// Register identifiers bound in the current `Dec`.
    pub fn register_bound_vids_dec(&mut self, index: Idx<Dec>) {
        let dec = self.arenas().get_dec(index);
        let bound_vids = dec.bound_vids(self.arenas());
        for v in bound_vids {
            // TODO: surface an error
            assert!(!v.is_builtin());
            self.resolver_mut().def_vid(v, DefLoc::Dec(index));
        }
    }

    /// Register type constructors bound in the current `Dec`.
    pub fn register_bound_tycons_dec(&mut self, index: Idx<Dec>) {
        let dec = self.arenas().get_dec(index);
        let bound_tycons = dec.bound_tycons(self.arenas());
        for v in bound_tycons {
            // TODO: surface an error
            assert!(!v.is_builtin());
            self.resolver_mut().def_ty(v, DefLoc::Dec(index));
        }
    }

    /// Register fixities bound in the current `Dec`.
    pub fn register_fixities(&mut self, index: Idx<Dec>) {
        let dec = self.arenas().get_dec(index);

        if let DecKind::Fixity { fixity, vids } = &dec.kind {
            let fixity = *fixity;
            let longvids = vids.iter().map(|v| LongVId::from(v.0)).collect::<Vec<_>>();
            for v in longvids {
                // TODO: this is actually allowed! But we should probably surface a warning.
                assert!(!v.is_builtin());
                self.resolver_mut().set_fixity(v, fixity);
            }
        }
    }

    /// Register identifiers declared in the the `Pat` of a pattern match.
    ///
    /// This needs to be done before lowering `Expr` on the RHS of the match,
    /// otherwise any names declared here will not be in scope in the `Expr`.
    pub fn register_pat_names_in_match(&mut self, index: Idx<Pat>) {
        let pat = self.arenas().get_pat(index);
        let bound_vids = pat.bound_vids(self.arenas());
        for v in bound_vids {
            // TODO: surface an error
            assert!(!v.is_builtin());
            self.resolver_mut().def_vid(v, DefLoc::Pat(index));
        }
    }

    /// Horrible hack to make while loop lowering work...
    pub fn fixup_vid_expr_ref(&mut self, expr: Idx<Expr>, loc: DefLoc) {
        let expr = self.arenas_mut().get_expr_mut(expr);
        if let ExprKind::VId {
            longvid: (_, defloc),
            ..
        } = &mut expr.kind
        {
            *defloc = loc;
        }
    }

    pub fn add_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.arenas_mut().alloc_expr(expr)
    }

    pub fn add_pat(&mut self, pat: Pat) -> Idx<Pat> {
        self.arenas_mut().alloc_pat(pat)
    }

    pub fn add_ty(&mut self, ty: Ty) -> Idx<Ty> {
        self.arenas_mut().alloc_ty(ty)
    }

    fn arenas_mut(&mut self) -> &mut impl FileArenaExt {
        &mut self.file.arenas
    }
}

/// Holds the results of early name resolution
#[derive(Debug, Clone)]
pub struct Resolver {
    // TODO: use this to track exception definitions too!
    values: ScopedMap<LongVId, DefLoc>,
    fixity: ScopedMap<LongVId, Fixity>,
    tys: ScopedMap<LongTyCon, DefLoc>,
}

impl Resolver {
    pub fn new() -> Self {
        let fixity = builtins::BUILTIN_INFIX
            .into_iter()
            .map(|(name, f)| (LongVId::from(VId::from_builtin(name)), f))
            .collect();
        let fixity = ScopedMap::from_map(fixity);

        let tys = builtins::BUILTIN_TYCONS
            .into_iter()
            .map(|name| (LongTyCon::from(TyCon::from_builtin(name)), DefLoc::Builtin))
            .collect();
        let tys = ScopedMap::from_map(tys);

        Self {
            values: ScopedMap::from_map(HashMap::new()),
            fixity,
            tys,
        }
    }

    pub fn def_vid(&mut self, vid: LongVId, loc: DefLoc) {
        self.values.insert(vid, loc);
    }

    pub fn def_ty(&mut self, ty: LongTyCon, loc: DefLoc) {
        self.tys.insert(ty, loc);
    }

    pub fn set_fixity(&mut self, vid: LongVId, fixity: Fixity) {
        if let Fixity::Nonfix = fixity {
            self.fixity.remove(&vid);
        } else {
            self.fixity.insert(vid, fixity);
        }
    }

    // TODO: add some kind of logging
    pub fn lookup_vid(&self, vid: &LongVId) -> DefLoc {
        self.values.lookup(vid).copied().unwrap_or(DefLoc::Missing)
    }

    // TODO: add some kind of logging
    pub fn lookup_ty(&self, ty: &LongTyCon) -> DefLoc {
        self.tys.lookup(ty).copied().unwrap_or(DefLoc::Missing)
    }

    pub fn lookup_fixity(&self, vid: &LongVId) -> Option<Fixity> {
        self.fixity.lookup(vid).copied()
    }

    pub fn enter_scope(&mut self) {
        self.values.enter_scope();
        self.tys.enter_scope();
        self.fixity.enter_scope();
    }

    pub fn exit_scope(&mut self) {
        self.values.exit_scope();
        self.tys.exit_scope();
        self.fixity.exit_scope();
    }
}

/// Store names in scope and handle rollback when exiting an inner scope.
#[derive(Debug, Clone)]
struct ScopedMap<K, V> {
    map: HashMap<K, V>,
    scope_stack: Vec<usize>,
    edits: Vec<(K, Option<V>)>,
}

impl<K, V> ScopedMap<K, V>
where
    K: Clone + PartialEq + Eq + std::hash::Hash,
    V: Clone,
{
    pub fn from_map(map: HashMap<K, V>) -> Self {
        Self {
            map,
            scope_stack: Vec::new(),
            edits: Vec::new(),
        }
    }

    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        let out = self.map.insert(k.clone(), v);
        self.edits.push((k, out.clone()));
        out
    }

    pub fn lookup(&self, k: &K) -> Option<&V> {
        self.map.get(k)
    }

    pub fn remove(&mut self, k: &K) {
        self.map.remove(k);
    }

    pub fn enter_scope(&mut self) {
        self.scope_stack.push(self.edits.len());
    }

    pub fn exit_scope(&mut self) {
        let scope_start = self.scope_stack.pop().unwrap();

        let revert = self.edits.split_off(scope_start);
        for (k, v) in revert.into_iter() {
            // Revert the edit, or remove the item if it wasn't previously in the map.
            if let Some(v) = v {
                self.map.insert(k, v);
            } else {
                self.map.remove(&k);
            }
        }
    }
}
