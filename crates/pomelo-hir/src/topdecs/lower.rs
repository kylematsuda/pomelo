use crate::arena::{Arena, Idx};
use crate::core::{lower::HirLower, BodyArena, BodyArenaImpl, Pat, PatKind};
use crate::identifiers::{LongVId, StrId, TyCon, VId};
use crate::topdecs::{CoreDec, File, FileArena, FileData, TopDec};
use pomelo_parse::ast;

impl File {
    pub fn from_syntax(node: ast::File) -> Self {
        let mut decs = vec![];
        let mut data = FileData::default();

        for dec in node.declarations() {
            decs.push(TopDec::Core(CoreDec::from_syntax(dec, &mut data)));
        }

        Self {
            decs,
            data: Box::new(data),
        }
    }
}

#[derive(Default, Debug, Clone)]
struct NameOnlyArena {
    vids: Arena<VId>,
    strids: Arena<StrId>,
    tycons: Arena<TyCon>,
}

impl NameOnlyArena {
    fn new(arena: BodyArenaImpl) -> Self {
        Self {
            vids: arena.vids,
            strids: arena.strids,
            tycons: arena.tycons,
        }
    }

    fn map_vid<A: FileArena>(&self, index: Idx<VId>, other_arena: &mut A) -> Idx<VId> {
        let vid = self.vids.get(index).expect("index is valid");
        other_arena.alloc_vid(vid.clone())
    }

    fn map_strid<A: FileArena>(&self, index: Idx<StrId>, other_arena: &mut A) -> Idx<StrId> {
        let strid = self.strids.get(index).expect("index is valid");
        other_arena.alloc_strid(strid.clone())
    }

    fn map_tycon<A: FileArena>(&self, index: Idx<TyCon>, other_arena: &mut A) -> Idx<TyCon> {
        let tycon = self.tycons.get(index).expect("index is valid");
        other_arena.alloc_tycon(tycon.clone())
    }

    fn map_longvid<A: FileArena>(&self, longvid: LongVId, other_arena: &mut A) -> LongVId {
        let LongVId { strids, vid } = longvid;
        let strids = strids
            .into_iter()
            .map(|s| self.map_strid(*s, other_arena))
            .collect();
        let vid = self.map_vid(vid, other_arena);
        LongVId { strids, vid }
    }
}

impl CoreDec {
    fn from_syntax<A: FileArena>(node: ast::Dec, arena: &mut A) -> Self {
        match node {
            ast::Dec::Val(v) => Self::lower_val(v, arena),
            ast::Dec::Fun(f) => Self::lower_fun(f, arena),
            ast::Dec::Type(t) => Self::lower_ty(t, arena),
            ast::Dec::Datatype(d) => Self::lower_datatype(d, arena),
            ast::Dec::DatatypeRep(r) => Self::lower_replication(r, arena),
            ast::Dec::Abstype(a) => Self::lower_abstype(a, arena),
            ast::Dec::Exception(e) => Self::lower_exception(e, arena),
            ast::Dec::Local(l) => Self::lower_local(l, arena),
            ast::Dec::Open(o) => Self::lower_open(o, arena),
            ast::Dec::Infix(i) => Self::lower_infix(i, arena),
            ast::Dec::Infixr(i) => Self::lower_infixr(i, arena),
            ast::Dec::Nonfix(n) => Self::lower_nonfix(n, arena),
            ast::Dec::Seq(s) => Self::lower_seq(s, arena),
        }
    }

    fn lower_val<A: FileArena>(node: ast::ValDec, arena: &mut A) -> Self {
        let mut vids = vec![];

        for valbind in node.bindings() {
            if let Some(pat) = valbind.pat() {
                vids.extend_from_slice(&Self::lower_pat_names(pat, arena));
            }
        }

        let ast_id = arena.alloc_ast_id(&node);

        Self::Val {
            names: vids.into_boxed_slice(),
            ast_id,
        }
    }

    fn lower_pat_names<A: FileArena>(pat: ast::Pat, arena: &mut A) -> Vec<LongVId> {
        let mut body_arena = BodyArenaImpl::default();
        let p = Pat::lower(pat, &mut body_arena);

        let mut names = vec![];
        Self::collect_pat_names(p, &mut body_arena, &mut names);

        let dummy_arena = NameOnlyArena::new(body_arena);

        names
            .iter_mut()
            .map(|name| dummy_arena.map_longvid(name.clone(), arena))
            .collect()
    }

    fn collect_pat_names<A: BodyArena>(pat: Idx<Pat>, arena: &A, names: &mut Vec<LongVId>) {
        if let Some(pat) = arena.get_pat(pat) {
            match &pat.kind {
                PatKind::VId { longvid, .. } => names.push(longvid.clone()),
                PatKind::Record { .. } => {}
                PatKind::Infix { lhs, vid: _, rhs } => {
                    Self::collect_pat_names(*lhs, arena, names);
                    Self::collect_pat_names(*rhs, arena, names);
                }
                PatKind::Typed { pat, .. } => {
                    Self::collect_pat_names(*pat, arena, names);
                }
                PatKind::Constructed { pat, .. } => {
                    Self::collect_pat_names(*pat, arena, names);
                }
                PatKind::Layered { vid, pat, .. } => {
                    names.push(LongVId {
                        strids: Box::new([]),
                        vid: *vid,
                    });
                    Self::collect_pat_names(*pat, arena, names);
                }
                PatKind::Missing | PatKind::Nil | PatKind::Wildcard | PatKind::Scon(_) => {}
            }
        }
    }

    fn lower_fun<A: FileArena>(_node: ast::FunDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_ty<A: FileArena>(_node: ast::TypeDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_datatype<A: FileArena>(_node: ast::DatatypeDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_replication<A: FileArena>(_node: ast::DatatypeRepDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_abstype<A: FileArena>(_node: ast::AbstypeDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_exception<A: FileArena>(_node: ast::ExceptionDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_local<A: FileArena>(_node: ast::LocalDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_open<A: FileArena>(_node: ast::OpenDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_infix<A: FileArena>(_node: ast::InfixDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_infixr<A: FileArena>(_node: ast::InfixrDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_nonfix<A: FileArena>(_node: ast::NonfixDec, _arena: &mut A) -> Self {
        todo!()
    }

    fn lower_seq<A: FileArena>(_node: ast::SeqDec, _arena: &mut A) -> Self {
        todo!()
    }
}
