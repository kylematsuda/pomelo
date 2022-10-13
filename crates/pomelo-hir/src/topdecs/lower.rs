use crate::arena::Idx;
use crate::core::{lower::HirLower, BodyArena, BodyArenaImpl, Pat, PatKind};
use crate::identifiers::{LongVId, Name, NameInterner, NameInternerImpl, StrId, TyCon, VId};
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
struct NameOnlyArena<I> {
    interner: I,
}

impl<I: NameInterner> NameOnlyArena<I> {
    fn new(arena: BodyArenaImpl<I>) -> Self {
        Self {
            interner: arena.name_interner,
        }
    }

    fn map_vid<A: NameInterner>(&mut self, vid: VId, other_arena: &mut A) -> VId {
        match vid {
            VId::Name(Name::String(index)) => {
                let s = other_arena.get(index);
                VId::Name(Name::String(self.interner.alloc(s)))
            }
            _ => vid,
        }
    }

    fn map_strid<A: NameInterner>(&mut self, strid: StrId, other_arena: &mut A) -> StrId {
        match strid {
            StrId::Name(Name::String(index)) => {
                let s = other_arena.get(index);
                StrId::Name(Name::String(self.interner.alloc(s)))
            }
            _ => strid,
        }
    }

    fn _map_tycon<A: NameInterner>(&mut self, tycon: TyCon, other_arena: &mut A) -> TyCon {
        match tycon {
            TyCon::Name(Name::String(index)) => {
                let s = other_arena.get(index);
                TyCon::Name(Name::String(self.interner.alloc(s)))
            }
            _ => tycon,
        }
    }

    fn map_longvid<A: FileArena>(&mut self, longvid: LongVId, other_arena: &mut A) -> LongVId {
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
        let mut body_arena = BodyArenaImpl::<NameInternerImpl>::default();
        let p = Pat::lower(pat, &mut body_arena);

        let mut names = vec![];
        Self::collect_pat_names(p, &mut body_arena, &mut names);

        // FIXME: doubt that this makes sense
        let mut dummy_arena = NameOnlyArena::new(body_arena);

        names
            .iter_mut()
            .map(|name| dummy_arena.map_longvid(name.clone(), arena))
            .collect()
    }

    fn collect_pat_names<A: BodyArena>(pat: Idx<Pat>, arena: &A, names: &mut Vec<LongVId>) {
        match &arena.get_pat(pat).kind {
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
            PatKind::Missing | PatKind::Wildcard | PatKind::Scon(_) => {}
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
