use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::{infix, util, HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{
    AstId, Builtin, DecKind, DefLoc, FileArena, Label, LongVId, NodeParent, Pat, PatKind, PatRow,
    Scon, Ty, VId,
};

impl HirLower for Pat {
    type AstType = ast::Pat;

    fn lower(ctx: &mut LoweringCtxt, ast: Self::AstType) -> Idx<Self> {
        let kind = match &ast {
            ast::Pat::Atomic(p) => Self::lower_atomic(ctx, p),
            ast::Pat::Typed(p) => Self::lower_typed(ctx, p),
            ast::Pat::ConsOrInfix(p) => return infix::fix_infix(ctx, p),
            ast::Pat::Layered(p) => Self::lower_layered(ctx, p),
        };
        let ast_id = AstId::Node(ctx.alloc_ast_id(&ast));
        let p = Self { kind, ast_id };
        ctx.add_pat(p)
    }

    fn missing(ctx: &mut LoweringCtxt) -> Idx<Self> {
        let p = Self {
            kind: PatKind::Missing,
            ast_id: AstId::Missing,
        };
        ctx.add_pat(p)
    }
}

impl HirLowerGenerated for Pat {
    type Kind = PatKind;

    fn generated(ctx: &mut LoweringCtxt, origin: NodeParent, kind: Self::Kind) -> Idx<Self> {
        let p = Pat {
            kind,
            ast_id: AstId::Generated(origin),
        };
        ctx.add_pat(p)
    }
}

impl Pat {
    pub fn vid_builtin(b: Builtin) -> PatKind {
        PatKind::VId {
            op: false,
            longvid: (
                LongVId::from_vid(VId::from_builtin(b)),
                Some(DefLoc::Builtin),
            ),
        }
    }

    fn lower_atomic(ctx: &mut LoweringCtxt, p: &ast::AtomicPat) -> PatKind {
        match p {
            ast::AtomicPat::Wildcard(_) => PatKind::Wildcard,
            ast::AtomicPat::SCon(p) => Self::lower_scon(ctx, p),
            ast::AtomicPat::VId(p) => Self::lower_vid(ctx, p),
            ast::AtomicPat::List(p) => Self::lower_list(ctx, p),
            ast::AtomicPat::Tuple(p) => Self::lower_tuple(ctx, p),
            ast::AtomicPat::Record(p) => Self::lower_record(ctx, p),
            ast::AtomicPat::Unit(_) => PatKind::Record { rows: Box::new([]) },
        }
    }

    fn lower_scon(_ctx: &mut LoweringCtxt, p: &ast::SConPat) -> PatKind {
        let scon = p.scon().map(Scon::lower).unwrap_or(Scon::Missing);
        PatKind::Scon(scon)
    }

    fn lower_vid(ctx: &mut LoweringCtxt, p: &ast::VIdPat) -> PatKind {
        let op = p.op();
        let longvid = LongVId::from_opt_node(ctx, p.longvid().as_ref());

        let loc = if longvid.is_builtin() {
            Some(DefLoc::Builtin)
        } else {
            // We need to look up if this is a constructor of a `datatype` variant.
            // Otherwise, it is just shadowing or declaring a variable and this block should return
            // `None`.
            if let DefLoc::Dec(index) = ctx.resolver().lookup_vid(&longvid) {
                let dec = ctx.arenas().get_dec(index);
                if let DecKind::Datatype { .. } = dec.kind {
                    Some(DefLoc::Dec(index))
                } else {
                    None
                }
            } else {
                None
            }
        };

        PatKind::VId {
            op,
            longvid: (longvid, loc),
        }
    }

    fn lower_list(ctx: &mut LoweringCtxt, p: &ast::ListPat) -> PatKind {
        let origin = ast::Pat::from(ast::AtomicPat::from(p.clone()));
        let parent = NodeParent::from_pat(ctx, &origin);
        util::lower_list(
            ctx,
            parent,
            p.pats(),
            |(vid, loc)| PatKind::VId {
                op: false,
                longvid: (vid, Some(loc)),
            },
            |lhs, vid, rhs| PatKind::Infix { lhs, vid, rhs },
        )
    }

    fn lower_tuple(ctx: &mut LoweringCtxt, p: &ast::TuplePat) -> PatKind {
        let mut rows = vec![];

        for (i, p) in p.pats().enumerate() {
            let pat = Pat::lower(ctx, p);
            let label = Label::Numeric((i + 1) as u32);
            rows.push(PatRow::Pattern { label, pat });
        }

        PatKind::Record {
            rows: rows.into_boxed_slice(),
        }
    }

    fn lower_record(ctx: &mut LoweringCtxt, p: &ast::RecordPat) -> PatKind {
        let rows = p.patrows().map(|p| PatRow::lower(ctx, &p)).collect();
        PatKind::Record { rows }
    }

    fn lower_typed(ctx: &mut LoweringCtxt, p: &ast::TypedPat) -> PatKind {
        let pat = Self::lower_opt(ctx, p.pat());
        let ty = Ty::lower_opt(ctx, p.ty());
        PatKind::Typed { pat, ty }
    }

    fn lower_layered(ctx: &mut LoweringCtxt, p: &ast::LayeredPat) -> PatKind {
        // Currently because of an error in the parser, the first part is getting parsed as
        // possibly a typed pat instead of its constituent parts.
        let mut pats = p.pats();

        let extract_vid = |p: Option<ast::Pat>| {
            if let Some(ast::Pat::Atomic(ast::AtomicPat::VId(vid))) = p {
                (vid.op(), vid.longvid())
            } else {
                (false, None)
            }
        };

        // TODO: make this more DRY, it's kinda ugly now
        let (op, vid, ty) = match pats.next() {
            Some(ast::Pat::Typed(t)) => {
                let ty = Ty::lower_opt(ctx, t.ty());
                let (op, vid) = extract_vid(t.pat());
                let vid = LongVId::from_opt_node(ctx, vid.as_ref())
                    .try_into_vid()
                    .unwrap_or(VId::Missing);
                (op, vid, Some(ty))
            }
            Some(t) => {
                let (op, vid) = extract_vid(Some(t));
                let vid = LongVId::from_opt_node(ctx, vid.as_ref())
                    .try_into_vid()
                    .unwrap_or(VId::Missing);
                (op, vid, None)
            }
            None => (false, VId::Missing, None),
        };

        let pat = Pat::lower_opt(ctx, pats.next());
        PatKind::Layered { op, vid, ty, pat }
    }
}

impl PatRow {
    fn lower(ctx: &mut LoweringCtxt, patrow: &ast::PatRow) -> Self {
        let pat = Pat::lower_opt(ctx, patrow.pat());
        let label = Label::from_token(ctx, patrow.label());
        PatRow::Pattern { label, pat }
    }
}
