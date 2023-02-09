use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::util;
use crate::lower::{HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{
    AstId, ConBind, DataBind, Dec, DecKind, DefLoc, Expr, ExprKind, Fixity, LongStrId, LongTyCon,
    LongVId, MRule, NameInterner, NodeParent, Pat, PatKind, Ty, TyCon, TypBind, VId, ValBind,
};

impl HirLower for Dec {
    type AstType = ast::Dec;

    fn lower(ctx: &mut LoweringCtxt, ast: Self::AstType) -> Idx<Self> {
        let ast_id = ctx.alloc_ast_id(&ast);
        ctx.make_rec_dec(AstId::Node(ast_id), |ctx, index| match &ast {
            ast::Dec::Val(v) => Self::lower_val(ctx, v, index),
            ast::Dec::Fun(f) => Self::lower_fun(ctx, f, index),
            ast::Dec::Type(t) => Self::lower_type(ctx, t),
            ast::Dec::Datatype(d) => Self::lower_datatype(ctx, d),
            ast::Dec::DatatypeRep(r) => Self::lower_replication(ctx, r),
            ast::Dec::Abstype(a) => Self::lower_abstype(ctx, a),
            ast::Dec::Exception(e) => Self::lower_exception(ctx, e),
            ast::Dec::Local(l) => Self::lower_local(ctx, l),
            ast::Dec::Open(o) => Self::lower_open(ctx, o),
            ast::Dec::Infix(i) => Self::lower_infix(ctx, i),
            ast::Dec::Infixr(i) => Self::lower_infixr(ctx, i),
            ast::Dec::Nonfix(n) => Self::lower_nonfix(ctx, n),
            ast::Dec::Seq(s) => Self::lower_seq(ctx, s),
        })
    }

    fn missing(ctx: &mut LoweringCtxt) -> Idx<Self> {
        ctx.make_rec_dec(AstId::Missing, |_, _| DecKind::Missing)
    }
}

impl HirLowerGenerated for Dec {
    type Kind = DecKind;

    fn generated(ctx: &mut LoweringCtxt, origin: NodeParent, kind: Self::Kind) -> Idx<Self> {
        ctx.make_rec_dec(AstId::Generated(origin), move |_, _| kind)
    }
}

impl Dec {
    fn _make_seq(ctx: &mut LoweringCtxt, parent: NodeParent, kinds: Vec<DecKind>) -> DecKind {
        let decs = kinds
            .into_iter()
            .map(|kind| Dec::generated(ctx, parent, kind))
            .collect();
        DecKind::Seq { decs }
    }

    fn lower_val(ctx: &mut LoweringCtxt, dec: &ast::ValDec, index: Idx<Dec>) -> DecKind {
        let tyvarseq = util::lower_tyvarseq(ctx, dec.tyvarseq());
        let bindings = dec
            .bindings()
            .map(|b| ValBind::lower(ctx, &b, index))
            .collect();
        DecKind::Val { tyvarseq, bindings }
    }

    fn lower_fun(ctx: &mut LoweringCtxt, dec: &ast::FunDec, index: Idx<Dec>) -> DecKind {
        let parent = NodeParent::from_dec(ctx, &ast::Dec::from(dec.clone()));
        let tyvarseq = util::lower_tyvarseq(ctx, dec.tyvarseq());
        let bindings = dec
            .bindings()
            .map(|b| ValBind::lower_fvalbind(ctx, parent, &b, index))
            .collect();
        DecKind::Val { tyvarseq, bindings }
    }

    fn lower_type(ctx: &mut LoweringCtxt, dec: &ast::TypeDec) -> DecKind {
        let bindings = dec.bindings().map(|b| TypBind::lower(ctx, &b)).collect();
        DecKind::Ty { bindings }
    }

    fn lower_datatype(ctx: &mut LoweringCtxt, dec: &ast::DatatypeDec) -> DecKind {
        // FIXME: Handle "withtype"
        if dec.withtype() {
            todo!()
        }

        let databinds = dec.databinds().map(|d| DataBind::lower(ctx, &d)).collect();
        DecKind::Datatype { databinds }
    }

    fn lower_replication(ctx: &mut LoweringCtxt, dec: &ast::DatatypeRepDec) -> DecKind {
        let lhs = TyCon::from_token(ctx, dec.tycon());
        let rhs = LongTyCon::from_opt_node(ctx, dec.longtycon().as_ref());
        let loc = ctx.resolver().lookup_ty(&rhs);
        DecKind::Replication {
            lhs,
            rhs: (rhs, loc),
        }
    }

    fn lower_abstype(ctx: &mut LoweringCtxt, dec: &ast::AbstypeDec) -> DecKind {
        // FIXME: Handle "withtype"
        if dec.withtype() {
            todo!()
        }

        let databinds = dec.databinds().map(|d| DataBind::lower(ctx, &d)).collect();
        let dec = Self::lower_opt(ctx, dec.dec());
        DecKind::Abstype { databinds, dec }
    }

    fn lower_exception(_ctx: &mut LoweringCtxt, _dec: &ast::ExceptionDec) -> DecKind {
        todo!()
    }

    fn lower_local(ctx: &mut LoweringCtxt, dec: &ast::LocalDec) -> DecKind {
        let inner = Self::lower_opt(ctx, dec.dec1());
        let outer = Self::lower_opt(ctx, dec.dec2());
        DecKind::Local { inner, outer }
    }

    /// TODO: this might need fixing, it doesn't do anything semantically yet...
    ///
    /// Maybe we should panic here instead?
    fn lower_open(ctx: &mut LoweringCtxt, dec: &ast::OpenDec) -> DecKind {
        let longstrids = dec
            .longstrids()
            .map(|s| LongStrId::from_node(ctx, &s))
            .collect();
        DecKind::Open { longstrids }
    }

    fn lower_infix(ctx: &mut LoweringCtxt, dec: &ast::InfixDec) -> DecKind {
        Self::lower_fix(ctx, dec.vids(), dec.fixity(), Fixity::Left)
    }

    fn lower_infixr(ctx: &mut LoweringCtxt, dec: &ast::InfixrDec) -> DecKind {
        Self::lower_fix(ctx, dec.vids(), dec.fixity(), Fixity::Right)
    }

    fn lower_nonfix(ctx: &mut LoweringCtxt, dec: &ast::NonfixDec) -> DecKind {
        Self::lower_fix(ctx, dec.vids(), None, |_| Fixity::Nonfix)
    }

    fn lower_fix(
        ctx: &mut LoweringCtxt,
        vids: impl Iterator<Item = ast::VId>,
        fixity: Option<ast::Fixity>,
        f: impl Fn(Option<u8>) -> Fixity,
    ) -> DecKind {
        let vids = util::lower_vids(ctx, vids);
        let fixity = f(fixity.map(|f| f.value()));
        DecKind::Fixity { fixity, vids }
    }

    fn lower_seq(ctx: &mut LoweringCtxt, dec: &ast::SeqDec) -> DecKind {
        let decs = dec.declarations().map(|d| Self::lower(ctx, d)).collect();
        DecKind::Seq { decs }
    }
}

impl ValBind {
    fn lower(ctx: &mut LoweringCtxt, b: &ast::ValBind, dec_index: Idx<Dec>) -> Self {
        let rec = b.rec();
        let pat = Pat::lower_opt(ctx, b.pat());

        // If this is recursive, then `expr` may refer to names bound in `pat`
        if rec {
            ctx.register_rec_pat(pat, dec_index);
        }

        let expr = Expr::lower_opt(ctx, b.expr());
        ValBind { rec, pat, expr }
    }

    fn lower_fvalbind(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        b: &ast::FvalBind,
        _dec_index: Idx<Dec>,
    ) -> Self {
        let mut mrules = vec![];
        let mut op = None;
        let mut vid = None;
        let mut n_parameters = None;

        // TODO: surface error instead of panicking
        if b.rows().count() == 0 {
            panic!("fvalbind with no rows")
        }

        // Each `FvalBindRow` generates a pattern match
        for row in b.rows() {
            // Ensure all rows have the same `<op>` keyword.
            Self::check_row_op(&mut op, row.op());

            // Ensure all rows bind the same `vid`
            Self::check_row_vid(&mut vid, VId::from_token(ctx, row.vid()));

            // Ensure all rows have the same number of parameters
            Self::check_row_arity(&mut n_parameters, row.atpats().count());

            // Make the match expression
            let pat = Self::make_row_pat(ctx, parent, &row, n_parameters.unwrap());
            let expr = Self::make_row_expr(ctx, parent, &row);
            mrules.push(MRule { pat, expr });
        }

        // The part below is a little confusing...
        // A multi argument function desugars to a chain of `fn`s,
        // e.g., `fun x y` becomes `fn x => (fn y => case (x, y) of .. )`.
        //
        // Since each `fn`'s expr is the next `fn`, we need to generate exprs in reverse order
        // (final case expr, then work backwards toward the first `fn`).
        // Each generated expr refers to one of the vid patterns in the chain of `fn`s,
        // so we have to generate those patterns first.

        // Generate the patterns `x`, `y` in each `fn` and the tuple expr `(x, y)`.
        let (fn_pats, tuple_expr) =
            Self::make_fresh_vids_and_outer_pat(ctx, parent, n_parameters.unwrap());

        // Create the `case` expr at the end of the chain of `fn`s.
        let kind = Expr::desugar_case(ctx, parent, tuple_expr, mrules.into_boxed_slice());
        let desugared_case_expr = Expr::generated(ctx, parent, kind);

        // Make the chain of `fn` pats. This function returns the `Idx` of the first `fn` in the
        // chain.
        let expr = Self::make_fn_chain(ctx, parent, fn_pats, desugared_case_expr);

        // Make the pat that binds the function name
        let vid_pat = Pat::generated(
            ctx,
            parent,
            PatKind::VId {
                op: op.unwrap(),
                longvid: (LongVId::from(vid.unwrap()), None),
            },
        );

        ValBind {
            rec: true,
            pat: vid_pat,
            expr,
        }
    }

    fn check_row_op(prev_op: &mut Option<bool>, row_op: bool) {
        // TODO: surface error here instead of panicking
        if let Some(op) = prev_op {
            assert_eq!(row_op, *op);
        } else {
            *prev_op = Some(row_op);
        }
    }

    fn check_row_vid(prev_vid: &mut Option<VId>, row_vid: VId) {
        // TODO: surface error here instead of panicking
        if let Some(vid) = prev_vid {
            assert_eq!(*vid, row_vid);
        } else {
            *prev_vid = Some(row_vid);
        }
    }

    fn check_row_arity(prev_n: &mut Option<usize>, row_n: usize) {
        // TODO: surface error here instead of panicking
        if let Some(n_parameters) = prev_n {
            assert_eq!(*n_parameters, row_n);
        } else {
            *prev_n = Some(row_n);
        }
    }

    fn make_row_pat(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        row: &ast::FvalBindRow,
        n_parameters: usize,
    ) -> Idx<Pat> {
        if n_parameters == 1 {
            let p = row.atpats().next().unwrap();
            Pat::lower(ctx, ast::Pat::from(p))
        } else {
            let it = row
                .atpats()
                .map(|p| Pat::lower(ctx, ast::Pat::from(p)))
                .collect::<Vec<_>>();
            Pat::generated(ctx, parent, Pat::make_tuple(it.into_iter()))
        }
    }

    fn make_row_expr(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        row: &ast::FvalBindRow,
    ) -> Idx<Expr> {
        let expr = Expr::lower_opt(ctx, row.expr());
        if let Some(ty) = row.ty() {
            let ty = Ty::lower(ctx, ty);
            Expr::generated(ctx, parent, ExprKind::Typed { expr, ty })
        } else {
            expr
        }
    }

    /// Returns:
    ///
    /// (1) list of pats referring to fresh vids to be used in the chain of `fn`s
    /// (2) tuple expr referring to all of these fresh vids to be consumed by the generated case
    /// expr
    fn make_fresh_vids_and_outer_pat(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        n_parameters: usize,
    ) -> (Vec<Idx<Pat>>, Idx<Expr>) {
        // Make `n_parameters` fresh vids
        let new_vids: Vec<_> = (0..n_parameters)
            .into_iter()
            .map(|_| LongVId::from(ctx.interner_mut().fresh_vid()))
            .collect();

        // Generate `vid` pats referencing the fresh vids
        let mut fn_pats = vec![];
        for new_vid in new_vids.iter() {
            let kind = PatKind::VId {
                op: false,
                longvid: (new_vid.clone(), None),
            };
            fn_pats.push(Pat::generated(ctx, parent, kind));
        }

        let tuple_expr = new_vids
            .iter()
            .enumerate()
            .map(|(i, vid)| {
                Expr::generated(
                    ctx,
                    parent,
                    ExprKind::VId {
                        op: false,
                        longvid: (vid.clone(), DefLoc::Pat(fn_pats[i])),
                    },
                )
            })
            .collect::<Vec<_>>();
        let tuple_expr = Expr::generated(ctx, parent, Expr::make_tuple(tuple_expr.into_iter()));

        (fn_pats, tuple_expr)
    }

    fn make_fn_chain(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        mut fn_pats: Vec<Idx<Pat>>,
        case_expr: Idx<Expr>,
    ) -> Idx<Expr> {
        // Make the last `fn` pat which uses the case expression.
        //
        // Note: this pops the last vid pat off of `fn_pats` so we
        // can just reverse iterate in the next block.
        let mut expr = Expr::generated(
            ctx,
            parent,
            ExprKind::Fn {
                match_: Box::new([MRule {
                    pat: fn_pats.pop().unwrap(),
                    expr: case_expr,
                }]),
            },
        );

        // Working backward up the chain of `fn`s, each new `fn` uses the previous `expr`.
        for pat in fn_pats.iter().rev() {
            let mrule = MRule { pat: *pat, expr };
            expr = Expr::generated(
                ctx,
                parent,
                ExprKind::Fn {
                    match_: Box::new([mrule]),
                },
            );
        }
        expr
    }
}

impl DataBind {
    fn lower(ctx: &mut LoweringCtxt, b: &ast::DataBind) -> Self {
        let tyvarseq = util::lower_tyvarseq(ctx, b.tyvarseq());
        let tycon = TyCon::from_token(ctx, b.tycon());
        let conbinds = b.conbinds().map(|c| ConBind::lower(ctx, &c)).collect();
        Self {
            tyvarseq,
            tycon,
            conbinds,
        }
    }
}

impl ConBind {
    fn lower(ctx: &mut LoweringCtxt, b: &ast::ConBind) -> Self {
        let op = b.op();
        let vid = VId::from_token(ctx, b.vid());
        let ty = b.ty().map(|t| Ty::lower(ctx, t));
        Self { op, vid, ty }
    }
}

impl TypBind {
    fn lower(ctx: &mut LoweringCtxt, b: &ast::TyBind) -> Self {
        let tyvarseq = util::lower_tyvarseq(ctx, b.tyvarseq());
        let tycon = TyCon::from_token(ctx, b.tycon());
        let ty = Ty::lower_opt(ctx, b.ty());
        Self {
            tyvarseq,
            tycon,
            ty,
        }
    }
}
