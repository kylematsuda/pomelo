use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::{util, HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{
    AstId, BuiltIn, Dec, DecKind, DefLoc, ExpRow, Expr, ExprKind, Label, LongVId, MRule,
    NameInterner, NodeParent, Pat, PatKind, PatRow, Scon, Ty, VId, ValBind,
};

impl HirLower for Expr {
    type AstType = ast::Expr;

    fn lower(ctx: &mut LoweringCtxt, ast: Self::AstType) -> Idx<Self> {
        let kind = Self::to_kind(ctx, &ast);
        let ast_id = AstId::Node(ctx.alloc_ast_id(&ast));
        ctx.add_expr(Self { kind, ast_id })
    }

    fn missing(ctx: &mut LoweringCtxt) -> Idx<Self> {
        let e = Self {
            kind: ExprKind::Missing,
            ast_id: AstId::Missing,
        };
        ctx.add_expr(e)
    }
}

impl HirLowerGenerated for Expr {
    type Kind = ExprKind;

    fn generated(ctx: &mut LoweringCtxt, origin: NodeParent, kind: Self::Kind) -> Idx<Self> {
        let e = Self {
            kind,
            ast_id: AstId::Generated(origin),
        };
        ctx.add_expr(e)
    }
}

impl Expr {
    fn to_kind(ctx: &mut LoweringCtxt, e: &ast::Expr) -> ExprKind {
        match &e {
            ast::Expr::Atomic(e) => Self::lower_atomic(ctx, &e),
            ast::Expr::Application(_) => todo!("remove this from the ast"),
            ast::Expr::Infix(_) => todo!("remove this from the ast"),
            ast::Expr::InfixOrApp(e) => Self::lower_infix_or_app(ctx, &e),
            ast::Expr::Typed(e) => Self::lower_typed(ctx, &e),
            ast::Expr::AndAlso(e) => Self::lower_andalso(ctx, &e),
            ast::Expr::OrElse(e) => Self::lower_orelse(ctx, &e),
            ast::Expr::Handle(e) => Self::lower_handle(ctx, &e),
            ast::Expr::Raise(e) => Self::lower_raise(ctx, &e),
            ast::Expr::If(e) => Self::lower_if(ctx, &e),
            ast::Expr::While(e) => Self::lower_while(ctx, &e),
            ast::Expr::Case(e) => Self::lower_case(ctx, &e),
            ast::Expr::Fn(e) => Self::lower_fn(ctx, &e),
        }
    }

    fn builtin_vid(b: BuiltIn) -> ExprKind {
        ExprKind::VId {
            op: false,
            longvid: (LongVId::from_vid(VId::from_builtin(b)), DefLoc::Builtin),
        }
    }

    fn lower_atomic(ctx: &mut LoweringCtxt, e: &ast::AtomicExpr) -> ExprKind {
        match e {
            ast::AtomicExpr::SCon(e) => Self::lower_scon(ctx, &e),
            ast::AtomicExpr::VId(e) => Self::lower_vid(ctx, &e),
            ast::AtomicExpr::Let(e) => Self::lower_let(ctx, &e),
            ast::AtomicExpr::Seq(e) => Self::lower_seq(ctx, &e),
            ast::AtomicExpr::Record(e) => Self::lower_record(ctx, &e),
            ast::AtomicExpr::Tuple(e) => Self::lower_tuple(ctx, &e),
            ast::AtomicExpr::Unit(e) => Self::lower_unit(ctx, &e),
            ast::AtomicExpr::List(e) => Self::lower_list(ctx, &e),
            ast::AtomicExpr::RecSel(e) => Self::lower_recsel(ctx, &e),
            ast::AtomicExpr::Paren(e) => Self::lower_paren(ctx, &e),
        }
    }

    fn lower_scon(_ctx: &mut LoweringCtxt, e: &ast::SConExpr) -> ExprKind {
        let scon = e.scon().map(Scon::lower).unwrap_or(Scon::Missing);
        ExprKind::Scon(scon)
    }

    fn lower_vid(ctx: &mut LoweringCtxt, e: &ast::VIdExpr) -> ExprKind {
        let op = e.op();
        let longvid = LongVId::from_opt_node(ctx, e.longvid().as_ref());
        let loc = ctx.resolver().lookup_vid(&longvid);
        ExprKind::VId {
            op,
            longvid: (longvid, loc),
        }
    }

    fn lower_let(ctx: &mut LoweringCtxt, e: &ast::LetExpr) -> ExprKind {
        ctx.enter_scope(|ctx| {
            let dec = Dec::lower_opt(ctx, e.dec());

            let exprs = e.exprs().map(|e| Self::lower(ctx, e)).collect();
            let kind = ExprKind::Seq { exprs };

            let origin = ast::Expr::from(ast::AtomicExpr::from(e.clone()));
            let parent = NodeParent::from_expr(ctx, &origin);
            let expr = Self::generated(ctx, parent, kind);

            ExprKind::Let { dec, expr }
        })
    }

    fn lower_seq(ctx: &mut LoweringCtxt, e: &ast::SeqExpr) -> ExprKind {
        let exprs = e.exprs().map(|e| Self::lower(ctx, e)).collect();
        ExprKind::Seq { exprs }
    }

    fn lower_record(ctx: &mut LoweringCtxt, e: &ast::RecordExpr) -> ExprKind {
        let rows = e.exprows().map(|e| ExpRow::lower(ctx, &e)).collect();
        ExprKind::Record { rows }
    }

    fn lower_tuple(ctx: &mut LoweringCtxt, e: &ast::TupleExpr) -> ExprKind {
        let mut rows = vec![];
        for (i, e) in e.exprs().enumerate() {
            let expr = Self::lower(ctx, e);
            let label = (i + 1) as u32;
            let exprow = ExpRow {
                expr,
                label: Label::Numeric(label),
            };
            rows.push(exprow);
        }
        ExprKind::Record {
            rows: rows.into_boxed_slice(),
        }
    }

    fn lower_unit(_ctx: &mut LoweringCtxt, _e: &ast::UnitExpr) -> ExprKind {
        ExprKind::Record { rows: Box::new([]) }
    }

    fn lower_list(ctx: &mut LoweringCtxt, e: &ast::ListExpr) -> ExprKind {
        let origin = ast::Expr::from(ast::AtomicExpr::from(e.clone()));
        let parent = NodeParent::from_expr(ctx, &origin);
        util::lower_list(
            ctx,
            parent,
            e.exprs(),
            |longvid| ExprKind::VId { op: false, longvid },
            |lhs, vid, rhs| ExprKind::Infix { lhs, vid, rhs },
        )
    }

    fn lower_recsel(ctx: &mut LoweringCtxt, e: &ast::RecSelExpr) -> ExprKind {
        let origin = ast::Expr::from(ast::AtomicExpr::from(e.clone()));
        let parent = NodeParent::from_expr(ctx, &origin);

        let label = Label::from_token(ctx, e.label());
        let newvid = LongVId::from_vid(ctx.interner_mut().fresh_vid());

        // This is a bit nasty... we need to generate an fn match on a record pattern.
        // See pg. 70 of the Definition.
        //
        // We use `enter_scope` because we want to restrict the scope of `newvid` to this generated match.
        ctx.enter_scope(|ctx| {
            // Generate inner vid pattern for the patrow
            let vid_pat = Pat::generated(
                ctx,
                parent,
                PatKind::VId {
                    op: false,
                    longvid: (newvid.clone(), None),
                },
            );

            // The full patter is the vid pattern and a wildcard pattern
            let rows = [
                PatRow::Pattern {
                    label,
                    pat: vid_pat,
                },
                PatRow::Wildcard,
            ]
            .into_iter()
            .collect();

            // The full enclosing record
            let record_pat = Pat::generated(ctx, parent, PatKind::Record { rows });

            // Resolve the reference to `newvid`
            ctx.register_pat_names_in_match(record_pat);
            let newvid_loc = ctx.resolver().lookup_vid(&newvid);

            // Generate expr for the rhs of the match
            let vid_expr = Expr::generated(
                ctx,
                parent,
                ExprKind::VId {
                    op: false,
                    longvid: (newvid.clone(), newvid_loc),
                },
            );

            // Generate the match
            let mrule = [MRule {
                pat: record_pat,
                expr: vid_expr,
            }]
            .into_iter()
            .collect();

            ExprKind::Fn { match_: mrule }
        })
    }

    fn lower_paren(ctx: &mut LoweringCtxt, e: &ast::ParenExpr) -> ExprKind {
        e.expr()
            .map(|e| Self::to_kind(ctx, &e))
            .unwrap_or(ExprKind::Missing)
    }

    fn lower_infix_or_app(_ctx: &mut LoweringCtxt, _e: &ast::InfixOrAppExpr) -> ExprKind {
        todo!()
    }

    fn lower_typed(ctx: &mut LoweringCtxt, e: &ast::TypedExpr) -> ExprKind {
        let ty = Ty::lower_opt(ctx, e.ty());
        let expr = Self::lower_opt(ctx, e.expr());
        ExprKind::Typed { expr, ty }
    }

    fn lower_andalso(ctx: &mut LoweringCtxt, e: &ast::AndAlsoExpr) -> ExprKind {
        let origin = ast::Expr::from(e.clone());
        let parent = NodeParent::from_expr(ctx, &origin);

        let false_expr = Self::generated(ctx, parent, Self::builtin_vid(BuiltIn::False));

        let expr_1 = Self::lower_opt(ctx, e.expr_1());
        let expr_2 = Self::lower_opt(ctx, e.expr_2());

        Self::desugar_if(ctx, parent, expr_1, expr_2, false_expr)
    }

    fn lower_orelse(ctx: &mut LoweringCtxt, e: &ast::OrElseExpr) -> ExprKind {
        let origin = ast::Expr::from(e.clone());
        let parent = NodeParent::from_expr(ctx, &origin);

        let true_expr = Self::generated(ctx, parent, Self::builtin_vid(BuiltIn::True));

        let expr_1 = Self::lower_opt(ctx, e.expr_1());
        let expr_2 = Self::lower_opt(ctx, e.expr_2());

        Self::desugar_if(ctx, parent, expr_1, true_expr, expr_2)
    }

    fn lower_handle(ctx: &mut LoweringCtxt, e: &ast::HandleExpr) -> ExprKind {
        let match_ = match e.match_expr() {
            Some(m) => util::lower_match(ctx, &m),
            None => Box::new([]),
        };
        let expr = Self::lower_opt(ctx, e.expr());
        ExprKind::Handle { expr, match_ }
    }

    fn lower_raise(ctx: &mut LoweringCtxt, e: &ast::RaiseExpr) -> ExprKind {
        let expr = Self::lower_opt(ctx, e.expr());
        ExprKind::Raise { expr }
    }

    fn lower_if(ctx: &mut LoweringCtxt, e: &ast::IfExpr) -> ExprKind {
        let origin = ast::Expr::from(e.clone());
        let parent = NodeParent::from_expr(ctx, &origin);

        let condition = Self::lower_opt(ctx, e.expr_1());
        let then_branch = Self::lower_opt(ctx, e.expr_2());
        let else_branch = Self::lower_opt(ctx, e.expr_3());

        Self::desugar_if(ctx, parent, condition, then_branch, else_branch)
    }

    fn lower_while(ctx: &mut LoweringCtxt, e: &ast::WhileExpr) -> ExprKind {
        let exp1 = Self::lower_opt(ctx, e.expr_1());
        let exp2 = Self::lower_opt(ctx, e.expr_2());

        let origin = ast::Expr::from(e.clone());
        let parent = NodeParent::from_expr(ctx, &origin);
        let newvid = LongVId::from_vid(ctx.interner_mut().fresh_vid());

        let unitexpr = Self::generated(ctx, parent, ExprKind::Record { rows: Box::new([]) });
        // Horrible hack: set the reference to the def of `newvid` as missing.
        // We will fill it in correctly at the end.
        let videxpr = Self::generated(
            ctx,
            parent,
            ExprKind::VId {
                op: false,
                longvid: (newvid.clone(), DefLoc::Missing),
            },
        );

        let appexpr = Self::generated(
            ctx,
            parent,
            ExprKind::Application {
                expr: videxpr,
                param: unitexpr,
            },
        );

        let seqexpr = Self::generated(
            ctx,
            parent,
            ExprKind::Seq {
                exprs: Box::new([exp2, appexpr]),
            },
        );

        let fn_pat = Pat::generated(ctx, parent, PatKind::Record { rows: Box::new([]) });
        let if_expr = Self::desugar_if(ctx, parent, exp1, seqexpr, unitexpr);
        let fn_inner_expr = Self::generated(ctx, parent, if_expr);
        let match_ = Box::new([MRule {
            pat: fn_pat,
            expr: fn_inner_expr,
        }]);
        let fn_expr = Self::generated(ctx, parent, ExprKind::Fn { match_ });

        let vidpat = Pat::generated(
            ctx,
            parent,
            PatKind::VId {
                op: false,
                longvid: (newvid, None),
            },
        );

        let dec = Dec::generated(
            ctx,
            parent,
            DecKind::Val {
                tyvarseq: Box::new([]),
                bindings: Box::new([ValBind {
                    rec: true,
                    pat: vidpat,
                    expr: fn_expr,
                }]),
            },
        );

        // Horrible hack: Fill in the correct reference to the definition of `newvid` now that the
        // `Dec` has been allocated.
        ctx.fixup_vid_expr_ref(videxpr, DefLoc::Dec(dec));

        ExprKind::Let { dec, expr: appexpr }
    }

    fn lower_case(ctx: &mut LoweringCtxt, e: &ast::CaseExpr) -> ExprKind {
        let origin = ast::Expr::from(e.clone());
        let parent = NodeParent::from_expr(ctx, &origin);

        let exp = Self::lower_opt(ctx, e.expr());
        let match_ = match e.match_expr() {
            None => Box::new([]),
            Some(m) => util::lower_match(ctx, &m),
        };

        Self::desugar_case(ctx, parent, exp, match_)
    }

    fn lower_fn(ctx: &mut LoweringCtxt, e: &ast::FnExpr) -> ExprKind {
        let match_ = match e.match_expr() {
            None => Box::new([]),
            Some(m) => util::lower_match(ctx, &m),
        };
        ExprKind::Fn { match_ }
    }

    fn desugar_if(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        condition: Idx<Expr>,
        then_branch: Idx<Expr>,
        else_branch: Idx<Expr>,
    ) -> ExprKind {
        let true_pat = Pat::generated(ctx, parent, Pat::vid_builtin(BuiltIn::True));
        let false_pat = Pat::generated(ctx, parent, Pat::vid_builtin(BuiltIn::False));

        let match_arms = [(true_pat, then_branch), (false_pat, else_branch)]
            .into_iter()
            .map(|(pat, expr)| MRule { pat, expr })
            .collect();

        Self::desugar_case(ctx, parent, condition, match_arms)
    }

    fn desugar_case(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        test: Idx<Expr>,
        match_: Box<[MRule]>,
    ) -> ExprKind {
        let lowered_case = Self::generated(ctx, parent, ExprKind::Fn { match_ });

        ExprKind::Application {
            expr: lowered_case,
            param: test,
        }
    }
}

impl ExpRow {
    fn lower(ctx: &mut LoweringCtxt, e: &ast::ExprRow) -> Self {
        let expr = Expr::lower_opt(ctx, e.expr());
        let label = Label::from_token(ctx, e.label());
        Self { label, expr }
    }
}
