use crate::arena::Idx;
use crate::core::{
    AstId, BodyArena, ConBind, DataBind, Dec, DecKind, ExBind, ExpRow, Expr, ExprKind, Fixity,
    FloatWrapper, MRule, NodeParent, Pat, PatKind, PatRow, Scon, TyKind, TyRow, Type,
};
use crate::identifiers::{BuiltIn, Label, LongStrId, LongTyCon, LongVId, TyCon, TyVar, VId};
use pomelo_parse::{ast, AstNode};

pub(crate) trait HirLower: Sized {
    type AstType: AstNode;

    fn missing<A: BodyArena>(arena: &mut A) -> Idx<Self>;
    fn lower<A: BodyArena>(ast: Self::AstType, arena: &mut A) -> Idx<Self>;

    fn lower_opt<A: BodyArena>(opt_ast: Option<Self::AstType>, arena: &mut A) -> Idx<Self> {
        match opt_ast {
            Some(a) => Self::lower(a, arena),
            None => Self::missing(arena),
        }
    }
}

pub(crate) trait HirLowerGenerated: HirLower {
    type Kind: Clone;
    fn generated<A: BodyArena>(origin: NodeParent, kind: Self::Kind, arena: &mut A) -> Idx<Self>;
}

// Used to lower lists [a1, a2, ... ] to a1 :: a2 :: ... :: nil
// Common code for both [`Expr`] and [`Pat`].
// This is factored out on its own since it's probably the most complicated
// part of this stage of lowering.
//
// elts: e.g., "expr.exprs()" or "pat.pats()"
// vid_kind: construct an `ExprKind::VId` or `PatKind::VId`
// infix_kind: construct an `ExprKind::Infix` or `PatKind::Infix`
fn lower_list<A: BodyArena, H: HirLowerGenerated>(
    origin: NodeParent,
    elts: impl Iterator<Item = H::AstType>,
    vid_kind: impl Fn(LongVId) -> H::Kind,
    infix_kind: impl Fn(Idx<H>, VId, Idx<H>) -> H::Kind,
    arena: &mut A,
) -> H::Kind {
    let mut rev_indexed = elts
        .map(|e| H::lower(e, arena))
        .enumerate()
        .collect::<Vec<_>>();
    rev_indexed.reverse();

    let nil = LongVId::from_vid(VId::from_builtin(BuiltIn::Nil));

    if rev_indexed.len() == 0 {
        vid_kind(nil.clone())
    } else {
        let cons = VId::from_builtin(BuiltIn::Cons);

        // The list ends with a nil pat
        let nil_expr = H::generated(origin.clone(), vid_kind(nil.clone()), arena);

        let mut last_idx = nil_expr;
        let mut last = vid_kind(nil.clone());

        // "::" is right-associative, so we walk the list of pats in reverse.
        // We allocate each generated infix expr in the arena, except for the
        // final one ("hd :: ( ... )"), whose `ExprKind` we need to return from the function
        for (i, p_idx) in rev_indexed {
            last = infix_kind(p_idx, cons, last_idx);
            if i == 0 {
                return last;
            }
            last_idx = H::generated(origin.clone(), last.clone(), arena);
        }
        last
    }
}

impl HirLower for Dec {
    type AstType = ast::Dec;

    fn lower<A: BodyArena>(ast: Self::AstType, arena: &mut A) -> Idx<Self> {
        let kind = match &ast {
            ast::Dec::Val(v) => Self::lower_val(v, arena),
            ast::Dec::Fun(f) => Self::lower_fun(f, arena),
            ast::Dec::Type(t) => Self::lower_type(t, arena),
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
        };

        let dec = Self {
            kind,
            ast_id: AstId::Node(arena.alloc_ast_id(&ast)),
        };
        arena.alloc_dec(dec)
    }

    fn missing<A: BodyArena>(arena: &mut A) -> Idx<Self> {
        let dec = Self {
            kind: DecKind::Missing,
            ast_id: AstId::Missing,
        };
        arena.alloc_dec(dec)
    }
}

impl HirLowerGenerated for Dec {
    type Kind = DecKind;

    fn generated<A: BodyArena>(origin: NodeParent, kind: Self::Kind, arena: &mut A) -> Idx<Self> {
        let dec = Self {
            kind,
            ast_id: AstId::Generated(origin),
        };
        arena.alloc_dec(dec)
    }
}

impl Dec {
    fn make_seq<A: BodyArena>(parent: NodeParent, kinds: Vec<DecKind>, arena: &mut A) -> DecKind {
        let decs = kinds
            .into_iter()
            .map(|kind| Dec::generated(parent.clone(), kind, arena))
            .collect();
        DecKind::Seq { decs }
    }

    fn lower_val<A: BodyArena>(dec: &ast::ValDec, arena: &mut A) -> DecKind {
        let tyvarseq: Box<[TyVar]> = dec
            .tyvarseq()
            .map(|t| TyVar::from_token(Some(t), arena))
            .collect();

        let bindings = dec
            .bindings()
            .map(|b| {
                let rec = b.rec();
                let pat = Pat::lower_opt(b.pat(), arena);
                let expr = Expr::lower_opt(b.expr(), arena);
                DecKind::Val {
                    rec,
                    tyvarseq: tyvarseq.clone(),
                    pat,
                    expr,
                }
            })
            .collect::<Vec<_>>();

        if bindings.len() == 1 {
            bindings.into_iter().next().unwrap()
        } else {
            let origin = NodeParent::from_dec(&ast::Dec::from(dec.clone()), arena);
            Self::make_seq(origin, bindings, arena)
        }
    }

    fn lower_fun<A: BodyArena>(_dec: &ast::FunDec, _arena: &mut A) -> DecKind {
        todo!()
    }

    fn _lower_fvalbind<A: BodyArena>(_fvalbind: &ast::FvalBind, _arena: &mut A) -> DecKind {
        // FIXME:
        todo!()
    }

    fn lower_type<A: BodyArena>(dec: &ast::TypeDec, arena: &mut A) -> DecKind {
        let bindings = dec
            .bindings()
            .map(|b| {
                let tyvarseq = b
                    .tyvarseq()
                    .map(|t| TyVar::from_token(Some(t), arena))
                    .collect();
                let tycon = TyCon::from_token(b.tycon(), arena);
                let ty = Type::lower_opt(b.ty(), arena);
                DecKind::Ty {
                    tyvarseq,
                    tycon,
                    ty,
                }
            })
            .collect::<Vec<_>>();

        if bindings.len() == 1 {
            bindings.into_iter().next().unwrap()
        } else {
            let origin = NodeParent::from_dec(&ast::Dec::from(dec.clone()), arena);
            Self::make_seq(origin, bindings, arena)
        }
    }

    // FIXME: Handle "withtype"
    fn lower_datatype<A: BodyArena>(dec: &ast::DatatypeDec, arena: &mut A) -> DecKind {
        let databinds = dec
            .databinds()
            .map(|b| DecKind::Datatype {
                databind: DataBind::lower(&b, arena),
            })
            .collect::<Vec<_>>();
        if databinds.len() == 1 {
            databinds.into_iter().next().unwrap()
        } else {
            let origin = NodeParent::from_dec(&ast::Dec::from(dec.clone()), arena);
            Self::make_seq(origin, databinds, arena)
        }
    }

    fn lower_replication<A: BodyArena>(dec: &ast::DatatypeRepDec, arena: &mut A) -> DecKind {
        let lhs = TyCon::from_token(dec.tycon(), arena);
        let rhs = LongTyCon::from_opt_node(dec.longtycon().as_ref(), arena);
        DecKind::Replication { lhs, rhs }
    }

    // FIXME: Handle "withtype"
    fn lower_abstype<A: BodyArena>(dec: &ast::AbstypeDec, arena: &mut A) -> DecKind {
        let databinds = dec
            .databinds()
            .map(|d| DataBind::lower(&d, arena))
            .collect();
        let dec = Self::lower_opt(dec.dec(), arena);
        DecKind::Abstype { databinds, dec }
    }

    fn lower_exception<A: BodyArena>(_dec: &ast::ExceptionDec, _arena: &mut A) -> DecKind {
        todo!()
    }

    fn lower_local<A: BodyArena>(dec: &ast::LocalDec, arena: &mut A) -> DecKind {
        let dec1 = Self::lower_opt(dec.dec1(), arena);
        let dec2 = Self::lower_opt(dec.dec2(), arena);
        DecKind::Local {
            inner: dec1,
            outer: dec2,
        }
    }

    fn lower_open<A: BodyArena>(dec: &ast::OpenDec, arena: &mut A) -> DecKind {
        let longstrids = dec
            .longstrids()
            .map(|s| LongStrId::from_node(&s, arena))
            .collect();
        DecKind::Open { longstrids }
    }

    fn lower_infix<A: BodyArena>(dec: &ast::InfixDec, arena: &mut A) -> DecKind {
        let vids = dec
            .vids()
            .map(|v| VId::from_token(Some(v), arena))
            .collect();
        let d = dec.fixity().map(|f| f.value());
        DecKind::Fixity {
            fixity: Fixity::Left(d),
            vids,
        }
    }

    fn lower_infixr<A: BodyArena>(dec: &ast::InfixrDec, arena: &mut A) -> DecKind {
        let vids = dec
            .vids()
            .map(|v| VId::from_token(Some(v), arena))
            .collect();
        let d = dec.fixity().map(|f| f.value());
        DecKind::Fixity {
            fixity: Fixity::Right(d),
            vids,
        }
    }

    fn lower_nonfix<A: BodyArena>(dec: &ast::NonfixDec, arena: &mut A) -> DecKind {
        let vids = dec
            .vids()
            .map(|v| VId::from_token(Some(v), arena))
            .collect();
        DecKind::Fixity {
            fixity: Fixity::Nonfix,
            vids,
        }
    }

    fn lower_seq<A: BodyArena>(dec: &ast::SeqDec, arena: &mut A) -> DecKind {
        let decs = dec.declarations().map(|d| Self::lower(d, arena)).collect();
        DecKind::Seq { decs }
    }
}

impl DataBind {
    pub fn lower<A: BodyArena>(databind: &ast::DataBind, arena: &mut A) -> Self {
        let tyvarseq = databind
            .tyvarseq()
            .map(|t| TyVar::from_token(Some(t), arena))
            .collect();
        let tycon = TyCon::from_token(databind.tycon(), arena);

        let conbinds = databind
            .conbinds()
            .map(|c| ConBind::lower(&c, arena))
            .collect();

        DataBind {
            tyvarseq,
            tycon,
            conbinds,
        }
    }
}

impl ConBind {
    pub fn lower<A: BodyArena>(conbind: &ast::ConBind, arena: &mut A) -> Self {
        let op = conbind.op();
        let vid = VId::from_token(conbind.vid(), arena);
        let ty = conbind.ty().map(|t| Type::lower(t, arena));
        ConBind { op, vid, ty }
    }
}

impl ExBind {
    pub fn lower<A: BodyArena>(exbind: &ast::ExBind, arena: &mut A) -> Self {
        todo!()
    }
}

impl HirLower for Expr {
    type AstType = ast::Expr;

    fn missing<A: BodyArena>(arena: &mut A) -> Idx<Self> {
        let e = Self {
            kind: ExprKind::Missing,
            ast_id: AstId::Missing,
        };
        arena.alloc_expr(e)
    }

    fn lower<A: BodyArena>(ast: Self::AstType, arena: &mut A) -> Idx<Self> {
        let kind = Self::to_kind(&ast, arena);
        let ast_id = AstId::Node(arena.alloc_ast_id(&ast));
        arena.alloc_expr(Self { kind, ast_id })
    }
}

impl HirLowerGenerated for Expr {
    type Kind = ExprKind;

    fn generated<A: BodyArena>(origin: NodeParent, kind: Self::Kind, arena: &mut A) -> Idx<Self> {
        let e = Self {
            kind,
            ast_id: AstId::Generated(origin),
        };
        arena.alloc_expr(e)
    }
}

impl Expr {
    fn to_kind<A: BodyArena>(expr: &ast::Expr, arena: &mut A) -> ExprKind {
        match &expr {
            ast::Expr::Atomic(e) => Self::lower_atomic(e, arena),
            ast::Expr::Application(e) => Self::lower_application(e, arena),
            ast::Expr::Infix(e) => Self::lower_infix(e, arena),
            ast::Expr::Typed(e) => Self::lower_typed(e, arena),
            ast::Expr::AndAlso(e) => Self::lower_andalso(e, arena),
            ast::Expr::OrElse(e) => Self::lower_orelse(e, arena),
            ast::Expr::Handle(e) => Self::lower_handle(e, arena),
            ast::Expr::Raise(e) => Self::lower_raise(e, arena),
            ast::Expr::If(e) => Self::lower_if(e, arena),
            ast::Expr::While(e) => Self::lower_while(e, arena),
            ast::Expr::Case(e) => Self::lower_case(e, arena),
            ast::Expr::Fn(e) => Self::lower_fn(e, arena),
        }
    }

    fn lower_atomic<A: BodyArena>(expr: &ast::AtomicExpr, arena: &mut A) -> ExprKind {
        match expr {
            ast::AtomicExpr::SCon(e) => Self::lower_scon(e, arena),
            ast::AtomicExpr::VId(e) => Self::lower_vid(e, arena),
            ast::AtomicExpr::Let(e) => Self::lower_let(e, arena),
            ast::AtomicExpr::Seq(e) => Self::lower_seq(e, arena),
            ast::AtomicExpr::Record(e) => Self::lower_record(e, arena),
            ast::AtomicExpr::Tuple(e) => Self::lower_tuple(e, arena),
            ast::AtomicExpr::Unit(e) => Self::lower_unit(e, arena),
            ast::AtomicExpr::List(e) => Self::lower_list(e, arena),
            ast::AtomicExpr::RecSel(e) => Self::lower_recsel(e, arena),
            ast::AtomicExpr::Paren(e) => Self::lower_paren(e, arena),
        }
    }

    fn lower_scon<A: BodyArena>(expr: &ast::SConExpr, _arena: &mut A) -> ExprKind {
        let scon = expr.scon().map(Scon::lower).unwrap_or(Scon::Missing);
        ExprKind::Scon(scon)
    }

    fn lower_vid<A: BodyArena>(expr: &ast::VIdExpr, arena: &mut A) -> ExprKind {
        let op = expr.op();
        let longvid = LongVId::from_opt_node(expr.longvid().as_ref(), arena);
        ExprKind::VId { op, longvid }
    }

    fn lower_let<A: BodyArena>(expr: &ast::LetExpr, arena: &mut A) -> ExprKind {
        let dec = Dec::lower_opt(expr.dec(), arena);

        let exprs = expr.exprs().map(|e| Self::lower(e, arena)).collect();
        let kind = ExprKind::Seq { exprs };

        let origin = ast::Expr::from(ast::AtomicExpr::from(expr.clone()));
        let expr = Self::generated(NodeParent::from_expr(&origin, arena), kind, arena);

        ExprKind::Let { dec, expr }
    }

    fn lower_seq<A: BodyArena>(expr: &ast::SeqExpr, arena: &mut A) -> ExprKind {
        let exprs = expr.exprs().map(|e| Self::lower(e, arena)).collect();
        ExprKind::Seq { exprs }
    }

    fn lower_unit<A: BodyArena>(_expr: &ast::UnitExpr, _arena: &mut A) -> ExprKind {
        ExprKind::Record { rows: Box::new([]) }
    }

    fn lower_record<A: BodyArena>(expr: &ast::RecordExpr, arena: &mut A) -> ExprKind {
        let rows = expr.exprows().map(|e| ExpRow::lower(e, arena)).collect();
        ExprKind::Record { rows }
    }

    fn lower_tuple<A: BodyArena>(expr: &ast::TupleExpr, arena: &mut A) -> ExprKind {
        let mut rows = vec![];
        for (i, e) in expr.exprs().enumerate() {
            let exp = Self::lower(e, arena);
            let label = (i + 1) as u32;
            let exprow = ExpRow::new_from_expr(exp, Label::Numeric(label));
            rows.push(exprow);
        }
        ExprKind::Record {
            rows: rows.into_boxed_slice(),
        }
    }

    fn lower_list<A: BodyArena>(expr: &ast::ListExpr, arena: &mut A) -> ExprKind {
        let origin = ast::Expr::from(ast::AtomicExpr::from(expr.clone()));
        lower_list(
            NodeParent::from_expr(&origin, arena),
            expr.exprs(),
            |longvid| ExprKind::VId { op: false, longvid },
            |lhs, vid, rhs| ExprKind::Infix { lhs, vid, rhs },
            arena,
        )
    }

    // "# lab" is lowered to "fn {lab=vid, .. } => vid"
    // where "vid" is a new (fresh) identifier
    fn lower_recsel<A: BodyArena>(expr: &ast::RecSelExpr, arena: &mut A) -> ExprKind {
        let origin = ast::Expr::from(ast::AtomicExpr::from(expr.clone()));
        let parent = NodeParent::from_expr(&origin, arena);

        let label = Label::from_token(expr.label(), arena);
        let newvid = LongVId::from_vid(arena.fresh_vid());

        // Generate inner pattern for the patrow,
        // the patrows (vid and wildcard), and the enclosing record pat
        let vid_pat = Pat::generated(
            parent.clone(),
            PatKind::VId {
                op: false,
                longvid: newvid.clone(),
            },
            arena,
        );
        let rows = [
            PatRow::Pattern {
                label,
                pat: vid_pat,
            },
            PatRow::Wildcard,
        ]
        .into_iter()
        .collect();
        let record_pat = Pat::generated(parent.clone(), PatKind::Record { rows }, arena);

        // Generate expr for the rhs of the match
        let vid_expr = Expr::generated(
            parent.clone(),
            ExprKind::VId {
                op: false,
                longvid: newvid,
            },
            arena,
        );

        // Generate the match
        let mrule = [MRule {
            pat: record_pat,
            expr: vid_expr,
        }]
        .into_iter()
        .collect();

        ExprKind::Fn { match_: mrule }
    }

    // FIXME: this is a bit gross, maybe need to rethink the traits??
    fn lower_paren<A: BodyArena>(expr: &ast::ParenExpr, arena: &mut A) -> ExprKind {
        match expr.expr() {
            Some(e) => Self::to_kind(&e, arena),
            None => ExprKind::Missing,
        }
    }

    fn lower_application<A: BodyArena>(expr: &ast::ApplicationExpr, arena: &mut A) -> ExprKind {
        let app = Self::lower_opt(expr.application(), arena);
        let param = Self::lower_opt(expr.atomic(), arena);
        ExprKind::Application { expr: app, param }
    }

    fn lower_infix<A: BodyArena>(expr: &ast::InfixExpr, arena: &mut A) -> ExprKind {
        let lhs = Self::lower_opt(expr.expr_1(), arena);
        let vid = VId::from_token(expr.vid(), arena);
        let rhs = Self::lower_opt(expr.expr_2(), arena);
        ExprKind::Infix { lhs, vid, rhs }
    }

    fn lower_typed<A: BodyArena>(expr: &ast::TypedExpr, arena: &mut A) -> ExprKind {
        let ty = Type::lower_opt(expr.ty(), arena);
        let expr = Self::lower_opt(expr.expr(), arena);
        ExprKind::Typed { expr, ty }
    }

    // Wait... is parsing of "true" and "false" wrong???
    // Currently they are parsed as bare (String?) identifiers.
    // This might be correct.. it also might be wrong...
    //
    // "exp1 andalso exp2" desugars to "if exp1 then exp2 else false"
    fn lower_andalso<A: BodyArena>(expr: &ast::AndAlsoExpr, arena: &mut A) -> ExprKind {
        let originating = ast::Expr::from(expr.clone());

        let vid_false = LongVId::from_vid(VId::from_builtin(BuiltIn::False));
        let false_expr = Self::generated(
            NodeParent::from_expr(&originating, arena),
            ExprKind::VId {
                op: false,
                longvid: vid_false,
            },
            arena,
        );

        let expr_1 = Self::lower_opt(expr.expr_1(), arena);
        let expr_2 = Self::lower_opt(expr.expr_2(), arena);

        Self::_lower_if(&originating, expr_1, expr_2, false_expr, arena)
    }

    // "exp1 orelse exp2" desugars to "if exp1 then true else exp2"
    fn lower_orelse<A: BodyArena>(expr: &ast::OrElseExpr, arena: &mut A) -> ExprKind {
        let originating = ast::Expr::from(expr.clone());

        let vid_true = LongVId::from_vid(VId::from_builtin(BuiltIn::True));
        let true_expr = Self::generated(
            NodeParent::from_expr(&originating, arena),
            ExprKind::VId {
                op: false,
                longvid: vid_true,
            },
            arena,
        );

        let expr_1 = Self::lower_opt(expr.expr_1(), arena);
        let expr_2 = Self::lower_opt(expr.expr_2(), arena);

        Self::_lower_if(&originating, expr_1, true_expr, expr_2, arena)
    }

    fn lower_handle<A: BodyArena>(expr: &ast::HandleExpr, arena: &mut A) -> ExprKind {
        let match_ = match expr.match_expr() {
            Some(m) => MRule::lower_from_match(&m, arena),
            None => Box::new([]),
        };
        let expr = Self::lower_opt(expr.expr(), arena);
        ExprKind::Handle { expr, match_ }
    }

    fn lower_raise<A: BodyArena>(expr: &ast::RaiseExpr, arena: &mut A) -> ExprKind {
        let expr = Self::lower_opt(expr.expr(), arena);
        ExprKind::Raise { expr }
    }

    fn lower_if<A: BodyArena>(expr: &ast::IfExpr, arena: &mut A) -> ExprKind {
        let expr1 = Self::lower_opt(expr.expr_1(), arena);
        let expr2 = Self::lower_opt(expr.expr_2(), arena);
        let expr3 = Self::lower_opt(expr.expr_3(), arena);
        Self::_lower_if(&ast::Expr::from(expr.clone()), expr1, expr2, expr3, arena)
    }

    fn _lower_if<A: BodyArena>(
        originating_expr: &ast::Expr,
        expr1: Idx<Expr>,
        expr2: Idx<Expr>,
        expr3: Idx<Expr>,
        arena: &mut A,
    ) -> ExprKind {
        let parent = NodeParent::from_expr(originating_expr, arena);

        let true_pat = Pat::generated(
            parent,
            PatKind::VId {
                op: false,
                longvid: LongVId::from_vid(VId::from_builtin(BuiltIn::True)),
            },
            arena,
        );
        let false_pat = Pat::generated(
            parent,
            PatKind::VId {
                op: false,
                longvid: LongVId::from_vid(VId::from_builtin(BuiltIn::False)),
            },
            arena,
        );

        let match_arms = [(true_pat, expr2), (false_pat, expr3)]
            .into_iter()
            .map(|(pat, expr)| MRule { pat, expr })
            .collect();

        Self::_lower_case(originating_expr, expr1, match_arms, arena)
    }

    fn lower_while<A: BodyArena>(expr: &ast::WhileExpr, arena: &mut A) -> ExprKind {
        let exp1 = Self::lower_opt(expr.expr_1(), arena);
        let exp2 = Self::lower_opt(expr.expr_2(), arena);

        let origin = ast::Expr::from(expr.clone());
        let parent = NodeParent::from_expr(&origin, arena);
        let newvid = LongVId::from_vid(arena.fresh_vid());

        let unitexpr = Self::generated(
            parent.clone(),
            ExprKind::Record { rows: Box::new([]) },
            arena,
        );
        let videxpr = Self::generated(
            parent.clone(),
            ExprKind::VId {
                op: false,
                longvid: newvid.clone(),
            },
            arena,
        );
        let appexpr = Self::generated(
            parent.clone(),
            ExprKind::Application {
                expr: videxpr,
                param: unitexpr,
            },
            arena,
        );

        let seqexpr = Self::generated(
            parent.clone(),
            ExprKind::Seq {
                exprs: Box::new([exp2, appexpr.clone()]),
            },
            arena,
        );

        let fn_pat = Pat::generated(
            parent.clone(),
            PatKind::Record { rows: Box::new([]) },
            arena,
        );
        let fn_inner_expr = Self::generated(
            parent.clone(),
            Self::_lower_if(&origin, exp1, seqexpr, unitexpr, arena),
            arena,
        );
        let match_ = Box::new([MRule {
            pat: fn_pat,
            expr: fn_inner_expr,
        }]);

        let fn_expr = Self::generated(parent.clone(), ExprKind::Fn { match_ }, arena);

        let vidpat = Pat::generated(
            parent.clone(),
            PatKind::VId {
                op: false,
                longvid: newvid.clone(),
            },
            arena,
        );

        let viddec = Dec::generated(
            parent.clone(),
            DecKind::Val {
                rec: true,
                tyvarseq: Box::new([]),
                pat: vidpat,
                expr: fn_expr,
            },
            arena,
        );

        ExprKind::Let {
            dec: viddec,
            expr: appexpr,
        }
    }

    fn lower_case<A: BodyArena>(expr: &ast::CaseExpr, arena: &mut A) -> ExprKind {
        let match_ = match expr.match_expr() {
            None => Box::new([]),
            Some(m) => MRule::lower_from_match(&m, arena),
        };
        let test = Self::lower_opt(expr.expr(), arena);
        Self::_lower_case(&ast::Expr::from(expr.clone()), test, match_, arena)
    }

    fn _lower_case<A: BodyArena>(
        originating_expr: &ast::Expr,
        test: Idx<Expr>,
        boxed_match: Box<[MRule]>,
        arena: &mut A,
    ) -> ExprKind {
        let lowered_case = ExprKind::Fn {
            match_: boxed_match,
        };
        let lowered_case = Self::generated(
            NodeParent::from_expr(&originating_expr, arena),
            lowered_case,
            arena,
        );

        ExprKind::Application {
            expr: lowered_case,
            param: test,
        }
    }

    fn lower_fn<A: BodyArena>(expr: &ast::FnExpr, arena: &mut A) -> ExprKind {
        let match_ = match expr.match_expr() {
            None => Box::new([]),
            Some(m) => MRule::lower_from_match(&m, arena),
        };
        ExprKind::Fn { match_ }
    }
}

impl ExpRow {
    pub fn lower<A: BodyArena>(exprow: ast::ExprRow, arena: &mut A) -> Self {
        let expr = Expr::lower_opt(exprow.expr(), arena);
        let label = Label::from_token(exprow.label(), arena);
        Self::new_from_expr(expr, label)
    }

    pub fn new_from_expr(expr: Idx<Expr>, label: Label) -> Self {
        Self { label, expr }
    }
}

impl MRule {
    pub fn lower<A: BodyArena>(mrule: &ast::Mrule, arena: &mut A) -> Self {
        let pat = Pat::lower_opt(mrule.pat(), arena);
        let expr = Expr::lower_opt(mrule.expr(), arena);
        Self { pat, expr }
    }

    pub fn lower_from_match<A: BodyArena>(match_expr: &ast::Match, arena: &mut A) -> Box<[Self]> {
        match_expr
            .mrules()
            .map(|m| Self::lower(&m, arena))
            .collect()
    }
}

impl Scon {
    pub fn lower(node: ast::Scon) -> Self {
        match node {
            ast::Scon::Int(s) => {
                let s = s.text();

                let i = if let Some('~') = s.chars().next() {
                    s[1..].parse::<i128>().map(|i| -i)
                } else {
                    s.parse()
                };
                i.map(Self::Int).unwrap_or_else(|_| Self::Missing)
            }
            ast::Scon::Word(s) => s
                .text()
                .parse()
                .map(Self::Word)
                .unwrap_or_else(|_| Self::Missing),
            ast::Scon::Real(s) => s
                .text()
                .parse::<f64>()
                .map(FloatWrapper::new)
                .map(Self::Real)
                .unwrap_or_else(|_| Self::Missing),
            ast::Scon::Char(s) => s
                .text()
                .chars()
                .next()
                .map(Self::Char)
                .unwrap_or_else(|| Self::Missing),
            ast::Scon::String(s) => {
                // FIXME: intern strings??
                let s = s.text().to_owned();
                Self::String(s)
            }
        }
    }
}

impl HirLower for Pat {
    type AstType = ast::Pat;

    fn missing<A: BodyArena>(arena: &mut A) -> Idx<Self> {
        let p = Self {
            kind: PatKind::Missing,
            ast_id: AstId::Missing,
        };
        arena.alloc_pat(p)
    }

    fn lower<A: BodyArena>(pat: ast::Pat, arena: &mut A) -> Idx<Self> {
        let kind = match pat.clone() {
            ast::Pat::Atomic(p) => Self::lower_atomic(p, arena),
            ast::Pat::Typed(p) => Self::lower_typed(p, arena),
            ast::Pat::Cons(p) => Self::lower_cons(p, arena),
            ast::Pat::ConsInfix(p) => Self::lower_cons_infix(p, arena),
            ast::Pat::Layered(p) => Self::lower_layered(p, arena),
        };
        let ast_id = AstId::Node(arena.alloc_ast_id(&pat));
        let p = Self { kind, ast_id };
        arena.alloc_pat(p)
    }
}

impl HirLowerGenerated for Pat {
    type Kind = PatKind;

    fn generated<A: BodyArena>(origin: NodeParent, kind: Self::Kind, arena: &mut A) -> Idx<Self> {
        let p = Pat {
            kind,
            ast_id: AstId::Generated(origin),
        };
        arena.alloc_pat(p)
    }
}

impl Pat {
    fn lower_atomic<A: BodyArena>(pat: ast::AtomicPat, arena: &mut A) -> PatKind {
        match pat {
            ast::AtomicPat::Wildcard(p) => Self::lower_wildcard(p, arena),
            ast::AtomicPat::SCon(p) => Self::lower_scon(p, arena),
            ast::AtomicPat::VId(p) => Self::lower_vid(p, arena),
            ast::AtomicPat::List(p) => Self::lower_list(p, arena),
            ast::AtomicPat::Tuple(p) => Self::lower_tuple(p, arena),
            ast::AtomicPat::Record(p) => Self::lower_record(p, arena),
            ast::AtomicPat::Unit(_) => PatKind::Record { rows: Box::new([]) },
        }
    }

    fn lower_wildcard<A: BodyArena>(_pat: ast::WildcardPat, _arena: &mut A) -> PatKind {
        PatKind::Wildcard
    }

    fn lower_scon<A: BodyArena>(pat: ast::SConPat, _arena: &mut A) -> PatKind {
        let scon = pat.scon().map(Scon::lower).unwrap_or(Scon::Missing);
        PatKind::Scon(scon)
    }

    fn lower_vid<A: BodyArena>(pat: ast::VIdPat, arena: &mut A) -> PatKind {
        let op = pat.op();
        let longvid = pat
            .longvid()
            .map(|node| LongVId::from_node(&node, arena))
            .unwrap_or_else(|| LongVId::missing());

        PatKind::VId { op, longvid }
    }

    // [pat1, pat2, ..., patn] lowers to pat1 :: pat2 :: ... :: patn :: nil
    fn lower_list<A: BodyArena>(pat: ast::ListPat, arena: &mut A) -> PatKind {
        let origin = ast::Pat::from(ast::AtomicPat::from(pat.clone()));
        lower_list(
            NodeParent::from_pat(&origin, arena),
            pat.pats(),
            |longvid| PatKind::VId { op: false, longvid },
            |lhs, vid, rhs| PatKind::Infix { lhs, vid, rhs },
            arena,
        )
    }

    fn lower_tuple<A: BodyArena>(tuple: ast::TuplePat, arena: &mut A) -> PatKind {
        let mut rows = vec![];

        for (i, p) in tuple.pats().enumerate() {
            let pat = Pat::lower(p, arena);
            let label = (i + 1) as u32;
            rows.push(PatRow::new_from_pat(pat, Label::Numeric(label), arena));
        }

        PatKind::Record {
            rows: rows.into_boxed_slice(),
        }
    }

    fn lower_record<A: BodyArena>(pat: ast::RecordPat, arena: &mut A) -> PatKind {
        let rows = pat.patrows().map(|p| PatRow::lower(p, arena)).collect();
        PatKind::Record { rows }
    }

    fn lower_typed<A: BodyArena>(pat: ast::TypedPat, arena: &mut A) -> PatKind {
        let p = Self::lower_opt(pat.pat(), arena);
        let ty = Type::lower_opt(pat.ty(), arena);
        PatKind::Typed { pat: p, ty }
    }

    fn lower_cons<A: BodyArena>(pat: ast::ConsPat, arena: &mut A) -> PatKind {
        let op = pat.op();
        let longvid = LongVId::from_opt_node(pat.longvid().as_ref(), arena);
        let pat_node = pat.atpat().map(ast::Pat::from);
        let pat = Pat::lower_opt(pat_node, arena);
        PatKind::Constructed { op, longvid, pat }
    }

    fn lower_cons_infix<A: BodyArena>(pat: ast::ConsInfixPat, arena: &mut A) -> PatKind {
        let lhs = Pat::lower_opt(pat.pat_1(), arena);
        let vid = VId::from_token(pat.vid(), arena);
        let rhs = Pat::lower_opt(pat.pat_2(), arena);
        PatKind::Infix { lhs, vid, rhs }
    }

    fn lower_layered<A: BodyArena>(pat: ast::LayeredPat, arena: &mut A) -> PatKind {
        let op = pat.op();
        let vid = VId::from_token(pat.vid(), arena);
        let ty = pat.ty().map(|t| Type::lower(t, arena));
        let pat = Pat::lower_opt(pat.pat(), arena);
        PatKind::Layered { op, vid, ty, pat }
    }
}

impl PatRow {
    pub fn lower<A: BodyArena>(patrow: ast::PatRow, arena: &mut A) -> Self {
        let pat = patrow
            .pat()
            .map(|node| Pat::lower(node, arena))
            .unwrap_or_else(|| Pat::missing(arena));
        let label = Label::from_token(patrow.label(), arena);
        Self::new_from_pat(pat, label, arena)
    }

    pub fn new_from_pat<A: BodyArena>(pat: Idx<Pat>, label: Label, arena: &mut A) -> Self {
        if let PatKind::Wildcard = arena.get_pat(pat).kind {
            Self::Wildcard
        } else {
            Self::Pattern { label, pat }
        }
    }
}

impl HirLower for Type {
    type AstType = ast::Ty;

    fn missing<A: BodyArena>(arena: &mut A) -> Idx<Self> {
        let t = Self {
            kind: TyKind::Missing,
            ast_id: AstId::Missing,
        };
        arena.alloc_ty(t)
    }

    fn lower<A: BodyArena>(ty: ast::Ty, arena: &mut A) -> Idx<Self> {
        let kind = match &ty {
            ast::Ty::Fun(t) => Self::lower_fun(t, arena),
            ast::Ty::Cons(t) => Self::lower_cons(t, arena),
            ast::Ty::Tuple(t) => Self::lower_tuple(t, arena),
            ast::Ty::TyVar(t) => Self::lower_tyvar(t, arena),
            ast::Ty::Record(t) => Self::lower_record(t, arena),
        };
        let t = Self {
            kind,
            ast_id: AstId::Node(arena.alloc_ast_id(&ty)),
        };
        arena.alloc_ty(t)
    }
}

impl HirLowerGenerated for Type {
    type Kind = TyKind;

    fn generated<A: BodyArena>(origin: NodeParent, kind: Self::Kind, arena: &mut A) -> Idx<Self> {
        let t = Self {
            kind,
            ast_id: AstId::Generated(origin),
        };
        arena.alloc_ty(t)
    }
}

impl Type {
    fn lower_fun<A: BodyArena>(ty: &ast::FunTy, arena: &mut A) -> TyKind {
        let domain = Self::lower_opt(ty.ty_1(), arena);
        let range = Self::lower_opt(ty.ty_2(), arena);
        TyKind::Function { domain, range }
    }

    fn lower_cons<A: BodyArena>(ty: &ast::ConsTy, arena: &mut A) -> TyKind {
        let tyseq = ty.tys().map(|t| Self::lower(t, arena)).collect();
        let longtycon = LongTyCon::from_opt_node(ty.longtycon().as_ref(), arena);
        TyKind::Constructed { tyseq, longtycon }
    }

    fn lower_tuple<A: BodyArena>(ty: &ast::TupleTy, arena: &mut A) -> TyKind {
        let mut tyrows = vec![];
        for (i, t) in ty.tys().enumerate() {
            let label = (i + 1) as u32;
            let tyrow = TyRow::new_from_ty(Self::lower(t, arena), Label::Numeric(label));
            tyrows.push(tyrow);
        }
        TyKind::Record {
            tyrows: tyrows.into_boxed_slice(),
        }
    }

    fn lower_tyvar<A: BodyArena>(ty: &ast::TyVarTy, arena: &mut A) -> TyKind {
        let idx = TyVar::from_token(ty.tyvar(), arena);
        TyKind::Var(idx)
    }

    fn lower_record<A: BodyArena>(ty: &ast::RecordTy, arena: &mut A) -> TyKind {
        let tyrows = ty.tyrows().map(|t| TyRow::lower(t, arena)).collect();
        TyKind::Record { tyrows }
    }
}

impl TyRow {
    pub fn lower<A: BodyArena>(tyrow: ast::TyRow, arena: &mut A) -> Self {
        let ty = Type::lower_opt(tyrow.ty(), arena);
        let label = Label::from_token(tyrow.label(), arena);
        Self { label, ty }
    }

    pub fn new_from_ty(ty: Idx<Type>, label: Label) -> Self {
        Self { label, ty }
    }
}
