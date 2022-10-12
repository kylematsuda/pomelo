use crate::arena::Idx;
use crate::core::{
    AstId, BodyArena, Dec, DecKind, ExpRow, Expr, ExprKind, FloatWrapper, MRule, NodeParent, Pat,
    PatKind, PatRow, Scon, TyKind, TyRow, Type,
};
use crate::identifiers::{BuiltIn, Label, LongTyCon, LongVId, TyVar, VId};
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
    type Kind;
    fn generated<A: BodyArena>(origin: NodeParent, kind: Self::Kind, arena: &mut A) -> Idx<Self>;
}

impl Dec {
    pub fn missing<A: BodyArena>(arena: &mut A) -> Idx<Self> {
        let d = Self {
            kind: DecKind::Missing,
            ast_id: AstId::Missing,
        };
        arena.alloc_dec(d)
    }

    pub fn mapped_at_node<A: BodyArena>(dec: ast::Dec, kind: DecKind, arena: &mut A) -> Idx<Self> {
        let ast_id = AstId::Node(arena.alloc_ast_id(&dec));
        let d = Self { kind, ast_id };
        arena.alloc_dec(d)
    }

    pub fn lower_opt<A: BodyArena>(opt_dec: Option<ast::Dec>, arena: &mut A) -> Idx<Self> {
        match opt_dec {
            Some(dec) => Self::lower(dec, arena),
            None => Self::missing(arena),
        }
    }

    pub fn lower<A: BodyArena>(dec: ast::Dec, arena: &mut A) -> Idx<Self> {
        match dec {
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
        }
    }

    fn lower_val<A: BodyArena>(_dec: ast::ValDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_fun<A: BodyArena>(_dec: ast::FunDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_type<A: BodyArena>(_dec: ast::TypeDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_datatype<A: BodyArena>(_dec: ast::DatatypeDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_replication<A: BodyArena>(_dec: ast::DatatypeRepDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_abstype<A: BodyArena>(_dec: ast::AbstypeDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_exception<A: BodyArena>(_dec: ast::ExceptionDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_local<A: BodyArena>(_dec: ast::LocalDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_open<A: BodyArena>(_dec: ast::OpenDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_infix<A: BodyArena>(_dec: ast::InfixDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_infixr<A: BodyArena>(_dec: ast::InfixrDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_nonfix<A: BodyArena>(_dec: ast::NonfixDec, _arena: &mut A) -> Idx<Self> {
        todo!()
    }

    fn lower_seq<A: BodyArena>(_dec: ast::SeqDec, _arena: &mut A) -> Idx<Self> {
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
        let kind = match &ast {
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
        };
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
            let exprow = ExpRow::new_from_expr(exp, Label::Numeric(i as u32), arena);
            rows.push(exprow);
        }
        ExprKind::Record {
            rows: rows.into_boxed_slice(),
        }
    }

    fn lower_list<A: BodyArena>(expr: &ast::ListExpr, arena: &mut A) -> ExprKind {
        let mut rev_expr_indexes = expr
            .exprs()
            .map(|e| Expr::lower(e, arena))
            .enumerate()
            .collect::<Vec<_>>();
        rev_expr_indexes.reverse();

        if rev_expr_indexes.len() == 0 {
            ExprKind::Nil
        } else {
            let cons = VId::from_builtin(BuiltIn::Cons, arena);

            // Remember our AST position, since lowering will generate new nodes
            let node = ast::Expr::from(ast::AtomicExpr::from(expr.clone()));
            let node = NodeParent::Expr(arena.alloc_ast_id(&node));

            // The list ends with a nil pat
            let nil_expr = Expr::generated(node.clone(), ExprKind::Nil, arena);

            let mut last_idx = nil_expr;
            let mut last = ExprKind::Nil;

            // "::" is right-associative, so we walk the list of pats in reverse.
            // We allocate each generated infix expr in the arena, except for the
            // final one ("hd :: ( ... )"), whose `ExprKind` we need to return from the function
            for (i, p_idx) in rev_expr_indexes {
                last = ExprKind::Infix {
                    lhs: p_idx,
                    vid: cons,
                    rhs: last_idx,
                };

                if i == 0 {
                    return last;
                }
                last_idx = Expr::generated(node.clone(), last.clone(), arena);
            }
            last
        }
    }

    fn lower_recsel<A: BodyArena>(_expr: &ast::RecSelExpr, _arena: &mut A) -> ExprKind {
        todo!()
    }

    fn lower_application<A: BodyArena>(expr: &ast::ApplicationExpr, arena: &mut A) -> ExprKind {
        let param = Self::lower_opt(expr.atomic().map(ast::Expr::from), arena);
        let expr = Self::lower_opt(expr.application().map(ast::Expr::from), arena);
        ExprKind::Application { expr, param }
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
        let originating = NodeParent::from_expr(&ast::Expr::from(expr.clone()), arena);

        let vid_false = LongVId::from_vid(VId::from_builtin(BuiltIn::False, arena));
        let false_expr = Self::generated(
            originating,
            ExprKind::VId {
                op: false,
                longvid: vid_false,
            },
            arena,
        );

        let expr_1 = Self::lower_opt(expr.expr_1(), arena);
        let expr_2 = Self::lower_opt(expr.expr_2(), arena);

        Self::_lower_if(originating, expr_1, expr_2, false_expr, arena)
    }

    // "exp1 orelse exp2" desugars to "if exp1 then true else exp2"
    fn lower_orelse<A: BodyArena>(expr: &ast::OrElseExpr, arena: &mut A) -> ExprKind {
        let originating = NodeParent::from_expr(&ast::Expr::from(expr.clone()), arena);

        let vid_true = LongVId::from_vid(VId::from_builtin(BuiltIn::True, arena));
        let true_expr = Self::generated(
            originating,
            ExprKind::VId {
                op: false,
                longvid: vid_true,
            },
            arena,
        );

        let expr_1 = Self::lower_opt(expr.expr_1(), arena);
        let expr_2 = Self::lower_opt(expr.expr_2(), arena);

        Self::_lower_if(originating, expr_1, true_expr, expr_2, arena)
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
        Self::_lower_if(
            NodeParent::from_expr(&ast::Expr::from(expr.clone()), arena),
            expr1,
            expr2,
            expr3,
            arena,
        )
    }

    fn _lower_if<A: BodyArena>(
        _originating_expr: NodeParent,
        _expr1: Idx<Expr>,
        _expr2: Idx<Expr>,
        _expr3: Idx<Expr>,
        _arena: &mut A,
    ) -> ExprKind {
        // FIXME: need to let ast_id be a different kind. like here, we need to generate some Pats
        // from an Expr. How represent this? Maybe make a new type for ast_id,
        //
        // enum Source<T> {
        //     Original(T),
        //     Generated(GeneratedFrom)
        // }
        //
        // enum GeneratedFrom {
        //      Dec(..),
        //      Expr(..),
        //      Pat(..),
        // }
        todo!();
    }

    fn lower_while<A: BodyArena>(_expr: &ast::WhileExpr, _arena: &mut A) -> ExprKind {
        // Similarly, need to create new patterns out of nowhere...
        todo!()
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
        let label = Label::from_token(exprow.label());
        Self::new_from_expr(expr, label, arena)
    }

    pub fn new_from_expr<A: BodyArena>(expr: Idx<Expr>, label: Label, arena: &mut A) -> Self {
        let label = arena.alloc_label(label);
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
            .unwrap_or_else(|| LongVId::missing(arena));

        PatKind::VId { op, longvid }
    }

    // [pat1, pat2, ..., patn] lowers to pat1 :: pat2 :: ... :: patn :: nil
    fn lower_list<A: BodyArena>(pat: ast::ListPat, arena: &mut A) -> PatKind {
        let mut rev_pat_indexes = pat
            .pats()
            .map(|p| Pat::lower(p, arena))
            .enumerate()
            .collect::<Vec<_>>();
        rev_pat_indexes.reverse();

        if rev_pat_indexes.len() == 0 {
            PatKind::Nil
        } else {
            let cons = VId::from_builtin(BuiltIn::Cons, arena);

            // Remember our AST position, since lowering will generate new nodes
            let node = ast::Pat::from(ast::AtomicPat::from(pat.clone()));
            let node = NodeParent::Pat(arena.alloc_ast_id(&node));

            // The list ends with a nil pat
            let nil_pat = Pat::generated(node.clone(), PatKind::Nil, arena);

            let mut last_idx = nil_pat;
            let mut last = PatKind::Nil;

            // "::" is right-associative, so we walk the list of pats in reverse.
            // We allocate each generated infix pat in the arena, except for the
            // final one ("hd :: ( ... )"), whose `PatKind` we need to return from the function
            for (i, p_idx) in rev_pat_indexes {
                last = PatKind::Infix {
                    lhs: p_idx,
                    vid: cons,
                    rhs: last_idx,
                };

                if i == 0 {
                    return last;
                }
                last_idx = Pat::generated(node.clone(), last.clone(), arena);
            }
            last
        }
    }

    fn lower_tuple<A: BodyArena>(tuple: ast::TuplePat, arena: &mut A) -> PatKind {
        let mut rows = vec![];

        for (i, p) in tuple.pats().enumerate() {
            let pat = Pat::lower(p, arena);
            let label = Label::Numeric(i as u32);
            rows.push(PatRow::new_from_pat(pat, label, arena));
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
        let label = Label::from_token(patrow.label());
        Self::new_from_pat(pat, label, arena)
    }

    pub fn new_from_pat<A: BodyArena>(pat: Idx<Pat>, label: Label, arena: &mut A) -> Self {
        if let PatKind::Wildcard = arena.get_pat(pat).expect("pat index is valid").kind {
            Self::Wildcard
        } else {
            let label = arena.alloc_label(label);
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
            let tyrow = TyRow::new_from_ty(Self::lower(t, arena), Label::Numeric(i as u32), arena);
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
        let label = arena.alloc_label(Label::from_token(tyrow.label()));
        Self { label, ty }
    }

    pub fn new_from_ty<A: BodyArena>(ty: Idx<Type>, label: Label, arena: &mut A) -> Self {
        let label = arena.alloc_label(label);
        Self { label, ty }
    }
}
