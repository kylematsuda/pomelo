use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::util::lower_tyvarseq;
use crate::lower::{HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{AstId, Dec, DecKind, Expr, NodeParent, Pat, ValBind};

impl HirLower for Dec {
    type AstType = ast::Dec;

    fn lower(ctx: &mut LoweringCtxt, ast: Self::AstType) -> Idx<Self> {
        let ast_id = ctx.alloc_ast_id(&ast);
        ctx.make_rec_dec(
            AstId::Node(ast_id),
            |ctx, index| match &ast {
                ast::Dec::Val(v) => Self::lower_val(ctx, &v, index),
                ast::Dec::Fun(f) => Self::lower_fun(ctx, &f, index),
                ast::Dec::Type(t) => Self::lower_type(ctx, &t),
                ast::Dec::Datatype(d) => Self::lower_datatype(ctx, &d),
                ast::Dec::DatatypeRep(r) => Self::lower_replication(ctx, &r),
                ast::Dec::Abstype(a) => Self::lower_abstype(ctx, &a),
                ast::Dec::Exception(e) => Self::lower_exception(ctx, &e),
                ast::Dec::Local(l) => Self::lower_local(ctx, &l),
                ast::Dec::Open(o) => Self::lower_open(ctx, &o),
                ast::Dec::Infix(i) => Self::lower_infix(ctx, &i),
                ast::Dec::Infixr(i) => Self::lower_infixr(ctx, &i),
                ast::Dec::Nonfix(n) => Self::lower_nonfix(ctx, &n),
                ast::Dec::Seq(s) => Self::lower_seq(ctx, &s),
            },
        )
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
        let tyvarseq = lower_tyvarseq(ctx, dec.tyvarseq());

        let bindings = dec
            .bindings()
            .map(|b| {
                let rec = b.rec();
                let pat = Pat::lower_opt(ctx, b.pat());

                // We need to register pat's bindings with the `Resolver`
                // before lowering `expr`!
                if rec {
                    ctx.register_rec_pat(pat, index);
                }

                let expr = Expr::lower_opt(ctx, b.expr());
                ValBind { rec, pat, expr }
            })
            .collect();

        DecKind::Val { tyvarseq, bindings }
    }

    fn lower_fun(_ctx: &mut LoweringCtxt, _dec: &ast::FunDec, _index: Idx<Dec>) -> DecKind {
        todo!() // this one is horrible...
    }

    fn lower_type(_ctx: &mut LoweringCtxt, _dec: &ast::TypeDec) -> DecKind {
        todo!()
    }

    fn lower_datatype(_ctx: &mut LoweringCtxt, _dec: &ast::DatatypeDec) -> DecKind {
        todo!()
    }

    fn lower_replication(_ctx: &mut LoweringCtxt, _dec: &ast::DatatypeRepDec) -> DecKind {
        todo!()
    }

    fn lower_abstype(_ctx: &mut LoweringCtxt, _dec: &ast::AbstypeDec) -> DecKind {
        todo!()
    }

    fn lower_exception(_ctx: &mut LoweringCtxt, _dec: &ast::ExceptionDec) -> DecKind {
        todo!()
    }

    fn lower_local(_ctx: &mut LoweringCtxt, _dec: &ast::LocalDec) -> DecKind {
        todo!()
    }

    fn lower_open(_ctx: &mut LoweringCtxt, _dec: &ast::OpenDec) -> DecKind {
        todo!()
    }

    fn lower_infix(_ctx: &mut LoweringCtxt, _dec: &ast::InfixDec) -> DecKind {
        todo!()
    }

    fn lower_infixr(_ctx: &mut LoweringCtxt, _dec: &ast::InfixrDec) -> DecKind {
        todo!()
    }

    fn lower_nonfix(_ctx: &mut LoweringCtxt, __dec: &ast::NonfixDec) -> DecKind {
        todo!()
    }

    fn lower_seq(_ctx: &mut LoweringCtxt, _dec: &ast::SeqDec) -> DecKind {
        todo!()
    }
}
