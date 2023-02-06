use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::util;
use crate::lower::{HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{
    AstId, ConBind, DataBind, Dec, DecKind, Expr, Fixity, LongStrId, LongTyCon, NodeParent, Pat,
    Ty, TyCon, TypBind, VId, ValBind,
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

    fn lower_fun(_ctx: &mut LoweringCtxt, _dec: &ast::FunDec, _index: Idx<Dec>) -> DecKind {
        todo!() // this one is horrible...
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
