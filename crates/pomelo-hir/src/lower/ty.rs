use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::{HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{AstId, Label, LongTyCon, NodeParent, Ty, TyKind, TyRow, TyVar};

impl HirLower for Ty {
    type AstType = ast::Ty;

    fn lower(ctx: &mut LoweringCtxt, ast: Self::AstType) -> Idx<Self> {
        let kind = match &ast {
            ast::Ty::Fun(t) => Self::lower_fun(ctx, t),
            ast::Ty::Cons(t) => Self::lower_cons(ctx, t),
            ast::Ty::Tuple(t) => Self::lower_tuple(ctx, t),
            ast::Ty::TyVar(t) => Self::lower_tyvar(ctx, t),
            ast::Ty::Record(t) => Self::lower_record(ctx, t),
        };
        let t = Self {
            kind,
            ast_id: AstId::Node(ctx.alloc_ast_id(&ast)),
        };
        ctx.add_ty(t)
    }

    fn missing(ctx: &mut LoweringCtxt) -> Idx<Self> {
        let t = Self {
            kind: TyKind::Missing,
            ast_id: AstId::Missing,
        };
        ctx.add_ty(t)
    }
}

impl HirLowerGenerated for Ty {
    type Kind = TyKind;

    fn generated(ctx: &mut LoweringCtxt, origin: NodeParent, kind: Self::Kind) -> Idx<Self> {
        let t = Self {
            kind,
            ast_id: crate::AstId::Generated(origin),
        };
        ctx.add_ty(t)
    }
}

impl Ty {
    fn lower_fun(ctx: &mut LoweringCtxt, t: &ast::FunTy) -> TyKind {
        let domain = Self::lower_opt(ctx, t.ty_1());
        let range = Self::lower_opt(ctx, t.ty_2());
        TyKind::Function { domain, range }
    }

    fn lower_cons(ctx: &mut LoweringCtxt, t: &ast::ConsTy) -> TyKind {
        let tyseq = t.tys().map(|t| Self::lower(ctx, t)).collect();
        let con = LongTyCon::from_opt_node(ctx, t.longtycon().as_ref());
        let loc = ctx.resolver().lookup_ty(&con);
        TyKind::Constructed {
            tyseq,
            longtycon: (con, loc),
        }
    }

    fn lower_tuple(ctx: &mut LoweringCtxt, t: &ast::TupleTy) -> TyKind {
        let mut tyrows = vec![];
        for (i, t) in t.tys().enumerate() {
            let label = Label::Numeric((i + 1) as u32);
            let tyrow = TyRow {
                label,
                ty: Self::lower(ctx, t),
            };
            tyrows.push(tyrow);
        }
        TyKind::Record {
            tyrows: tyrows.into_boxed_slice(),
        }
    }

    fn lower_tyvar(ctx: &mut LoweringCtxt, t: &ast::TyVarTy) -> TyKind {
        let idx = TyVar::from_token(ctx, t.tyvar());
        TyKind::Var(idx)
    }

    fn lower_record(ctx: &mut LoweringCtxt, t: &ast::RecordTy) -> TyKind {
        let tyrows = t.tyrows().map(|t| TyRow::lower(ctx, &t)).collect();
        TyKind::Record { tyrows }
    }
}

impl TyRow {
    fn lower(ctx: &mut LoweringCtxt, tyrow: &ast::TyRow) -> Self {
        let ty = Ty::lower_opt(ctx, tyrow.ty());
        let label = Label::from_token(ctx, tyrow.label());
        Self { label, ty }
    }
}
