use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::{HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{
    Builtin, DefLoc, Expr, FloatWrapper, LongVId, MRule, NodeParent, Pat, Scon, TyVar, VId,
};

// Used to lower lists [a1, a2, ... ] to a1 :: a2 :: ... :: nil
// Common code for both [`Expr`] and [`Pat`].
// This is factored out on its own since it's probably the most complicated
// part of this stage of lowering.
//
// elts: e.g., "expr.exprs()" or "pat.pats()"
// vid_kind: construct an `ExprKind::VId` or `PatKind::VId`
// infix_kind: construct an `ExprKind::Infix` or `PatKind::Infix`
pub(super) fn lower_list<H: HirLowerGenerated>(
    ctx: &mut LoweringCtxt,
    origin: NodeParent,
    elts: impl Iterator<Item = H::AstType>,
    vid_kind: impl Fn((LongVId, DefLoc)) -> H::Kind,
    infix_kind: impl Fn(Idx<H>, (VId, DefLoc), Idx<H>) -> H::Kind,
) -> H::Kind {
    let mut rev_indexed = elts
        .map(|e| H::lower(ctx, e))
        .enumerate()
        .collect::<Vec<_>>();
    rev_indexed.reverse();

    let nil = (
        LongVId::from(VId::from_builtin(Builtin::Nil)),
        DefLoc::Builtin,
    );

    if rev_indexed.is_empty() {
        vid_kind(nil)
    } else {
        let cons = VId::from_builtin(Builtin::Cons);

        // The list ends with a nil pat
        let nil_expr = H::generated(ctx, origin, vid_kind(nil.clone()));

        let mut last_idx = nil_expr;
        let mut last = vid_kind(nil);

        // "::" is right-associative, so we walk the list of pats in reverse.
        // We allocate each generated infix expr in the arena, except for the
        // final one ("hd :: ( ... )"), whose `ExprKind` we need to return from the function
        for (i, p_idx) in rev_indexed {
            last = infix_kind(p_idx, (cons, DefLoc::Builtin), last_idx);
            if i == 0 {
                return last;
            }
            last_idx = H::generated(ctx, origin, last.clone());
        }
        last
    }
}

pub(super) fn lower_tyvarseq(
    ctx: &mut LoweringCtxt,
    tyvarseq: impl Iterator<Item = ast::TyVar>,
) -> Box<[TyVar]> {
    tyvarseq.map(|t| TyVar::from_token(ctx, Some(t))).collect()
}

pub(super) fn lower_vids(
    ctx: &mut LoweringCtxt,
    vids: impl Iterator<Item = ast::VId>,
) -> Box<[(VId, DefLoc)]> {
    vids.map(|v| {
        let vid = VId::from_token(ctx, Some(v));
        let loc = ctx.resolver().lookup_vid(&LongVId::from(vid));
        (vid, loc)
    })
    .collect()
}

impl NodeParent {
    pub(super) fn from_expr(ctx: &mut LoweringCtxt, expr: &ast::Expr) -> Self {
        let id = ctx.alloc_ast_id(expr);
        Self::Expr(id)
    }

    pub(super) fn from_pat(ctx: &mut LoweringCtxt, pat: &ast::Pat) -> Self {
        let id = ctx.alloc_ast_id(pat);
        Self::Pat(id)
    }
}

pub(crate) fn lower_match(ctx: &mut LoweringCtxt, match_expr: &ast::Match) -> Box<[MRule]> {
    match_expr.mrules().map(|m| MRule::lower(ctx, &m)).collect()
}

impl MRule {
    fn lower(ctx: &mut LoweringCtxt, mrule: &ast::Mrule) -> Self {
        ctx.enter_scope(|ctx| {
            let pat = Pat::lower_opt(ctx, mrule.pat());
            ctx.register_pat_names_in_match(pat);
            let expr = Expr::lower_opt(ctx, mrule.expr());
            Self { pat, expr }
        })
    }
}

impl Scon {
    // TODO: do this in lexing???
    //
    // This also doesn't respect the lexical definitions of these...
    pub(crate) fn lower(node: ast::Scon) -> Self {
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
