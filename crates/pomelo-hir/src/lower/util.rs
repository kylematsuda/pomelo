use pomelo_parse::ast;

use crate::arena::Idx;
use crate::identifiers::{BuiltIn, LongVId, TyVar, VId};
use crate::lower::{HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{DefLoc, Expr, FloatWrapper, MRule, NodeParent, Pat, Scon};

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
        LongVId::from_vid(VId::from_builtin(BuiltIn::Nil)),
        DefLoc::Builtin,
    );

    if rev_indexed.is_empty() {
        vid_kind(nil)
    } else {
        let cons = VId::from_builtin(BuiltIn::Cons);

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
        let loc = ctx.resolver().lookup_vid(&LongVId::from_vid(vid));
        (vid, loc)
    })
    .collect()
}

impl NodeParent {
    pub fn from_expr(ctx: &mut LoweringCtxt, expr: &ast::Expr) -> Self {
        let id = ctx.alloc_ast_id(expr);
        Self::Expr(id)
    }

    pub fn from_pat(ctx: &mut LoweringCtxt, pat: &ast::Pat) -> Self {
        let id = ctx.alloc_ast_id(pat);
        Self::Pat(id)
    }

    pub fn from_dec(ctx: &mut LoweringCtxt, dec: &ast::Dec) -> Self {
        let id = ctx.alloc_ast_id(dec);
        Self::Dec(id)
    }

    pub fn as_span(&self, ctx: &LoweringCtxt) -> Option<(usize, usize)> {
        match self {
            Self::Dec(d) => ctx.get_ast_span(*d),
            Self::Expr(e) => ctx.get_ast_span(*e),
            Self::Pat(p) => ctx.get_ast_span(*p),
        }
    }
}

pub(crate) fn lower_match(ctx: &mut LoweringCtxt, match_expr: &ast::Match) -> Box<[MRule]> {
    match_expr.mrules().map(|m| MRule::lower(ctx, &m)).collect()
}

impl MRule {
    // FIXME: we need to register the bindings in the pat
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
