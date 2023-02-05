use pomelo_parse::ast;

use crate::arena::Idx;
use crate::identifiers::{BuiltIn, LongVId, VId};
use crate::lower::{HirLowerGenerated, LoweringCtxt};
use crate::NodeParent;
use crate::TyVar;

// Used to lower lists [a1, a2, ... ] to a1 :: a2 :: ... :: nil
// Common code for both [`Expr`] and [`Pat`].
// This is factored out on its own since it's probably the most complicated
// part of this stage of lowering.
//
// elts: e.g., "expr.exprs()" or "pat.pats()"
// vid_kind: construct an `ExprKind::VId` or `PatKind::VId`
// infix_kind: construct an `ExprKind::Infix` or `PatKind::Infix`
pub(super) fn _lower_list<H: HirLowerGenerated>(
    ctx: &mut LoweringCtxt,
    origin: NodeParent,
    elts: impl Iterator<Item = H::AstType>,
    vid_kind: impl Fn(LongVId) -> H::Kind,
    infix_kind: impl Fn(Idx<H>, VId, Idx<H>) -> H::Kind,
) -> H::Kind {
    let mut rev_indexed = elts
        .map(|e| H::lower(ctx, e))
        .enumerate()
        .collect::<Vec<_>>();
    rev_indexed.reverse();

    let nil = LongVId::from_vid(VId::from_builtin(BuiltIn::Nil));

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
            last = infix_kind(p_idx, cons, last_idx);
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
    tyvarseq
        .map(|t| TyVar::from_token(Some(t), ctx.interner_mut()))
        .collect()
}
