//! Resolve infix vs applicative expressions and patterns.
//!
//! Pratt parsing strategy that basically follows [this
//! blog post](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html).
use std::iter::Peekable;

use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::{HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{Builtin, Expr, ExprKind, Fixity, LongVId, NodeParent};

#[rustfmt::skip]
pub(crate) const BUILTINS: [(Builtin, Fixity); 18] = [
    (Builtin::Star,	    Fixity::Left(Some(7))),
    (Builtin::Slash,	Fixity::Left(Some(7))),
    (Builtin::Div,		Fixity::Left(Some(7))),
    (Builtin::Mod,		Fixity::Left(Some(7))),
    (Builtin::Plus,		Fixity::Left(Some(6))),
    (Builtin::Minus,	Fixity::Left(Some(6))),
    (Builtin::Carat,	Fixity::Left(Some(6))),
    (Builtin::Cons,		Fixity::Right(Some(5))),
    (Builtin::At,		Fixity::Right(Some(5))),
    (Builtin::Eq,		Fixity::Left(Some(4))),
    (Builtin::Ineq,		Fixity::Left(Some(4))),
    (Builtin::Gtr,		Fixity::Left(Some(4))),
    (Builtin::GtrEq,    Fixity::Left(Some(4))),
    (Builtin::Less,		Fixity::Left(Some(4))),
    (Builtin::LessEq,	Fixity::Left(Some(4))),
    (Builtin::RefAssign,Fixity::Left(Some(3))),
    (Builtin::O,		Fixity::Left(Some(3))),
    (Builtin::Before,	Fixity::Left(Some(0))),
];

/// Maximum user-defined fixity is 9
const FN_APPL: Fixity = Fixity::Left(Some(10));

pub(crate) fn fix_infix(ctx: &mut LoweringCtxt, e: &ast::InfixOrAppExpr) -> Idx<Expr> {
    let origin = ast::Expr::from(e.clone());
    let parent = NodeParent::from_expr(ctx, &origin);

    for e in e.exprs() {
        eprintln!("{}", e);
    }

    let mut peekable = e.exprs().peekable();
    fix_infix_bp(ctx, parent, &mut peekable, 0)
}

// Pratt parser
fn fix_infix_bp<I: Iterator<Item = ast::Expr>>(
    ctx: &mut LoweringCtxt,
    parent: NodeParent,
    e: &mut Peekable<I>,
    min_bp: u8,
) -> Idx<Expr> {
    let mut lhs = Expr::lower_opt(ctx, e.next());

    loop {
        let next = match e.peek() {
            None => break,
            Some(e) => e.clone(),
        };

        if let Some((l_bp, r_bp)) = get_bp(ctx, &next) {
            if l_bp < min_bp {
                break;
            }

            // Eat the infix_vid
            let infix_vid = get_vid(ctx, &e.next().unwrap()).unwrap();
            let loc = ctx.resolver().lookup_vid(&infix_vid);
            let vid = infix_vid.try_into_vid().unwrap();

            // Parse rhs expr
            let rhs = fix_infix_bp(ctx, parent, e, r_bp);

            lhs = Expr::generated(
                ctx,
                parent,
                ExprKind::Infix {
                    lhs,
                    vid: (vid, loc),
                    rhs,
                },
            );
        } else {
            // FN application
            let (l_bp, _) = fixity_to_bp(FN_APPL);

            if l_bp < min_bp {
                break;
            }

            let rhs = fix_infix_bp(ctx, parent, e, l_bp);

            lhs = Expr::generated(
                ctx,
                parent,
                ExprKind::Application {
                    expr: lhs,
                    param: rhs,
                },
            );
        }
    }
    lhs
}

fn get_bp(ctx: &mut LoweringCtxt, e: &ast::Expr) -> Option<(u8, u8)> {
    let longvid = get_vid(ctx, e)?;
    let fix = match ctx.resolver().lookup_fixity(&longvid) {
        None | Some(Fixity::Nonfix) => return None,
        Some(fix) => fix,
    };
    Some(fixity_to_bp(fix))
}

fn get_vid(ctx: &mut LoweringCtxt, e: &ast::Expr) -> Option<LongVId> {
    if let ast::Expr::Atomic(ast::AtomicExpr::VId(e)) = e {
        e.longvid().map(|l| LongVId::from_node(ctx, &l))
    } else {
        None
    }
}

// Binding power is twice as fixity strength so we can encode left or right associativity in the
// (left, right) binding power.
fn fixity_to_bp(f: Fixity) -> (u8, u8) {
    match f {
        Fixity::Nonfix => panic!("we assume that this is infix!"),
        Fixity::Left(n) => {
            let base = n.unwrap_or(0) * 2;
            (base, base + 1)
        }
        Fixity::Right(n) => {
            let base = n.unwrap_or(0) * 2;
            (base + 1, base)
        }
    }
}
