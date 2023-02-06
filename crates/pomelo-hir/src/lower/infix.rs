//! Resolve infix vs applicative expressions and patterns.
//!
//! Pratt parsing strategy that basically follows [this
//! blog post](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html).
use std::iter::Peekable;

use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::{HirLowerGenerated, LoweringCtxt};
use crate::{
    Builtin, DefLoc, Expr, ExprKind, FileArena, Fixity, LongVId, NodeParent, Pat, PatKind, VId,
};

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

pub(super) trait ResolveExprOrPat: HirLowerGenerated {
    type Node; // `ast::ConsOrInfixPat` or `ast::InfixOrAppExpr`
    fn parent(ctx: &mut LoweringCtxt, node: &Self::Node) -> NodeParent;
    fn get_vid(ctx: &mut LoweringCtxt, node: &Self::AstType) -> Option<LongVId>;
    fn get_inner_iter(node: &Self::Node) -> Box<dyn Iterator<Item = Self::AstType>>;
    fn generate_infix(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        lhs: Idx<Self>,
        vid: (VId, DefLoc),
        rhs: Idx<Self>,
    ) -> Idx<Self>;
    fn generate_app(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        lhs: Idx<Self>,
        rhs: Idx<Self>,
    ) -> Idx<Self>;
}

impl ResolveExprOrPat for Expr {
    type Node = ast::InfixOrAppExpr;

    fn parent(ctx: &mut LoweringCtxt, node: &Self::Node) -> NodeParent {
        let origin = ast::Expr::from(node.clone());
        NodeParent::from_expr(ctx, &origin)
    }

    fn get_vid(ctx: &mut LoweringCtxt, node: &Self::AstType) -> Option<LongVId> {
        if let ast::Expr::Atomic(ast::AtomicExpr::VId(e)) = node {
            e.longvid().map(|l| LongVId::from_node(ctx, &l))
        } else {
            None
        }
    }

    fn get_inner_iter(node: &Self::Node) -> Box<dyn Iterator<Item = Self::AstType>> {
        Box::new(node.exprs())
    }

    fn generate_infix(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        lhs: Idx<Self>,
        vid: (VId, DefLoc),
        rhs: Idx<Self>,
    ) -> Idx<Self> {
        Self::generated(ctx, parent, ExprKind::Infix { lhs, vid, rhs })
    }

    fn generate_app(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        lhs: Idx<Self>,
        rhs: Idx<Self>,
    ) -> Idx<Self> {
        Self::generated(
            ctx,
            parent,
            ExprKind::Application {
                expr: lhs,
                param: rhs,
            },
        )
    }
}

impl ResolveExprOrPat for Pat {
    type Node = ast::ConsOrInfixPat;

    fn parent(ctx: &mut LoweringCtxt, node: &Self::Node) -> NodeParent {
        let origin = ast::Pat::from(node.clone());
        NodeParent::from_pat(ctx, &origin)
    }

    fn get_vid(ctx: &mut LoweringCtxt, node: &Self::AstType) -> Option<LongVId> {
        if let ast::Pat::Atomic(ast::AtomicPat::VId(p)) = node {
            p.longvid().map(|l| LongVId::from_node(ctx, &l))
        } else {
            None
        }
    }

    fn get_inner_iter(node: &Self::Node) -> Box<dyn Iterator<Item = Self::AstType>> {
        Box::new(node.pats())
    }

    fn generate_infix(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        lhs: Idx<Self>,
        vid: (VId, DefLoc),
        rhs: Idx<Self>,
    ) -> Idx<Self> {
        Self::generated(ctx, parent, PatKind::Infix { lhs, vid, rhs })
    }

    fn generate_app(
        ctx: &mut LoweringCtxt,
        parent: NodeParent,
        lhs: Idx<Self>,
        rhs: Idx<Self>,
    ) -> Idx<Self> {
        let constructor = ctx.arenas().get_pat(lhs);

        if let PatKind::VId { op, longvid } = &constructor.kind {
            let op = *op;
            // TODO: log an error here instead of crashing if the `DefLoc` is not found
            let (longvid, defloc) = (longvid.0.clone(), longvid.1.unwrap());
            Self::generated(
                ctx,
                parent,
                PatKind::Constructed {
                    op,
                    longvid: (longvid, defloc),
                    pat: rhs,
                },
            )
        } else {
            panic!("figure out whether this is valid and how to handle it")
        }
    }
}

pub(super) fn fix_infix<T: ResolveExprOrPat>(ctx: &mut LoweringCtxt, t: &T::Node) -> Idx<T> {
    let parent = T::parent(ctx, t);
    let mut peekable = T::get_inner_iter(t).peekable();
    fix_infix_bp(ctx, parent, &mut peekable, 0)
}

// Pratt parser
fn fix_infix_bp<T: ResolveExprOrPat>(
    ctx: &mut LoweringCtxt,
    parent: NodeParent,
    it: &mut Peekable<Box<dyn Iterator<Item = T::AstType>>>,
    min_bp: u8,
) -> Idx<T> {
    let mut lhs = T::lower_opt(ctx, it.next());

    loop {
        let next = match it.peek() {
            None => break,
            Some(e) => e.clone(),
        };

        if let Some((l_bp, r_bp)) = get_bp::<T>(ctx, &next) {
            if l_bp < min_bp {
                break;
            }

            // Eat the infix_vid
            let infix_vid = T::get_vid(ctx, &it.next().unwrap()).unwrap();
            let loc = ctx.resolver().lookup_vid(&infix_vid);
            let vid = infix_vid.try_into_vid().unwrap();

            // Parse rhs expr
            let rhs = fix_infix_bp(ctx, parent, it, r_bp);
            lhs = T::generate_infix(ctx, parent, lhs, (vid, loc), rhs);
        } else {
            // FN application
            let (l_bp, _) = fixity_to_bp(FN_APPL);

            if l_bp < min_bp {
                break;
            }

            let rhs = fix_infix_bp(ctx, parent, it, l_bp);
            lhs = T::generate_app(ctx, parent, lhs, rhs);
        }
    }
    lhs
}

fn get_bp<T: ResolveExprOrPat>(ctx: &mut LoweringCtxt, node: &T::AstType) -> Option<(u8, u8)> {
    let longvid = T::get_vid(ctx, node)?;
    let fix = match ctx.resolver().lookup_fixity(&longvid) {
        None | Some(Fixity::Nonfix) => return None,
        Some(fix) => fix,
    };
    Some(fixity_to_bp(fix))
}

// fn get_vid(ctx: &mut LoweringCtxt, e: &ast::Expr) -> Option<LongVId> {
//     if let ast::Expr::Atomic(ast::AtomicExpr::VId(e)) = e {
//         e.longvid().map(|l| LongVId::from_node(ctx, &l))
//     } else {
//         None
//     }
// }

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
