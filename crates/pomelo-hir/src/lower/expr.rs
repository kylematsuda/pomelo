use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::{HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{Expr, ExprKind, NodeParent};

impl HirLower for Expr {
    type AstType = ast::Expr;

    fn lower(_ctx: &mut LoweringCtxt, _ast: Self::AstType) -> Idx<Self> {
        todo!()
    }

    fn missing(_ctx: &mut LoweringCtxt) -> Idx<Self> {
        todo!()
    }
}

impl HirLowerGenerated for Expr {
    type Kind = ExprKind;

    fn generated(_ctx: &mut LoweringCtxt, _origin: NodeParent, _kind: Self::Kind) -> Idx<Self> {
        todo!()
    }
}
