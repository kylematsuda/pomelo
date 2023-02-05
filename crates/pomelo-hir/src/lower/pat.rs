use pomelo_parse::ast;

use crate::arena::Idx;
use crate::lower::{HirLower, HirLowerGenerated, LoweringCtxt};
use crate::{NodeParent, Pat, PatKind};

impl HirLower for Pat {
    type AstType = ast::Pat;

    fn lower(_ctx: &mut LoweringCtxt, _ast: Self::AstType) -> Idx<Self> {
        todo!()
    }

    fn missing(_ctx: &mut LoweringCtxt) -> Idx<Self> {
        todo!()
    }
}

impl HirLowerGenerated for Pat {
    type Kind = PatKind;

    fn generated(_ctx: &mut LoweringCtxt, _origin: NodeParent, _kind: Self::Kind) -> Idx<Self> {
        todo!()
    }
}
