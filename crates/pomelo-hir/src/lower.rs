pub mod context;
pub use context::LoweringCtxt;

pub mod dec;
pub mod expr;
pub mod pat;
mod util;

use pomelo_parse::AstNode;

use crate::arena::Idx;
use crate::NodeParent;

trait HirLower: Sized {
    type AstType: AstNode;

    fn missing(ctx: &mut LoweringCtxt) -> Idx<Self>;
    fn lower(ctx: &mut LoweringCtxt, ast: Self::AstType) -> Idx<Self>;

    fn lower_opt(ctx: &mut LoweringCtxt, opt_ast: Option<Self::AstType>) -> Idx<Self> {
        match opt_ast {
            Some(a) => Self::lower(ctx, a),
            None => Self::missing(ctx),
        }
    }
}

trait HirLowerGenerated: HirLower {
    type Kind: Clone;
    fn generated(ctx: &mut LoweringCtxt, origin: NodeParent, kind: Self::Kind) -> Idx<Self>;
}
