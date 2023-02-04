pub mod context;
pub use context::LoweringCtxt;

use pomelo_parse::ast;

use crate::body::Body;
use crate::TopDec;

/// Lower the AST to HIR
///
/// `LoweringCtxt` contains `crate::File`
pub fn lower(root: ast::File) -> LoweringCtxt {
    let mut ctx = LoweringCtxt::default();

    for d in root.declarations() {
        let body = Body::from_syntax(d, &mut ctx);
        ctx.file_mut().add_dec(TopDec::new(body));
    }

    ctx
}
