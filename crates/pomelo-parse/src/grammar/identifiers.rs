use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn longvid(p: &mut Parser) {
    let _ng = p.start_node(LONG_VID);
    p.expect(IDENT);
}

pub(crate) fn vid(p: &mut Parser) {
    let _ng = p.start_node(VID);
    p.expect(IDENT);
}
