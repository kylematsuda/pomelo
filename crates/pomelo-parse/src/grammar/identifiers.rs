use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn longvid(p: &mut Parser) {
    let _ng = p.start_node(LONG_VID);
    grammar::sequential(p, ident, DOT);
}

fn ident(p: &mut Parser) {
    p.expect(IDENT);
}

pub(crate) fn vid(p: &mut Parser) {
    let _ng = p.start_node(VID);
    p.expect(IDENT);
}

pub(crate) fn longstrid(p: &mut Parser) {
    let _ng = p.start_node(LONG_STR_ID);
    grammar::sequential(p, strid, DOT);
}

// TODO: make sure this is a valid STRID?
pub(crate) fn strid(p: &mut Parser) {
    p.expect(IDENT);
}
