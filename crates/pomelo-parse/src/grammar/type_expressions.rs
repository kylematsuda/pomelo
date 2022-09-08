use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn ty(p: &mut Parser) {
    let _ng = p.start_node(TY);
    match p.peek() {
        TY_VAR => {
            p.eat(TY_VAR);
        }
        _ => unimplemented!(),
    }
}

pub(crate) fn tyvarseq(p: &mut Parser) {
    let _ng = p.start_node(TY_VAR_SEQ);
    while !p.is_eof() {
        match p.peek() {
            TY_VAR => {
                p.eat(TY_VAR);
                p.eat_trivia();
            }
            _ => break,
        }
    }
}

pub(crate) fn tycon(p: &mut Parser) {
    let _ng = p.start_node(TY_CON);
    p.expect(IDENT);
}
