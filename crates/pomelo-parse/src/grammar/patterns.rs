use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn pattern(p: &mut Parser) {
    let _ng = p.start_node(PAT);
    atomic_pattern(p);
}

pub(crate) fn atomic_pattern(p: &mut Parser) {
    let _ng = p.start_node(AT_PAT);

    match p.peek() {
        UNDERSCORE => {
            let _ng = p.start_node(WILDCARD_PAT);
            p.expect(UNDERSCORE);
        }
        k if k.is_special_constant() => {
            let _ng = p.start_node(SCON_PAT);
            p.eat_any();
        }
        OP_KW => {
            p.eat(OP_KW);
            p.eat_trivia();
            grammar::longvid(p);
        }
        L_BRACE => {
            let _ng = p.start_node(RECORD_PAT);
            p.expect(L_BRACE);
            p.eat_trivia();
            patrow(p);
            p.eat_trivia();
            p.expect(R_BRACE);
        }
        IDENT => grammar::longvid(p),
        _ => p.error("expected atomic pattern"),
    }
}

pub(crate) fn patrow(p: &mut Parser) {
    let _ng = p.start_node(PAT_ROW);
    todo!();
}
