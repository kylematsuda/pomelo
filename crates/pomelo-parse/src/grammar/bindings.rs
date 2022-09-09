use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn valbind(p: &mut Parser) {
    let _ng = p.start_node(VAL_BIND);
    grammar::pattern(p);
    p.eat_trivia();
    p.expect(EQ);
    p.eat_trivia();
    grammar::expression(p);

    match p.peek() {
        AND_KW => {
            p.eat(AND_KW);
            p.eat_trivia();
            valbind(p);
        }
        _ => {}
    }
}

pub(crate) fn fvalbind(p: &mut Parser) {
    let _ng = p.start_node(FVAL_BIND);

    let mut row = 0;
    fvalbind_row(p, row);
    p.eat_trivia();

    while p.peek() == PIPE {
        row += 1;
        fvalbind_row(p, row);
        p.eat_trivia();
    }

    match p.peek() {
        AND_KW => {
            p.eat(AND_KW);
            p.eat_trivia();
            fvalbind(p);
        }
        _ => {}
    }
}

pub(crate) fn fvalbind_row(p: &mut Parser, row_index: u32) {
    let _ng = p.start_node(FVAL_BIND_ROW);

    if row_index > 0 {
        p.expect(PIPE);
        p.eat_trivia();
    }

    // Optional <op>
    p.eat(OP_KW);
    p.eat_trivia();
    grammar::vid(p);
    p.eat_trivia();

    grammar::atomic_pattern(p);
    p.eat_trivia();

    while !p.is_eof() {
        match p.peek() {
            COLON => {
                p.eat(COLON);
                p.eat_trivia();
                grammar::ty(p);
                p.eat_trivia();
                fvalbind_row_end(p);
                return;
            }
            EQ => {
                fvalbind_row_end(p);
                return;
            }
            _ => {
                grammar::atomic_pattern(p);
                p.eat_trivia();
            }
        }
    }
    p.error("unexpected EOF");
}

pub(crate) fn fvalbind_row_end(p: &mut Parser) {
    p.expect(EQ);
    p.eat_trivia();
    grammar::expression(p);
    p.eat_trivia();
}
