use crate::grammar;
use crate::{Checkpoint, Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn ty(p: &mut Parser) {
    let _ng = p.start_node(TY);
    let outer = p.checkpoint();
    let inner = p.checkpoint();

    match p.peek() {
        TY_VAR => {
            p.eat(TY_VAR);
        }
        IDENT => {}
        L_BRACE => record_ty(p),
        L_PAREN => {
            p.expect(L_PAREN);
            p.eat_trivia();

            ty(p);
            p.eat_trivia();

            p.expect(R_PAREN);
        }
        _ => p.error("expected type expression"),
    }

    let t = p.peek_token_next_nontrivia(0);
    match t.map(|t| (t.kind(), t.text())) {
        Some((IDENT, "*")) => tuple_type(p, outer, inner),
        Some((THIN_ARROW, _)) => function_type(p, outer, inner),
        _ => {}
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

fn longtycon(p: &mut Parser) {
    let _ng = p.start_node(LONG_TY_CON);

    p.expect(IDENT);
}

fn record_ty(p: &mut Parser) {
    let _ng = p.start_node(RECORD_TY_EXP);

    assert!(p.eat(L_BRACE));
    p.eat_trivia();

    if p.eat(R_BRACE) {
        return;
    }

    grammar::sequential(p, tyrow, COMMA);
    p.eat_trivia();

    p.expect(R_BRACE);
}

fn tyrow(p: &mut Parser) {
    let _ng = p.start_node(TY_ROW);

    p.expect(IDENT);
    p.eat_trivia();

    p.expect(COLON);
    p.eat_trivia();

    ty(p);
}

fn tuple_type(p: &mut Parser, outer: Checkpoint, inner: Checkpoint) {
    let _ng = p.start_node_at(outer, TUPLE_TY_EXP);
    {
        let _ng = p.start_node_at(inner, TY);
    } // enclose the last type parsed
    p.eat_trivia();

    // This remaps the IDENT token into a STAR
    assert_eq!(p.eat_mapped(STAR), IDENT);
    p.eat_trivia();

    ty(p)
}

fn function_type(p: &mut Parser, outer: Checkpoint, inner: Checkpoint) {
    let _ng = p.start_node_at(outer, FUN_TY_EXP);
    {
        let _ng = p.start_node_at(inner, TY);
    } // enclose the last type parsed
    p.eat_trivia();

    // This remaps the IDENT token into a STAR
    assert!(p.eat(THIN_ARROW));
    p.eat_trivia();

    ty(p)
}
