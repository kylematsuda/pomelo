use crate::grammar;
use crate::{parser::Token, Checkpoint, Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn ty(p: &mut Parser) {
    let _ng = p.start_node(TY);
    let outer = p.checkpoint();
    let inner = p.checkpoint();

    match p.peek() {
        TY_VAR => {
            let tycon = p.checkpoint();
            p.eat(TY_VAR);

            if p.peek_next_nontrivia(0) == TY_VAR {
                tycon_exp(p, tycon);
            }
        }
        IDENT => {
            let tycon = p.checkpoint();
            tycon_exp(p, tycon);
        }
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
    match t.map(Token::kind).unwrap_or(EOF) {
        IDENT if t.map(Token::text) == Some("*") => tuple_type(p, outer, inner),
        THIN_ARROW => function_type(p, outer, inner),
        TY_VAR | IDENT | L_BRACE | L_PAREN => extend_tycon_exp(p, outer, inner),
        _ => {}
    }
}

fn tycon_exp(p: &mut Parser, checkpoint: Checkpoint) {
    let _ng = p.start_node_at(checkpoint, TY_CON_EXP);
    p.eat_trivia();

    while p.eat_through_trivia(TY_VAR) {}
    p.eat_trivia();

    longtycon(p);
}

fn extend_tycon_exp(p: &mut Parser, outer: Checkpoint, inner: Checkpoint) {
    let _ng = p.start_node_at(outer, TY_CON_EXP);
    {
        let _ng = p.start_node_at(inner, TY);
    } // enclose the last type parsed
    p.eat_trivia();

    ty(p);
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
    let ident = |p: &mut Parser| p.expect(IDENT);
    grammar::sequential(p, ident, DOT);
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
