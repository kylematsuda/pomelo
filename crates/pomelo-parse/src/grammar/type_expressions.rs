use crate::grammar;
use crate::{parser::Token, Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn ty(p: &mut Parser) {
    fun_ty(p)
}

fn fun_ty(p: &mut Parser) {
    grammar::precedence_climber(
        p,
        TY,
        FUN_TY_EXP,
        tuple_ty,
        |p| p.eat_through_trivia(THIN_ARROW),
        tuple_ty,
    )
}

fn tuple_ty(p: &mut Parser) {
    grammar::precedence_climber(
        p,
        TY,
        TUPLE_TY_EXP,
        tycon_seq,
        |p| {
            // This is a little convoluted.
            // At the lexing stage, we don't know 
            // how to determine the function of '*':
            // as an operator/identifier, or as part of
            // a tuple type expression. As a result,
            // '*' does not have it's own LexKind.
            let t = p.peek_token_next_nontrivia(0);
            match t.map(Token::kind).unwrap_or(EOF) {
                // Only take the IDENT if it holds a "*"
                IDENT if Some("*") == t.map(Token::text) => {
                    p.eat_trivia();
                    assert_eq!(p.eat_mapped(STAR), IDENT);
                    true
                },
                _ => false
            }
        },
        tycon_seq,
    )
}

fn tycon_seq(p: &mut Parser) {
    let ty_checkpoint = p.checkpoint();
    let con_checkpoint = p.checkpoint();

    tycon_ty(p);

    let continue_if = |k: SyntaxKind| {
        k.is_ty_atom() || k == IDENT
    };

    if continue_if(p.peek_next_nontrivia(0)) {
        let _ng_ty = p.start_node_at(ty_checkpoint, TY);
        let _ng_con = p.start_node_at(con_checkpoint, TY_CON_EXP);

        while continue_if(p.peek_next_nontrivia(0)) {
            // Need to detect if we're on the final <longtycon>
            // TODO: refactor! this is super ugly
            if p.peek_next_nontrivia(0) == IDENT && is_last_tycon(p) {
                p.eat_trivia();
                longtycon(p);
                return;
            } else {
                p.eat_trivia();
                tycon_ty(p);
            }
        }
    }
}

fn is_last_tycon(p: &Parser) -> bool {
    let mut lookahead = 1;

    let mut dotted = false;
    loop {
        match p.peek_next_nontrivia(lookahead) {
            EOF => return true,
            k if k.is_ty_atom() => return false,
            DOT => { dotted = true; }
            IDENT => {
                if !dotted {
                    return false; 
                }
            },
            _ => return true, 
        }
        lookahead += 1;
    }
}

fn tycon_ty(p: &mut Parser) {
    let ty_checkpoint = p.checkpoint();
    let con_checkpoint = p.checkpoint();

    let mut count = 0;
    while p.peek_next_nontrivia(0).is_ty_atom() {
        ty_atom(p);
        p.eat_trivia();
        count += 1;
    }

    if count == 1 {
        // May be a single TY.
        // In this, case, no need to nest it inside a TY_CON_EXP.
        if p.peek_next_nontrivia(0) != IDENT {
            return;
        }
    }
    p.eat_trivia();

    let _ng_ty = p.start_node_at(ty_checkpoint, TY);
    let _ng_con = p.start_node_at(con_checkpoint, TY_CON_EXP);
    longtycon(p);
}

fn ty_atom(p: &mut Parser) {
    let _ng = p.start_node(TY);

    match p.peek() {
        TY_VAR => p.expect(TY_VAR),
        L_BRACE => record_ty(p),
        L_PAREN => {
            p.expect(L_PAREN);
            p.eat_trivia();

            ty(p);
            p.eat_trivia();

            p.expect(R_PAREN);
        },
        _ => p.error("expected type expression"),
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
