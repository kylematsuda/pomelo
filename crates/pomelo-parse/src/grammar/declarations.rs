use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;


pub(crate) fn declaration(p: &mut Parser) {
    let outer = p.checkpoint();
    let inner = p.checkpoint();
    declaration_inner(p);

    // If we parse another declaration, then we need to convert
    // this declaration to a SEQ_DEC
    let is_seq = |k: SyntaxKind| k.is_dec_kw() || k == SEMICOLON;
    if is_seq(p.peek_next_nontrivia(0)) {
        let _ng = p.start_node_at(outer, DEC);
        let _nng = p.start_node_at(inner, SEQ_DEC);

        p.eat_trivia();
        p.eat(SEMICOLON);
        p.eat_trivia();
        declaration_inner(p);

        while is_seq(p.peek_next_nontrivia(0)) {
            p.eat_trivia();
            p.eat(SEMICOLON);
            p.eat_trivia();
            declaration_inner(p);
        }
    }
}

pub(crate) fn declaration_inner(p: &mut Parser) {
    let _ng = p.start_node(DEC);

    match p.peek() {
        VAL_KW => val_declaration(p),
        FUN_KW => fun_declaration(p),
        TYPE_KW => type_declaration(p),
        DATATYPE_KW => unimplemented!(),
        ABSTYPE_KW => unimplemented!(),
        EXCEPTION_KW => unimplemented!(),
        LOCAL_KW => local_declaration(p),
        OPEN_KW => open_declaration(p),
        // Sequential decs are handled in `declaration()` 
        INFIX_KW | INFIXR_KW | NONFIX_KW => infix_or_nonfix(p),
        _ => {} // declarations can be empty...
    }
}

pub(crate) fn val_declaration(p: &mut Parser) {
    let _ng = p.start_node(VAL_DEC);
    assert_eq!(p.eat_any(), VAL_KW);
    p.eat_trivia();
    grammar::valbind(p);
}

pub(crate) fn fun_declaration(p: &mut Parser) {
    let _ng = p.start_node(FUN_DEC);
    assert_eq!(p.eat_any(), FUN_KW);
    p.eat_trivia();
    grammar::tyvarseq(p);
    p.eat_trivia();
    grammar::fvalbind(p);
}

pub(crate) fn type_declaration(p: &mut Parser) {
    let _ng = p.start_node(TY_DEC);
    assert_eq!(p.eat_any(), TYPE_KW);
    p.eat_trivia();
    typbind(p);
}

pub(crate) fn typbind(p: &mut Parser) {
    let _ng = p.start_node(TY_BIND);
    grammar::tyvarseq(p);
    p.eat_trivia();
    grammar::tycon(p);
    p.eat_trivia();
    p.expect(EQ);
    p.eat_trivia();
    grammar::ty(p);
}

fn local_declaration(p: &mut Parser) {
    let _ng = p.start_node(LOCAL_DEC);

    assert!(p.eat(LOCAL_KW));
    p.eat_trivia();

    declaration(p);
    p.eat_trivia();

    p.expect(IN_KW);
    p.eat_trivia();

    declaration(p);
    p.eat_trivia();

    p.expect(END_KW);
}

fn open_declaration(p: &mut Parser) {
    let _ng = p.start_node(OPEN_DEC);
    
    assert!(p.eat(OPEN_KW));
    p.eat_trivia();

    if p.peek() != IDENT {
        p.error("expected structure identifier");
    } else {
        while p.peek_next_nontrivia(0) == IDENT {
            p.eat_trivia();
            grammar::longstrid(p);
        }
    }
}

fn infix_or_nonfix(p: &mut Parser) {
    let checkpoint = p.checkpoint();

    let kind = if p.eat(INFIX_KW) {
        INFIX_DEC
    } else if p.eat(INFIXR_KW) {
        INFIXR_DEC
    } else if p.eat(NONFIX_KW) {
        NONFIX_DEC
    } else {
        unreachable!()
    };
    p.eat_trivia();

    let _ng = p.start_node_at(checkpoint, kind);

    if kind == INFIX_DEC || kind == INFIXR_DEC {
        // Optionally parse a fixity
        if p.peek() == INT {
            let _ng = p.start_node(FIXITY);
            p.expect(INT);
        }
    }
    p.eat_trivia();

    p.expect(IDENT); // need at least 1 IDENT
    while p.eat_through_trivia(IDENT) {}
}
