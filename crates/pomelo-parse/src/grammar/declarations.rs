use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn declaration(p: &mut Parser) {
    let _ng = p.start_node(DEC);

    match p.peek() {
        VAL_KW => val_declaration(p),
        FUN_KW => fun_declaration(p),
        TYPE_KW => type_declaration(p),
        DATATYPE_KW => unimplemented!(),
        ABSTYPE_KW => unimplemented!(),
        EXCEPTION_KW => unimplemented!(),
        LOCAL_KW => unimplemented!(),
        OPEN_KW => unimplemented!(),
        // Here, need to handle sequential declaration
        INFIX_KW => unimplemented!(),
        INFIXR_KW => unimplemented!(),
        NONFIX_KW => unimplemented!(),
        _ => p.error("expected declaration"),
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
