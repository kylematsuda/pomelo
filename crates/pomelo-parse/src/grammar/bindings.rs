//! Functions to parse bindings.
use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub fn valbind(p: &mut Parser) {
    let _ng = p.start_node(VAL_BIND);

    // Optional `rec`
    p.eat(REC_KW);
    p.eat_trivia();

    grammar::pattern(p);
    p.eat_trivia();

    p.expect(EQ);
    p.eat_trivia();

    // If this is a `val rec`, this must be an `fn` expr.
    // However, it's probably too restrictive to do this here.. save it for a validation pass
    // on the AST.
    grammar::expression(p);
}

pub fn fvalbind(p: &mut Parser) {
    let _ng = p.start_node(FVAL_BIND);

    let mut row = 0;
    fvalbind_row(p, row);
    p.eat_trivia();

    while p.peek() == PIPE {
        row += 1;
        fvalbind_row(p, row);
        p.eat_trivia();
    }

    // FIXME: "and" is likely not handled correctly... looks like these will nest
    if p.peek() == AND_KW {
        p.eat(AND_KW);
        p.eat_trivia();
        fvalbind(p);
    }
}

pub fn fvalbind_row(p: &mut Parser, row_index: u32) {
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

    while p.peek().is_atomic_pat_start() {
        grammar::atomic_pattern(p);
        p.eat_trivia();
    }

    if p.eat(COLON) {
        p.eat_trivia();
        grammar::ty(p);
        p.eat_trivia();
    }

    p.expect(EQ);
    p.eat_trivia();

    grammar::expression(p);
}

pub fn typbind(p: &mut Parser) {
    let _ng = p.start_node(TY_BIND);
    grammar::tyvarseq(p);
    p.eat_trivia();
    grammar::tycon(p);
    p.eat_trivia();
    p.expect(EQ);
    p.eat_trivia();
    grammar::ty(p);
}

pub(crate) fn databind(p: &mut Parser) {
    let _ng = p.start_node(DATA_BIND);
    grammar::tyvarseq(p);
    p.eat_trivia();
    grammar::tycon(p);
    p.eat_trivia();
    p.expect(EQ);
    p.eat_trivia();
    grammar::conbind(p);
}

pub fn conbind(p: &mut Parser) {
    grammar::sequential(p, conbind_inner, PIPE)
}

fn conbind_inner(p: &mut Parser) {
    let _ng = p.start_node(CON_BIND);

    // <op>
    p.eat(OP_KW);
    p.eat_trivia();

    // vid
    grammar::vid(p);

    // <of ty>
    if p.eat_through_trivia(OF_KW) {
        p.eat_trivia();
        grammar::ty(p);
    }
}

pub fn exbind(p: &mut Parser) {
    let _ng = p.start_node(EX_BIND);

    // <op>
    p.eat(OP_KW);
    p.eat_trivia();

    grammar::vid(p);
    p.eat_trivia();

    // <of ty>
    if p.eat_through_trivia(OF_KW) {
        p.eat_trivia();

        grammar::ty(p);
    // = <op> longvid
    } else if p.eat_through_trivia(EQ) {
        p.eat_trivia();

        p.eat(OP_KW);
        p.eat_trivia();

        grammar::longvid(p);
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::check_with_f;
    use expect_test::expect;

    #[test]
    fn valbind_simple() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "val simple = valbind",
            expect![[r#"
                VAL_DEC@0..20
                  VAL_KW@0..3 "val"
                  WHITESPACE@3..4
                  VAL_BIND@4..20
                    VID_PAT@4..10
                      LONG_VID@4..10
                        VID@4..10 "simple"
                    WHITESPACE@10..11
                    EQ@11..12 "="
                    WHITESPACE@12..13
                    VID_EXP@13..20
                      LONG_VID@13..20
                        VID@13..20 "valbind"
            "#]],
        )
    }

    #[test]
    fn valbind_multiple() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "val simple = valbind and another = one and yet = another",
            expect![[r#"
                VAL_DEC@0..56
                  VAL_KW@0..3 "val"
                  WHITESPACE@3..4
                  VAL_BIND@4..20
                    VID_PAT@4..10
                      LONG_VID@4..10
                        VID@4..10 "simple"
                    WHITESPACE@10..11
                    EQ@11..12 "="
                    WHITESPACE@12..13
                    VID_EXP@13..20
                      LONG_VID@13..20
                        VID@13..20 "valbind"
                  WHITESPACE@20..21
                  AND_KW@21..24 "and"
                  WHITESPACE@24..25
                  VAL_BIND@25..38
                    VID_PAT@25..32
                      LONG_VID@25..32
                        VID@25..32 "another"
                    WHITESPACE@32..33
                    EQ@33..34 "="
                    WHITESPACE@34..35
                    VID_EXP@35..38
                      LONG_VID@35..38
                        VID@35..38 "one"
                  WHITESPACE@38..39
                  AND_KW@39..42 "and"
                  WHITESPACE@42..43
                  VAL_BIND@43..56
                    VID_PAT@43..46
                      LONG_VID@43..46
                        VID@43..46 "yet"
                    WHITESPACE@46..47
                    EQ@47..48 "="
                    WHITESPACE@48..49
                    VID_EXP@49..56
                      LONG_VID@49..56
                        VID@49..56 "another"
            "#]],
        )
    }
}
