use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn valbind(p: &mut Parser) {
    let _ng = p.start_node(VAL_BIND);

    // Optionally eat a REC
    // TODO: a "val rec" can only bind to an "fn" expression.
    // Should probably handle this correctly... once we parse "fn"s
    let rec = p.eat(REC_KW);
    p.eat_trivia();

    grammar::sequential(p, |p| valbind_inner(p, rec), AND_KW);
}

fn valbind_inner(p: &mut Parser, rec: bool) {
    grammar::pattern(p);
    p.eat_trivia();

    p.expect(EQ);
    p.eat_trivia();

    if !rec {
        grammar::expression(p);
    } else {
        // We may only eat an fn-match expression
        if p.peek() != FN_KW {
            p.error("val rec can only take bindings of form <pat> = fn <match>");
            return;
        } else {
            grammar::fn_match(p);
        }
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
                DEC@0..20
                  VAL_DEC@0..20
                    VAL_KW@0..3 "val"
                    WHITESPACE@3..4
                    VAL_BIND@4..20
                      PAT@4..10
                        AT_PAT@4..10
                          VID_PAT@4..10
                            LONG_VID@4..10
                              VID@4..10 "simple"
                      WHITESPACE@10..11
                      EQ@11..12 "="
                      WHITESPACE@12..13
                      EXP@13..20
                        AT_EXP@13..20
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
                DEC@0..56
                  VAL_DEC@0..56
                    VAL_KW@0..3 "val"
                    WHITESPACE@3..4
                    VAL_BIND@4..56
                      PAT@4..10
                        AT_PAT@4..10
                          VID_PAT@4..10
                            LONG_VID@4..10
                              VID@4..10 "simple"
                      WHITESPACE@10..11
                      EQ@11..12 "="
                      WHITESPACE@12..13
                      EXP@13..20
                        AT_EXP@13..20
                          VID_EXP@13..20
                            LONG_VID@13..20
                              VID@13..20 "valbind"
                      WHITESPACE@20..21
                      AND_KW@21..24 "and"
                      WHITESPACE@24..25
                      PAT@25..32
                        AT_PAT@25..32
                          VID_PAT@25..32
                            LONG_VID@25..32
                              VID@25..32 "another"
                      WHITESPACE@32..33
                      EQ@33..34 "="
                      WHITESPACE@34..35
                      EXP@35..38
                        AT_EXP@35..38
                          VID_EXP@35..38
                            LONG_VID@35..38
                              VID@35..38 "one"
                      WHITESPACE@38..39
                      AND_KW@39..42 "and"
                      WHITESPACE@42..43
                      PAT@43..46
                        AT_PAT@43..46
                          VID_PAT@43..46
                            LONG_VID@43..46
                              VID@43..46 "yet"
                      WHITESPACE@46..47
                      EQ@47..48 "="
                      WHITESPACE@48..49
                      EXP@49..56
                        AT_EXP@49..56
                          VID_EXP@49..56
                            LONG_VID@49..56
                              VID@49..56 "another"
            "#]],
        )
    }
}
