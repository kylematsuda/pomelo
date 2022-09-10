use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn expression(p: &mut Parser) {
    handle_exp(p)
}

fn handle_exp(p: &mut Parser) {
    grammar::precedence_climber_once(
        p,
        EXP,
        HANDLE_EXP,
        orelse_exp,
        |p| p.eat_through_trivia(HANDLE_KW),
        grammar::match_exp,
    );
}

fn orelse_exp(p: &mut Parser) {
    grammar::precedence_climber_once(
        p,
        EXP,
        ORELSE_EXP,
        andalso_exp,
        |p| p.eat_through_trivia(ORELSE_KW),
        andalso_exp,
    );
}

fn andalso_exp(p: &mut Parser) {
    grammar::precedence_climber_once(
        p,
        EXP,
        ANDALSO_EXP,
        typed_exp,
        |p| p.eat_through_trivia(ANDALSO_KW),
        typed_exp,
    );
}

fn typed_exp(p: &mut Parser) {
    grammar::precedence_climber_once(
        p,
        EXP,
        TY_EXP,
        keyword_or_infexp,
        |p| p.eat_through_trivia(COLON),
        grammar::ty,
    );
}

fn keyword_or_infexp(p: &mut Parser) {
    match p.peek() {
        FN_KW => fn_match(p),
        CASE_KW => case_match(p),
        WHILE_KW => while_exp(p),
        IF_KW => if_exp(p),
        RAISE_KW => raise_exp(p),
        _ => infexp(p),
    }
}

/// This one is done a bit differently for now...
/// It seems like it will be easier to parser any expression
/// containing infix operators as a flat sequence of terms.
///
/// This way, we can assign operator precedence later when we
/// have resolved it.
fn infexp(p: &mut Parser) {
    grammar::precedence_climber(
        p,
        EXP,
        INFIX_EXP,
        appexp,
        |p| p.peek_next_nontrivia(0) == IDENT,
        |p| {
            p.eat_trivia();
            grammar::vid(p);
            p.eat_trivia();

            appexp(p);
        },
    )
}

// This is disgusting... need to figure out a better way to make sure
// these guys left associate
fn appexp(p: &mut Parser) {
    let continue_if = |p: &mut Parser| p.peek_next_nontrivia(0).is_atomic_exp_start();

    let outer_checkpoint = p.checkpoint();
    let inner_checkpoint = p.checkpoint();

    atomic_inner(p);

    if !continue_if(p) {
        let _ng = p.start_node_at(outer_checkpoint, EXP);
        return;
    } else {
        while continue_if(p) {
            p.eat_trivia();
            atomic_inner(p);

            let _ng_outer = p.start_node_at(outer_checkpoint.clone(), EXP);
            let _ng_inner = p.start_node_at(inner_checkpoint.clone(), APP_EXP);
        }
    }
}

pub(crate) fn fn_match(p: &mut Parser) {
    let _ng_exp = p.start_node(EXP);
    let _ng = p.start_node(FN_EXP);

    assert!(p.eat(FN_KW));
    p.eat_trivia();

    grammar::match_exp(p)
}

fn case_match(p: &mut Parser) {
    let _ng_exp = p.start_node(EXP);
    let _ng = p.start_node(CASE_MATCH_EXP);

    assert!(p.eat(CASE_KW));
    p.eat_trivia();

    expression(p);
    p.eat_trivia();

    p.expect(OF_KW);
    p.eat_trivia();

    grammar::match_exp(p)
}

fn while_exp(p: &mut Parser) {
    let _ng_exp = p.start_node(EXP);
    let _ng = p.start_node(WHILE_EXP);

    assert!(p.eat(WHILE_KW));
    p.eat_trivia();

    expression(p);
    p.eat_trivia();

    p.expect(DO_KW);
    p.eat_trivia();

    expression(p);
}

fn if_exp(p: &mut Parser) {
    let _ng_exp = p.start_node(EXP);
    let _ng = p.start_node(IF_EXP);

    assert!(p.eat(IF_KW));
    p.eat_trivia();

    expression(p);
    p.eat_trivia();

    p.expect(THEN_KW);
    p.eat_trivia();

    expression(p);
    p.eat_trivia();

    p.expect(ELSE_KW);
    p.eat_trivia();

    expression(p);
}

fn raise_exp(p: &mut Parser) {
    let _ng_exp = p.start_node(EXP);
    let _ng = p.start_node(RAISE_EXP);

    assert!(p.eat(RAISE_KW));
    p.eat_trivia();

    expression(p);
}

fn atomic_inner(p: &mut Parser) {
    let _ng = p.start_node(AT_EXP);

    match p.peek() {
        k if k.is_special_constant() => {
            let _ng = p.start_node(SCON_EXP);
            p.eat_any();
        }
        OP_KW | IDENT => {
            let _ng = p.start_node(VID_EXP);
            p.eat(OP_KW);
            p.eat_trivia();
            grammar::longvid(p);
        }
        L_BRACE => record_exp(p),
        HASH => {
            let _ng = p.start_node(RECORD_SEL_EXP);
            p.expect(HASH);
            p.eat_trivia();
            p.expect(IDENT);
        }
        L_PAREN => {
            // Unit expression can contain whitespace,
            // e.g., "val (    ) = ..." is acceptable
            // to some level.
            //
            // Although SML/NJ errors if there are too many
            // spaces. And it seems like newlines in the middle
            // should not be allowed?
            if p.peek_next_nontrivia(1) == R_PAREN {
                let _ng = p.start_node(UNIT_EXP);
                p.expect(L_PAREN);
                p.eat_trivia();
                p.expect(R_PAREN);
            } else {
                other_parenthesized(p)
            }
        }
        L_BRACKET => listexp(p),
        LET_KW => let_dec(p),
        _ => p.error("unexpected token"),
    }
}

fn record_exp(p: &mut Parser) {
    let _ng = p.start_node(RECORD_EXP);
    p.expect(L_BRACE);
    p.eat_trivia();

    exprow(p);
    p.eat_trivia();
    while p.eat(COMMA) {
        p.eat_trivia();
        exprow(p);
        p.eat_trivia();
    }
    p.expect(R_BRACE);
}

fn exprow(p: &mut Parser) {
    let _ng = p.start_node(EXP_ROW);
    p.expect(IDENT);
    p.eat_trivia();
    p.expect(EQ);
    p.eat_trivia();
    grammar::expression(p);
}

fn listexp(p: &mut Parser) {
    let _ng = p.start_node(LIST_EXP);
    p.expect(L_BRACKET);
    p.eat_trivia();

    if p.eat(R_BRACKET) {
        return;
    }

    grammar::sequential(p, expression, COMMA);
    p.expect(R_BRACKET)
}

fn let_dec(p: &mut Parser) {
    let _ng = p.start_node(LET_DEC);

    p.expect(LET_KW);
    p.eat_trivia();

    grammar::declaration(p);
    p.eat_trivia();

    p.expect(IN_KW);
    p.eat_trivia();

    grammar::sequential(p, expression, SEMICOLON);
    p.eat_trivia();

    p.expect(END_KW)
}

fn other_parenthesized(p: &mut Parser) {
    let checkpoint = p.checkpoint();

    p.expect(L_PAREN);
    p.eat_trivia();

    expression(p);
    p.eat_trivia();

    match p.peek() {
        R_PAREN => {
            p.expect(R_PAREN);
            return;
        }
        COMMA => {
            p.expect(COMMA);
            p.eat_trivia();

            let ng = p.start_node_at(checkpoint, TUPLE_EXP);
            finish_tuple_expression(p, ng);
        }
        SEMICOLON => {
            p.expect(SEMICOLON);
            p.eat_trivia();

            let ng = p.start_node_at(checkpoint, SEQ_EXP);
            finish_seq_expression(p, ng);
        }
        _ => p.error(
            "expected closing ')', tuple (\", exp, ... )\"), or sequence (\"; expr; ... )\")",
        ),
    }
}

fn finish_tuple_expression(p: &mut Parser, _ng: crate::NodeGuard) {
    grammar::sequential(p, expression, COMMA);
    p.expect(R_PAREN)
}

fn finish_seq_expression(p: &mut Parser, _ng: crate::NodeGuard) {
    grammar::sequential(p, expression, SEMICOLON);
    p.expect(R_PAREN)
}

#[cfg(test)]
mod tests {
    use crate::tests::check_with_f;
    use expect_test::expect;

    #[test]
    fn scons_tuple() {
        check_with_f(
            false,
            super::expression,
            "(1.0, 0x0FA, #\"A\", 0wxFF, 0, \"test string\")",
            expect![[r##"
                EXP@0..43
                  AT_EXP@0..43
                    TUPLE_EXP@0..43
                      L_PAREN@0..1 "("
                      EXP@1..4
                        AT_EXP@1..4
                          SCON_EXP@1..4
                            REAL@1..4 "1.0"
                      COMMA@4..5 ","
                      WHITESPACE@5..6
                      EXP@6..11
                        AT_EXP@6..11
                          SCON_EXP@6..11
                            INT@6..11 "0x0FA"
                      COMMA@11..12 ","
                      WHITESPACE@12..13
                      EXP@13..17
                        AT_EXP@13..17
                          SCON_EXP@13..17
                            CHAR@13..17 "#\"A\""
                      COMMA@17..18 ","
                      WHITESPACE@18..19
                      EXP@19..24
                        AT_EXP@19..24
                          SCON_EXP@19..24
                            WORD@19..24 "0wxFF"
                      COMMA@24..25 ","
                      WHITESPACE@25..26
                      EXP@26..27
                        AT_EXP@26..27
                          SCON_EXP@26..27
                            INT@26..27 "0"
                      COMMA@27..28 ","
                      WHITESPACE@28..29
                      EXP@29..42
                        AT_EXP@29..42
                          SCON_EXP@29..42
                            STRING@29..42 "\"test string\""
                      R_PAREN@42..43 ")"
            "##]],
        )
    }

    #[test]
    fn longvid() {
        check_with_f(
            false,
            super::expression,
            "I.Am.A.Long.List.Of.Modules.x",
            expect![[r#"
                EXP@0..29
                  AT_EXP@0..29
                    VID_EXP@0..29
                      LONG_VID@0..29
                        IDENT@0..1 "I"
                        DOT@1..2 "."
                        IDENT@2..4 "Am"
                        DOT@4..5 "."
                        IDENT@5..6 "A"
                        DOT@6..7 "."
                        IDENT@7..11 "Long"
                        DOT@11..12 "."
                        IDENT@12..16 "List"
                        DOT@16..17 "."
                        IDENT@17..19 "Of"
                        DOT@19..20 "."
                        IDENT@20..27 "Modules"
                        DOT@27..28 "."
                        IDENT@28..29 "x"
            "#]],
        )
    }

    #[test]
    fn op_longvid() {
        check_with_f(
            false,
            super::expression,
            "op I.Am.A.Long.List.Of.Modules.x",
            expect![[r#"
                EXP@0..32
                  AT_EXP@0..32
                    VID_EXP@0..32
                      OP_KW@0..2 "op"
                      WHITESPACE@2..3
                      LONG_VID@3..32
                        IDENT@3..4 "I"
                        DOT@4..5 "."
                        IDENT@5..7 "Am"
                        DOT@7..8 "."
                        IDENT@8..9 "A"
                        DOT@9..10 "."
                        IDENT@10..14 "Long"
                        DOT@14..15 "."
                        IDENT@15..19 "List"
                        DOT@19..20 "."
                        IDENT@20..22 "Of"
                        DOT@22..23 "."
                        IDENT@23..30 "Modules"
                        DOT@30..31 "."
                        IDENT@31..32 "x"
            "#]],
        )
    }

    #[test]
    fn record() {
        check_with_f(
            false,
            super::expression,
            "{ hi = i, am = a, record = x }",
            expect![[r#"
                EXP@0..30
                  AT_EXP@0..30
                    RECORD_EXP@0..30
                      L_BRACE@0..1 "{"
                      WHITESPACE@1..2
                      EXP_ROW@2..8
                        IDENT@2..4 "hi"
                        WHITESPACE@4..5
                        EQ@5..6 "="
                        WHITESPACE@6..7
                        EXP@7..8
                          AT_EXP@7..8
                            VID_EXP@7..8
                              LONG_VID@7..8
                                IDENT@7..8 "i"
                      COMMA@8..9 ","
                      WHITESPACE@9..10
                      EXP_ROW@10..16
                        IDENT@10..12 "am"
                        WHITESPACE@12..13
                        EQ@13..14 "="
                        WHITESPACE@14..15
                        EXP@15..16
                          AT_EXP@15..16
                            VID_EXP@15..16
                              LONG_VID@15..16
                                IDENT@15..16 "a"
                      COMMA@16..17 ","
                      WHITESPACE@17..18
                      EXP_ROW@18..28
                        IDENT@18..24 "record"
                        WHITESPACE@24..25
                        EQ@25..26 "="
                        WHITESPACE@26..27
                        EXP@27..28
                          AT_EXP@27..28
                            VID_EXP@27..28
                              LONG_VID@27..28
                                IDENT@27..28 "x"
                      WHITESPACE@28..29
                      R_BRACE@29..30 "}"
            "#]],
        )
    }

    #[test]
    fn record_selector() {
        check_with_f(
            false,
            super::expression,
            "# hi",
            expect![[r##"
                EXP@0..4
                  AT_EXP@0..4
                    RECORD_SEL_EXP@0..4
                      HASH@0..1 "#"
                      WHITESPACE@1..2
                      IDENT@2..4 "hi"
            "##]],
        )
    }

    #[test]
    fn list() {
        check_with_f(
            false,
            super::expression,
            "[1, 2, 3, 4, 5]",
            expect![[r#"
                EXP@0..15
                  AT_EXP@0..15
                    LIST_EXP@0..15
                      L_BRACKET@0..1 "["
                      EXP@1..2
                        AT_EXP@1..2
                          SCON_EXP@1..2
                            INT@1..2 "1"
                      COMMA@2..3 ","
                      WHITESPACE@3..4
                      EXP@4..5
                        AT_EXP@4..5
                          SCON_EXP@4..5
                            INT@4..5 "2"
                      COMMA@5..6 ","
                      WHITESPACE@6..7
                      EXP@7..8
                        AT_EXP@7..8
                          SCON_EXP@7..8
                            INT@7..8 "3"
                      COMMA@8..9 ","
                      WHITESPACE@9..10
                      EXP@10..11
                        AT_EXP@10..11
                          SCON_EXP@10..11
                            INT@10..11 "4"
                      COMMA@11..12 ","
                      WHITESPACE@12..13
                      EXP@13..14
                        AT_EXP@13..14
                          SCON_EXP@13..14
                            INT@13..14 "5"
                      R_BRACKET@14..15 "]"
            "#]],
        )
    }

    #[test]
    fn let_dec() {
        check_with_f(
            false,
            super::expression,
            "let 
                val x = 1
            in 
                x
            end",
            expect![[r#"
                EXP@0..80
                  AT_EXP@0..80
                    LET_DEC@0..80
                      LET_KW@0..3 "let"
                      WHITESPACE@3..21
                      DEC@21..30
                        VAL_DEC@21..30
                          VAL_KW@21..24 "val"
                          WHITESPACE@24..25
                          VAL_BIND@25..30
                            PAT@25..26
                              AT_PAT@25..26
                                VID_PAT@25..26
                                  LONG_VID@25..26
                                    IDENT@25..26 "x"
                            WHITESPACE@26..27
                            EQ@27..28 "="
                            WHITESPACE@28..29
                            EXP@29..30
                              AT_EXP@29..30
                                SCON_EXP@29..30
                                  INT@29..30 "1"
                      WHITESPACE@30..43
                      IN_KW@43..45 "in"
                      WHITESPACE@45..63
                      EXP@63..64
                        AT_EXP@63..64
                          VID_EXP@63..64
                            LONG_VID@63..64
                              IDENT@63..64 "x"
                      WHITESPACE@64..77
                      END_KW@77..80 "end"
            "#]],
        )
    }

    #[test]
    fn let_multi_dec() {
        check_with_f(
            false,
            super::expression,
            "let 
                val x = 1
                val here = \"are\";
                val some = random
                fun other decs = 0 ;
                val only = some 
                val have = \"semicolons\"
            in 
                x
            end",
            expect![[r#"
                EXP@0..258
                  AT_EXP@0..258
                    LET_DEC@0..258
                      LET_KW@0..3 "let"
                      WHITESPACE@3..21
                      DEC@21..208
                        SEQ_DEC@21..208
                          DEC@21..30
                            VAL_DEC@21..30
                              VAL_KW@21..24 "val"
                              WHITESPACE@24..25
                              VAL_BIND@25..30
                                PAT@25..26
                                  AT_PAT@25..26
                                    VID_PAT@25..26
                                      LONG_VID@25..26
                                        IDENT@25..26 "x"
                                WHITESPACE@26..27
                                EQ@27..28 "="
                                WHITESPACE@28..29
                                EXP@29..30
                                  AT_EXP@29..30
                                    SCON_EXP@29..30
                                      INT@29..30 "1"
                          WHITESPACE@30..47
                          DEC@47..63
                            VAL_DEC@47..63
                              VAL_KW@47..50 "val"
                              WHITESPACE@50..51
                              VAL_BIND@51..63
                                PAT@51..55
                                  AT_PAT@51..55
                                    VID_PAT@51..55
                                      LONG_VID@51..55
                                        IDENT@51..55 "here"
                                WHITESPACE@55..56
                                EQ@56..57 "="
                                WHITESPACE@57..58
                                EXP@58..63
                                  AT_EXP@58..63
                                    SCON_EXP@58..63
                                      STRING@58..63 "\"are\""
                          SEMICOLON@63..64 ";"
                          WHITESPACE@64..81
                          DEC@81..98
                            VAL_DEC@81..98
                              VAL_KW@81..84 "val"
                              WHITESPACE@84..85
                              VAL_BIND@85..98
                                PAT@85..89
                                  AT_PAT@85..89
                                    VID_PAT@85..89
                                      LONG_VID@85..89
                                        IDENT@85..89 "some"
                                WHITESPACE@89..90
                                EQ@90..91 "="
                                WHITESPACE@91..92
                                EXP@92..98
                                  AT_EXP@92..98
                                    VID_EXP@92..98
                                      LONG_VID@92..98
                                        IDENT@92..98 "random"
                          WHITESPACE@98..115
                          DEC@115..134
                            FUN_DEC@115..134
                              FUN_KW@115..118 "fun"
                              WHITESPACE@118..119
                              TY_VAR_SEQ@119..119
                              FVAL_BIND@119..134
                                FVAL_BIND_ROW@119..134
                                  VID@119..124
                                    IDENT@119..124 "other"
                                  WHITESPACE@124..125
                                  AT_PAT@125..129
                                    VID_PAT@125..129
                                      LONG_VID@125..129
                                        IDENT@125..129 "decs"
                                  WHITESPACE@129..130
                                  EQ@130..131 "="
                                  WHITESPACE@131..132
                                  EXP@132..133
                                    AT_EXP@132..133
                                      SCON_EXP@132..133
                                        INT@132..133 "0"
                                  WHITESPACE@133..134
                          SEMICOLON@134..135 ";"
                          WHITESPACE@135..152
                          DEC@152..167
                            VAL_DEC@152..167
                              VAL_KW@152..155 "val"
                              WHITESPACE@155..156
                              VAL_BIND@156..167
                                PAT@156..160
                                  AT_PAT@156..160
                                    VID_PAT@156..160
                                      LONG_VID@156..160
                                        IDENT@156..160 "only"
                                WHITESPACE@160..161
                                EQ@161..162 "="
                                WHITESPACE@162..163
                                EXP@163..167
                                  AT_EXP@163..167
                                    VID_EXP@163..167
                                      LONG_VID@163..167
                                        IDENT@163..167 "some"
                          WHITESPACE@167..185
                          DEC@185..208
                            VAL_DEC@185..208
                              VAL_KW@185..188 "val"
                              WHITESPACE@188..189
                              VAL_BIND@189..208
                                PAT@189..193
                                  AT_PAT@189..193
                                    VID_PAT@189..193
                                      LONG_VID@189..193
                                        IDENT@189..193 "have"
                                WHITESPACE@193..194
                                EQ@194..195 "="
                                WHITESPACE@195..196
                                EXP@196..208
                                  AT_EXP@196..208
                                    SCON_EXP@196..208
                                      STRING@196..208 "\"semicolons\""
                      WHITESPACE@208..221
                      IN_KW@221..223 "in"
                      WHITESPACE@223..241
                      EXP@241..242
                        AT_EXP@241..242
                          VID_EXP@241..242
                            LONG_VID@241..242
                              IDENT@241..242 "x"
                      WHITESPACE@242..255
                      END_KW@255..258 "end"
            "#]],
        )
    }

    #[test]
    fn single_parenthesized() {
        check_with_f(
            false,
            super::expression,
            "(\"a single parenthesized exp\")",
            expect![[r#"
                EXP@0..30
                  AT_EXP@0..30
                    L_PAREN@0..1 "("
                    EXP@1..29
                      AT_EXP@1..29
                        SCON_EXP@1..29
                          STRING@1..29 "\"a single parenthesiz ..."
                    R_PAREN@29..30 ")"
            "#]],
        )
    }

    #[test]
    fn tuple() {
        check_with_f(
            false,
            super::expression,
            "(i, \"am\", #\"a\", tuple, 0x0, \"pression\", 20)",
            expect![[r##"
                EXP@0..43
                  AT_EXP@0..43
                    TUPLE_EXP@0..43
                      L_PAREN@0..1 "("
                      EXP@1..2
                        AT_EXP@1..2
                          VID_EXP@1..2
                            LONG_VID@1..2
                              IDENT@1..2 "i"
                      COMMA@2..3 ","
                      WHITESPACE@3..4
                      EXP@4..8
                        AT_EXP@4..8
                          SCON_EXP@4..8
                            STRING@4..8 "\"am\""
                      COMMA@8..9 ","
                      WHITESPACE@9..10
                      EXP@10..14
                        AT_EXP@10..14
                          SCON_EXP@10..14
                            CHAR@10..14 "#\"a\""
                      COMMA@14..15 ","
                      WHITESPACE@15..16
                      EXP@16..21
                        AT_EXP@16..21
                          VID_EXP@16..21
                            LONG_VID@16..21
                              IDENT@16..21 "tuple"
                      COMMA@21..22 ","
                      WHITESPACE@22..23
                      EXP@23..26
                        AT_EXP@23..26
                          SCON_EXP@23..26
                            INT@23..26 "0x0"
                      COMMA@26..27 ","
                      WHITESPACE@27..28
                      EXP@28..38
                        AT_EXP@28..38
                          SCON_EXP@28..38
                            STRING@28..38 "\"pression\""
                      COMMA@38..39 ","
                      WHITESPACE@39..40
                      EXP@40..42
                        AT_EXP@40..42
                          SCON_EXP@40..42
                            INT@40..42 "20"
                      R_PAREN@42..43 ")"
            "##]],
        )
    }

    #[test]
    fn expression_sequence() {
        check_with_f(
            false,
            super::expression,
            "(here; is; a ; sequence ; \"of\" ; expressions)",
            expect![[r#"
                EXP@0..45
                  AT_EXP@0..45
                    SEQ_EXP@0..45
                      L_PAREN@0..1 "("
                      EXP@1..5
                        AT_EXP@1..5
                          VID_EXP@1..5
                            LONG_VID@1..5
                              IDENT@1..5 "here"
                      SEMICOLON@5..6 ";"
                      WHITESPACE@6..7
                      EXP@7..9
                        AT_EXP@7..9
                          VID_EXP@7..9
                            LONG_VID@7..9
                              IDENT@7..9 "is"
                      SEMICOLON@9..10 ";"
                      WHITESPACE@10..11
                      EXP@11..12
                        AT_EXP@11..12
                          VID_EXP@11..12
                            LONG_VID@11..12
                              IDENT@11..12 "a"
                      WHITESPACE@12..13
                      SEMICOLON@13..14 ";"
                      WHITESPACE@14..15
                      EXP@15..23
                        AT_EXP@15..23
                          VID_EXP@15..23
                            LONG_VID@15..23
                              IDENT@15..23 "sequence"
                      WHITESPACE@23..24
                      SEMICOLON@24..25 ";"
                      WHITESPACE@25..26
                      EXP@26..30
                        AT_EXP@26..30
                          SCON_EXP@26..30
                            STRING@26..30 "\"of\""
                      WHITESPACE@30..31
                      SEMICOLON@31..32 ";"
                      WHITESPACE@32..33
                      EXP@33..44
                        AT_EXP@33..44
                          VID_EXP@33..44
                            LONG_VID@33..44
                              IDENT@33..44 "expressions"
                      R_PAREN@44..45 ")"
            "#]],
        )
    }

    #[test]
    fn typed_expression() {
        check_with_f(
            false,
            super::expression,
            "x : 'a",
            expect![[r#"
                EXP@0..6
                  TY_EXP@0..6
                    EXP@0..1
                      AT_EXP@0..1
                        VID_EXP@0..1
                          LONG_VID@0..1
                            IDENT@0..1 "x"
                    WHITESPACE@1..2
                    COLON@2..3 ":"
                    WHITESPACE@3..4
                    TY@4..6
                      TY_VAR@4..6 "'a"
            "#]],
        )
    }

    #[test]
    fn another_typed_expression() {
        check_with_f(
            false,
            super::expression,
            "[[1], [2], [3], [4], [5]] : int list list",
            expect![[r#"
                EXP@0..41
                  TY_EXP@0..41
                    EXP@0..25
                      AT_EXP@0..25
                        LIST_EXP@0..25
                          L_BRACKET@0..1 "["
                          EXP@1..4
                            AT_EXP@1..4
                              LIST_EXP@1..4
                                L_BRACKET@1..2 "["
                                EXP@2..3
                                  AT_EXP@2..3
                                    SCON_EXP@2..3
                                      INT@2..3 "1"
                                R_BRACKET@3..4 "]"
                          COMMA@4..5 ","
                          WHITESPACE@5..6
                          EXP@6..9
                            AT_EXP@6..9
                              LIST_EXP@6..9
                                L_BRACKET@6..7 "["
                                EXP@7..8
                                  AT_EXP@7..8
                                    SCON_EXP@7..8
                                      INT@7..8 "2"
                                R_BRACKET@8..9 "]"
                          COMMA@9..10 ","
                          WHITESPACE@10..11
                          EXP@11..14
                            AT_EXP@11..14
                              LIST_EXP@11..14
                                L_BRACKET@11..12 "["
                                EXP@12..13
                                  AT_EXP@12..13
                                    SCON_EXP@12..13
                                      INT@12..13 "3"
                                R_BRACKET@13..14 "]"
                          COMMA@14..15 ","
                          WHITESPACE@15..16
                          EXP@16..19
                            AT_EXP@16..19
                              LIST_EXP@16..19
                                L_BRACKET@16..17 "["
                                EXP@17..18
                                  AT_EXP@17..18
                                    SCON_EXP@17..18
                                      INT@17..18 "4"
                                R_BRACKET@18..19 "]"
                          COMMA@19..20 ","
                          WHITESPACE@20..21
                          EXP@21..24
                            AT_EXP@21..24
                              LIST_EXP@21..24
                                L_BRACKET@21..22 "["
                                EXP@22..23
                                  AT_EXP@22..23
                                    SCON_EXP@22..23
                                      INT@22..23 "5"
                                R_BRACKET@23..24 "]"
                          R_BRACKET@24..25 "]"
                    WHITESPACE@25..26
                    COLON@26..27 ":"
                    WHITESPACE@27..28
                    TY@28..41
                      TY_CON_EXP@28..41
                        TY@28..31
                          TY_CON_EXP@28..31
                            LONG_TY_CON@28..31
                              IDENT@28..31 "int"
                        WHITESPACE@31..32
                        TY@32..36
                          TY_CON_EXP@32..36
                            LONG_TY_CON@32..36
                              IDENT@32..36 "list"
                        WHITESPACE@36..37
                        LONG_TY_CON@37..41
                          IDENT@37..41 "list"
            "#]],
        )
    }

    #[test]
    fn one_application() {
        check_with_f(
            false,
            super::expression,
            "a b",
            expect![[r#"
            EXP@0..3
              APP_EXP@0..3
                AT_EXP@0..1
                  VID_EXP@0..1
                    LONG_VID@0..1
                      IDENT@0..1 "a"
                WHITESPACE@1..2
                AT_EXP@2..3
                  VID_EXP@2..3
                    LONG_VID@2..3
                      IDENT@2..3 "b"
            "#]],
        )
    }

    #[test]
    fn two_applications() {
        check_with_f(
            false,
            super::expression,
            "a b c",
            expect![[r#"
                EXP@0..5
                  APP_EXP@0..5
                    EXP@0..3
                      APP_EXP@0..3
                        AT_EXP@0..1
                          VID_EXP@0..1
                            LONG_VID@0..1
                              IDENT@0..1 "a"
                        WHITESPACE@1..2
                        AT_EXP@2..3
                          VID_EXP@2..3
                            LONG_VID@2..3
                              IDENT@2..3 "b"
                    WHITESPACE@3..4
                    AT_EXP@4..5
                      VID_EXP@4..5
                        LONG_VID@4..5
                          IDENT@4..5 "c"
            "#]],
        )
    }

    #[test]
    fn many_applications() {
        check_with_f(
            false,
            super::expression,
            "a b c d e f g x",
            expect![[r#"
                EXP@0..15
                  APP_EXP@0..15
                    EXP@0..13
                      APP_EXP@0..13
                        EXP@0..11
                          APP_EXP@0..11
                            EXP@0..9
                              APP_EXP@0..9
                                EXP@0..7
                                  APP_EXP@0..7
                                    EXP@0..5
                                      APP_EXP@0..5
                                        EXP@0..3
                                          APP_EXP@0..3
                                            AT_EXP@0..1
                                              VID_EXP@0..1
                                                LONG_VID@0..1
                                                  IDENT@0..1 "a"
                                            WHITESPACE@1..2
                                            AT_EXP@2..3
                                              VID_EXP@2..3
                                                LONG_VID@2..3
                                                  IDENT@2..3 "b"
                                        WHITESPACE@3..4
                                        AT_EXP@4..5
                                          VID_EXP@4..5
                                            LONG_VID@4..5
                                              IDENT@4..5 "c"
                                    WHITESPACE@5..6
                                    AT_EXP@6..7
                                      VID_EXP@6..7
                                        LONG_VID@6..7
                                          IDENT@6..7 "d"
                                WHITESPACE@7..8
                                AT_EXP@8..9
                                  VID_EXP@8..9
                                    LONG_VID@8..9
                                      IDENT@8..9 "e"
                            WHITESPACE@9..10
                            AT_EXP@10..11
                              VID_EXP@10..11
                                LONG_VID@10..11
                                  IDENT@10..11 "f"
                        WHITESPACE@11..12
                        AT_EXP@12..13
                          VID_EXP@12..13
                            LONG_VID@12..13
                              IDENT@12..13 "g"
                    WHITESPACE@13..14
                    AT_EXP@14..15
                      VID_EXP@14..15
                        LONG_VID@14..15
                          IDENT@14..15 "x"
            "#]],
        )
    }

    #[test]
    fn fn_application() {
        check_with_f(
            false,
            super::expression,
            "(fn x => x) (fn y => y) 1",
            expect![[r#"
                EXP@0..25
                  APP_EXP@0..25
                    EXP@0..23
                      APP_EXP@0..23
                        AT_EXP@0..11
                          L_PAREN@0..1 "("
                          EXP@1..10
                            FN_EXP@1..10
                              FN_KW@1..3 "fn"
                              WHITESPACE@3..4
                              MATCH@4..10
                                MRULE@4..10
                                  PAT@4..5
                                    AT_PAT@4..5
                                      VID_PAT@4..5
                                        LONG_VID@4..5
                                          IDENT@4..5 "x"
                                  WHITESPACE@5..6
                                  THICK_ARROW@6..8 "=>"
                                  WHITESPACE@8..9
                                  EXP@9..10
                                    AT_EXP@9..10
                                      VID_EXP@9..10
                                        LONG_VID@9..10
                                          IDENT@9..10 "x"
                          R_PAREN@10..11 ")"
                        WHITESPACE@11..12
                        AT_EXP@12..23
                          L_PAREN@12..13 "("
                          EXP@13..22
                            FN_EXP@13..22
                              FN_KW@13..15 "fn"
                              WHITESPACE@15..16
                              MATCH@16..22
                                MRULE@16..22
                                  PAT@16..17
                                    AT_PAT@16..17
                                      VID_PAT@16..17
                                        LONG_VID@16..17
                                          IDENT@16..17 "y"
                                  WHITESPACE@17..18
                                  THICK_ARROW@18..20 "=>"
                                  WHITESPACE@20..21
                                  EXP@21..22
                                    AT_EXP@21..22
                                      VID_EXP@21..22
                                        LONG_VID@21..22
                                          IDENT@21..22 "y"
                          R_PAREN@22..23 ")"
                    WHITESPACE@23..24
                    AT_EXP@24..25
                      SCON_EXP@24..25
                        INT@24..25 "1"
            "#]],
        )
    }

    #[test]
    fn infix() {
        check_with_f(
            false,
            super::expression,
            "x + y",
            expect![[r#"
                EXP@0..25
                  APP_EXP@0..25
            "#]]
        )
    }

    #[test]
    fn several_infix() {
        check_with_f(
            false,
            super::expression,
            "a + b * c / d",
            expect![[r#"
                EXP@0..25
                  APP_EXP@0..25
            "#]]
        )
    }
}
