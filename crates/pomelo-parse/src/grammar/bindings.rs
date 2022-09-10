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
                              IDENT@4..10 "simple"
                      WHITESPACE@10..11
                      EQ@11..12 "="
                      WHITESPACE@12..13
                      EXP@13..20
                        AT_EXP@13..20
                          VID_EXP@13..20
                            LONG_VID@13..20
                              IDENT@13..20 "valbind"
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
                              IDENT@4..10 "simple"
                      WHITESPACE@10..11
                      EQ@11..12 "="
                      WHITESPACE@12..13
                      EXP@13..20
                        AT_EXP@13..20
                          VID_EXP@13..20
                            LONG_VID@13..20
                              IDENT@13..20 "valbind"
                      WHITESPACE@20..21
                      AND_KW@21..24 "and"
                      WHITESPACE@24..25
                      PAT@25..32
                        AT_PAT@25..32
                          VID_PAT@25..32
                            LONG_VID@25..32
                              IDENT@25..32 "another"
                      WHITESPACE@32..33
                      EQ@33..34 "="
                      WHITESPACE@34..35
                      EXP@35..38
                        AT_EXP@35..38
                          VID_EXP@35..38
                            LONG_VID@35..38
                              IDENT@35..38 "one"
                      WHITESPACE@38..39
                      AND_KW@39..42 "and"
                      WHITESPACE@42..43
                      PAT@43..46
                        AT_PAT@43..46
                          VID_PAT@43..46
                            LONG_VID@43..46
                              IDENT@43..46 "yet"
                      WHITESPACE@46..47
                      EQ@47..48 "="
                      WHITESPACE@48..49
                      EXP@49..56
                        AT_EXP@49..56
                          VID_EXP@49..56
                            LONG_VID@49..56
                              IDENT@49..56 "another"
            "#]],
        )
    }

    #[test]
    fn valbind_rec() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "val rec simple = fn x => x",
            expect![[r#"
                DEC@0..26
                  VAL_DEC@0..26
                    VAL_KW@0..3 "val"
                    WHITESPACE@3..4
                    VAL_BIND@4..26
                      REC_KW@4..7 "rec"
                      WHITESPACE@7..8
                      PAT@8..14
                        AT_PAT@8..14
                          VID_PAT@8..14
                            LONG_VID@8..14
                              IDENT@8..14 "simple"
                      WHITESPACE@14..15
                      EQ@15..16 "="
                      WHITESPACE@16..17
                      EXP@17..26
                        FN_EXP@17..26
                          FN_KW@17..19 "fn"
                          WHITESPACE@19..20
                          MATCH@20..26
                            MRULE@20..26
                              PAT@20..21
                                AT_PAT@20..21
                                  VID_PAT@20..21
                                    LONG_VID@20..21
                                      IDENT@20..21 "x"
                              WHITESPACE@21..22
                              THICK_ARROW@22..24 "=>"
                              WHITESPACE@24..25
                              EXP@25..26
                                AT_EXP@25..26
                                  VID_EXP@25..26
                                    LONG_VID@25..26
                                      IDENT@25..26 "x"
            "#]],
        )
    }

    #[test]
    fn bad_valbind_rec() {
        // val rec can only bind a fn-match exp
        check_with_f(
            true,
            crate::grammar::declaration,
            "val rec simple = bad",
            expect![[r#"
                DEC@0..17
                  VAL_DEC@0..17
                    VAL_KW@0..3 "val"
                    WHITESPACE@3..4
                    VAL_BIND@4..17
                      REC_KW@4..7 "rec"
                      WHITESPACE@7..8
                      PAT@8..14
                        AT_PAT@8..14
                          VID_PAT@8..14
                            LONG_VID@8..14
                              IDENT@8..14 "simple"
                      WHITESPACE@14..15
                      EQ@15..16 "="
                      WHITESPACE@16..17
                      ERROR@17..17 ""
            "#]],
        )
    }

    #[test]
    fn fun_patterns() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "fun myfun 0 0 = 0
           | myfun 1 0 = 1
           | myfun 0 _ = 1
           | myfun 1 x = x",
            expect![[r#"
            DEC@0..98
              FUN_DEC@0..98
                FUN_KW@0..3 "fun"
                WHITESPACE@3..4
                TY_VAR_SEQ@4..4
                FVAL_BIND@4..98
                  FVAL_BIND_ROW@4..29
                    VID@4..9
                      IDENT@4..9 "myfun"
                    WHITESPACE@9..10
                    AT_PAT@10..11
                      SCON_PAT@10..11
                        INT@10..11 "0"
                    WHITESPACE@11..12
                    AT_PAT@12..13
                      SCON_PAT@12..13
                        INT@12..13 "0"
                    WHITESPACE@13..14
                    EQ@14..15 "="
                    WHITESPACE@15..16
                    EXP@16..17
                      AT_EXP@16..17
                        SCON_EXP@16..17
                          INT@16..17 "0"
                    WHITESPACE@17..29
                  FVAL_BIND_ROW@29..56
                    PIPE@29..30 "|"
                    WHITESPACE@30..31
                    VID@31..36
                      IDENT@31..36 "myfun"
                    WHITESPACE@36..37
                    AT_PAT@37..38
                      SCON_PAT@37..38
                        INT@37..38 "1"
                    WHITESPACE@38..39
                    AT_PAT@39..40
                      SCON_PAT@39..40
                        INT@39..40 "0"
                    WHITESPACE@40..41
                    EQ@41..42 "="
                    WHITESPACE@42..43
                    EXP@43..44
                      AT_EXP@43..44
                        SCON_EXP@43..44
                          INT@43..44 "1"
                    WHITESPACE@44..56
                  FVAL_BIND_ROW@56..83
                    PIPE@56..57 "|"
                    WHITESPACE@57..58
                    VID@58..63
                      IDENT@58..63 "myfun"
                    WHITESPACE@63..64
                    AT_PAT@64..65
                      SCON_PAT@64..65
                        INT@64..65 "0"
                    WHITESPACE@65..66
                    AT_PAT@66..67
                      WILDCARD_PAT@66..67
                        UNDERSCORE@66..67 "_"
                    WHITESPACE@67..68
                    EQ@68..69 "="
                    WHITESPACE@69..70
                    EXP@70..71
                      AT_EXP@70..71
                        SCON_EXP@70..71
                          INT@70..71 "1"
                    WHITESPACE@71..83
                  FVAL_BIND_ROW@83..98
                    PIPE@83..84 "|"
                    WHITESPACE@84..85
                    VID@85..90
                      IDENT@85..90 "myfun"
                    WHITESPACE@90..91
                    AT_PAT@91..92
                      SCON_PAT@91..92
                        INT@91..92 "1"
                    WHITESPACE@92..93
                    AT_PAT@93..94
                      VID_PAT@93..94
                        LONG_VID@93..94
                          IDENT@93..94 "x"
                    WHITESPACE@94..95
                    EQ@95..96 "="
                    WHITESPACE@96..97
                    EXP@97..98
                      AT_EXP@97..98
                        VID_EXP@97..98
                          LONG_VID@97..98
                            IDENT@97..98 "x"
        "#]],
        )
    }

    #[test]
    fn fun_constant_patterns() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "fun myfun 0 0 = 0
           | myfun 1 0 = 1
           | myfun 0 1 = 1
           | myfun 1 1 = 0",
            expect![[r#"
            DEC@0..98
              FUN_DEC@0..98
                FUN_KW@0..3 "fun"
                WHITESPACE@3..4
                TY_VAR_SEQ@4..4
                FVAL_BIND@4..98
                  FVAL_BIND_ROW@4..29
                    VID@4..9
                      IDENT@4..9 "myfun"
                    WHITESPACE@9..10
                    AT_PAT@10..11
                      SCON_PAT@10..11
                        INT@10..11 "0"
                    WHITESPACE@11..12
                    AT_PAT@12..13
                      SCON_PAT@12..13
                        INT@12..13 "0"
                    WHITESPACE@13..14
                    EQ@14..15 "="
                    WHITESPACE@15..16
                    EXP@16..17
                      AT_EXP@16..17
                        SCON_EXP@16..17
                          INT@16..17 "0"
                    WHITESPACE@17..29
                  FVAL_BIND_ROW@29..56
                    PIPE@29..30 "|"
                    WHITESPACE@30..31
                    VID@31..36
                      IDENT@31..36 "myfun"
                    WHITESPACE@36..37
                    AT_PAT@37..38
                      SCON_PAT@37..38
                        INT@37..38 "1"
                    WHITESPACE@38..39
                    AT_PAT@39..40
                      SCON_PAT@39..40
                        INT@39..40 "0"
                    WHITESPACE@40..41
                    EQ@41..42 "="
                    WHITESPACE@42..43
                    EXP@43..44
                      AT_EXP@43..44
                        SCON_EXP@43..44
                          INT@43..44 "1"
                    WHITESPACE@44..56
                  FVAL_BIND_ROW@56..83
                    PIPE@56..57 "|"
                    WHITESPACE@57..58
                    VID@58..63
                      IDENT@58..63 "myfun"
                    WHITESPACE@63..64
                    AT_PAT@64..65
                      SCON_PAT@64..65
                        INT@64..65 "0"
                    WHITESPACE@65..66
                    AT_PAT@66..67
                      SCON_PAT@66..67
                        INT@66..67 "1"
                    WHITESPACE@67..68
                    EQ@68..69 "="
                    WHITESPACE@69..70
                    EXP@70..71
                      AT_EXP@70..71
                        SCON_EXP@70..71
                          INT@70..71 "1"
                    WHITESPACE@71..83
                  FVAL_BIND_ROW@83..98
                    PIPE@83..84 "|"
                    WHITESPACE@84..85
                    VID@85..90
                      IDENT@85..90 "myfun"
                    WHITESPACE@90..91
                    AT_PAT@91..92
                      SCON_PAT@91..92
                        INT@91..92 "1"
                    WHITESPACE@92..93
                    AT_PAT@93..94
                      SCON_PAT@93..94
                        INT@93..94 "1"
                    WHITESPACE@94..95
                    EQ@95..96 "="
                    WHITESPACE@96..97
                    EXP@97..98
                      AT_EXP@97..98
                        SCON_EXP@97..98
                          INT@97..98 "0"
        "#]],
        )
    }
    #[test]
    fn tybind() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "type 'a 'b 'c mytype = 'd",
            expect![[r#"
            DEC@0..25
              TY_DEC@0..25
                TYPE_KW@0..4 "type"
                WHITESPACE@4..5
                TY_BIND@5..25
                  TY_VAR_SEQ@5..14
                    TY_VAR@5..7 "'a"
                    WHITESPACE@7..8
                    TY_VAR@8..10 "'b"
                    WHITESPACE@10..11
                    TY_VAR@11..13 "'c"
                    WHITESPACE@13..14
                  TY_CON@14..20
                    IDENT@14..20 "mytype"
                  WHITESPACE@20..21
                  EQ@21..22 "="
                  WHITESPACE@22..23
                  TY@23..25
                    TY_VAR@23..25 "'d"
        "#]],
        )
    }
}
