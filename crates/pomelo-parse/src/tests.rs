use crate::Parser;
use expect_test::{expect, Expect};

pub(crate) fn check(should_error: bool, src: &str, expect: Expect) {
    check_with_f(should_error, crate::grammar::declaration, src, expect)
}

pub(crate) fn check_with_f<F>(should_error: bool, parse_function: F, src: &str, expect: Expect)
where
    F: FnMut(&mut Parser),
{
    let parser = Parser::new(src);
    let tree = parser.parse_inner(parse_function);

    let actual: String = format!("{}", tree);
    expect.assert_eq(&actual);

    assert_eq!(tree.has_errors(), should_error);
}

#[test]
fn declare_int() {
    check(
        false,
        "val a = 1",
        expect![[r#"
            DEC@0..9
              VAL_DEC@0..9
                VAL_KW@0..3 "val"
                WHITESPACE@3..4
                VAL_BIND@4..9
                  PAT@4..5
                    AT_PAT@4..5
                      LONG_VID@4..5
                        IDENT@4..5 "a"
                  WHITESPACE@5..6
                  EQ@6..7 "="
                  WHITESPACE@7..8
                  EXP@8..9
                    AT_EXP@8..9
                      SCON_EXP@8..9
                        INT@8..9 "1"
        "#]],
    )
}

#[test]
fn declare_float() {
    check(
        false,
        "val a = 1",
        expect![[r#"
            DEC@0..9
              VAL_DEC@0..9
                VAL_KW@0..3 "val"
                WHITESPACE@3..4
                VAL_BIND@4..9
                  PAT@4..5
                    AT_PAT@4..5
                      LONG_VID@4..5
                        IDENT@4..5 "a"
                  WHITESPACE@5..6
                  EQ@6..7 "="
                  WHITESPACE@7..8
                  EXP@8..9
                    AT_EXP@8..9
                      SCON_EXP@8..9
                        INT@8..9 "1"
        "#]],
    )
}

#[test]
fn declare_char() {
    check(
        false,
        "val a = #\"c\"",
        expect![[r##"
            DEC@0..12
              VAL_DEC@0..12
                VAL_KW@0..3 "val"
                WHITESPACE@3..4
                VAL_BIND@4..12
                  PAT@4..5
                    AT_PAT@4..5
                      LONG_VID@4..5
                        IDENT@4..5 "a"
                  WHITESPACE@5..6
                  EQ@6..7 "="
                  WHITESPACE@7..8
                  EXP@8..12
                    AT_EXP@8..12
                      SCON_EXP@8..12
                        CHAR@8..12 "#\"c\""
        "##]],
    )
}

#[test]
fn declare_string() {
    check(
        false,
        "val a = \"hello\"",
        expect![[r#"
            DEC@0..15
              VAL_DEC@0..15
                VAL_KW@0..3 "val"
                WHITESPACE@3..4
                VAL_BIND@4..15
                  PAT@4..5
                    AT_PAT@4..5
                      LONG_VID@4..5
                        IDENT@4..5 "a"
                  WHITESPACE@5..6
                  EQ@6..7 "="
                  WHITESPACE@7..8
                  EXP@8..15
                    AT_EXP@8..15
                      SCON_EXP@8..15
                        STRING@8..15 "\"hello\""
        "#]],
    )
}

#[test]
fn declare_type() {
    check(
        false,
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

#[test]
fn declare_fun_constant_patterns() {
    check(
        false,
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
fn declare_fun_variable_patterns() {
    check(
        false,
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
