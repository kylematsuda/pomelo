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
                      VID_PAT@4..5
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
                      VID_PAT@4..5
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
                      VID_PAT@4..5
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
                      VID_PAT@4..5
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
