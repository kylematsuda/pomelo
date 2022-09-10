use crate::grammar;
use crate::{parser::Token, Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn ty(p: &mut Parser) {
    fun_ty(p)
}

fn fun_ty(p: &mut Parser) {
    grammar::precedence_climber_right(
        p,
        TY,
        FUN_TY_EXP,
        tuple_ty,
        |p| p.eat_through_trivia(THIN_ARROW),
        fun_ty,
    )
}

fn tuple_ty(p: &mut Parser) {
    grammar::precedence_climber_flat(
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
                }
                _ => false,
            }
        },
        tycon_seq,
    )
}

fn tycon_seq(p: &mut Parser) {
    grammar::precedence_climber_flat(
        p,
        TY,
        TY_CON_EXP,
        ty_atom_or_longtycon,
        |p| {
            // We can't accept '*' as an identifier here!
            let t = p.peek_token_next_nontrivia(0);
            match t.map(Token::kind) {
                Some(k) if k.is_ty_atom() => true,
                Some(IDENT) => {
                    if let Some("*") = t.map(Token::text) {
                        false
                    } else {
                        true
                    }
                }
                _ => false,
            }
        },
        ty_atom_or_longtycon,
    )
}

fn ty_atom_or_longtycon(p: &mut Parser) {
    if p.peek().is_ty_atom() {
        p.eat_trivia();
        ty_atom(p);
    } else if p.peek() == IDENT {
        p.eat_trivia();
        longtycon(p);
    }
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
        }
        _ => p.error("expected type expression"),
    }
}

pub(crate) fn tyvarseq(p: &mut Parser) {
    let _ng = p.start_node(TY_VAR_SEQ);
    while p.eat_through_trivia(TY_VAR) {}
}

pub(crate) fn tycon(p: &mut Parser) {
    let _ng = p.start_node(TY_CON);
    p.expect(IDENT);
}

fn longtycon(p: &mut Parser) {
    let _ty = p.start_node(TY);
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

#[cfg(test)]
mod tests {
    use crate::tests::check_with_f;
    use expect_test::expect;

    #[test]
    fn tyvar() {
        check_with_f(
            false,
            super::ty,
            "'a",
            expect![[r#"
                TY@0..2
                  TY_VAR@0..2 "'a"
            "#]],
        )
    }

    #[test]
    fn record_ty() {
        check_with_f(
            false,
            super::ty,
            "{ a: 'a, b: 'b, c: int, apples: orange }",
            expect![[r#"
                TY@0..40
                  RECORD_TY_EXP@0..40
                    L_BRACE@0..1 "{"
                    WHITESPACE@1..2
                    TY_ROW@2..7
                      IDENT@2..3 "a"
                      COLON@3..4 ":"
                      WHITESPACE@4..5
                      TY@5..7
                        TY_VAR@5..7 "'a"
                    COMMA@7..8 ","
                    WHITESPACE@8..9
                    TY_ROW@9..14
                      IDENT@9..10 "b"
                      COLON@10..11 ":"
                      WHITESPACE@11..12
                      TY@12..14
                        TY_VAR@12..14 "'b"
                    COMMA@14..15 ","
                    WHITESPACE@15..16
                    TY_ROW@16..22
                      IDENT@16..17 "c"
                      COLON@17..18 ":"
                      WHITESPACE@18..19
                      TY@19..22
                        LONG_TY_CON@19..22
                          IDENT@19..22 "int"
                    COMMA@22..23 ","
                    WHITESPACE@23..24
                    TY_ROW@24..38
                      IDENT@24..30 "apples"
                      COLON@30..31 ":"
                      WHITESPACE@31..32
                      TY@32..38
                        LONG_TY_CON@32..38
                          IDENT@32..38 "orange"
                    WHITESPACE@38..39
                    R_BRACE@39..40 "}"
            "#]],
        )
    }

    #[test]
    fn constructed_type() {
        check_with_f(
            false,
            super::ty,
            "'a list list list 'b test int",
            expect![[r#"
                TY@0..29
                  TY_CON_EXP@0..29
                    TY@0..2
                      TY_VAR@0..2 "'a"
                    WHITESPACE@2..3
                    TY@3..7
                      LONG_TY_CON@3..7
                        IDENT@3..7 "list"
                    WHITESPACE@7..8
                    TY@8..12
                      LONG_TY_CON@8..12
                        IDENT@8..12 "list"
                    WHITESPACE@12..13
                    TY@13..17
                      LONG_TY_CON@13..17
                        IDENT@13..17 "list"
                    WHITESPACE@17..18
                    TY@18..20
                      TY_VAR@18..20 "'b"
                    WHITESPACE@20..21
                    TY@21..25
                      LONG_TY_CON@21..25
                        IDENT@21..25 "test"
                    WHITESPACE@25..26
                    TY@26..29
                      LONG_TY_CON@26..29
                        IDENT@26..29 "int"
            "#]],
        )
    }

    #[test]
    fn tuple_type() {
        check_with_f(
            false,
            super::ty,
            "'a * int * real * 'b * 'c list * string",
            expect![[r#"
                TY@0..39
                  TUPLE_TY_EXP@0..39
                    TY@0..2
                      TY_VAR@0..2 "'a"
                    WHITESPACE@2..3
                    STAR@3..4 "*"
                    WHITESPACE@4..5
                    TY@5..8
                      LONG_TY_CON@5..8
                        IDENT@5..8 "int"
                    WHITESPACE@8..9
                    STAR@9..10 "*"
                    WHITESPACE@10..11
                    TY@11..15
                      LONG_TY_CON@11..15
                        IDENT@11..15 "real"
                    WHITESPACE@15..16
                    STAR@16..17 "*"
                    WHITESPACE@17..18
                    TY@18..20
                      TY_VAR@18..20 "'b"
                    WHITESPACE@20..21
                    STAR@21..22 "*"
                    WHITESPACE@22..23
                    TY@23..30
                      TY_CON_EXP@23..30
                        TY@23..25
                          TY_VAR@23..25 "'c"
                        WHITESPACE@25..26
                        TY@26..30
                          LONG_TY_CON@26..30
                            IDENT@26..30 "list"
                    WHITESPACE@30..31
                    STAR@31..32 "*"
                    WHITESPACE@32..33
                    TY@33..39
                      LONG_TY_CON@33..39
                        IDENT@33..39 "string"
            "#]],
        )
    }

    #[test]
    fn function_type() {
        check_with_f(
            false,
            super::ty,
            "int -> real -> int -> 'a",
            expect![[r#"
                TY@0..24
                  FUN_TY_EXP@0..24
                    TY@0..3
                      LONG_TY_CON@0..3
                        IDENT@0..3 "int"
                    WHITESPACE@3..4
                    THIN_ARROW@4..6 "->"
                    WHITESPACE@6..7
                    TY@7..24
                      FUN_TY_EXP@7..24
                        TY@7..11
                          LONG_TY_CON@7..11
                            IDENT@7..11 "real"
                        WHITESPACE@11..12
                        THIN_ARROW@12..14 "->"
                        WHITESPACE@14..15
                        TY@15..24
                          FUN_TY_EXP@15..24
                            TY@15..18
                              LONG_TY_CON@15..18
                                IDENT@15..18 "int"
                            WHITESPACE@18..19
                            THIN_ARROW@19..21 "->"
                            WHITESPACE@21..22
                            TY@22..24
                              TY_VAR@22..24 "'a"
            "#]],
        )
    }

    #[test]
    fn function_type_grouped() {
        check_with_f(
            false,
            super::ty,
            "(int -> real) -> int -> 'a",
            expect![[r#"
                TY@0..26
                  FUN_TY_EXP@0..26
                    TY@0..13
                      L_PAREN@0..1 "("
                      TY@1..12
                        FUN_TY_EXP@1..12
                          TY@1..4
                            LONG_TY_CON@1..4
                              IDENT@1..4 "int"
                          WHITESPACE@4..5
                          THIN_ARROW@5..7 "->"
                          WHITESPACE@7..8
                          TY@8..12
                            LONG_TY_CON@8..12
                              IDENT@8..12 "real"
                      R_PAREN@12..13 ")"
                    WHITESPACE@13..14
                    THIN_ARROW@14..16 "->"
                    WHITESPACE@16..17
                    TY@17..26
                      FUN_TY_EXP@17..26
                        TY@17..20
                          LONG_TY_CON@17..20
                            IDENT@17..20 "int"
                        WHITESPACE@20..21
                        THIN_ARROW@21..23 "->"
                        WHITESPACE@23..24
                        TY@24..26
                          TY_VAR@24..26 "'a"
            "#]],
        )
    }
}
