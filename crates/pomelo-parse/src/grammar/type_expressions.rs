use crate::grammar;
use crate::{parser::Token, Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn ty(p: &mut Parser) {
    fun_ty(p)
}

fn fun_ty(p: &mut Parser) {
    grammar::descend_right(
        p,
        FUN_TY,
        tuple_ty,
        |p| p.eat_through_trivia(THIN_ARROW),
        fun_ty,
    )
}

fn tuple_ty(p: &mut Parser) {
    grammar::descend_flat(
        p,
        TUPLE_TY_EXP,
        tycon_seq,
        |p| {
            if star_ident(p) {
                p.eat_trivia();
                assert_eq!(p.eat_mapped(STAR), IDENT);
                true
            } else {
                false
            }
        },
        tycon_seq,
    )
}

fn star_ident(p: &Parser) -> bool {
    match p.peek_token_next_nontrivia(0).map(Token::text) {
        Some("*") => true,
        _ => false,
    }
}

fn tycon_seq(p: &mut Parser) {
    let outer = p.checkpoint();

    if p.peek_next_nontrivia(0).is_ty_atom() {
        p.eat_trivia();
        ty_atom(p);

        // If there's just a single ty atom, 
        // then this is an AtomicTy
        if !non_star_ident(p) {
            return;
        } 
    }

    let _ng = p.start_node_at(outer, CON_TY);

    // Consume types in a flat sequence
    while non_star_ident(p) {
        p.eat_trivia();

        if p.peek().is_ty_atom() {
            ty_atom(p);
        } else if p.peek() == IDENT {
            let checkpoint = p.checkpoint();
            longtycon(p);

            // If this is not the final longtycon, turn it into a 
            // constructed type node
            if non_star_ident(p) {
                let _ng = p.start_node_at(checkpoint, CON_TY);
            }
        } else {
            p.error("Expected type constructor.");
        }
    }
}

fn non_star_ident(p: &Parser) -> bool {
    let t = p.peek_token_next_nontrivia(0);
    let k = t.map(Token::kind).unwrap_or(EOF);

    if k.is_ty_atom() {
        true
    } else if k == IDENT {
        t.map(Token::text) != Some("*")
    } else {
        false
    }
}

fn ty_atom(p: &mut Parser) {
    match p.peek() {
        TYVAR => {
            let _ng = p.start_node(TYVAR_TY);
            p.expect(TYVAR)
        }
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
    while p.eat_through_trivia(TYVAR) {}
}

pub(crate) fn tycon(p: &mut Parser) {
    if p.peek() == IDENT {
        if p.peek_text().contains('*') {
            p.error("type constructors cannot contain stars (\'*\')");
        }
        // TODO: decide if eating in this situation is the best policy
        p.eat_mapped(TY_CON);
    } else {
        p.error("expected type constructor");
    }
}

pub(crate) fn longtycon(p: &mut Parser) {
    let _ng = p.start_node(LONG_TY_CON);

    // A longtycon is a sequence of strids separated by DOTs,
    // then a VID
    while p.is_strid() && p.peek_next_nontrivia(1) == DOT {
        grammar::strid(p);
        p.eat_trivia();
        assert!(p.eat(DOT));
        p.eat_trivia();
    }
    tycon(p)
}

fn record_ty(p: &mut Parser) {
    let _ng = p.start_node(RECORD_TY);

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

    grammar::label(p);
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
            super::ty_atom,
            "'a",
            expect![[r#"
                TYVAR_TY@0..2
                  TYVAR@0..2 "'a"
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
                RECORD_TY@0..40
                  L_BRACE@0..1 "{"
                  WHITESPACE@1..2
                  TY_ROW@2..7
                    LAB@2..3 "a"
                    COLON@3..4 ":"
                    WHITESPACE@4..5
                    TYVAR_TY@5..7
                      TYVAR@5..7 "'a"
                  COMMA@7..8 ","
                  WHITESPACE@8..9
                  TY_ROW@9..14
                    LAB@9..10 "b"
                    COLON@10..11 ":"
                    WHITESPACE@11..12
                    TYVAR_TY@12..14
                      TYVAR@12..14 "'b"
                  COMMA@14..15 ","
                  WHITESPACE@15..16
                  TY_ROW@16..22
                    LAB@16..17 "c"
                    COLON@17..18 ":"
                    WHITESPACE@18..19
                    CON_TY@19..22
                      LONG_TY_CON@19..22
                        TY_CON@19..22 "int"
                  COMMA@22..23 ","
                  WHITESPACE@23..24
                  TY_ROW@24..38
                    LAB@24..30 "apples"
                    COLON@30..31 ":"
                    WHITESPACE@31..32
                    CON_TY@32..38
                      LONG_TY_CON@32..38
                        TY_CON@32..38 "orange"
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
                CON_TY@0..29
                  TYVAR_TY@0..2
                    TYVAR@0..2 "'a"
                  WHITESPACE@2..3
                  CON_TY@3..7
                    LONG_TY_CON@3..7
                      TY_CON@3..7 "list"
                  WHITESPACE@7..8
                  CON_TY@8..12
                    LONG_TY_CON@8..12
                      TY_CON@8..12 "list"
                  WHITESPACE@12..13
                  CON_TY@13..17
                    LONG_TY_CON@13..17
                      TY_CON@13..17 "list"
                  WHITESPACE@17..18
                  TYVAR_TY@18..20
                    TYVAR@18..20 "'b"
                  WHITESPACE@20..21
                  CON_TY@21..25
                    LONG_TY_CON@21..25
                      TY_CON@21..25 "test"
                  WHITESPACE@25..26
                  LONG_TY_CON@26..29
                    TY_CON@26..29 "int"
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
                TUPLE_TY_EXP@0..39
                  TYVAR_TY@0..2
                    TYVAR@0..2 "'a"
                  WHITESPACE@2..3
                  STAR@3..4 "*"
                  WHITESPACE@4..5
                  CON_TY@5..8
                    LONG_TY_CON@5..8
                      TY_CON@5..8 "int"
                  WHITESPACE@8..9
                  STAR@9..10 "*"
                  WHITESPACE@10..11
                  CON_TY@11..15
                    LONG_TY_CON@11..15
                      TY_CON@11..15 "real"
                  WHITESPACE@15..16
                  STAR@16..17 "*"
                  WHITESPACE@17..18
                  TYVAR_TY@18..20
                    TYVAR@18..20 "'b"
                  WHITESPACE@20..21
                  STAR@21..22 "*"
                  WHITESPACE@22..23
                  CON_TY@23..30
                    TYVAR_TY@23..25
                      TYVAR@23..25 "'c"
                    WHITESPACE@25..26
                    LONG_TY_CON@26..30
                      TY_CON@26..30 "list"
                  WHITESPACE@30..31
                  STAR@31..32 "*"
                  WHITESPACE@32..33
                  CON_TY@33..39
                    LONG_TY_CON@33..39
                      TY_CON@33..39 "string"
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
                FUN_TY@0..24
                  CON_TY@0..3
                    LONG_TY_CON@0..3
                      TY_CON@0..3 "int"
                  WHITESPACE@3..4
                  THIN_ARROW@4..6 "->"
                  WHITESPACE@6..7
                  FUN_TY@7..24
                    CON_TY@7..11
                      LONG_TY_CON@7..11
                        TY_CON@7..11 "real"
                    WHITESPACE@11..12
                    THIN_ARROW@12..14 "->"
                    WHITESPACE@14..15
                    FUN_TY@15..24
                      CON_TY@15..18
                        LONG_TY_CON@15..18
                          TY_CON@15..18 "int"
                      WHITESPACE@18..19
                      THIN_ARROW@19..21 "->"
                      WHITESPACE@21..22
                      TYVAR_TY@22..24
                        TYVAR@22..24 "'a"
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
                FUN_TY@0..26
                  L_PAREN@0..1 "("
                  FUN_TY@1..12
                    CON_TY@1..4
                      LONG_TY_CON@1..4
                        TY_CON@1..4 "int"
                    WHITESPACE@4..5
                    THIN_ARROW@5..7 "->"
                    WHITESPACE@7..8
                    CON_TY@8..12
                      LONG_TY_CON@8..12
                        TY_CON@8..12 "real"
                  R_PAREN@12..13 ")"
                  WHITESPACE@13..14
                  THIN_ARROW@14..16 "->"
                  WHITESPACE@16..17
                  FUN_TY@17..26
                    CON_TY@17..20
                      LONG_TY_CON@17..20
                        TY_CON@17..20 "int"
                    WHITESPACE@20..21
                    THIN_ARROW@21..23 "->"
                    WHITESPACE@23..24
                    TYVAR_TY@24..26
                      TYVAR@24..26 "'a"
            "#]],
        )
    }

    #[test]
    fn funky_constructed_type() {
        check_with_f(
            false,
            super::ty,
            "'a int tree",
            expect![[r#"
                CON_TY@0..11
                  TYVAR_TY@0..2
                    TYVAR@0..2 "'a"
                  WHITESPACE@2..3
                  CON_TY@3..6
                    LONG_TY_CON@3..6
                      TY_CON@3..6 "int"
                  WHITESPACE@6..7
                  LONG_TY_CON@7..11
                    TY_CON@7..11 "tree"
            "#]],
        )
    }
}
