use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn pattern(p: &mut Parser) {
    layered_pat(p)
}

fn layered_pat(p: &mut Parser) {
    grammar::precedence_climber_once(
        p,
        //     PAT,
        LAYERED_PAT,
        typed_pat,
        |p| p.eat_through_trivia(AS_KW),
        pattern,
    )
}

fn typed_pat(p: &mut Parser) {
    grammar::precedence_climber_once(
        p,
        // PAT,
        TY_PAT,
        infixed_pat,
        |p| p.eat_through_trivia(COLON),
        grammar::ty,
    )
}

/// Similar to infix expressions,
/// we don't have enough information here
/// to decide which operators are infix.
fn infixed_pat(p: &mut Parser) {
    grammar::precedence_climber_flat(
        p,
        // PAT,
        UNRES_INFIX_APP_PAT,
        atomic_in_pat,
        |p| p.peek_next_nontrivia(0).is_atomic_pat_start(),
        |p| {
            p.eat_trivia();
            atomic_in_pat(p);
        },
    )
}

fn atomic_in_pat(p: &mut Parser) {
    // let _ng = p.start_node(PAT);
    atomic_pattern(p);
}

pub(crate) fn atomic_pattern(p: &mut Parser) {
    //    let _ng = p.start_node(AT_PAT);

    match p.peek() {
        UNDERSCORE => {
            let _ng = p.start_node(WILDCARD_PAT);
            p.expect(UNDERSCORE);
        }
        k if k.is_special_constant() => {
            let _ng = p.start_node(SCON_PAT);

            if p.peek() == REAL {
                p.error("real constants may not appear in patterns");
            }
            p.eat_any();
        }
        OP_KW | IDENT => {
            let _ng = p.start_node(VID_PAT);
            p.eat(OP_KW);
            p.eat_trivia();
            grammar::longvid(p);
        }
        L_BRACE => record_pat(p),
        L_PAREN => parenthesized_pat(p),
        L_BRACKET => list_pat(p),
        _ => p.error("expected atomic pattern"),
    }
}

pub(crate) fn record_pat(p: &mut Parser) {
    let _ng = p.start_node(RECORD_PAT);
    p.expect(L_BRACE);
    p.eat_trivia();

    // Could have an empty record
    if p.eat(R_BRACE) {
        return;
    }

    patrow(p);
    p.eat_trivia();

    p.expect(R_BRACE);
}

fn patrow(p: &mut Parser) {
    let _ng = p.start_node(PAT_ROW);
    patrow_inner(p);
}

fn patrow_inner(p: &mut Parser) {
    match p.peek() {
        ELLIPSIS => {
            wildcard_patrow(p);

            // Error recovery
            if p.peek_next_nontrivia(0) == COMMA {
                p.eat_trivia();
                p.error("no additional patterns are allowed after a wildcard pattern");
                p.eat(COMMA);
                p.eat_trivia();
                patrow_inner(p);
            }
        }
        IDENT | INT => {
            pattern_row_or_label_as_var(p);

            if p.eat_through_trivia(COMMA) {
                p.eat_trivia();
                patrow_inner(p);
            }
        }
        _ => p.error("expected record pattern"),
    }
}

fn wildcard_patrow(p: &mut Parser) {
    let _ng = p.start_node(WILDCARD_PAT);
    p.expect(ELLIPSIS);
}

fn pattern_row_or_label_as_var(p: &mut Parser) {
    assert_eq!(p.peek(), IDENT);
    if p.peek_next_nontrivia(1) == EQ {
        pattern_row(p)
    } else {
        label_as_variable(p)
    }
}

fn pattern_row(p: &mut Parser) {
    let _ng = p.start_node(PAT_ROW_PAT);

    grammar::label(p);
    p.eat_trivia();

    p.expect(EQ);
    p.eat_trivia();

    pattern(p);
}

fn label_as_variable(p: &mut Parser) {
    let _ng = p.start_node(LAB_AS_VAR_PAT);

    grammar::longvid(p);

    if p.eat_through_trivia(COLON) {
        p.eat_trivia();
        let _ng2 = p.start_node(LAB_AS_VAR_TY);
        grammar::ty(p);
    }

    if p.eat_through_trivia(AS_KW) {
        p.eat_trivia();
        let _ng2 = p.start_node(LAB_AS_VAR_AS_PAT);
        pattern(p);
    }
}

fn parenthesized_pat(p: &mut Parser) {
    // Unit expression can contain whitespace,
    // e.g., "val (    ) = ..." is acceptable
    // to some level.
    //
    // Although SML/NJ errors if there are too many
    // spaces. And it seems like newlines in the middle
    // should not be allowed?
    if p.peek_next_nontrivia(1) == R_PAREN {
        let _ng = p.start_node(UNIT_PAT);
        p.expect(L_PAREN);
        p.eat_trivia();
        p.expect(R_PAREN);
    } else {
        other_paren_pat(p)
    }
}

fn other_paren_pat(p: &mut Parser) {
    let checkpoint = p.checkpoint();

    p.expect(L_PAREN);
    p.eat_trivia();

    pattern(p);
    p.eat_trivia();

    match p.peek() {
        R_PAREN => {
            p.expect(R_PAREN);
            return;
        }
        COMMA => {
            p.expect(COMMA);
            p.eat_trivia();

            let _ng = p.start_node_at(checkpoint, TUPLE_PAT);
            // Do not move the following statements out of this scope
            grammar::sequential(p, pattern, COMMA);
            p.expect(R_PAREN);
        }
        _ => p.error(
            "expected closing ')', tuple (\", exp, ... )\"), or sequence (\"; expr; ... )\")",
        ),
    }
}

fn list_pat(p: &mut Parser) {
    let _ng = p.start_node(LIST_PAT);

    p.expect(L_BRACKET);
    p.eat_trivia();

    if p.eat(R_BRACKET) {
        return;
    }

    grammar::sequential(p, pattern, COMMA);
    p.expect(R_BRACKET);
}

#[cfg(test)]
mod tests {
    use crate::tests::check_with_f;
    use expect_test::expect;

    #[test]
    fn may_not_contain_real_constant() {
        check_with_f(
            true,
            super::pattern,
            "100.0",
            expect![[r#"
                SCON_PAT@0..5
                  ERROR@0..0 ""
                  REAL@0..5 "100.0"
            "#]],
        )
    }

    #[test]
    fn wildcard_pat() {
        check_with_f(
            false,
            super::pattern,
            "_",
            expect![[r#"
                WILDCARD_PAT@0..1
                  UNDERSCORE@0..1 "_"
            "#]],
        )
    }

    #[test]
    fn scon_tuple_pat() {
        check_with_f(
            false,
            super::pattern,
            "(0, 0xFF, 0w10, ~2, #\"c\", \"a string\")",
            expect![[r##"
                  TUPLE_PAT@0..37
                    L_PAREN@0..1 "("
                    SCON_PAT@1..2
                      INT@1..2 "0"
                    COMMA@2..3 ","
                    WHITESPACE@3..4
                    SCON_PAT@4..8
                      INT@4..8 "0xFF"
                    COMMA@8..9 ","
                    WHITESPACE@9..10
                    SCON_PAT@10..14
                      WORD@10..14 "0w10"
                    COMMA@14..15 ","
                    WHITESPACE@15..16
                    SCON_PAT@16..18
                      INT@16..18 "~2"
                    COMMA@18..19 ","
                    WHITESPACE@19..20
                    SCON_PAT@20..24
                      CHAR@20..24 "#\"c\""
                    COMMA@24..25 ","
                    WHITESPACE@25..26
                    SCON_PAT@26..36
                      STRING@26..36 "\"a string\""
                    R_PAREN@36..37 ")"
            "##]],
        )
    }

    #[test]
    fn vid_pat() {
        check_with_f(
            false,
            super::pattern,
            "A.Long.Identifier.x",
            expect![[r#"
                  VID_PAT@0..19
                    LONG_VID@0..19
                      STRID@0..1 "A"
                      DOT@1..2 "."
                      STRID@2..6 "Long"
                      DOT@6..7 "."
                      STRID@7..17 "Identifier"
                      DOT@17..18 "."
                      VID@18..19 "x"
            "#]],
        )
    }

    #[test]
    fn op_vid_pat() {
        check_with_f(
            false,
            super::pattern,
            "op A.Long.Identifier.x",
            expect![[r#"
                  VID_PAT@0..22
                    OP_KW@0..2 "op"
                    WHITESPACE@2..3
                    LONG_VID@3..22
                      STRID@3..4 "A"
                      DOT@4..5 "."
                      STRID@5..9 "Long"
                      DOT@9..10 "."
                      STRID@10..20 "Identifier"
                      DOT@20..21 "."
                      VID@21..22 "x"
            "#]],
        )
    }

    #[test]
    fn empty_record_pat() {
        check_with_f(
            false,
            super::pattern,
            "{ }",
            expect![[r#"
                RECORD_PAT@0..3
                  L_BRACE@0..1 "{"
                  WHITESPACE@1..2
                  R_BRACE@2..3 "}"
            "#]],
        )
    }

    #[test]
    fn wildcard_record_pat() {
        check_with_f(
            false,
            super::pattern,
            "{ ... }",
            expect![[r#"
                  RECORD_PAT@0..7
                    L_BRACE@0..1 "{"
                    WHITESPACE@1..2
                    PAT_ROW@2..5
                      WILDCARD_PAT@2..5
                        ELLIPSIS@2..5 "..."
                    WHITESPACE@5..6
                    R_BRACE@6..7 "}"
            "#]],
        )
    }

    #[test]
    fn pattern_row_pat() {
        check_with_f(
            false,
            super::pattern,
            "{ x = 10, y = _ }",
            expect![[r#"
                  RECORD_PAT@0..17
                    L_BRACE@0..1 "{"
                    WHITESPACE@1..2
                    PAT_ROW@2..15
                      PAT_ROW_PAT@2..8
                        LAB@2..3 "x"
                        WHITESPACE@3..4
                        EQ@4..5 "="
                        WHITESPACE@5..6
                        SCON_PAT@6..8
                          INT@6..8 "10"
                      COMMA@8..9 ","
                      WHITESPACE@9..10
                      PAT_ROW_PAT@10..15
                        LAB@10..11 "y"
                        WHITESPACE@11..12
                        EQ@12..13 "="
                        WHITESPACE@13..14
                        WILDCARD_PAT@14..15
                          UNDERSCORE@14..15 "_"
                    WHITESPACE@15..16
                    R_BRACE@16..17 "}"
            "#]],
        )
    }

    #[test]
    fn label_as_variable_pat() {
        check_with_f(
            false,
            super::pattern,
            "{ My.Module.x : 'a as (a, tuple), Another.Module.y }",
            expect![[r#"
                  RECORD_PAT@0..52
                    L_BRACE@0..1 "{"
                    WHITESPACE@1..2
                    PAT_ROW@2..50
                      LAB_AS_VAR_PAT@2..32
                        LONG_VID@2..13
                          STRID@2..4 "My"
                          DOT@4..5 "."
                          STRID@5..11 "Module"
                          DOT@11..12 "."
                          VID@12..13 "x"
                        WHITESPACE@13..14
                        COLON@14..15 ":"
                        WHITESPACE@15..16
                        LAB_AS_VAR_TY@16..18
                          TY_VAR_TY@16..18
                            TY_VAR@16..18 "'a"
                        WHITESPACE@18..19
                        AS_KW@19..21 "as"
                        WHITESPACE@21..22
                        LAB_AS_VAR_AS_PAT@22..32
                          TUPLE_PAT@22..32
                            L_PAREN@22..23 "("
                            VID_PAT@23..24
                              LONG_VID@23..24
                                VID@23..24 "a"
                            COMMA@24..25 ","
                            WHITESPACE@25..26
                            VID_PAT@26..31
                              LONG_VID@26..31
                                VID@26..31 "tuple"
                            R_PAREN@31..32 ")"
                      COMMA@32..33 ","
                      WHITESPACE@33..34
                      LAB_AS_VAR_PAT@34..50
                        LONG_VID@34..50
                          STRID@34..41 "Another"
                          DOT@41..42 "."
                          STRID@42..48 "Module"
                          DOT@48..49 "."
                          VID@49..50 "y"
                    WHITESPACE@50..51
                    R_BRACE@51..52 "}"
            "#]],
        )
    }

    #[test]
    fn mixed_record_pat() {
        check_with_f(
            false,
            super::pattern,
            "{ a = 1, My.Module.x : 'a as (a, tuple), ... }",
            expect![[r#"
                RECORD_PAT@0..46
                  L_BRACE@0..1 "{"
                  WHITESPACE@1..2
                  PAT_ROW@2..44
                    PAT_ROW_PAT@2..7
                      LAB@2..3 "a"
                      WHITESPACE@3..4
                      EQ@4..5 "="
                      WHITESPACE@5..6
                      SCON_PAT@6..7
                        INT@6..7 "1"
                    COMMA@7..8 ","
                    WHITESPACE@8..9
                    LAB_AS_VAR_PAT@9..39
                      LONG_VID@9..20
                        STRID@9..11 "My"
                        DOT@11..12 "."
                        STRID@12..18 "Module"
                        DOT@18..19 "."
                        VID@19..20 "x"
                      WHITESPACE@20..21
                      COLON@21..22 ":"
                      WHITESPACE@22..23
                      LAB_AS_VAR_TY@23..25
                        TY_VAR_TY@23..25
                          TY_VAR@23..25 "'a"
                      WHITESPACE@25..26
                      AS_KW@26..28 "as"
                      WHITESPACE@28..29
                      LAB_AS_VAR_AS_PAT@29..39
                        TUPLE_PAT@29..39
                          L_PAREN@29..30 "("
                          VID_PAT@30..31
                            LONG_VID@30..31
                              VID@30..31 "a"
                          COMMA@31..32 ","
                          WHITESPACE@32..33
                          VID_PAT@33..38
                            LONG_VID@33..38
                              VID@33..38 "tuple"
                          R_PAREN@38..39 ")"
                    COMMA@39..40 ","
                    WHITESPACE@40..41
                    WILDCARD_PAT@41..44
                      ELLIPSIS@41..44 "..."
                  WHITESPACE@44..45
                  R_BRACE@45..46 "}"
            "#]],
        )
    }

    #[test]
    fn record_pat_ellipsis_must_come_last() {
        check_with_f(
            true,
            super::pattern,
            "{ M.x as (a, b), ..., c = d }",
            expect![[r#"
                RECORD_PAT@0..29
                  L_BRACE@0..1 "{"
                  WHITESPACE@1..2
                  PAT_ROW@2..27
                    LAB_AS_VAR_PAT@2..15
                      LONG_VID@2..5
                        STRID@2..3 "M"
                        DOT@3..4 "."
                        VID@4..5 "x"
                      WHITESPACE@5..6
                      AS_KW@6..8 "as"
                      WHITESPACE@8..9
                      LAB_AS_VAR_AS_PAT@9..15
                        TUPLE_PAT@9..15
                          L_PAREN@9..10 "("
                          VID_PAT@10..11
                            LONG_VID@10..11
                              VID@10..11 "a"
                          COMMA@11..12 ","
                          WHITESPACE@12..13
                          VID_PAT@13..14
                            LONG_VID@13..14
                              VID@13..14 "b"
                          R_PAREN@14..15 ")"
                    COMMA@15..16 ","
                    WHITESPACE@16..17
                    WILDCARD_PAT@17..20
                      ELLIPSIS@17..20 "..."
                    ERROR@20..20 ""
                    COMMA@20..21 ","
                    WHITESPACE@21..22
                    PAT_ROW_PAT@22..27
                      LAB@22..23 "c"
                      WHITESPACE@23..24
                      EQ@24..25 "="
                      WHITESPACE@25..26
                      VID_PAT@26..27
                        LONG_VID@26..27
                          VID@26..27 "d"
                  WHITESPACE@27..28
                  R_BRACE@28..29 "}"
            "#]],
        )
    }

    #[test]
    fn unit_pat() {
        check_with_f(
            false,
            super::pattern,
            "()",
            expect![[r#"
                    UNIT_PAT@0..2
                      L_PAREN@0..1 "("
                      R_PAREN@1..2 ")"
            "#]],
        )
    }

    #[test]
    fn tuple_pat() {
        check_with_f(
            false,
            super::pattern,
            "(x, y, 0, 0xF, #\"a\", op My.Long.id)",
            expect![[r##"
                    TUPLE_PAT@0..35
                      L_PAREN@0..1 "("
                      VID_PAT@1..2
                        LONG_VID@1..2
                          VID@1..2 "x"
                      COMMA@2..3 ","
                      WHITESPACE@3..4
                      VID_PAT@4..5
                        LONG_VID@4..5
                          VID@4..5 "y"
                      COMMA@5..6 ","
                      WHITESPACE@6..7
                      SCON_PAT@7..8
                        INT@7..8 "0"
                      COMMA@8..9 ","
                      WHITESPACE@9..10
                      SCON_PAT@10..13
                        INT@10..13 "0xF"
                      COMMA@13..14 ","
                      WHITESPACE@14..15
                      SCON_PAT@15..19
                        CHAR@15..19 "#\"a\""
                      COMMA@19..20 ","
                      WHITESPACE@20..21
                      VID_PAT@21..34
                        OP_KW@21..23 "op"
                        WHITESPACE@23..24
                        LONG_VID@24..34
                          STRID@24..26 "My"
                          DOT@26..27 "."
                          STRID@27..31 "Long"
                          DOT@31..32 "."
                          VID@32..34 "id"
                      R_PAREN@34..35 ")"
            "##]],
        )
    }

    #[test]
    fn list_pat() {
        check_with_f(
            false,
            super::pattern,
            "[x, y, 0, 1500, 0xF, #\"a\", op My.Long.id]",
            expect![[r##"
                    LIST_PAT@0..41
                      L_BRACKET@0..1 "["
                      VID_PAT@1..2
                        LONG_VID@1..2
                          VID@1..2 "x"
                      COMMA@2..3 ","
                      WHITESPACE@3..4
                      VID_PAT@4..5
                        LONG_VID@4..5
                          VID@4..5 "y"
                      COMMA@5..6 ","
                      WHITESPACE@6..7
                      SCON_PAT@7..8
                        INT@7..8 "0"
                      COMMA@8..9 ","
                      WHITESPACE@9..10
                      SCON_PAT@10..14
                        INT@10..14 "1500"
                      COMMA@14..15 ","
                      WHITESPACE@15..16
                      SCON_PAT@16..19
                        INT@16..19 "0xF"
                      COMMA@19..20 ","
                      WHITESPACE@20..21
                      SCON_PAT@21..25
                        CHAR@21..25 "#\"a\""
                      COMMA@25..26 ","
                      WHITESPACE@26..27
                      VID_PAT@27..40
                        OP_KW@27..29 "op"
                        WHITESPACE@29..30
                        LONG_VID@30..40
                          STRID@30..32 "My"
                          DOT@32..33 "."
                          STRID@33..37 "Long"
                          DOT@37..38 "."
                          VID@38..40 "id"
                      R_BRACKET@40..41 "]"
            "##]],
        )
    }

    #[test]
    fn paren_pat() {
        check_with_f(
            false,
            super::pattern,
            "[(x), (y, 0), ([10, 0xF, #\"a\", op My.Long.id])]",
            expect![[r##"
                    LIST_PAT@0..47
                      L_BRACKET@0..1 "["
                      L_PAREN@1..2 "("
                      VID_PAT@2..3
                        LONG_VID@2..3
                          VID@2..3 "x"
                      R_PAREN@3..4 ")"
                      COMMA@4..5 ","
                      WHITESPACE@5..6
                      TUPLE_PAT@6..12
                        L_PAREN@6..7 "("
                        VID_PAT@7..8
                          LONG_VID@7..8
                            VID@7..8 "y"
                        COMMA@8..9 ","
                        WHITESPACE@9..10
                        SCON_PAT@10..11
                          INT@10..11 "0"
                        R_PAREN@11..12 ")"
                      COMMA@12..13 ","
                      WHITESPACE@13..14
                      L_PAREN@14..15 "("
                      LIST_PAT@15..45
                        L_BRACKET@15..16 "["
                        SCON_PAT@16..18
                          INT@16..18 "10"
                        COMMA@18..19 ","
                        WHITESPACE@19..20
                        SCON_PAT@20..23
                          INT@20..23 "0xF"
                        COMMA@23..24 ","
                        WHITESPACE@24..25
                        SCON_PAT@25..29
                          CHAR@25..29 "#\"a\""
                        COMMA@29..30 ","
                        WHITESPACE@30..31
                        VID_PAT@31..44
                          OP_KW@31..33 "op"
                          WHITESPACE@33..34
                          LONG_VID@34..44
                            STRID@34..36 "My"
                            DOT@36..37 "."
                            STRID@37..41 "Long"
                            DOT@41..42 "."
                            VID@42..44 "id"
                        R_BRACKET@44..45 "]"
                      R_PAREN@45..46 ")"
                      R_BRACKET@46..47 "]"
            "##]],
        )
    }

    #[test]
    fn layered_pat_simple() {
        check_with_f(
            false,
            super::pattern,
            "vid as pat",
            expect![[r#"
                  LAYERED_PAT@0..10
                    VID_PAT@0..3
                      LONG_VID@0..3
                        VID@0..3 "vid"
                    WHITESPACE@3..4
                    AS_KW@4..6 "as"
                    WHITESPACE@6..7
                    VID_PAT@7..10
                      LONG_VID@7..10
                        VID@7..10 "pat"
            "#]],
        )
    }

    #[test]
    fn layered_pat_complicated() {
        check_with_f(
            false,
            super::pattern,
            "op myid : 'a list as [my, complicated, \"pat\"]",
            expect![[r#"
                  LAYERED_PAT@0..45
                    TY_PAT@0..17
                      VID_PAT@0..7
                        OP_KW@0..2 "op"
                        WHITESPACE@2..3
                        LONG_VID@3..7
                          VID@3..7 "myid"
                      WHITESPACE@7..8
                      COLON@8..9 ":"
                      WHITESPACE@9..10
                      TY_CON_EXP@10..17
                        TY_VAR_TY@10..12
                          TY_VAR@10..12 "'a"
                        WHITESPACE@12..13
                        LONG_TY_CON@13..17
                          TY_CON@13..17 "list"
                    WHITESPACE@17..18
                    AS_KW@18..20 "as"
                    WHITESPACE@20..21
                    LIST_PAT@21..45
                      L_BRACKET@21..22 "["
                      VID_PAT@22..24
                        LONG_VID@22..24
                          VID@22..24 "my"
                      COMMA@24..25 ","
                      WHITESPACE@25..26
                      VID_PAT@26..37
                        LONG_VID@26..37
                          VID@26..37 "complicated"
                      COMMA@37..38 ","
                      WHITESPACE@38..39
                      SCON_PAT@39..44
                        STRING@39..44 "\"pat\""
                      R_BRACKET@44..45 "]"
            "#]],
        )
    }

    #[test]
    fn typed_pat() {
        check_with_f(
            false,
            super::pattern,
            "{ x = y } : myrecord",
            expect![[r#"
                TY_PAT@0..20
                  RECORD_PAT@0..9
                    L_BRACE@0..1 "{"
                    WHITESPACE@1..2
                    PAT_ROW@2..7
                      PAT_ROW_PAT@2..7
                        LAB@2..3 "x"
                        WHITESPACE@3..4
                        EQ@4..5 "="
                        WHITESPACE@5..6
                        VID_PAT@6..7
                          LONG_VID@6..7
                            VID@6..7 "y"
                    WHITESPACE@7..8
                    R_BRACE@8..9 "}"
                  WHITESPACE@9..10
                  COLON@10..11 ":"
                  WHITESPACE@11..12
                  LONG_TY_CON@12..20
                    TY_CON@12..20 "myrecord"
            "#]],
        )
    }

    #[test]
    fn infix_pat() {
        check_with_f(
            false,
            super::pattern,
            "pat + another :: last",
            expect![[r#"
                  UNRES_INFIX_APP_PAT@0..21
                    VID_PAT@0..3
                      LONG_VID@0..3
                        VID@0..3 "pat"
                    WHITESPACE@3..4
                    VID_PAT@4..5
                      LONG_VID@4..5
                        VID@4..5 "+"
                    WHITESPACE@5..6
                    VID_PAT@6..13
                      LONG_VID@6..13
                        VID@6..13 "another"
                    WHITESPACE@13..14
                    VID_PAT@14..16
                      LONG_VID@14..16
                        VID@14..16 "::"
                    WHITESPACE@16..17
                    VID_PAT@17..21
                      LONG_VID@17..21
                        VID@17..21 "last"
            "#]],
        )
    }
}
