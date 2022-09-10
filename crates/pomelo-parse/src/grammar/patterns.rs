use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn pattern(p: &mut Parser) {
    match p.peek() {
        OP_KW | IDENT => layered_pat(p),
        _ => typed_pat(p),
    }
}

fn layered_pat(p: &mut Parser) {
    let mut lookahead = 0;
    // Optional <op>
    if p.peek() == OP_KW {
        lookahead += 1;
    }

    // Next we expect a <vid>
    let k = p.peek_next_nontrivia(lookahead);
    if k == IDENT {
        lookahead += 1;
    } else {
        typed_pat(p);
        return;
    }

    // Next, we expect "as" or ":".
    // If neither of these are present,
    // then this is not a layered pat.
    let k = p.peek_next_nontrivia(lookahead);
    if k == AS_KW || k == COLON {
        do_layered_pat(p);
    } else {
        typed_pat(p);
    }
}

fn do_layered_pat(p: &mut Parser) {
    let _ng = p.start_node(PAT);
    let _ng_inner = p.start_node(LAYERED_PAT);

    p.eat(OP_KW);
    p.eat_trivia();

    grammar::vid(p);
    p.eat_trivia();

    if p.eat(COLON) {
        p.eat_trivia();
        grammar::ty(p);
        p.eat_trivia();
    }

    p.expect(AS_KW);
    pattern(p);
}

fn typed_pat(p: &mut Parser) {
    grammar::precedence_climber_once(
        p,
        PAT,
        TY_PAT,
        infixed_pat,
        |p| p.eat_through_trivia(COLON),
        grammar::ty,
    )
}

fn infixed_pat(p: &mut Parser) {
    grammar::precedence_climber_once(
        p,
        PAT,
        INFIX_CONS_PAT,
        at_pat_or_constructed,
        |p| p.peek_next_nontrivia(0) == IDENT,
        |p| {
            p.eat_trivia();
            grammar::vid(p);
            p.eat_trivia();

            at_pat_or_constructed(p);
        },
    )
}

fn at_pat_or_constructed(p: &mut Parser) {
    let _ng = p.start_node(PAT);

    let outer = p.checkpoint();
    let inner = p.checkpoint();

    match p.peek() {
        OP_KW | IDENT => {
            p.eat(OP_KW);
            p.eat_trivia();

            grammar::longvid(p);

            // Parse <op><longvid> <atpat>
            if p.peek_next_nontrivia(0).is_atomic_pat_start() {
                let _ng_cons = p.start_node_at(outer, CONS_PAT);
                atomic_pattern(p);
            } else {
                // Oops, we've just parsed an <atpat>.
                // Correctly wrap it in AT_PAT + VID_PAT
                let _ng_at = p.start_node_at(outer, AT_PAT);
                let _ng_vid = p.start_node_at(inner, VID_PAT);
            }
        }
        _ => atomic_pattern(p),
    }
}

pub(crate) fn atomic_pattern(p: &mut Parser) {
    let _ng = p.start_node(AT_PAT);

    match p.peek() {
        UNDERSCORE => {
            let _ng = p.start_node(WILDCARD_PAT);
            p.expect(UNDERSCORE);
        }
        k if k.is_special_constant() => {
            let _ng = p.start_node(SCON_PAT);
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
        IDENT => {
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
    p.expect(IDENT);

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
        let _ng = p.start_node(UNIT_EXP);
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
    fn wildcard_pat() {
        check_with_f(
            false,
            super::pattern,
            "_",
            expect![[r#"
                PAT@0..1
                  AT_PAT@0..1
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
            "(0, 0xFF, 11.5, 0w10, 1.0E5, #\"c\", \"a string\")",
            expect![[r##"
                PAT@0..46
                  AT_PAT@0..46
                    TUPLE_PAT@0..46
                      L_PAREN@0..1 "("
                      PAT@1..2
                        AT_PAT@1..2
                          SCON_PAT@1..2
                            INT@1..2 "0"
                      COMMA@2..3 ","
                      WHITESPACE@3..4
                      PAT@4..8
                        AT_PAT@4..8
                          SCON_PAT@4..8
                            INT@4..8 "0xFF"
                      COMMA@8..9 ","
                      WHITESPACE@9..10
                      PAT@10..14
                        AT_PAT@10..14
                          SCON_PAT@10..14
                            REAL@10..14 "11.5"
                      COMMA@14..15 ","
                      WHITESPACE@15..16
                      PAT@16..20
                        AT_PAT@16..20
                          SCON_PAT@16..20
                            WORD@16..20 "0w10"
                      COMMA@20..21 ","
                      WHITESPACE@21..22
                      PAT@22..27
                        AT_PAT@22..27
                          SCON_PAT@22..27
                            REAL@22..27 "1.0E5"
                      COMMA@27..28 ","
                      WHITESPACE@28..29
                      PAT@29..33
                        AT_PAT@29..33
                          SCON_PAT@29..33
                            CHAR@29..33 "#\"c\""
                      COMMA@33..34 ","
                      WHITESPACE@34..35
                      PAT@35..45
                        AT_PAT@35..45
                          SCON_PAT@35..45
                            STRING@35..45 "\"a string\""
                      R_PAREN@45..46 ")"
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
                PAT@0..19
                  AT_PAT@0..19
                    VID_PAT@0..19
                      LONG_VID@0..19
                        IDENT@0..1 "A"
                        DOT@1..2 "."
                        IDENT@2..6 "Long"
                        DOT@6..7 "."
                        IDENT@7..17 "Identifier"
                        DOT@17..18 "."
                        IDENT@18..19 "x"
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
                PAT@0..22
                  AT_PAT@0..22
                    VID_PAT@0..22
                      OP_KW@0..2 "op"
                      WHITESPACE@2..3
                      LONG_VID@3..22
                        IDENT@3..4 "A"
                        DOT@4..5 "."
                        IDENT@5..9 "Long"
                        DOT@9..10 "."
                        IDENT@10..20 "Identifier"
                        DOT@20..21 "."
                        IDENT@21..22 "x"
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
                PAT@0..3
                  AT_PAT@0..3
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
                PAT@0..7
                  AT_PAT@0..7
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
                PAT@0..17
                  AT_PAT@0..17
                    RECORD_PAT@0..17
                      L_BRACE@0..1 "{"
                      WHITESPACE@1..2
                      PAT_ROW@2..15
                        PAT_ROW_PAT@2..8
                          IDENT@2..3 "x"
                          WHITESPACE@3..4
                          EQ@4..5 "="
                          WHITESPACE@5..6
                          PAT@6..8
                            AT_PAT@6..8
                              SCON_PAT@6..8
                                INT@6..8 "10"
                        COMMA@8..9 ","
                        WHITESPACE@9..10
                        PAT_ROW_PAT@10..15
                          IDENT@10..11 "y"
                          WHITESPACE@11..12
                          EQ@12..13 "="
                          WHITESPACE@13..14
                          PAT@14..15
                            AT_PAT@14..15
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
                PAT@0..52
                  AT_PAT@0..52
                    RECORD_PAT@0..52
                      L_BRACE@0..1 "{"
                      WHITESPACE@1..2
                      PAT_ROW@2..50
                        LAB_AS_VAR_PAT@2..32
                          LONG_VID@2..13
                            IDENT@2..4 "My"
                            DOT@4..5 "."
                            IDENT@5..11 "Module"
                            DOT@11..12 "."
                            IDENT@12..13 "x"
                          WHITESPACE@13..14
                          COLON@14..15 ":"
                          WHITESPACE@15..16
                          LAB_AS_VAR_TY@16..19
                            TY@16..18
                              TY_VAR@16..18 "'a"
                            WHITESPACE@18..19
                          AS_KW@19..21 "as"
                          WHITESPACE@21..22
                          LAB_AS_VAR_AS_PAT@22..32
                            PAT@22..32
                              AT_PAT@22..32
                                TUPLE_PAT@22..32
                                  L_PAREN@22..23 "("
                                  PAT@23..24
                                    AT_PAT@23..24
                                      VID_PAT@23..24
                                        LONG_VID@23..24
                                          IDENT@23..24 "a"
                                  COMMA@24..25 ","
                                  WHITESPACE@25..26
                                  PAT@26..31
                                    AT_PAT@26..31
                                      VID_PAT@26..31
                                        LONG_VID@26..31
                                          IDENT@26..31 "tuple"
                                  R_PAREN@31..32 ")"
                        COMMA@32..33 ","
                        WHITESPACE@33..34
                        LAB_AS_VAR_PAT@34..50
                          LONG_VID@34..50
                            IDENT@34..41 "Another"
                            DOT@41..42 "."
                            IDENT@42..48 "Module"
                            DOT@48..49 "."
                            IDENT@49..50 "y"
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
                PAT@0..46
                  AT_PAT@0..46
                    RECORD_PAT@0..46
                      L_BRACE@0..1 "{"
                      WHITESPACE@1..2
                      PAT_ROW@2..44
                        PAT_ROW_PAT@2..7
                          IDENT@2..3 "a"
                          WHITESPACE@3..4
                          EQ@4..5 "="
                          WHITESPACE@5..6
                          PAT@6..7
                            AT_PAT@6..7
                              SCON_PAT@6..7
                                INT@6..7 "1"
                        COMMA@7..8 ","
                        WHITESPACE@8..9
                        LAB_AS_VAR_PAT@9..39
                          LONG_VID@9..20
                            IDENT@9..11 "My"
                            DOT@11..12 "."
                            IDENT@12..18 "Module"
                            DOT@18..19 "."
                            IDENT@19..20 "x"
                          WHITESPACE@20..21
                          COLON@21..22 ":"
                          WHITESPACE@22..23
                          LAB_AS_VAR_TY@23..26
                            TY@23..25
                              TY_VAR@23..25 "'a"
                            WHITESPACE@25..26
                          AS_KW@26..28 "as"
                          WHITESPACE@28..29
                          LAB_AS_VAR_AS_PAT@29..39
                            PAT@29..39
                              AT_PAT@29..39
                                TUPLE_PAT@29..39
                                  L_PAREN@29..30 "("
                                  PAT@30..31
                                    AT_PAT@30..31
                                      VID_PAT@30..31
                                        LONG_VID@30..31
                                          IDENT@30..31 "a"
                                  COMMA@31..32 ","
                                  WHITESPACE@32..33
                                  PAT@33..38
                                    AT_PAT@33..38
                                      VID_PAT@33..38
                                        LONG_VID@33..38
                                          IDENT@33..38 "tuple"
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
                PAT@0..29
                  AT_PAT@0..29
                    RECORD_PAT@0..29
                      L_BRACE@0..1 "{"
                      WHITESPACE@1..2
                      PAT_ROW@2..27
                        LAB_AS_VAR_PAT@2..15
                          LONG_VID@2..5
                            IDENT@2..3 "M"
                            DOT@3..4 "."
                            IDENT@4..5 "x"
                          WHITESPACE@5..6
                          AS_KW@6..8 "as"
                          WHITESPACE@8..9
                          LAB_AS_VAR_AS_PAT@9..15
                            PAT@9..15
                              AT_PAT@9..15
                                TUPLE_PAT@9..15
                                  L_PAREN@9..10 "("
                                  PAT@10..11
                                    AT_PAT@10..11
                                      VID_PAT@10..11
                                        LONG_VID@10..11
                                          IDENT@10..11 "a"
                                  COMMA@11..12 ","
                                  WHITESPACE@12..13
                                  PAT@13..14
                                    AT_PAT@13..14
                                      VID_PAT@13..14
                                        LONG_VID@13..14
                                          IDENT@13..14 "b"
                                  R_PAREN@14..15 ")"
                        COMMA@15..16 ","
                        WHITESPACE@16..17
                        WILDCARD_PAT@17..20
                          ELLIPSIS@17..20 "..."
                        ERROR@20..20 ""
                        COMMA@20..21 ","
                        WHITESPACE@21..22
                        PAT_ROW_PAT@22..27
                          IDENT@22..23 "c"
                          WHITESPACE@23..24
                          EQ@24..25 "="
                          WHITESPACE@25..26
                          PAT@26..27
                            AT_PAT@26..27
                              VID_PAT@26..27
                                LONG_VID@26..27
                                  IDENT@26..27 "d"
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
                PAT@0..2
                  AT_PAT@0..2
                    UNIT_EXP@0..2
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
            "(x, y, 0, 15.0, 0xF, #\"a\", op My.Long.id)",
            expect![[r##"
                PAT@0..41
                  AT_PAT@0..41
                    TUPLE_PAT@0..41
                      L_PAREN@0..1 "("
                      PAT@1..2
                        AT_PAT@1..2
                          VID_PAT@1..2
                            LONG_VID@1..2
                              IDENT@1..2 "x"
                      COMMA@2..3 ","
                      WHITESPACE@3..4
                      PAT@4..5
                        AT_PAT@4..5
                          VID_PAT@4..5
                            LONG_VID@4..5
                              IDENT@4..5 "y"
                      COMMA@5..6 ","
                      WHITESPACE@6..7
                      PAT@7..8
                        AT_PAT@7..8
                          SCON_PAT@7..8
                            INT@7..8 "0"
                      COMMA@8..9 ","
                      WHITESPACE@9..10
                      PAT@10..14
                        AT_PAT@10..14
                          SCON_PAT@10..14
                            REAL@10..14 "15.0"
                      COMMA@14..15 ","
                      WHITESPACE@15..16
                      PAT@16..19
                        AT_PAT@16..19
                          SCON_PAT@16..19
                            INT@16..19 "0xF"
                      COMMA@19..20 ","
                      WHITESPACE@20..21
                      PAT@21..25
                        AT_PAT@21..25
                          SCON_PAT@21..25
                            CHAR@21..25 "#\"a\""
                      COMMA@25..26 ","
                      WHITESPACE@26..27
                      PAT@27..40
                        AT_PAT@27..40
                          VID_PAT@27..40
                            OP_KW@27..29 "op"
                            WHITESPACE@29..30
                            LONG_VID@30..40
                              IDENT@30..32 "My"
                              DOT@32..33 "."
                              IDENT@33..37 "Long"
                              DOT@37..38 "."
                              IDENT@38..40 "id"
                      R_PAREN@40..41 ")"
            "##]],
        )
    }

    #[test]
    fn list_pat() {
        check_with_f(
            false,
            super::pattern,
            "[x, y, 0, 15.0, 0xF, #\"a\", op My.Long.id]",
            expect![[r##"
                PAT@0..41
                  AT_PAT@0..41
                    LIST_PAT@0..41
                      L_BRACKET@0..1 "["
                      PAT@1..2
                        AT_PAT@1..2
                          VID_PAT@1..2
                            LONG_VID@1..2
                              IDENT@1..2 "x"
                      COMMA@2..3 ","
                      WHITESPACE@3..4
                      PAT@4..5
                        AT_PAT@4..5
                          VID_PAT@4..5
                            LONG_VID@4..5
                              IDENT@4..5 "y"
                      COMMA@5..6 ","
                      WHITESPACE@6..7
                      PAT@7..8
                        AT_PAT@7..8
                          SCON_PAT@7..8
                            INT@7..8 "0"
                      COMMA@8..9 ","
                      WHITESPACE@9..10
                      PAT@10..14
                        AT_PAT@10..14
                          SCON_PAT@10..14
                            REAL@10..14 "15.0"
                      COMMA@14..15 ","
                      WHITESPACE@15..16
                      PAT@16..19
                        AT_PAT@16..19
                          SCON_PAT@16..19
                            INT@16..19 "0xF"
                      COMMA@19..20 ","
                      WHITESPACE@20..21
                      PAT@21..25
                        AT_PAT@21..25
                          SCON_PAT@21..25
                            CHAR@21..25 "#\"a\""
                      COMMA@25..26 ","
                      WHITESPACE@26..27
                      PAT@27..40
                        AT_PAT@27..40
                          VID_PAT@27..40
                            OP_KW@27..29 "op"
                            WHITESPACE@29..30
                            LONG_VID@30..40
                              IDENT@30..32 "My"
                              DOT@32..33 "."
                              IDENT@33..37 "Long"
                              DOT@37..38 "."
                              IDENT@38..40 "id"
                      R_BRACKET@40..41 "]"
            "##]],
        )
    }

    #[test]
    fn paren_pat() {
        check_with_f(
            false,
            super::pattern,
            "[(x), (y, 0), ([15.0, 0xF, #\"a\", op My.Long.id])]",
            expect![[r##"
                PAT@0..49
                  AT_PAT@0..49
                    LIST_PAT@0..49
                      L_BRACKET@0..1 "["
                      PAT@1..4
                        AT_PAT@1..4
                          L_PAREN@1..2 "("
                          PAT@2..3
                            AT_PAT@2..3
                              VID_PAT@2..3
                                LONG_VID@2..3
                                  IDENT@2..3 "x"
                          R_PAREN@3..4 ")"
                      COMMA@4..5 ","
                      WHITESPACE@5..6
                      PAT@6..12
                        AT_PAT@6..12
                          TUPLE_PAT@6..12
                            L_PAREN@6..7 "("
                            PAT@7..8
                              AT_PAT@7..8
                                VID_PAT@7..8
                                  LONG_VID@7..8
                                    IDENT@7..8 "y"
                            COMMA@8..9 ","
                            WHITESPACE@9..10
                            PAT@10..11
                              AT_PAT@10..11
                                SCON_PAT@10..11
                                  INT@10..11 "0"
                            R_PAREN@11..12 ")"
                      COMMA@12..13 ","
                      WHITESPACE@13..14
                      PAT@14..48
                        AT_PAT@14..48
                          L_PAREN@14..15 "("
                          PAT@15..47
                            AT_PAT@15..47
                              LIST_PAT@15..47
                                L_BRACKET@15..16 "["
                                PAT@16..20
                                  AT_PAT@16..20
                                    SCON_PAT@16..20
                                      REAL@16..20 "15.0"
                                COMMA@20..21 ","
                                WHITESPACE@21..22
                                PAT@22..25
                                  AT_PAT@22..25
                                    SCON_PAT@22..25
                                      INT@22..25 "0xF"
                                COMMA@25..26 ","
                                WHITESPACE@26..27
                                PAT@27..31
                                  AT_PAT@27..31
                                    SCON_PAT@27..31
                                      CHAR@27..31 "#\"a\""
                                COMMA@31..32 ","
                                WHITESPACE@32..33
                                PAT@33..46
                                  AT_PAT@33..46
                                    VID_PAT@33..46
                                      OP_KW@33..35 "op"
                                      WHITESPACE@35..36
                                      LONG_VID@36..46
                                        IDENT@36..38 "My"
                                        DOT@38..39 "."
                                        IDENT@39..43 "Long"
                                        DOT@43..44 "."
                                        IDENT@44..46 "id"
                                R_BRACKET@46..47 "]"
                          R_PAREN@47..48 ")"
                      R_BRACKET@48..49 "]"
            "##]],
        )
    }
}
