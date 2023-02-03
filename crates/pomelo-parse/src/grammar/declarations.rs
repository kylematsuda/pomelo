//! Functions to parse declarations.
use crate::grammar;
use crate::{Parser, SyntaxKind};
use SyntaxKind::*;

pub fn declaration(p: &mut Parser) {
    // let outer = p.checkpoint();
    let inner = p.checkpoint();
    declaration_inner(p);

    // If we parse another declaration, then we need to convert
    // this declaration to a SEQ_DEC
    let is_seq = |k: SyntaxKind| k.is_dec_kw() || k == SEMICOLON;
    if is_seq(p.peek_next_nontrivia(0)) {
        // let _ng = p.start_node_at(outer, DEC);
        let _nng = p.start_node_at(inner, SEQ_DEC);

        p.eat_trivia();
        p.eat(SEMICOLON);
        p.eat_trivia();
        declaration_inner(p);

        while is_seq(p.peek_next_nontrivia(0)) {
            p.eat_trivia();
            p.eat(SEMICOLON);
            p.eat_trivia();
            declaration_inner(p);
        }
    }
}

pub fn declaration_inner(p: &mut Parser) {
    // let _ng = p.start_node(DEC);

    match p.peek() {
        VAL_KW => val_declaration(p),
        FUN_KW => fun_declaration(p),
        TYPE_KW => type_declaration(p),
        DATATYPE_KW => datatype_declaration(p),
        ABSTYPE_KW => abstype_declaration(p),
        EXCEPTION_KW => exception_declaration(p),
        LOCAL_KW => local_declaration(p),
        OPEN_KW => open_declaration(p),
        // Sequential decs are handled in `declaration()`
        INFIX_KW | INFIXR_KW | NONFIX_KW => infix_or_nonfix(p),
        _ => {} // declarations can be empty...
    }
}

pub fn val_declaration(p: &mut Parser) {
    let _ng = p.start_node(VAL_DEC);
    assert_eq!(p.eat_any(), VAL_KW);
    p.eat_trivia();

    grammar::sequential(p, grammar::valbind, AND_KW);
}

pub fn fun_declaration(p: &mut Parser) {
    let _ng = p.start_node(FUN_DEC);
    assert_eq!(p.eat_any(), FUN_KW);
    p.eat_trivia();
    grammar::tyvarseq(p);
    p.eat_trivia();
    grammar::sequential(p, grammar::fvalbind, AND_KW);
}

pub fn type_declaration(p: &mut Parser) {
    let _ng = p.start_node(TY_DEC);
    assert_eq!(p.eat_any(), TYPE_KW);
    p.eat_trivia();
    grammar::sequential(p, grammar::typbind, AND_KW);
}

pub fn datatype_declaration(p: &mut Parser) {
    let checkpoint = p.checkpoint();
    assert_eq!(p.eat_any(), DATATYPE_KW);
    p.eat_trivia();

    // Replication
    if p.peek_next_nontrivia(2) == DATATYPE_KW {
        let _ng = p.start_node_at(checkpoint, DATATYPE_REP);

        grammar::tycon(p);
        p.eat_trivia();

        p.expect(EQ);
        p.eat_trivia();

        assert!(p.eat(DATATYPE_KW));
        p.eat_trivia();

        grammar::longtycon(p);
    } else {
        // Datatype dec
        let _ng = p.start_node_at(checkpoint, DATATYPE_DEC);
        grammar::sequential(p, grammar::databind, AND_KW);

        if p.eat_through_trivia(WITHTYPE_KW) {
            p.eat_trivia();
            grammar::typbind(p)
        }
    }
}

pub fn abstype_declaration(p: &mut Parser) {
    let _ng = p.start_node(ABSTYPE_DEC);

    assert!(p.eat(ABSTYPE_KW));
    p.eat_trivia();

    // Databind(s)
    grammar::sequential(p, grammar::databind, AND_KW);
    p.eat_trivia();

    // <withtype typbind>
    if p.eat(WITHTYPE_KW) {
        p.eat_trivia();
        grammar::typbind(p);
        p.eat_trivia();
    }

    p.expect(WITH_KW);
    p.eat_trivia();

    grammar::declaration(p);
    p.eat_trivia();

    p.expect(END_KW);
}

pub fn exception_declaration(p: &mut Parser) {
    let _ng = p.start_node(EXCEPT_DEC);

    assert!(p.eat(EXCEPTION_KW));
    p.eat_trivia();

    grammar::sequential(p, grammar::exbind, AND_KW)
}

fn local_declaration(p: &mut Parser) {
    let _ng = p.start_node(LOCAL_DEC);

    assert!(p.eat(LOCAL_KW));
    p.eat_trivia();

    declaration(p);
    p.eat_trivia();

    p.expect(IN_KW);
    p.eat_trivia();

    declaration(p);
    p.eat_trivia();

    p.expect(END_KW);
}

fn open_declaration(p: &mut Parser) {
    let _ng = p.start_node(OPEN_DEC);

    assert!(p.eat(OPEN_KW));
    p.eat_trivia();

    if p.peek() != IDENT {
        p.error("expected structure identifier");
    } else {
        while p.peek_next_nontrivia(0) == IDENT {
            p.eat_trivia();
            grammar::longstrid(p);
        }
    }
}

fn infix_or_nonfix(p: &mut Parser) {
    let checkpoint = p.checkpoint();

    let kind = if p.eat(INFIX_KW) {
        INFIX_DEC
    } else if p.eat(INFIXR_KW) {
        INFIXR_DEC
    } else if p.eat(NONFIX_KW) {
        NONFIX_DEC
    } else {
        unreachable!()
    };
    p.eat_trivia();

    let _ng = p.start_node_at(checkpoint, kind);

    if kind == INFIX_DEC || kind == INFIXR_DEC {
        // Optionally parse a fixity
        if p.peek() == INT {
            let _ng = p.start_node(FIXITY);
            p.expect(INT);
        }
    }
    p.eat_trivia();

    if !p.is_vid() {
        p.error("expected VId");
    }

    while p.next_nontrivia_is_vid() {
        p.eat_trivia();
        grammar::vid(p);
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::check_with_f;
    use expect_test::expect;

    #[test]
    fn declare_int() {
        check_with_f(
            false,
            super::declaration,
            "val a = 1",
            expect![[r#"
                  VAL_DEC@0..9
                    VAL_KW@0..3 "val"
                    WHITESPACE@3..4
                    VAL_BIND@4..9
                      VID_PAT@4..5
                        LONG_VID@4..5
                          VID@4..5 "a"
                      WHITESPACE@5..6
                      EQ@6..7 "="
                      WHITESPACE@7..8
                      SCON_EXP@8..9
                        INT@8..9 "1"
            "#]],
        )
    }

    #[test]
    fn declaration_seq() {
        check_with_f(
            false,
            super::declaration,
            "val a = 1;
            val b = 2.0 
            val c = #\"c\"
            val d = 0x15;
            val e = [\"some\", \"strings\"]",
            expect![[r##"
                  SEQ_DEC@0..126
                    VAL_DEC@0..9
                      VAL_KW@0..3 "val"
                      WHITESPACE@3..4
                      VAL_BIND@4..9
                        VID_PAT@4..5
                          LONG_VID@4..5
                            VID@4..5 "a"
                        WHITESPACE@5..6
                        EQ@6..7 "="
                        WHITESPACE@7..8
                        SCON_EXP@8..9
                          INT@8..9 "1"
                    SEMICOLON@9..10 ";"
                    WHITESPACE@10..23
                    VAL_DEC@23..34
                      VAL_KW@23..26 "val"
                      WHITESPACE@26..27
                      VAL_BIND@27..34
                        VID_PAT@27..28
                          LONG_VID@27..28
                            VID@27..28 "b"
                        WHITESPACE@28..29
                        EQ@29..30 "="
                        WHITESPACE@30..31
                        SCON_EXP@31..34
                          REAL@31..34 "2.0"
                    WHITESPACE@34..48
                    VAL_DEC@48..60
                      VAL_KW@48..51 "val"
                      WHITESPACE@51..52
                      VAL_BIND@52..60
                        VID_PAT@52..53
                          LONG_VID@52..53
                            VID@52..53 "c"
                        WHITESPACE@53..54
                        EQ@54..55 "="
                        WHITESPACE@55..56
                        SCON_EXP@56..60
                          CHAR@56..60 "#\"c\""
                    WHITESPACE@60..73
                    VAL_DEC@73..85
                      VAL_KW@73..76 "val"
                      WHITESPACE@76..77
                      VAL_BIND@77..85
                        VID_PAT@77..78
                          LONG_VID@77..78
                            VID@77..78 "d"
                        WHITESPACE@78..79
                        EQ@79..80 "="
                        WHITESPACE@80..81
                        SCON_EXP@81..85
                          INT@81..85 "0x15"
                    SEMICOLON@85..86 ";"
                    WHITESPACE@86..99
                    VAL_DEC@99..126
                      VAL_KW@99..102 "val"
                      WHITESPACE@102..103
                      VAL_BIND@103..126
                        VID_PAT@103..104
                          LONG_VID@103..104
                            VID@103..104 "e"
                        WHITESPACE@104..105
                        EQ@105..106 "="
                        WHITESPACE@106..107
                        LIST_EXP@107..126
                          L_BRACKET@107..108 "["
                          SCON_EXP@108..114
                            STRING@108..114 "\"some\""
                          COMMA@114..115 ","
                          WHITESPACE@115..116
                          SCON_EXP@116..125
                            STRING@116..125 "\"strings\""
                          R_BRACKET@125..126 "]"
            "##]],
        )
    }

    #[test]
    fn valbind_rec() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "val rec simple = fn x => x",
            expect![[r#"
                  VAL_DEC@0..26
                    VAL_KW@0..3 "val"
                    WHITESPACE@3..4
                    VAL_BIND@4..26
                      REC_KW@4..7 "rec"
                      WHITESPACE@7..8
                      VID_PAT@8..14
                        LONG_VID@8..14
                          VID@8..14 "simple"
                      WHITESPACE@14..15
                      EQ@15..16 "="
                      WHITESPACE@16..17
                      FN_EXP@17..26
                        FN_KW@17..19 "fn"
                        WHITESPACE@19..20
                        MATCH@20..26
                          MRULE@20..26
                            VID_PAT@20..21
                              LONG_VID@20..21
                                VID@20..21 "x"
                            WHITESPACE@21..22
                            THICK_ARROW@22..24 "=>"
                            WHITESPACE@24..25
                            VID_EXP@25..26
                              LONG_VID@25..26
                                VID@25..26 "x"
            "#]],
        )
    }

    #[test]
    fn desugared_while_val_rec() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "val rec x = fn () => if exp1 then (exp2; x()) else ()",
            expect![[r#"
                VAL_DEC@0..53
                  VAL_KW@0..3 "val"
                  WHITESPACE@3..4
                  VAL_BIND@4..53
                    REC_KW@4..7 "rec"
                    WHITESPACE@7..8
                    VID_PAT@8..9
                      LONG_VID@8..9
                        VID@8..9 "x"
                    WHITESPACE@9..10
                    EQ@10..11 "="
                    WHITESPACE@11..12
                    FN_EXP@12..53
                      FN_KW@12..14 "fn"
                      WHITESPACE@14..15
                      MATCH@15..53
                        MRULE@15..53
                          UNIT_PAT@15..17
                            L_PAREN@15..16 "("
                            R_PAREN@16..17 ")"
                          WHITESPACE@17..18
                          THICK_ARROW@18..20 "=>"
                          WHITESPACE@20..21
                          IF_EXP@21..53
                            IF_KW@21..23 "if"
                            WHITESPACE@23..24
                            VID_EXP@24..28
                              LONG_VID@24..28
                                VID@24..28 "exp1"
                            WHITESPACE@28..29
                            THEN_KW@29..33 "then"
                            WHITESPACE@33..34
                            SEQ_EXP@34..45
                              L_PAREN@34..35 "("
                              VID_EXP@35..39
                                LONG_VID@35..39
                                  VID@35..39 "exp2"
                              SEMICOLON@39..40 ";"
                              WHITESPACE@40..41
                              INFIX_OR_APP_EXP@41..44
                                VID_EXP@41..42
                                  LONG_VID@41..42
                                    VID@41..42 "x"
                                UNIT_EXP@42..44
                                  L_PAREN@42..43 "("
                                  R_PAREN@43..44 ")"
                              R_PAREN@44..45 ")"
                            WHITESPACE@45..46
                            ELSE_KW@46..50 "else"
                            WHITESPACE@50..51
                            UNIT_EXP@51..53
                              L_PAREN@51..52 "("
                              R_PAREN@52..53 ")"
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
                  VAL_DEC@0..17
                    VAL_KW@0..3 "val"
                    WHITESPACE@3..4
                    VAL_BIND@4..17
                      REC_KW@4..7 "rec"
                      WHITESPACE@7..8
                      VID_PAT@8..14
                        LONG_VID@8..14
                          VID@8..14 "simple"
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
                  FUN_DEC@0..98
                    FUN_KW@0..3 "fun"
                    WHITESPACE@3..4
                    FVAL_BIND@4..98
                      FVAL_BIND_ROW@4..17
                        VID@4..9 "myfun"
                        WHITESPACE@9..10
                        SCON_PAT@10..11
                          INT@10..11 "0"
                        WHITESPACE@11..12
                        SCON_PAT@12..13
                          INT@12..13 "0"
                        WHITESPACE@13..14
                        EQ@14..15 "="
                        WHITESPACE@15..16
                        SCON_EXP@16..17
                          INT@16..17 "0"
                      WHITESPACE@17..29
                      FVAL_BIND_ROW@29..44
                        PIPE@29..30 "|"
                        WHITESPACE@30..31
                        VID@31..36 "myfun"
                        WHITESPACE@36..37
                        SCON_PAT@37..38
                          INT@37..38 "1"
                        WHITESPACE@38..39
                        SCON_PAT@39..40
                          INT@39..40 "0"
                        WHITESPACE@40..41
                        EQ@41..42 "="
                        WHITESPACE@42..43
                        SCON_EXP@43..44
                          INT@43..44 "1"
                      WHITESPACE@44..56
                      FVAL_BIND_ROW@56..71
                        PIPE@56..57 "|"
                        WHITESPACE@57..58
                        VID@58..63 "myfun"
                        WHITESPACE@63..64
                        SCON_PAT@64..65
                          INT@64..65 "0"
                        WHITESPACE@65..66
                        WILDCARD_PAT@66..67
                          UNDERSCORE@66..67 "_"
                        WHITESPACE@67..68
                        EQ@68..69 "="
                        WHITESPACE@69..70
                        SCON_EXP@70..71
                          INT@70..71 "1"
                      WHITESPACE@71..83
                      FVAL_BIND_ROW@83..98
                        PIPE@83..84 "|"
                        WHITESPACE@84..85
                        VID@85..90 "myfun"
                        WHITESPACE@90..91
                        SCON_PAT@91..92
                          INT@91..92 "1"
                        WHITESPACE@92..93
                        VID_PAT@93..94
                          LONG_VID@93..94
                            VID@93..94 "x"
                        WHITESPACE@94..95
                        EQ@95..96 "="
                        WHITESPACE@96..97
                        VID_EXP@97..98
                          LONG_VID@97..98
                            VID@97..98 "x"
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
                  FUN_DEC@0..98
                    FUN_KW@0..3 "fun"
                    WHITESPACE@3..4
                    FVAL_BIND@4..98
                      FVAL_BIND_ROW@4..17
                        VID@4..9 "myfun"
                        WHITESPACE@9..10
                        SCON_PAT@10..11
                          INT@10..11 "0"
                        WHITESPACE@11..12
                        SCON_PAT@12..13
                          INT@12..13 "0"
                        WHITESPACE@13..14
                        EQ@14..15 "="
                        WHITESPACE@15..16
                        SCON_EXP@16..17
                          INT@16..17 "0"
                      WHITESPACE@17..29
                      FVAL_BIND_ROW@29..44
                        PIPE@29..30 "|"
                        WHITESPACE@30..31
                        VID@31..36 "myfun"
                        WHITESPACE@36..37
                        SCON_PAT@37..38
                          INT@37..38 "1"
                        WHITESPACE@38..39
                        SCON_PAT@39..40
                          INT@39..40 "0"
                        WHITESPACE@40..41
                        EQ@41..42 "="
                        WHITESPACE@42..43
                        SCON_EXP@43..44
                          INT@43..44 "1"
                      WHITESPACE@44..56
                      FVAL_BIND_ROW@56..71
                        PIPE@56..57 "|"
                        WHITESPACE@57..58
                        VID@58..63 "myfun"
                        WHITESPACE@63..64
                        SCON_PAT@64..65
                          INT@64..65 "0"
                        WHITESPACE@65..66
                        SCON_PAT@66..67
                          INT@66..67 "1"
                        WHITESPACE@67..68
                        EQ@68..69 "="
                        WHITESPACE@69..70
                        SCON_EXP@70..71
                          INT@70..71 "1"
                      WHITESPACE@71..83
                      FVAL_BIND_ROW@83..98
                        PIPE@83..84 "|"
                        WHITESPACE@84..85
                        VID@85..90 "myfun"
                        WHITESPACE@90..91
                        SCON_PAT@91..92
                          INT@91..92 "1"
                        WHITESPACE@92..93
                        SCON_PAT@93..94
                          INT@93..94 "1"
                        WHITESPACE@94..95
                        EQ@95..96 "="
                        WHITESPACE@96..97
                        SCON_EXP@97..98
                          INT@97..98 "0"
            "#]],
        )
    }

    #[test]
    fn tydec() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "type 'a 'b 'c mytype = 'd",
            expect![[r#"
                TY_DEC@0..25
                  TYPE_KW@0..4 "type"
                  WHITESPACE@4..5
                  TY_BIND@5..25
                    TYVAR@5..7 "'a"
                    WHITESPACE@7..8
                    TYVAR@8..10 "'b"
                    WHITESPACE@10..11
                    TYVAR@11..13 "'c"
                    WHITESPACE@13..14
                    TY_CON@14..20 "mytype"
                    WHITESPACE@20..21
                    EQ@21..22 "="
                    WHITESPACE@22..23
                    TYVAR_TY@23..25
                      TYVAR@23..25 "'d"
            "#]],
        )
    }

    #[test]
    fn infixes() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "infix 7 * / mod div 
            infix 6 + - ^
            infixr 5 :: @
            infix 4 = <> > >= < <=
            infix 3 := o 
            infix 0 before
            infix 2 myinfix 
            nonfix myinfix",
            expect![[r#"
                SEQ_DEC@0..216
                  INFIX_DEC@0..19
                    INFIX_KW@0..5 "infix"
                    WHITESPACE@5..6
                    FIXITY@6..7
                      INT@6..7 "7"
                    WHITESPACE@7..8
                    VID@8..9 "*"
                    WHITESPACE@9..10
                    VID@10..11 "/"
                    WHITESPACE@11..12
                    VID@12..15 "mod"
                    WHITESPACE@15..16
                    VID@16..19 "div"
                  WHITESPACE@19..33
                  INFIX_DEC@33..46
                    INFIX_KW@33..38 "infix"
                    WHITESPACE@38..39
                    FIXITY@39..40
                      INT@39..40 "6"
                    WHITESPACE@40..41
                    VID@41..42 "+"
                    WHITESPACE@42..43
                    VID@43..44 "-"
                    WHITESPACE@44..45
                    VID@45..46 "^"
                  WHITESPACE@46..59
                  INFIXR_DEC@59..72
                    INFIXR_KW@59..65 "infixr"
                    WHITESPACE@65..66
                    FIXITY@66..67
                      INT@66..67 "5"
                    WHITESPACE@67..68
                    VID@68..70 "::"
                    WHITESPACE@70..71
                    VID@71..72 "@"
                  WHITESPACE@72..85
                  INFIX_DEC@85..107
                    INFIX_KW@85..90 "infix"
                    WHITESPACE@90..91
                    FIXITY@91..92
                      INT@91..92 "4"
                    WHITESPACE@92..93
                    VID@93..94 "="
                    WHITESPACE@94..95
                    VID@95..97 "<>"
                    WHITESPACE@97..98
                    VID@98..99 ">"
                    WHITESPACE@99..100
                    VID@100..102 ">="
                    WHITESPACE@102..103
                    VID@103..104 "<"
                    WHITESPACE@104..105
                    VID@105..107 "<="
                  WHITESPACE@107..120
                  INFIX_DEC@120..132
                    INFIX_KW@120..125 "infix"
                    WHITESPACE@125..126
                    FIXITY@126..127
                      INT@126..127 "3"
                    WHITESPACE@127..128
                    VID@128..130 ":="
                    WHITESPACE@130..131
                    VID@131..132 "o"
                  WHITESPACE@132..146
                  INFIX_DEC@146..160
                    INFIX_KW@146..151 "infix"
                    WHITESPACE@151..152
                    FIXITY@152..153
                      INT@152..153 "0"
                    WHITESPACE@153..154
                    VID@154..160 "before"
                  WHITESPACE@160..173
                  INFIX_DEC@173..188
                    INFIX_KW@173..178 "infix"
                    WHITESPACE@178..179
                    FIXITY@179..180
                      INT@179..180 "2"
                    WHITESPACE@180..181
                    VID@181..188 "myinfix"
                  WHITESPACE@188..202
                  NONFIX_DEC@202..216
                    NONFIX_KW@202..208 "nonfix"
                    WHITESPACE@208..209
                    VID@209..216 "myinfix"
            "#]],
        )
    }

    #[test]
    fn datatype_dec() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "datatype 'a tree = Node | Leaf of 'a",
            expect![[r#"
                DATATYPE_DEC@0..36
                  DATATYPE_KW@0..8 "datatype"
                  WHITESPACE@8..9
                  DATA_BIND@9..36
                    TYVAR@9..11 "'a"
                    WHITESPACE@11..12
                    TY_CON@12..16 "tree"
                    WHITESPACE@16..17
                    EQ@17..18 "="
                    WHITESPACE@18..19
                    CON_BIND@19..23
                      VID@19..23 "Node"
                    WHITESPACE@23..24
                    PIPE@24..25 "|"
                    WHITESPACE@25..26
                    CON_BIND@26..36
                      VID@26..30 "Leaf"
                      WHITESPACE@30..31
                      OF_KW@31..33 "of"
                      WHITESPACE@33..34
                      TYVAR_TY@34..36
                        TYVAR@34..36 "'a"
            "#]],
        )
    }

    #[test]
    fn option() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "datatype 'a option = NONE | SOME of 'a",
            expect![[r#"
                DATATYPE_DEC@0..38
                  DATATYPE_KW@0..8 "datatype"
                  WHITESPACE@8..9
                  DATA_BIND@9..38
                    TYVAR@9..11 "'a"
                    WHITESPACE@11..12
                    TY_CON@12..18 "option"
                    WHITESPACE@18..19
                    EQ@19..20 "="
                    WHITESPACE@20..21
                    CON_BIND@21..25
                      VID@21..25 "NONE"
                    WHITESPACE@25..26
                    PIPE@26..27 "|"
                    WHITESPACE@27..28
                    CON_BIND@28..38
                      VID@28..32 "SOME"
                      WHITESPACE@32..33
                      OF_KW@33..35 "of"
                      WHITESPACE@35..36
                      TYVAR_TY@36..38
                        TYVAR@36..38 "'a"
            "#]],
        )
    }

    #[test]
    fn datarep() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "datatype mytype = datatype SomeStructure.myothertype",
            expect![[r#"
                DATATYPE_REP@0..52
                  DATATYPE_KW@0..8 "datatype"
                  WHITESPACE@8..9
                  TY_CON@9..15 "mytype"
                  WHITESPACE@15..16
                  EQ@16..17 "="
                  WHITESPACE@17..18
                  DATATYPE_KW@18..26 "datatype"
                  WHITESPACE@26..27
                  LONG_TY_CON@27..52
                    STRID@27..40 "SomeStructure"
                    DOT@40..41 "."
                    TY_CON@41..52 "myothertype"
            "#]],
        )
    }

    #[test]
    fn abstype() {
        check_with_f(
            false,
            crate::grammar::declaration,
            // Stack overflow: https://stackoverflow.com/questions/7296795/sml-whats-the-difference-between-using-abstype-and-using-a-signature-to-hide-t
            // simplified
            "abstype AbsSet = absset of int list with
    val empty = absset([])
end",
            expect![[r#"
                ABSTYPE_DEC@0..71
                  ABSTYPE_KW@0..7 "abstype"
                  WHITESPACE@7..8
                  DATA_BIND@8..35
                    TY_CON@8..14 "AbsSet"
                    WHITESPACE@14..15
                    EQ@15..16 "="
                    WHITESPACE@16..17
                    CON_BIND@17..35
                      VID@17..23 "absset"
                      WHITESPACE@23..24
                      OF_KW@24..26 "of"
                      WHITESPACE@26..27
                      CON_TY@27..35
                        CON_TY@27..30
                          LONG_TY_CON@27..30
                            TY_CON@27..30 "int"
                        WHITESPACE@30..31
                        LONG_TY_CON@31..35
                          TY_CON@31..35 "list"
                  WHITESPACE@35..36
                  WITH_KW@36..40 "with"
                  WHITESPACE@40..45
                  VAL_DEC@45..67
                    VAL_KW@45..48 "val"
                    WHITESPACE@48..49
                    VAL_BIND@49..67
                      VID_PAT@49..54
                        LONG_VID@49..54
                          VID@49..54 "empty"
                      WHITESPACE@54..55
                      EQ@55..56 "="
                      WHITESPACE@56..57
                      INFIX_OR_APP_EXP@57..67
                        VID_EXP@57..63
                          LONG_VID@57..63
                            VID@57..63 "absset"
                        PAREN_EXP@63..67
                          L_PAREN@63..64 "("
                          LIST_EXP@64..66
                            L_BRACKET@64..65 "["
                            R_BRACKET@65..66 "]"
                          R_PAREN@66..67 ")"
                  WHITESPACE@67..68
                  END_KW@68..71 "end"
            "#]],
        )
    }

    #[test]
    fn exception_dec() {
        check_with_f(
            false,
            crate::grammar::declaration,
            "exception MyException of string and MyOtherException = MyModule.MyOtherException",
            expect![[r#"
                EXCEPT_DEC@0..80
                  EXCEPTION_KW@0..9 "exception"
                  WHITESPACE@9..10
                  EX_BIND@10..31
                    VID@10..21 "MyException"
                    WHITESPACE@21..22
                    OF_KW@22..24 "of"
                    WHITESPACE@24..25
                    CON_TY@25..31
                      LONG_TY_CON@25..31
                        TY_CON@25..31 "string"
                  WHITESPACE@31..32
                  AND_KW@32..35 "and"
                  WHITESPACE@35..36
                  EX_BIND@36..80
                    VID@36..52 "MyOtherException"
                    WHITESPACE@52..53
                    EQ@53..54 "="
                    WHITESPACE@54..55
                    LONG_VID@55..80
                      STRID@55..63 "MyModule"
                      DOT@63..64 "."
                      VID@64..80 "MyOtherException"
            "#]],
        )
    }
}
