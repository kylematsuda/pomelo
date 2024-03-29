//! Functions to parse matches.
use crate::grammar;
use crate::{Parser, SyntaxKind};
use SyntaxKind::*;

pub fn match_exp(p: &mut Parser) {
    let _ng = p.start_node(MATCH);
    grammar::sequential(p, mrule, PIPE);
}

fn mrule(p: &mut Parser) {
    let _ng = p.start_node(MRULE);

    grammar::pattern(p);
    p.eat_trivia();

    p.expect(THICK_ARROW);
    p.eat_trivia();

    grammar::expression(p);
}

#[cfg(test)]
mod tests {
    use crate::tests::check_with_f;
    use expect_test::expect;

    #[test]
    fn match_clause() {
        check_with_f(
            false,
            super::match_exp,
            "apat => (a, rule)
           | another_pat => [another, rule]
           | 0 => 154.45",
            expect![[r#"
                MATCH@0..86
                  MRULE@0..17
                    VID_PAT@0..4
                      LONG_VID@0..4
                        VID@0..4 "apat"
                    WHITESPACE@4..5
                    THICK_ARROW@5..7 "=>"
                    WHITESPACE@7..8
                    TUPLE_EXP@8..17
                      L_PAREN@8..9 "("
                      VID_EXP@9..10
                        LONG_VID@9..10
                          VID@9..10 "a"
                      COMMA@10..11 ","
                      WHITESPACE@11..12
                      VID_EXP@12..16
                        LONG_VID@12..16
                          VID@12..16 "rule"
                      R_PAREN@16..17 ")"
                  WHITESPACE@17..29
                  PIPE@29..30 "|"
                  WHITESPACE@30..31
                  MRULE@31..61
                    VID_PAT@31..42
                      LONG_VID@31..42
                        VID@31..42 "another_pat"
                    WHITESPACE@42..43
                    THICK_ARROW@43..45 "=>"
                    WHITESPACE@45..46
                    LIST_EXP@46..61
                      L_BRACKET@46..47 "["
                      VID_EXP@47..54
                        LONG_VID@47..54
                          VID@47..54 "another"
                      COMMA@54..55 ","
                      WHITESPACE@55..56
                      VID_EXP@56..60
                        LONG_VID@56..60
                          VID@56..60 "rule"
                      R_BRACKET@60..61 "]"
                  WHITESPACE@61..73
                  PIPE@73..74 "|"
                  WHITESPACE@74..75
                  MRULE@75..86
                    SCON_PAT@75..76
                      INT@75..76 "0"
                    WHITESPACE@76..77
                    THICK_ARROW@77..79 "=>"
                    WHITESPACE@79..80
                    SCON_EXP@80..86
                      REAL@80..86 "154.45"
            "#]],
        )
    }
}
