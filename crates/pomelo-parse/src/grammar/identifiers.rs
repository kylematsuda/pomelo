//! Functions to parse identifiers.

use crate::grammar;
use crate::{Parser, SyntaxKind};
use SyntaxKind::*;

pub fn longvid(p: &mut Parser) {
    let _ng = p.start_node(LONG_VID);

    // A longvid is a sequence of strids separated by DOTs,
    // then a VID
    while p.is_strid() && p.peek_next_nontrivia(1) == DOT {
        strid(p);
        p.eat_trivia();
        assert!(p.eat(DOT));
        p.eat_trivia();
    }
    vid(p)
}

pub fn vid(p: &mut Parser) {
    if p.is_vid() {
        p.eat_mapped(VID);
    } else {
        p.error("expected identifier");
    }
}

pub fn longstrid(p: &mut Parser) {
    let _ng = p.start_node(LONG_STRID);
    grammar::sequential(p, strid, DOT);
}

// TODO: make sure this is a valid STRID?
// This should be handled in Parser::is_strid
pub fn strid(p: &mut Parser) {
    if p.is_strid() {
        p.eat_mapped(STRID);
    } else {
        p.error("expected structure identifier");
    }
}

pub fn label(p: &mut Parser) {
    match p.peek() {
        IDENT | INT => {
            if p.peek_text().starts_with('0') {
                p.error("record labels may not start with \'0\'");
            }
            p.eat_mapped(LAB);
        }
        _ => p.error("expected label"),
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::check_with_f;
    use expect_test::expect;

    #[test]
    fn vid_alpha() {
        // Since VID is now a token,
        // we need to parse it inside some node
        check_with_f(
            false,
            crate::grammar::declaration,
            "fun iamavid123456 x = 0",
            expect![[r#"
                FUN_DEC@0..23
                  FUN_KW@0..3 "fun"
                  WHITESPACE@3..4
                  FVAL_BIND@4..23
                    FVAL_BIND_ROW@4..23
                      VID@4..17 "iamavid123456"
                      WHITESPACE@17..18
                      VID_PAT@18..19
                        LONG_VID@18..19
                          VID@18..19 "x"
                      WHITESPACE@19..20
                      EQ@20..21 "="
                      WHITESPACE@21..22
                      SCON_EXP@22..23
                        INT@22..23 "0"
            "#]],
        )
    }

    #[test]
    fn longvid() {
        check_with_f(
            false,
            super::longvid,
            "A.Long.Vid.StrId.x",
            expect![[r#"
                LONG_VID@0..18
                  STRID@0..1 "A"
                  DOT@1..2 "."
                  STRID@2..6 "Long"
                  DOT@6..7 "."
                  STRID@7..10 "Vid"
                  DOT@10..11 "."
                  STRID@11..16 "StrId"
                  DOT@16..17 "."
                  VID@17..18 "x"
            "#]],
        )
    }
}
