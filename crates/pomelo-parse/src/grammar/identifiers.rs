use crate::grammar;
use crate::{Parser, SyntaxKind};

use SyntaxKind::*;

pub(crate) fn longvid(p: &mut Parser) {
    let _ng = p.start_node(LONG_VID);
    grammar::sequential(p, ident, DOT);
}

fn ident(p: &mut Parser) {
    p.expect(IDENT);
}

pub(crate) fn vid(p: &mut Parser) {
    if p.is_vid() {
        p.eat_mapped(VID);
    } else {
        p.error("expected identifier");
    }
}

pub(crate) fn longstrid(p: &mut Parser) {
    let _ng = p.start_node(LONG_STR_ID);
    grammar::sequential(p, strid, DOT);
}

// TODO: make sure this is a valid STRID?
pub(crate) fn strid(p: &mut Parser) {
    p.expect(IDENT);
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
                DEC@0..23
                  FUN_DEC@0..23
                    FUN_KW@0..3 "fun"
                    WHITESPACE@3..4
                    TY_VAR_SEQ@4..4
                    FVAL_BIND@4..23
                      FVAL_BIND_ROW@4..23
                        VID@4..17 "iamavid123456"
                        WHITESPACE@17..18
                        AT_PAT@18..19
                          VID_PAT@18..19
                            LONG_VID@18..19
                              IDENT@18..19 "x"
                        WHITESPACE@19..20
                        EQ@20..21 "="
                        WHITESPACE@21..22
                        EXP@22..23
                          AT_EXP@22..23
                            SCON_EXP@22..23
                              INT@22..23 "0"
            "#]]
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
                  IDENT@0..1 "A"
                  DOT@1..2 "."
                  IDENT@2..6 "Long"
                  DOT@6..7 "."
                  IDENT@7..10 "Vid"
                  DOT@10..11 "."
                  IDENT@11..16 "StrId"
                  DOT@16..17 "."
                  IDENT@17..18 "x"
            "#]]
        )
    }
}
