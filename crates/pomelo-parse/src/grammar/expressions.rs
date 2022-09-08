use crate::{Parser, Marker, CompletedMarker};
use pomelo_syntax::SyntaxKind;

use SyntaxKind::*;

pub(crate) fn atexp(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();  

    match p.peek() {
        INT | REAL | WORD | CHAR | STRING => {
            return Some(scon(p, m))
        }
        OP_KW | IDENT => {
            return longvid(p, m)
        }
        L_BRACE => {
            return record_exp(p, m)
        }
        HASH => {
            return record_selector(p, m)
        }
        L_PAREN => {
            return parentheses(p, m)
        }
        L_BRACKET => {
            return list(p, m)
        }
        LET_KW => {
            return local_declaration(p, m)
        }
        _ => ()
    }
    None
}

pub(crate) fn scon(p: &mut Parser, m: Marker) -> CompletedMarker {
    let mm = p.start();
    p.eat_any();
    p.complete(mm, SCON_EXP);
    p.complete(m, AT_EXP)
}

pub(crate) fn longvid(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
    todo!()
}

pub(crate) fn record_exp(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
    todo!()
}

pub(crate) fn record_selector(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
    todo!()
}

pub(crate) fn parentheses(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
    todo!()
}

pub(crate) fn list(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
    todo!()
}

pub(crate) fn local_declaration(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
    todo!()
}

