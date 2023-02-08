//! Functions to parse the SML Core language.
pub(crate) mod bindings;
pub(crate) use bindings::*;

pub(crate) mod declarations;
pub(crate) use declarations::*;

pub(crate) mod expressions;
pub(crate) use expressions::*;

pub(crate) mod matches;
pub(crate) use matches::*;

pub(crate) mod patterns;
pub(crate) use patterns::*;

pub(crate) mod type_expressions;
pub(crate) use type_expressions::*;

pub(crate) mod identifiers;
pub(crate) use identifiers::*;

pub(crate) mod combinators;
pub(crate) use combinators::*;

use crate::{Parser, SyntaxKind};
use SyntaxKind::*;

/// Parse an entire source file.
pub(crate) fn source_file(p: &mut Parser) {
    let _ng = p.start_node(FILE);

    while !p.is_eof() {
        match p.peek_next_nontrivia(0) {
            k if k.is_dec_kw() => {
                p.eat_trivia();
                declaration_inner(p);
                p.eat_trivia();
            }
            SEMICOLON => {
                p.eat_trivia();
                assert!(p.eat(SEMICOLON));
                p.eat_trivia();
            }
            EOF => {
                p.eat_trivia();
                return;
            }
            _ => {
                // The program is a sequence of declarations.
                // If we find something else, it's probably because we errored.
                // Best effort for now, we will discard tokens until we get to a keyword.
                while !p.is_eof() {
                    // Now try to recover based on what the keyword is.
                    match p.peek() {
                        // We can parse a declaration right away, so just break.
                        k if k.is_dec_kw() => {
                            break;
                        }
                        // These are the keywords that don't start a declaration, but do tell us
                        // what kind of term is coming next.
                        //
                        // We need to eat the keyword though.
                        WITH_KW | END_KW => {
                            p.eat_any();
                            break;
                        }
                        // These start expressions, so we need to not eat the kw.
                        LET_KW | WHILE_KW | CASE_KW | IF_KW | RAISE_KW | FN_KW => {
                            expression(p);
                            break;
                        }
                        // These happen in the middle of expressions, so we need to eat the kw.
                        ANDALSO_KW | ORELSE_KW | THEN_KW | ELSE_KW => {
                            p.eat_any();
                            expression(p);
                            break;
                        }
                        HANDLE_KW => {
                            p.eat_any();
                            match_exp(p);
                            break;
                        }
                        // Don't know where we are, so we need to eat tokens to get out of this
                        // mess
                        _ => {
                            p.eat_any();
                        }
                    }
                }
            }
        }
    }
}
