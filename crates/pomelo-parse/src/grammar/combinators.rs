//! Utilities for combining parsers.
use crate::{Parser, SyntaxKind};

/// This function does not eat trailing trivia
pub fn sequential<F>(p: &mut Parser, parse_function: F, delimiter: SyntaxKind)
where
    F: Fn(&mut Parser),
{
    parse_function(p);
    while p.eat_through_trivia(delimiter) {
        p.eat_trivia();
        parse_function(p);
    }
}

/// outer_node_kind: e.g. EXP
/// inner_node_kind: e.g., ORELSE_EXP
pub fn descend_once(
    p: &mut Parser,
    inner_node_kind: SyntaxKind,
    before: impl Fn(&mut Parser),
    continue_if: impl Fn(&mut Parser) -> bool,
    after: impl Fn(&mut Parser),
) {
    let inner_checkpoint = p.checkpoint();

    before(p);

    if continue_if(p) {
        let _ng_inner = p.start_node_at(inner_checkpoint, inner_node_kind);

        p.eat_trivia();
        after(p)
    }
}

/// outer_node_kind: e.g. EXP
/// inner_node_kind: e.g., ORELSE_EXP
///
/// NOTE: This folds all the `after` nodes into the same node in a flat structure.
/// To make them right-associate, use `precedence_climber_once` recursively
/// (i.e, with `after` set to the enclosing function.)
pub fn descend_flat(
    p: &mut Parser,
    inner_node_kind: SyntaxKind,
    before: impl Fn(&mut Parser),
    continue_if: impl Fn(&mut Parser) -> bool,
    after: impl Fn(&mut Parser),
) {
    let inner_checkpoint = p.checkpoint();

    before(p);

    if continue_if(p) {
        let _ng_inner = p.start_node_at(inner_checkpoint, inner_node_kind);

        p.eat_trivia();
        after(p);

        while continue_if(p) {
            p.eat_trivia();
            after(p);
        }
    }
}

/// outer_node_kind: e.g. EXP
/// inner_node_kind: e.g., ORELSE_EXP
///
/// This folds expressions right associatively.
/// It should be called recursively by putting the
/// calling function in the `caller` arg.
pub fn descend_right(
    p: &mut Parser,
    inner_node_kind: SyntaxKind,
    before: impl Fn(&mut Parser),
    continue_if: impl Fn(&mut Parser) -> bool,
    caller: impl Fn(&mut Parser),
) {
    descend_flat(p, inner_node_kind, before, continue_if, caller)
}
