use crate::{Parser, SyntaxKind};

/// This function does not eat trailing trivia
pub(crate) fn sequential<F>(p: &mut Parser, parse_function: F, delimiter: SyntaxKind)
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
pub(crate) fn precedence_climber(
    p: &mut Parser,
    outer_node_kind: SyntaxKind,
    inner_node_kind: SyntaxKind,
    before: impl Fn(&mut Parser),
    continue_if: impl Fn(&mut Parser) -> bool,
    after: impl Fn(&mut Parser),
) {
    let outer_checkpoint = p.checkpoint();
    let inner_checkpoint = p.checkpoint();

    before(p);

    if continue_if(p) {
        let _ng_outer = p.start_node_at(outer_checkpoint, outer_node_kind);
        let _ng_inner = p.start_node_at(inner_checkpoint, inner_node_kind);

        p.eat_trivia();
        after(p)
    }
}

