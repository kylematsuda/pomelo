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

/// This function does not eat trailing trivia
pub(crate) fn sequential_with<F, Pred>(p: &mut Parser, parse_function: F, predicate: Pred)
where
    F: Fn(&mut Parser),
    Pred: Fn(&mut Parser) -> bool,
{
    parse_function(p);
    while predicate(p) {
        p.eat_trivia();
        parse_function(p);
    }
}