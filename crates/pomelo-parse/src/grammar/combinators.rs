use crate::{Parser, SyntaxKind};

pub(crate) fn sequential<F>(p: &mut Parser, parse_function: F, delimiter: SyntaxKind)
where
    F: Fn(&mut Parser),
{
    parse_function(p);
    p.eat_trivia();

    while p.eat(delimiter) {
        p.eat_trivia();
        parse_function(p);
        p.eat_trivia();
    }
}

pub(crate) fn sequential_with<F, Pred>(p: &mut Parser, parse_function: F, predicate: Pred)
where
    F: Fn(&mut Parser),
    Pred: Fn(&mut Parser) -> bool,
{
    parse_function(p);
    p.eat_trivia();

    while predicate(p) {
        p.eat_trivia();
        parse_function(p);
        p.eat_trivia();
    }
}
