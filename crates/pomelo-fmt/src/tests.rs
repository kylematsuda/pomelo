use expect_test::Expect;
use pomelo_parse::{ast, AstNode, Parser};

use crate::printer::Printer;

pub fn check_dec(src: &str, width: usize, expect: Expect) {
    let parser = Parser::new(src);
    let tree = parser.parse_dec();
    assert!(!tree.has_errors());

    let mut printer = Printer::new(width);
    printer.dec(&ast::Dec::cast(tree.syntax()).unwrap());
    let actual = printer.output();

    expect.assert_eq(&actual);
}

pub fn check_expr(src: &str, width: usize, expect: Expect) {
    let parser = Parser::new(src);
    let tree = parser.parse_expr();
    assert!(!tree.has_errors());

    let mut printer = Printer::new(width);
    printer.expr(&ast::Expr::cast(tree.syntax()).unwrap());
    let actual = printer.output();

    expect.assert_eq(&actual);
}
