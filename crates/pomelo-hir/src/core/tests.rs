use pomelo_parse::language::SML;
use pomelo_parse::{AstNode, Parser, SyntaxTree};

use expect_test::{expect, Expect};

use crate::arena::Idx;
use crate::core::pretty::HirPrettyPrint;
use crate::core::{lower::HirLower, BodyArenaImpl, Expr, Pat};
use crate::identifiers::NameInternerImpl;

pub(crate) fn check<H, F>(src: &str, parse_with: F, expect: Expect)
where
    H: HirLower + HirPrettyPrint,
    <H as HirLower>::AstType: AstNode<Language = SML>,
    Idx<H>: HirPrettyPrint,
    F: Fn(Parser) -> SyntaxTree,
{
    let parser = Parser::new(src);
    let tree = parse_with(parser);

    let node = H::AstType::cast(tree.syntax());

    let mut arena = BodyArenaImpl::<NameInternerImpl>::default();
    let actual = H::lower_opt(node, &mut arena).pretty(&arena);
    expect.assert_eq(&actual);
}

#[test]
fn lower_tuple_pat() {
    let src = "(a, b, c)";
    check::<Pat, _>(
        src,
        |p| p.parse_pat(),
        expect![[r##"{ 1=a, 2=b, 3=c }"##]],
    )
}

#[test]
fn lower_list_pat() {
    let src = "[a, b, c]";
    check::<Pat, _>(
        src,
        |p| p.parse_pat(),
        expect![[r##"a :: b :: c :: nil"##]],
    )
}

fn lower_tuple_expr() {
    let src = "(a, b, c)";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"{ 1=a, 2=b, 3=c }"##]],
    )
}

#[test]
fn lower_list_expr() {
    let src = "[1, 2, 3]";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"1 :: 2 :: 3 :: nil"##]],
    )
}
