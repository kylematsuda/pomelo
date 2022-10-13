use pomelo_parse::language::SML;
use pomelo_parse::{AstNode, Parser, SyntaxTree};

use expect_test::{expect, Expect};

use crate::arena::Idx;
use crate::core::pretty::HirPrettyPrint;
use crate::core::{lower::HirLower, BodyArenaImpl, Expr};
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
fn lower_list_expr() {
    let src = "[1, 2, 3]";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"1 :: 2 :: 3 :: nil"##]],
    )
}
