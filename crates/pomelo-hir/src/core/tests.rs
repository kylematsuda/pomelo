use pomelo_parse::language::SML;
use pomelo_parse::{AstNode, Parser, SyntaxTree};

use expect_test::{expect, Expect};

use crate::arena::Idx;
use crate::core::pretty::HirPrettyPrint;
use crate::core::{lower::HirLower, BodyArenaImpl, Expr, Pat, Type};
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

    for e in tree.errors() {
        eprintln!("{:?}", e);
    }

    let node = H::AstType::cast(tree.syntax());

    let mut arena = BodyArenaImpl::<NameInternerImpl>::default();
    let actual = H::lower_opt(node, &mut arena).pretty(&arena);
    expect.assert_eq(&actual);
}

#[test]
fn lower_unit_pat() {
    let src = "()";
    check::<Pat, _>(src, |p| p.parse_pat(), expect![[r##"{  }"##]])
}

#[test]
fn lower_tuple_pat() {
    let src = "(a, b, c)";
    check::<Pat, _>(src, |p| p.parse_pat(), expect![[r##"{ 1=a, 2=b, 3=c }"##]])
}

#[test]
fn lower_list_pat() {
    let src = "[a, b, c]";
    check::<Pat, _>(src, |p| p.parse_pat(), expect![[r##"a :: b :: c :: nil"##]])
}

/// TODO: test pattrow lowering

#[test]
fn lower_unit_expr() {
    let src = "()";
    check::<Expr, _>(src, |p| p.parse_expr(), expect![[r##"{  }"##]])
}

#[test]
fn lower_tuple_expr() {
    let src = "(a, b, c)";
    check::<Expr, _>(src, |p| p.parse_expr(), expect![[r##"{ 1=a, 2=b, 3=c }"##]])
}

#[test]
fn lower_recsel_expr() {
    let src = "# x";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"fn { x=_temp0, ... } => _temp0"##]],
    )
}

#[test]
fn lower_case_expr() {
    let src = "case x of true => 1 | false => 0";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"(fn true => 1 | false => 0) x"##]],
    )
}

#[test]
fn lower_if_expr() {
    let src = "if exp1 then exp2 else exp3";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"(fn true => exp2 | false => exp3) exp1"##]],
    )
}

#[test]
fn lower_andalso_expr() {
    let src = "exp1 andalso exp2";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"(fn true => true | false => exp2) exp1"##]],
    )
}

#[test]
fn lower_orelse_expr() {
    let src = "exp1 orelse exp2";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"(fn true => exp2 | false => false) exp1"##]],
    )
}

#[test]
fn lower_seq_expr() {
    let src = "(exp1 ; exp2 ; exp3 ; exp)";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"(exp1; exp2; exp3; exp)"##]],
    )
}

#[test]
fn lower_while_expr() {
    let src = "while exp1 do exp2";
    check::<Expr, _>(src, |p| p.parse_expr(), expect![[r##"TODO"##]])
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

#[test]
fn lower_product_ty() {
    let src = "ty1 * ty2 * ty3 * ty4";
    check::<Type, _>(
        src,
        |p| p.parse_type(),
        expect![[r##"{ 1:ty1, 2:ty2, 3:ty3, 4:ty4 }"##]],
    )
}
