use pomelo_parse::language::SML;
use pomelo_parse::{AstNode, Parser, SyntaxTree};

use expect_test::{expect, Expect};

use crate::arena::Idx;
use crate::pretty::HirPrettyPrint;
use crate::{lower::HirLower, Dec, Expr, Pat, Ty};

fn check<H, F>(src: &str, parse_with: F, expect: Expect)
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

    eprintln!("{}", tree);

    let node = H::AstType::cast(tree.syntax());

    let mut ctx = crate::lower::LoweringCtxt::default();
    let actual = H::lower_opt(&mut ctx, node).pretty(ctx.arenas());
    expect.assert_eq(&actual);
}

#[test]
fn lower_valdec() {
    let src = "val x = 3";
    check::<Dec, _>(src, |p| p.parse_dec(), expect![[r##"val x = 3"##]])
}

#[test]
fn lower_valdec_rec() {
    let src = "val rec x = fn () => if exp1 then (exp2; x()) else ()";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect!["val rec x = (fn {  } => (fn true => (exp2; x {  }) | false => {  }) exp1)"],
    )
}

#[test]
fn lower_datatype_no_data() {
    let src = "datatype direction = NORTH | SOUTH | EAST | WEST";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect![[r##"datatype direction = NORTH | SOUTH | EAST | WEST"##]],
    )
}

#[test]
fn lower_datatype_option() {
    let src = "datatype 'a option = NONE | SOME of 'a";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect![[r##"datatype 'a option = NONE | SOME of 'a"##]],
    )
}

#[test]
fn lower_datarep() {
    let src = "datatype mytype = datatype MyModule.myothertype";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect![[r##"datatype mytype = datatype MyModule.myothertype"##]],
    )
}

#[test]
fn lower_abstype() {
    let src = "abstype AbsSet = absset of int list with
    val empty = absset([])
end";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect!["abstype  AbsSet = absset of int list with val empty = absset nil end"],
    )
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
fn lower_app_expr() {
    let src = "a b";
    check::<Expr, _>(src, |p| p.parse_expr(), expect![[r##"a b"##]])
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
        expect![[r##"(fn { x=_temp0, .. } => _temp0)"##]],
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
        expect![[r##"(fn true => exp2 | false => false) exp1"##]],
    )
}

#[test]
fn lower_orelse_expr() {
    let src = "exp1 orelse exp2";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"(fn true => true | false => exp2) exp1"##]],
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
    check::<Expr, _>(src, |p| p.parse_expr(), expect!["let val rec _temp0 = (fn {  } => (fn true => (exp2; _temp0 {  }) | false => {  }) exp1) in _temp0 {  } end"])
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
    check::<Ty, _>(
        src,
        |p| p.parse_type(),
        expect![[r##"{ 1:ty1, 2:ty2, 3:ty3, 4:ty4 }"##]],
    )
}
