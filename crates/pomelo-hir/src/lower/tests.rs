use pomelo_parse::language::SML;
use pomelo_parse::{AstNode, Parser, SyntaxTree};

use expect_test::{expect, Expect};

use crate::arena::Idx;
use crate::pretty::HirPrettyPrint;
use crate::{lower::HirLower, Dec, DefLoc, Expr, FileArena, Pat, Ty};

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

    let mut ctx = crate::lower::LoweringCtxt::new();
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
fn lower_fun_dec() {
    let src = r#"
        fun myfun 0 0 = 0
          | myfun _ _ = 1
    "#;
    // TODO: format this better...?
    check::<Dec, _>(src, |p| p.parse_dec(), expect!["val rec myfun = (fn _temp0 => (fn _temp1 => (fn { 1=0, 2=0 } => 0 | { 1=_, 2=_ } => 1) { 1=_temp0, 2=_temp1 }))"])
}

#[test]
fn lower_fun_dec_1_arg() {
    let src = r#"
        fun myfun 0 = 1
          | myfun x = x
    "#;
    // TODO: format this better...?
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect!["val rec myfun = (fn _temp0 => (fn 0 => 1 | x => x) { 1=_temp0 })"],
    )
}

#[test]
fn lower_fun_dec_3_arg() {
    let src = r#"
        fun myfun 0 _ _ = 0
          | myfun _ 1 _ = 1
          | myfun _ _ 2 = 2
    "#;
    // TODO: format this better...?
    check::<Dec, _>(src, |p| p.parse_dec(), expect!["val rec myfun = (fn _temp0 => (fn _temp1 => (fn _temp2 => (fn { 1=0, 2=_, 3=_ } => 0 | { 1=_, 2=1, 3=_ } => 1 | { 1=_, 2=_, 3=2 } => 2) { 1=_temp0, 2=_temp1, 3=_temp2 })))"])
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
        expect!["abstype AbsSet = absset of int list with val empty = absset nil end"],
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
    check::<Pat, _>(
        src,
        |p| p.parse_pat(),
        expect![[r##"(a :: (b :: (c :: nil)))"##]],
    )
}

#[test]
fn lower_infix_pat() {
    let src = "a :: b";
    check::<Pat, _>(src, |p| p.parse_pat(), expect![[r##"(a :: b)"##]])
}

#[test]
fn lower_layered_pat() {
    let src = "x as a :: b";
    check::<Pat, _>(src, |p| p.parse_pat(), expect![[r##"x as (a :: b)"##]])
}

#[test]
fn lower_constructed_pat() {
    let src = "datatype 'a test = Yes of 'a | No ; val Yes x = y ; val a :: Yes b = c";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect![[r##"datatype 'a test = Yes of 'a | No; val Yes x = y; val (a :: Yes b) = c"##]],
    )
}

/// TODO: test pattrow lowering

#[test]
fn lower_unit_expr() {
    let src = "()";
    check::<Expr, _>(src, |p| p.parse_expr(), expect![[r##"{  }"##]])
}

#[test]
fn lower_basic_app_expr() {
    let src = "a b";
    check::<Expr, _>(src, |p| p.parse_expr(), expect![[r##"a b"##]])
}

#[test]
fn lower_many_app_expr() {
    let src = "a b c d e f g";
    check::<Expr, _>(src, |p| p.parse_expr(), expect![[r##"a b c d e f g"##]])
}

#[test]
fn lower_more_complicated_app_expr() {
    let src = "1 + f 2 * 3 + 4";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"((1 + (f 2 * 3)) + 4)"##]],
    )
}

#[test]
fn lower_infix_dec() {
    let src = "val a = 1 + f 2 * 3 + 4 and b = 3 div 4 + 5";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect![[r##"val a = ((1 + (f 2 * 3)) + 4) and b = ((3 div 4) + 5)"##]],
    )
}

#[test]
fn lower_infix_with_op() {
    let src = "op +(1, 2) + 3";
    check::<Expr, _>(
        src,
        |p| p.parse_expr(),
        expect![[r##"(op+ { 1=1, 2=2 } + 3)"##]],
    )
}

#[test]
fn lower_user_defined_fixity() {
    let src = "infix 9 f; val a = b f c + 1";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect![[r##"infix 9 f; val a = ((b f c) + 1)"##]],
    )
}

#[test]
fn lower_user_defined_weak_fixity() {
    let src = "infix 1 f; val a = b f c + 1";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect![[r##"infix 1 f; val a = (b f (c + 1))"##]],
    )
}

#[test]
fn lower_left_infix() {
    let src = "infix 9 f; val a = b f c f d + 1";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect![[r##"infix 9 f; val a = (((b f c) f d) + 1)"##]],
    )
}

#[test]
fn lower_right_infix() {
    let src = "infixr 9 f; val a = b f c f d + 1";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect![[r##"infixr 9 f; val a = ((b f (c f d)) + 1)"##]],
    )
}

#[test]
fn lower_nonfix() {
    let src = "infix 9 f; nonfix f; val a = b f c f d";
    check::<Dec, _>(
        src,
        |p| p.parse_dec(),
        expect![[r##"infix 9 f; nonfix f; val a = b f c f d"##]],
    )
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
        expect![[r##"(1 :: (2 :: (3 :: nil)))"##]],
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

#[test]
fn vid_references() {
    let src = r#"
        val a = 1;
        val b = a;
        val a = b;
        val rec c = fn x => c x;
    "#;

    let ast = Parser::new(src).parse();
    let (hir, errs) = crate::lower_ast_to_hir(ast);
    assert!(errs.is_empty());
    let topdecs = hir.topdecs();

    // "val b = a"
    let index = topdecs[1];
    // Get valbind "b = a"
    let (_, valbind) = hir.get_dec(index).val().unwrap();
    // Get vid expr "a"
    let (_, (_, loc)) = hir.get_expr(valbind[0].expr).vid().unwrap();
    assert!(*loc == DefLoc::Dec(topdecs[0]));

    // "val a = b"
    let index = topdecs[2];
    // Get valbind "a = b"
    let (_, valbind) = hir.get_dec(index).val().unwrap();
    // Get vid expr "b"
    let (_, (_, loc)) = hir.get_expr(valbind[0].expr).vid().unwrap();
    assert!(*loc == DefLoc::Dec(topdecs[1]));

    let index = topdecs[3];
    let (_, valbind) = hir.get_dec(index).val().unwrap();
    let fn_expr = hir.get_expr(valbind[0].expr).fn_expr().unwrap();
    let (match_expr, _) = hir.get_expr(fn_expr[0].expr).application().unwrap();
    let (_, (_, loc)) = hir.get_expr(match_expr).vid().unwrap();
    assert!(*loc == DefLoc::Dec(topdecs[3]));
}

#[test]
fn shadowed_references() {
    let src = r#"
        val a = 1;
        val b = let val a = a in a end;
        val c = a;
    "#;

    let ast = Parser::new(src).parse();
    let (hir, errs) = crate::lower_ast_to_hir(ast);

    for e in errs.iter() {
        println!("{e}");
    }

    assert!(errs.is_empty());
    let topdecs = hir.topdecs();

    let index = topdecs[1];
    let (_, valbind) = hir.get_dec(index).val().unwrap();
    let (dec, expr) = hir.get_expr(valbind[0].expr).let_expr().unwrap();
    let (_, (_, loc)) = hir.get_expr(expr).vid().unwrap();
    assert!(*loc == DefLoc::Dec(dec));

    let index = topdecs[2];
    let (_, valbind) = hir.get_dec(index).val().unwrap();
    let (_, (_, loc)) = hir.get_expr(valbind[0].expr).vid().unwrap();
    assert!(*loc == DefLoc::Dec(topdecs[0]));
}

#[test]
fn data_constructor_refs() {
    let src = r#"
        datatype 'a option = None | Some of 'a;
        val zero = fn None => None 
                    | Some _ => Some 0;
    "#;

    let ast = Parser::new(src).parse();
    let (hir, errs) = crate::lower_ast_to_hir(ast);

    for e in errs.iter() {
        println!("{e}");
    }

    assert!(errs.is_empty());
    let topdecs = hir.topdecs();

    let index = topdecs[1];
    let (_, valbind) = hir.get_dec(index).val().unwrap();
    let match_ = hir.get_expr(valbind[0].expr).fn_expr().unwrap();

    // Check first match arm
    let (pat, expr) = (hir.get_pat(match_[0].pat), hir.get_expr(match_[0].expr));
    let (_, (_, loc)) = pat.vid().unwrap();
    assert!(*loc == Some(DefLoc::Dec(topdecs[0])));
    let (_, (_, loc)) = expr.vid().unwrap();
    assert!(*loc == DefLoc::Dec(topdecs[0]));

    // Check second match arm
    let (pat, expr) = (hir.get_pat(match_[1].pat), hir.get_expr(match_[1].expr));
    let (_, (_, loc), _) = pat.cons().unwrap();
    assert!(*loc == DefLoc::Dec(topdecs[0]));
    let (expr, _) = expr.application().unwrap();
    let (_, (_, loc)) = hir.get_expr(expr).vid().unwrap();
    assert!(*loc == DefLoc::Dec(topdecs[0]));
}
