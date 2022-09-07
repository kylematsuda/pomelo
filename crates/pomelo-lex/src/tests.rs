use expect_test::{expect, Expect};

use crate::*;

fn check(src: &str, expect: Expect) {
    let actual: String = lex(src)
        .into_iter()
        .map(|token| format!("{:?}\n", token))
        .collect();
    expect.assert_eq(&actual)
}

#[test]
fn whitespace() {
    check(
        "\n          \n\r\n\r\n\t\t\n\r\n \n \n \n \r \n     \n",
        expect![[r#"
            Token { len: 37, kind: Whitespace }
        "#]],
    )
}

#[test]
fn just_a_paren_not_a_comment() {
    check(
        "( hi i am not quite a comment! *)",
        expect![[r#"
            Token { len: 1, kind: LParen }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 3, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 5, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 7, kind: Ident }
            Token { len: 1, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Ident }
            Token { len: 1, kind: RParen }
        "#]],
    )
}

#[test]
fn comment() {
    check(
        "(* hi i am a comment: %$%\\[{]}'`~*() *)",
        expect![[r#"
            Token { len: 39, kind: Comment { terminated: true } }
        "#]],
    )
}

#[test]
fn unterminated_comment() {
    check(
        "(* whoops i am unterminated!",
        expect![[r#"
            Token { len: 28, kind: Comment { terminated: false } }
        "#]],
    )
}

#[test]
fn char() {
    check(
        "#\"g\" #\"o\" #\"o\" #\"d\"",
        expect![[r#"
        Token { len: 4, kind: Char { terminated: true } }
        Token { len: 1, kind: Whitespace }
        Token { len: 4, kind: Char { terminated: true } }
        Token { len: 1, kind: Whitespace }
        Token { len: 4, kind: Char { terminated: true } }
        Token { len: 1, kind: Whitespace }
        Token { len: 4, kind: Char { terminated: true } }
    "#]],
    )
}

#[test]
fn char_unterminated() {
    check(
        "#\"w \"uh oh\"",
        expect![[r#"
        Token { len: 4, kind: Char { terminated: false } }
        Token { len: 7, kind: String { terminated: true } }
    "#]],
    )
}

#[test]
fn char_too_long() {
    // This should give an initial unterminated Char
    // followed by a bunch of Idents
    check(
        "#\"i am a char that is many chars\"",
        expect![[r#"
        Token { len: 4, kind: Char { terminated: false } }
        Token { len: 2, kind: Ident }
        Token { len: 1, kind: Whitespace }
        Token { len: 1, kind: Ident }
        Token { len: 1, kind: Whitespace }
        Token { len: 4, kind: Ident }
        Token { len: 1, kind: Whitespace }
        Token { len: 4, kind: Ident }
        Token { len: 1, kind: Whitespace }
        Token { len: 2, kind: Ident }
        Token { len: 1, kind: Whitespace }
        Token { len: 4, kind: Ident }
        Token { len: 1, kind: Whitespace }
        Token { len: 5, kind: Ident }
        Token { len: 1, kind: String { terminated: false } }
    "#]],
    )
}

#[test]
fn not_a_char_just_a_hash() {
    check(
        "# \"i am not a char\"",
        expect![[r#"
        Token { len: 1, kind: Hash }
        Token { len: 1, kind: Whitespace }
        Token { len: 17, kind: String { terminated: true } }
        "#]],
    )
}

#[test]
fn single_hash_is_hash() {
    check(
        "# ",
        expect![[r#"
        Token { len: 1, kind: Hash }
        Token { len: 1, kind: Whitespace }
        "#]],
    )
}

#[test]
fn double_hash_is_ident() {
    check(
        "## #| #!",
        expect![[r#"
        Token { len: 2, kind: Ident }
        Token { len: 1, kind: Whitespace }
        Token { len: 2, kind: Ident }
        Token { len: 1, kind: Whitespace }
        Token { len: 2, kind: Ident }
        "#]],
    )
}

#[test]
fn string() {
    check(
        "\"hi i am a string\"",
        expect![[r#"
            Token { len: 18, kind: String { terminated: true } }
        "#]],
    )
}

#[test]
fn unterminated_string() {
    check(
        "\"whoops i am unterminated!",
        expect![[r#"
            Token { len: 26, kind: String { terminated: false } }
        "#]],
    )
}

#[test]
fn eq() {
    check(
        "= - ",
        expect![[r#"
        Token { len: 1, kind: Eq }
        Token { len: 1, kind: Whitespace }
        Token { len: 1, kind: Minus }
        Token { len: 1, kind: Whitespace }
        "#]],
    )
}

#[test]
fn eq_arrows() {
    check(
        "=> ->",
        expect![[r#"
        Token { len: 2, kind: ThickArrow }
        Token { len: 1, kind: Whitespace }
        Token { len: 2, kind: ThinArrow }
        "#]],
    )
}

#[test]
fn eq_weird_idents() {
    check(
        "===> -<> -!><",
        expect![[r#"
            Token { len: 4, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 3, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 4, kind: Ident }
        "#]],
    )
}

#[test]
fn eq_mixed() {
    check(
        "= - ==> <= => =!>< ->",
        expect![[r#"
            Token { len: 1, kind: Eq }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Minus }
            Token { len: 1, kind: Whitespace }
            Token { len: 3, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: ThickArrow }
            Token { len: 1, kind: Whitespace }
            Token { len: 4, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: ThinArrow }
        "#]],
    )
}

#[test]
fn dot_vs_ellipsis() {
    check(
        ". .. ... ....",
        expect![[r#"
        Token { len: 1, kind: Dot }
        Token { len: 1, kind: Whitespace }
        Token { len: 1, kind: Dot }
        Token { len: 1, kind: Dot }
        Token { len: 1, kind: Whitespace }
        Token { len: 3, kind: Ellipsis }
        Token { len: 1, kind: Whitespace }
        Token { len: 3, kind: Ellipsis }
        Token { len: 1, kind: Dot }
        "#]],
    )
}

#[test]
fn reserved_symb() {
    check(
        "()[]{},:;..._| => -> #",
        expect![[r#"
            Token { len: 1, kind: LParen }
            Token { len: 1, kind: RParen }
            Token { len: 1, kind: LBracket }
            Token { len: 1, kind: RBracket }
            Token { len: 1, kind: LBrace }
            Token { len: 1, kind: RBrace }
            Token { len: 1, kind: Comma }
            Token { len: 1, kind: Colon }
            Token { len: 1, kind: Semicolon }
            Token { len: 3, kind: Ellipsis }
            Token { len: 1, kind: Underscore }
            Token { len: 1, kind: Pipe }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: ThickArrow }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: ThinArrow }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Hash }
        "#]],
    )
}

#[test]
fn alphanumeric_ident() {
    check(
        "and i am all15 alpha_numeric' 'and happy100",
        expect![[r#"
            Token { len: 3, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 5, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 14, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 4, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 8, kind: Ident }
        "#]],
    )
}

#[test]
fn bad_alphanumeric_ident() {
    // Symbolic idents and alphanumeric idents are lexed
    // as separate tokens
    //
    // Note: number literals are not yet supported,
    // hence they are Kind::Unknown for now
    check(
        "_cant 10start .1with numbers! or$ include* %symbols ",
        expect![[r#"
            Token { len: 1, kind: Underscore }
            Token { len: 4, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: Int }
            Token { len: 5, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Dot }
            Token { len: 1, kind: Int }
            Token { len: 4, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 7, kind: Ident }
            Token { len: 1, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: Ident }
            Token { len: 1, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 7, kind: Ident }
            Token { len: 1, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Ident }
            Token { len: 7, kind: Ident }
            Token { len: 1, kind: Whitespace }
        "#]],
    )
}

#[test]
fn ints() {
    check(
        "10 ~20 39 0xFA 1 120 ~0xFFFFFC",
        expect![[r#"
            Token { len: 2, kind: Int }
            Token { len: 1, kind: Whitespace }
            Token { len: 3, kind: Int }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: Int }
            Token { len: 1, kind: Whitespace }
            Token { len: 4, kind: Int }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Int }
            Token { len: 1, kind: Whitespace }
            Token { len: 3, kind: Int }
            Token { len: 1, kind: Whitespace }
            Token { len: 9, kind: Int }
        "#]],
    )
}

#[test]
fn bad_ints() {
    check(
        "10a0 2b 3F x0F AA 00xA",
        expect![[r#"
            Token { len: 2, kind: Int }
            Token { len: 2, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Int }
            Token { len: 1, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Int }
            Token { len: 1, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 3, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: Ident }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: Int }
            Token { len: 2, kind: Ident }
        "#]],
    )
}

#[test]
fn reals() {
    check(
        "10.0 2.2 3.0e7 0.5e~3 1.5 120.0",
        expect![[r#"
            Token { len: 4, kind: Real }
            Token { len: 1, kind: Whitespace }
            Token { len: 3, kind: Real }
            Token { len: 1, kind: Whitespace }
            Token { len: 5, kind: Real }
            Token { len: 1, kind: Whitespace }
            Token { len: 6, kind: Real }
            Token { len: 1, kind: Whitespace }
            Token { len: 3, kind: Real }
            Token { len: 1, kind: Whitespace }
            Token { len: 5, kind: Real }
        "#]],
    )
}

#[test]
fn bad_reals() {
    check(
        "10. .2 3.0e7.0 5e~3 15 1.0.2",
        expect![[r#"
            Token { len: 2, kind: Int }
            Token { len: 1, kind: Dot }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Dot }
            Token { len: 1, kind: Int }
            Token { len: 1, kind: Whitespace }
            Token { len: 5, kind: Real }
            Token { len: 1, kind: Dot }
            Token { len: 1, kind: Int }
            Token { len: 1, kind: Whitespace }
            Token { len: 1, kind: Int }
            Token { len: 1, kind: Ident }
            Token { len: 2, kind: Int }
            Token { len: 1, kind: Whitespace }
            Token { len: 2, kind: Int }
            Token { len: 1, kind: Whitespace }
            Token { len: 3, kind: Real }
            Token { len: 1, kind: Dot }
            Token { len: 1, kind: Int }
        "#]],
    )
}

#[test]
fn words() {
    check(
        "0w10 0wx20 0w39 0wxFA 0w1 0w120 0wxFFFFFC",
        expect![[r#"
            Token { len: 4, kind: Word }
            Token { len: 1, kind: Whitespace }
            Token { len: 5, kind: Word }
            Token { len: 1, kind: Whitespace }
            Token { len: 4, kind: Word }
            Token { len: 1, kind: Whitespace }
            Token { len: 5, kind: Word }
            Token { len: 1, kind: Whitespace }
            Token { len: 3, kind: Word }
            Token { len: 1, kind: Whitespace }
            Token { len: 5, kind: Word }
            Token { len: 1, kind: Whitespace }
            Token { len: 9, kind: Word }
        "#]],
    )
}
