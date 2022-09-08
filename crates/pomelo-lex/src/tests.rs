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
            LexToken { len: 37, kind: Whitespace }
        "#]],
    )
}

#[test]
fn just_a_paren_not_a_comment() {
    check(
        "( hi i am not quite a comment! *)",
        expect![[r#"
            LexToken { len: 1, kind: LParen }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 5, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 7, kind: Ident }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: RParen }
        "#]],
    )
}

#[test]
fn comment() {
    check(
        "(* hi i am a comment: %$%\\[{]}'`~*() *)",
        expect![[r#"
            LexToken { len: 39, kind: Comment { terminated: true } }
        "#]],
    )
}

#[test]
fn unterminated_comment() {
    check(
        "(* whoops i am unterminated!",
        expect![[r#"
            LexToken { len: 28, kind: Comment { terminated: false } }
        "#]],
    )
}

#[test]
fn char() {
    check(
        "#\"g\" #\"o\" #\"o\" #\"d\"",
        expect![[r#"
            LexToken { len: 4, kind: Char { terminated: true } }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Char { terminated: true } }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Char { terminated: true } }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Char { terminated: true } }
        "#]],
    )
}

#[test]
fn char_unterminated() {
    check(
        "#\"w \"uh oh\"",
        expect![[r#"
            LexToken { len: 4, kind: Char { terminated: false } }
            LexToken { len: 7, kind: String { terminated: true } }
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
            LexToken { len: 4, kind: Char { terminated: false } }
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 5, kind: Ident }
            LexToken { len: 1, kind: String { terminated: false } }
        "#]],
    )
}

#[test]
fn not_a_char_just_a_hash() {
    check(
        "# \"i am not a char\"",
        expect![[r#"
            LexToken { len: 1, kind: Hash }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 17, kind: String { terminated: true } }
        "#]],
    )
}

#[test]
fn single_hash_is_hash() {
    check(
        "# ",
        expect![[r#"
            LexToken { len: 1, kind: Hash }
            LexToken { len: 1, kind: Whitespace }
        "#]],
    )
}

#[test]
fn double_hash_is_ident() {
    check(
        "## #| #!",
        expect![[r#"
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Ident }
        "#]],
    )
}

#[test]
fn string() {
    check(
        "\"hi i am a string\"",
        expect![[r#"
            LexToken { len: 18, kind: String { terminated: true } }
        "#]],
    )
}

#[test]
fn unterminated_string() {
    check(
        "\"whoops i am unterminated!",
        expect![[r#"
            LexToken { len: 26, kind: String { terminated: false } }
        "#]],
    )
}

#[test]
fn eq() {
    check(
        "= - ",
        expect![[r#"
            LexToken { len: 1, kind: Eq }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Minus }
            LexToken { len: 1, kind: Whitespace }
        "#]],
    )
}

#[test]
fn eq_arrows() {
    check(
        "=> ->",
        expect![[r#"
            LexToken { len: 2, kind: ThickArrow }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: ThinArrow }
        "#]],
    )
}

#[test]
fn eq_weird_idents() {
    check(
        "===> -<> -!><",
        expect![[r#"
            LexToken { len: 4, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Ident }
        "#]],
    )
}

#[test]
fn eq_mixed() {
    check(
        "= - ==> <= => =!>< ->",
        expect![[r#"
            LexToken { len: 1, kind: Eq }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Minus }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: ThickArrow }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: ThinArrow }
        "#]],
    )
}

#[test]
fn dot_vs_ellipsis() {
    check(
        ". .. ... ....",
        expect![[r#"
            LexToken { len: 1, kind: Dot }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Dot }
            LexToken { len: 1, kind: Dot }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Ellipsis }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Ellipsis }
            LexToken { len: 1, kind: Dot }
        "#]],
    )
}

#[test]
fn reserved_symb() {
    check(
        "()[]{},:;..._| => -> #",
        expect![[r#"
            LexToken { len: 1, kind: LParen }
            LexToken { len: 1, kind: RParen }
            LexToken { len: 1, kind: LBracket }
            LexToken { len: 1, kind: RBracket }
            LexToken { len: 1, kind: LBrace }
            LexToken { len: 1, kind: RBrace }
            LexToken { len: 1, kind: Comma }
            LexToken { len: 1, kind: Colon }
            LexToken { len: 1, kind: Semicolon }
            LexToken { len: 3, kind: Ellipsis }
            LexToken { len: 1, kind: Underscore }
            LexToken { len: 1, kind: Pipe }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: ThickArrow }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: ThinArrow }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Hash }
        "#]],
    )
}

#[test]
fn alphanumeric_ident() {
    check(
        "and i am all15 alpha_numeric' 'and happy100",
        expect![[r#"
            LexToken { len: 3, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 5, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 14, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 8, kind: Ident }
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
            LexToken { len: 1, kind: Underscore }
            LexToken { len: 4, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Int }
            LexToken { len: 5, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Dot }
            LexToken { len: 1, kind: Int }
            LexToken { len: 4, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 7, kind: Ident }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 7, kind: Ident }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 7, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
        "#]],
    )
}

#[test]
fn ints() {
    check(
        "10 ~20 39 0xFA 1 120 ~0xFFFFFC",
        expect![[r#"
            LexToken { len: 2, kind: Int }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Int }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Int }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Int }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Int }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Int }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 9, kind: Int }
        "#]],
    )
}

#[test]
fn bad_ints() {
    check(
        "10a0 2b 3F x0F AA 00xA",
        expect![[r#"
            LexToken { len: 2, kind: Int }
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Int }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Int }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Ident }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Int }
            LexToken { len: 2, kind: Ident }
        "#]],
    )
}

#[test]
fn reals() {
    check(
        "10.0 2.2 3.0e7 0.5e~3 1.5 120.0",
        expect![[r#"
            LexToken { len: 4, kind: Real }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Real }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 5, kind: Real }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 6, kind: Real }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Real }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 5, kind: Real }
        "#]],
    )
}

#[test]
fn bad_reals() {
    check(
        "10. .2 3.0e7.0 5e~3 15 1.0.2",
        expect![[r#"
            LexToken { len: 2, kind: Int }
            LexToken { len: 1, kind: Dot }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Dot }
            LexToken { len: 1, kind: Int }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 5, kind: Real }
            LexToken { len: 1, kind: Dot }
            LexToken { len: 1, kind: Int }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 1, kind: Int }
            LexToken { len: 1, kind: Ident }
            LexToken { len: 2, kind: Int }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 2, kind: Int }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Real }
            LexToken { len: 1, kind: Dot }
            LexToken { len: 1, kind: Int }
        "#]],
    )
}

#[test]
fn words() {
    check(
        "0w10 0wx20 0w39 0wxFA 0w1 0w120 0wxFFFFFC",
        expect![[r#"
            LexToken { len: 4, kind: Word }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 5, kind: Word }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 4, kind: Word }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 5, kind: Word }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 3, kind: Word }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 5, kind: Word }
            LexToken { len: 1, kind: Whitespace }
            LexToken { len: 9, kind: Word }
        "#]],
    )
}
