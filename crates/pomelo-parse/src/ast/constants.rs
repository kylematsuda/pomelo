use crate::{impl_ast_token, AstToken, SyntaxToken};
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Int {
    syntax: SyntaxToken,
}

impl_ast_token!(Int, INT);

impl Int {
    // Sketchy for now
    // Obviously will fail if the number is negated ("~")
    pub fn parse<F: FromStr>(&self) -> F
    where
        <F as FromStr>::Err: std::fmt::Debug,
    {
        self.syntax().text().parse::<F>().unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Real {
    syntax: SyntaxToken,
}

impl_ast_token!(Real, REAL);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Word {
    syntax: SyntaxToken,
}

impl_ast_token!(Word, WORD);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Char {
    syntax: SyntaxToken,
}

impl_ast_token!(Char, CHAR);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct String {
    syntax: SyntaxToken,
}

impl_ast_token!(String, STRING);
