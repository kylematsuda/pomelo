use crate::{impl_ast_token, SyntaxToken};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Int {
    syntax: SyntaxToken,
}

impl_ast_token!(Int, INT);

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
