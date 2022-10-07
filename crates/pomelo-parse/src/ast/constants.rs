use crate::{impl_ast_token, AstToken, SyntaxKind, SyntaxToken};
use std::str::FromStr;

use SyntaxKind::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Scon {
    Int(SyntaxToken),
    Real(SyntaxToken),
    Word(SyntaxToken),
    Char(SyntaxToken),
    String(SyntaxToken),
}

impl AstToken for Scon {
    fn syntax(&self) -> &SyntaxToken {
        match self {
            Self::Int(s) => &s,
            Self::Real(s) => &s,
            Self::Word(s) => &s,
            Self::Char(s) => &s,
            Self::String(s) => &s,
        }
    }

    fn cast(token: SyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        match token.kind() {
            INT => Some(Self::Int(token)),
            REAL => Some(Self::Real(token)),
            WORD => Some(Self::Word(token)),
            CHAR => Some(Self::Char(token)),
            STRING => Some(Self::String(token)),
            _ => None,
        }
    }
}

//  #[derive(Debug, Clone, PartialEq, Eq, Hash)]
//  pub struct Int {
//      syntax: SyntaxToken,
//  }
//
//  impl_ast_token!(Int, INT);
//
//  impl Int {
//      // Sketchy for now
//      // Obviously will fail if the number is negated ("~")
//      pub fn parse<F: FromStr>(&self) -> F
//      where
//          <F as FromStr>::Err: std::fmt::Debug,
//      {
//          self.syntax().text().parse::<F>().unwrap()
//      }
//  }
//
//  #[derive(Debug, Clone, PartialEq, Eq, Hash)]
//  pub struct Real {
//      syntax: SyntaxToken,
//  }
//
//  impl_ast_token!(Real, REAL);
//
//  #[derive(Debug, Clone, PartialEq, Eq, Hash)]
//  pub struct Word {
//      syntax: SyntaxToken,
//  }
//
//  impl_ast_token!(Word, WORD);
//
//  #[derive(Debug, Clone, PartialEq, Eq, Hash)]
//  pub struct Char {
//      syntax: SyntaxToken,
//  }
//
//  impl_ast_token!(Char, CHAR);
//
//  #[derive(Debug, Clone, PartialEq, Eq, Hash)]
//  pub struct String {
//      syntax: SyntaxToken,
//  }
//
//  impl_ast_token!(String, STRING);
