pub mod grammar;

pub mod syntax;

pub mod parser;
pub use parser::{Parser, Marker, CompletedMarker};

use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum LpError {
    #[error("LexError at {}-{}: {msg}", .span.0, .span.1)]
    LexError { msg: &'static str, span: (usize, usize) },
    #[error("ParseError at {}-{}: {msg}", .span.0, .span.1)]
    ParseError { msg: &'static str, span: (usize, usize) }
}

impl LpError {
    pub fn from_lex(msg: &'static str, span: (usize, usize)) -> Self {
        Self::LexError { msg, span }
    }

    pub fn from_parse(msg: &'static str, span: (usize, usize)) -> Self {
        Self::ParseError { msg, span }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
