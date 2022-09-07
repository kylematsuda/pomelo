mod cursor;

#[cfg(test)]
mod tests;

pub mod kind;
pub use kind::Kind;

pub mod token;
pub use token::Token;

pub fn lex<'a>(src: &'a str) -> Vec<Token> {
    let ls = token::LexedStr::new(src);
    ls.lex()
}
