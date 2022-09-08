mod cursor;

#[cfg(test)]
mod tests;

pub mod kind;
pub use kind::LexKind;

pub mod token;
pub use token::LexToken;

pub fn lex<'a>(src: &'a str) -> Vec<LexToken> {
    let ls = token::LexedStr::new(src);
    ls.lex()
}
