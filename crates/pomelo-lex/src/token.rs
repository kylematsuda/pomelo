use crate::cursor::Cursor;
use crate::Kind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
    len: usize,
    kind: Kind,
}

#[derive(Debug, Clone)]
pub struct LexedStr<'a> {
    input: &'a str,
    tokens: Vec<Token>,
    errors: Vec<LexError>,
}

#[derive(Debug, Clone)]
struct LexError {
    msg: String,
    token: usize,
}

impl Token {
    pub fn new(len: usize, kind: Kind) -> Self {
        Self { len, kind }
    }
}

impl<'a> LexedStr<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Self {
            input,
            tokens: Vec::new(),
            errors: Vec::new(),
        };
        l.lex();
        l
    }

    fn push(&mut self, token: Option<Token>, error: Option<LexError>) {
        if let Some(t) = token {
            self.tokens.push(t);
        }
        if let Some(e) = error {
            self.errors.push(e);
        }
    }

    fn lex(&mut self) {
        let mut cursor = Cursor::new(self.input);

        while !cursor.is_eof() {
            let (token, err) = cursor.next_token();
            self.push(token, err);
        }
    }
}

impl<'a> Cursor<'a> {
    fn next_token(&mut self) -> (Option<Token>, Option<LexError>) {
        todo!()
    }
}
