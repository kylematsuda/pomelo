#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LexKind {
    Whitespace,
    Comment { terminated: bool },
    Eq,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Pipe,
    Hash,
    Comma,
    Dot,
    Ellipsis,
    Underscore,
    ThickArrow,
    ThinArrow,
    Int,
    Word,
    Real,
    Char { terminated: bool },
    String { terminated: bool },
    Ident,
    Unknown,
}

impl LexKind {
    pub fn from_char(c: char) -> Option<Self> {
        use LexKind::*;

        let token = match c {
            '=' => Eq,
            ':' => Colon,
            ';' => Semicolon,
            '(' => LParen,
            ')' => RParen,
            '[' => LBracket,
            ']' => RBracket,
            '{' => LBrace,
            '}' => RBrace,
            '|' => Pipe,
            '#' => Hash,
            ',' => Comma,
            '.' => Dot,
            '_' => Underscore,
            _ => return None,
        };
        Some(token)
    }
}