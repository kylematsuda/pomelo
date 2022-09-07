#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LexemeKind {
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
    Minus,
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

impl LexemeKind {
    pub fn from_char(c: char) -> Option<Self> {
        use LexemeKind::*;

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
            '-' => Minus,
            ',' => Comma,
            '.' => Dot,
            '_' => Underscore,
            _ => return None,
        };
        Some(token)
    }
}
