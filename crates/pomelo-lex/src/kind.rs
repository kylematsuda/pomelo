#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kind {
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

impl Kind {
    pub fn from_char(c: char) -> Option<Self> {
        use Kind::*;

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
