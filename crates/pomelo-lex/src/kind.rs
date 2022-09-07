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
    // Bang,
    // Percent,
    // Ampersand,
    // Dollar,
    Hash,
    // Plus,
    Minus,
    // Slash,
    // Less,
    // Gtr,
    // Question,
    // At,
    Comma,
    Dot,
    Ellipsis,
    Underscore,
    // Backslash,
    // Tilde,
    // Backtick,
    // Carat,
    // Star,
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
            //            '!' => Bang,
            //            '%' => Percent,
            //            '&' => Ampersand,
            //            '$' => Dollar,
            '#' => Hash,
            //            '+' => Plus,
            '-' => Minus,
            //           '/' => Slash,
            //           '<' => Less,
            //           '>' => Gtr,
            //           '?' => Question,
            //           '@' => At,
            ',' => Comma,
            '.' => Dot,
            '_' => Underscore,
            //            '\\' => Backslash,
            //            '~' => Tilde,
            //            '`' => Backtick,
            //            '^' => Carat,
            //            '*' => Star,
            _ => return None,
        };
        Some(token)
    }
}
