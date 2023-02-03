//! Lexical tokens and their kinds

/// Kinds of lexical tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LexKind {
    /// Any non-empty sequence of whitespace chars.
    Whitespace,
    /// Example: "(* this is a comment *)".
    Comment { terminated: bool },
    /// `=`
    Eq,
    /// `:`
    Colon,
    /// `;`
    Semicolon,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `|`
    Pipe,
    /// `#`
    Hash,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `...`
    Ellipsis,
    /// `_`
    Underscore,
    /// `=>`
    ThickArrow,
    /// `->`
    ThinArrow,
    /// An integer constant (decimal or hexadecimal).
    Int,
    /// An unsigned constant (decimal or hexadecimal).
    Word,
    /// An real constant (with possible exponential part).
    Real,
    /// An char constant (e.g., `#"a"`).
    Char { terminated: bool },
    /// An string constant (e.g., `"abc"`).
    String { terminated: bool },
    /// Keyword or (symbolic or alphanumeric) identifier.
    Ident,
    /// Unknown character.
    Unknown,
}

impl LexKind {
    /// Returns `Some(kind)` if `c` matches a single character token,
    /// otherwise `None`.
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

/// A token is represented by a `LexKind` and a length of source code (span).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LexToken {
    len: usize,
    kind: LexKind,
}

impl LexToken {
    pub fn new(len: usize, kind: LexKind) -> Self {
        Self { len, kind }
    }

    pub fn kind(&self) -> LexKind {
        self.kind
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}
