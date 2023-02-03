use crate::cursor::{Cursor, EOF_CHAR};
use crate::LexKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LexToken {
    len: usize,
    kind: LexKind,
}

impl LexToken {
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

#[derive(Debug, Clone)]
pub(crate) struct LexedStr<'a> {
    input: &'a str,
    tokens: Vec<LexToken>,
}

impl LexToken {
    pub fn new(len: usize, kind: LexKind) -> Self {
        Self { len, kind }
    }
}

impl<'a> LexedStr<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            input,
            tokens: Vec::new(),
        }
    }

    pub(crate) fn lex(mut self) -> Vec<LexToken> {
        let mut cursor = Cursor::new(self.input);

        while !cursor.is_eof() {
            let token = cursor.next_token();
            self.tokens.push(token);
        }

        self.tokens
    }
}

fn is_whitespace(c: char) -> bool {
    char::is_whitespace(c)
}

fn is_ident_alpha_start(c: char) -> bool {
    char::is_alphabetic(c) || c == '\''
}

fn is_ident_alpha_cont(c: char) -> bool {
    char::is_alphanumeric(c) || c == '\'' || c == '_'
}

fn is_ident_symb(c: char) -> bool {
    matches!(
        c,
        '!' | '%'
            | '&'
            | '$'
            | '#'
            | '+'
            | '-'
            | '/'
            | ':'
            | '<'
            | '='
            | '>'
            | '?'
            | '@'
            | '\\'
            | '~'
            | '`'
            | '^'
            | '|'
            | '*'
    )
}

fn is_reserved_symbol_start(c: char) -> bool {
    // "...", "=>", and "->" are covered in other cases of the lexer
    matches!(
        c,
        '(' | ')' | '[' | ']' | '{' | '}' | ',' | ':' | ';' | '_' | '|' | '#'
    )
}

fn is_number_start(c: char) -> bool {
    char::is_ascii_digit(&c)
}

impl<'a> Cursor<'a> {
    fn next_token(&mut self) -> LexToken {
        self.reset_len_consumed();

        let c = self.bump().expect("caller checks Cursor::is_eof");

        let kind = match c {
            c if is_whitespace(c) => self.whitespace(),

            '(' => match self.first() {
                '*' => self.comment(),
                _ => LexKind::LParen,
            },

            '#' => match self.first() {
                '"' => self.literal_char(),
                c if is_ident_symb(c) => self.ident_symb(),
                _ => LexKind::Hash,
            },

            '"' => self.literal_string(),

            '~' => {
                if self.first().is_ascii_digit() {
                    self.number_negation()
                } else {
                    self.ident_symb()
                }
            }

            '=' => {
                let c1 = self.first();
                let c2 = self.second();

                if c1 == '>' && !is_ident_symb(c2) {
                    self.arrow(c)
                } else if is_ident_symb(c1) {
                    self.ident_symb()
                } else {
                    LexKind::Eq
                }
            }

            '-' => {
                let c1 = self.first();
                let c2 = self.second();

                if c1 == '>' && !is_ident_symb(c2) {
                    self.arrow(c)
                } else {
                    self.ident_symb()
                }
            }

            ':' => {
                if is_ident_symb(self.first()) {
                    self.ident_symb()
                } else {
                    LexKind::Colon
                }
            }

            '|' => {
                if is_ident_symb(self.first()) {
                    self.ident_symb()
                } else {
                    LexKind::Pipe
                }
            }

            '.' => {
                let c1 = self.first();
                let c2 = self.second();

                if c1 == '.' && c2 == '.' {
                    self.ellipsis()
                } else {
                    LexKind::Dot
                }
            }

            c if is_reserved_symbol_start(c) => {
                LexKind::from_char(c).expect("non-compound reserved symbols are covered by Kind")
            }
            c if is_ident_alpha_start(c) => self.ident_alpha(),
            c if is_ident_symb(c) => self.ident_symb(),
            c if is_number_start(c) => self.number(c),
            _ => LexKind::Unknown,
        };

        LexToken::new(self.len_consumed(), kind)
    }

    fn whitespace(&mut self) -> LexKind {
        self.eat_while(is_whitespace);
        LexKind::Whitespace
    }

    fn comment(&mut self) -> LexKind {
        assert_eq!(self.bump().unwrap(), '*');

        while let Some(c) = self.bump() {
            if c == '*' && self.first() == ')' {
                        self.bump();
                        return LexKind::Comment { terminated: true };
            }
        }
        // EOF reached
        LexKind::Comment { terminated: false }
    }

    fn literal_char(&mut self) -> LexKind {
        assert_eq!(self.bump().unwrap(), '"');

        match self.bump() {
            Some(EOF_CHAR) | None => return LexKind::Char { terminated: false },
            _ => {}
        }

        match self.bump() {
            Some('"') => LexKind::Char { terminated: true },
            _ => LexKind::Char { terminated: false },
        }
    }

    fn literal_string(&mut self) -> LexKind {
        while let Some(c) = self.bump() {
            match c {
                '"' => return LexKind::String { terminated: true },

                // Handle escape sequences
                '\\' => {
                    self.bump();
                }

                _ => (),
            }
        }
        // EOF reached
        LexKind::String { terminated: false }
    }

    fn number_negation(&mut self) -> LexKind {
        let first = self.bump().expect("a digit follows the ~");
        self.number(first)
    }

    fn arrow(&mut self, c: char) -> LexKind {
        self.bump();

        match c {
            '-' => LexKind::ThinArrow,
            '=' => LexKind::ThickArrow,
            _ => unreachable!(),
        }
    }

    fn ellipsis(&mut self) -> LexKind {
        assert_eq!(self.bump().unwrap(), '.');
        assert_eq!(self.bump().unwrap(), '.');
        LexKind::Ellipsis
    }

    fn ident_alpha(&mut self) -> LexKind {
        self.eat_while(is_ident_alpha_cont);
        LexKind::Ident
    }

    fn ident_symb(&mut self) -> LexKind {
        self.eat_while(is_ident_symb);
        LexKind::Ident
    }

    fn number(&mut self, first: char) -> LexKind {
        if first == '0' {
            match self.first() {
                'x' => return self.hexadecimal(),
                'w' => return self.word(),
                _ => {}
            }
        }
        self.decimal_or_real()
    }

    fn decimal_or_real(&mut self) -> LexKind {
        let mut is_real = false;

        while !self.is_eof() {
            match self.first() {
                '.' => {
                    // A decimal point must always be followed by another
                    // decimal digit.
                    if !is_real && self.second().is_ascii_digit() {
                        is_real = true;
                        self.bump();
                        self.bump();
                    } else {
                        // We might have a erroneous number with two decimal
                        // places, e.g., "123.56.78". This is lexed as
                        // ["123.56", ".", "78"]
                        break;
                    }
                }
                'e' | 'E' => {
                    if is_real && (self.second().is_ascii_digit() || self.second() == '~') {
                        self.bump();
                        return self.exponent();
                    } else {
                        // Numbers without a decimal point cannot have
                        // exponents; e.g., "1e7" is illegal.
                        break;
                    }
                }
                c if c.is_ascii_digit() => {
                    self.bump();
                }
                _ => break,
            }
        }

        if is_real {
            LexKind::Real
        } else {
            LexKind::Int
        }
    }

    fn exponent(&mut self) -> LexKind {
        if self.first() == '~' {
            self.bump();
        }
        self.eat_while(|c| char::is_ascii_digit(&c));
        LexKind::Real
    }

    fn hexadecimal(&mut self) -> LexKind {
        assert_eq!(self.bump().unwrap(), 'x');
        self.eat_while(|c| char::is_ascii_hexdigit(&c));
        LexKind::Int
    }

    fn word(&mut self) -> LexKind {
        assert_eq!(self.bump().unwrap(), 'w');
        if self.first() == 'x' {
            self.bump();
            self.eat_while(|c| char::is_ascii_hexdigit(&c));
        } else {
            self.eat_while(|c| char::is_ascii_digit(&c));
        }
        LexKind::Word
    }
}
