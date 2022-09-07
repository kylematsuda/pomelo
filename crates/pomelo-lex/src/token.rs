use crate::cursor::{Cursor, EOF_CHAR};
use crate::LexemeKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
    len: usize,
    kind: LexemeKind,
}

#[derive(Debug, Clone)]
pub(crate) struct LexedStr<'a> {
    input: &'a str,
    tokens: Vec<Token>,
}

impl Token {
    pub fn new(len: usize, kind: LexemeKind) -> Self {
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

    pub(crate) fn lex(mut self) -> Vec<Token> {
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
    fn next_token(&mut self) -> Token {
        self.reset_len_consumed();

        let c = self.bump().expect("caller checks Cursor::is_eof");

        let kind = match c {
            c if is_whitespace(c) => self.whitespace(),

            '(' => match self.first() {
                '*' => self.comment(),
                _ => LexemeKind::LParen,
            },

            '#' => match self.first() {
                '"' => self.literal_char(),
                c if is_ident_symb(c) => self.ident_symb(),
                _ => LexemeKind::Hash,
            },

            '"' => self.literal_string(),

            '~' => if self.first().is_ascii_digit() {
                self.number_negation()
            } else { self.ident_symb() }

            '=' | '-' => {
                let c1 = self.first();
                let c2 = self.second();

                if is_ident_symb(c1) && is_ident_symb(c2) {
                    self.ident_symb()
                } else if c1 == '>' {
                    self.arrow(c)
                } else {
                    LexemeKind::from_char(c).expect("non-compound reserved symbols are covered by Kind")
                }
            }

            '.' => {
                let c1 = self.first();
                let c2 = self.second();

                if c1 == '.' && c2 == '.' {
                    self.ellipsis()
                } else {
                    LexemeKind::from_char(c).expect("non-compound reserved symbols are covered by Kind")
                }
            }

            c if is_reserved_symbol_start(c) => {
                LexemeKind::from_char(c).expect("non-compound reserved symbols are covered by Kind")
            }
            c if is_ident_alpha_start(c) => self.ident_alpha(),
            c if is_ident_symb(c) => self.ident_symb(),
            c if is_number_start(c) => self.number(c),
            _ => LexemeKind::Unknown,
        };

        Token::new(self.len_consumed(), kind)
    }

    fn whitespace(&mut self) -> LexemeKind {
        self.eat_while(is_whitespace);
        LexemeKind::Whitespace
    }

    fn comment(&mut self) -> LexemeKind {
        assert_eq!(self.bump().unwrap(), '*');

        while let Some(c) = self.bump() {
            match c {
                '*' => {
                    if self.first() == ')' {
                        self.bump();
                        return LexemeKind::Comment { terminated: true };
                    }
                }
                _ => {}
            }
        }
        // EOF reached
        LexemeKind::Comment { terminated: false }
    }

    fn literal_char(&mut self) -> LexemeKind {
        assert_eq!(self.bump().unwrap(), '"');

        match self.bump() {
            Some(EOF_CHAR) | None => return LexemeKind::Char { terminated: false },
            _ => {}
        }

        match self.bump() {
            Some('"') => return LexemeKind::Char { terminated: true },
            _ => return LexemeKind::Char { terminated: false },
        }
    }

    fn literal_string(&mut self) -> LexemeKind {
        while let Some(c) = self.bump() {
            match c {
                '"' => return LexemeKind::String { terminated: true },

                // Handle escape sequences
                '\\' => {
                    self.bump();
                }

                _ => (),
            }
        }
        // EOF reached
        LexemeKind::String { terminated: false }
    }

    fn number_negation(&mut self) -> LexemeKind {
        let first = self.bump().expect("a digit follows the ~");
        self.number(first)
    }

    fn arrow(&mut self, c: char) -> LexemeKind {
        self.bump();

        match c {
            '-' => LexemeKind::ThinArrow,
            '=' => LexemeKind::ThickArrow,
            _ => unreachable!(),
        }
    }

    fn ellipsis(&mut self) -> LexemeKind {
        assert_eq!(self.bump().unwrap(), '.');
        assert_eq!(self.bump().unwrap(), '.');
        LexemeKind::Ellipsis
    }

    fn ident_alpha(&mut self) -> LexemeKind {
        self.eat_while(is_ident_alpha_cont);
        LexemeKind::Ident
    }

    fn ident_symb(&mut self) -> LexemeKind {
        self.eat_while(is_ident_symb);
        LexemeKind::Ident
    }

    fn number(&mut self, first: char) -> LexemeKind {
        match first {
            '0' => match self.first() {
                'x' => return self.hexadecimal(),
                'w' => return self.word(),
                _ => {}
            },
            _ => {}
        }
        self.decimal_or_real()
    }

    fn decimal_or_real(&mut self) -> LexemeKind {
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
                },
                'e' | 'E' => {
                    if is_real && (self.second().is_ascii_digit() || self.second() == '~') {
                        self.bump();
                        return self.exponent();                
                    } else {
                        // Numbers without a decimal point cannot have 
                        // exponents; e.g., "1e7" is illegal.
                        break;
                    }
                },
                c if c.is_ascii_digit() => {
                    self.bump();
                },
                _ => break,
            }
        }

        if is_real {
            LexemeKind::Real
        } else {
            LexemeKind::Int
        }
    }

    fn exponent(&mut self) -> LexemeKind {
        if self.first() == '~' {
            self.bump();
        }
        self.eat_while(|c| char::is_ascii_digit(&c));
        LexemeKind::Real
    }

    fn hexadecimal(&mut self) -> LexemeKind {
        assert_eq!(self.bump().unwrap(), 'x');
        self.eat_while(|c| char::is_ascii_hexdigit(&c));
        LexemeKind::Int
    }

    fn word(&mut self) -> LexemeKind {
        assert_eq!(self.bump().unwrap(), 'w');
        if self.first() == 'x' {
            self.bump();
            self.eat_while(|c| char::is_ascii_hexdigit(&c));
        } else {
            self.eat_while(|c| char::is_ascii_digit(&c));
        }
        LexemeKind::Word
    }
}
