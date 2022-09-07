use crate::cursor::{Cursor, EOF_CHAR};
use crate::Kind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
    len: usize,
    kind: Kind,
}

#[derive(Debug, Clone)]
pub(crate) struct LexedStr<'a> {
    input: &'a str,
    tokens: Vec<Token>,
}

impl Token {
    pub fn new(len: usize, kind: Kind) -> Self {
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
                _ => Kind::LParen,
            },

            '#' => match self.first() {
                '"' => self.literal_char(),
                c if is_ident_symb(c) => self.ident_symb(),
                _ => Kind::Hash,
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
                    Kind::from_char(c).expect("non-compound reserved symbols are covered by Kind")
                }
            }

            '.' => {
                let c1 = self.first();
                let c2 = self.second();

                if c1 == '.' && c2 == '.' {
                    self.ellipsis()
                } else {
                    Kind::from_char(c).expect("non-compound reserved symbols are covered by Kind")
                }
            }

            c if is_reserved_symbol_start(c) => {
                Kind::from_char(c).expect("non-compound reserved symbols are covered by Kind")
            }
            c if is_ident_alpha_start(c) => self.ident_alpha(),
            c if is_ident_symb(c) => self.ident_symb(),
            c if is_number_start(c) => self.number(c),
            _ => Kind::Unknown,
        };

        Token::new(self.len_consumed(), kind)
    }

    fn whitespace(&mut self) -> Kind {
        self.eat_while(is_whitespace);
        Kind::Whitespace
    }

    fn comment(&mut self) -> Kind {
        assert_eq!(self.bump().unwrap(), '*');

        while let Some(c) = self.bump() {
            match c {
                '*' => {
                    if self.first() == ')' {
                        self.bump();
                        return Kind::Comment { terminated: true };
                    }
                }
                _ => {}
            }
        }
        // EOF reached
        Kind::Comment { terminated: false }
    }

    fn literal_char(&mut self) -> Kind {
        assert_eq!(self.bump().unwrap(), '"');

        match self.bump() {
            Some(EOF_CHAR) | None => return Kind::Char { terminated: false },
            _ => {}
        }

        match self.bump() {
            Some('"') => return Kind::Char { terminated: true },
            _ => return Kind::Char { terminated: false },
        }
    }

    fn literal_string(&mut self) -> Kind {
        while let Some(c) = self.bump() {
            match c {
                '"' => return Kind::String { terminated: true },

                // Handle escape sequences
                '\\' => {
                    self.bump();
                }

                _ => (),
            }
        }
        // EOF reached
        Kind::String { terminated: false }
    }

    fn number_negation(&mut self) -> Kind {
        let first = self.bump().expect("a digit follows the ~");
        self.number(first)
    }

    fn arrow(&mut self, c: char) -> Kind {
        self.bump();

        match c {
            '-' => Kind::ThinArrow,
            '=' => Kind::ThickArrow,
            _ => unreachable!(),
        }
    }

    fn ellipsis(&mut self) -> Kind {
        assert_eq!(self.bump().unwrap(), '.');
        assert_eq!(self.bump().unwrap(), '.');
        Kind::Ellipsis
    }

    fn ident_alpha(&mut self) -> Kind {
        self.eat_while(is_ident_alpha_cont);
        Kind::Ident
    }

    fn ident_symb(&mut self) -> Kind {
        self.eat_while(is_ident_symb);
        Kind::Ident
    }

    fn number(&mut self, first: char) -> Kind {
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

    fn decimal_or_real(&mut self) -> Kind {
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
            Kind::Real
        } else {
            Kind::Int
        }
    }

    fn exponent(&mut self) -> Kind {
        if self.first() == '~' {
            self.bump();
        }
        self.eat_while(|c| char::is_ascii_digit(&c));
        Kind::Real
    }

    fn hexadecimal(&mut self) -> Kind {
        assert_eq!(self.bump().unwrap(), 'x');
        self.eat_while(|c| char::is_ascii_hexdigit(&c));
        Kind::Int
    }

    fn word(&mut self) -> Kind {
        assert_eq!(self.bump().unwrap(), 'w');
        if self.first() == 'x' {
            self.bump();
            self.eat_while(|c| char::is_ascii_hexdigit(&c));
        } else {
            self.eat_while(|c| char::is_ascii_digit(&c));
        }
        Kind::Word
    }
}
