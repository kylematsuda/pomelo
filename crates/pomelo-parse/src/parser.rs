use pomelo_lex::{lex, LexToken};
use pomelo_syntax::{GreenNodeBuilder, SyntaxKind, Token};

use crate::LpError;

use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug, Clone)]
pub struct Input(Vec<Token>, Vec<LpError>);

impl Input {
    pub fn from_lexed<'a>(src: &'a str, lex_tokens: &'a [LexToken]) -> Self {
        let mut pos = 0;
        let mut tokens = vec![];
        let mut errs = vec![];

        for t in lex_tokens { 
            let len = t.len();
            let (tok, err) = Token::convert(&src[pos..pos+len], t.kind()); 

            tokens.push(tok);
            if let Some(e) = err {
                let e = LpError::from_lex(e, (pos, pos + len)); 
                errs.push(e);
            }
            pos += len;
        }
        Self(tokens, errs)
    }
}

#[derive(Debug, Clone)]
pub enum Event {
    Start { kind: SyntaxKind },
    Token { kind: SyntaxKind, text: String },
    Finish,
    Error { msg: String, span: (usize, usize) },
}

impl Event {
    pub fn tombstone() -> Self {
        Self::Start { kind: SyntaxKind::TOMBSTONE }
    }
}

/// This closely follows the impl in rust-analyzer
#[derive(Debug, Clone)]
pub struct Parser<'i> {
    tokens: Peekable<Iter<'i, Token>>,
    events: Vec<Event>,
}

impl<'i> Parser<'i> {
    pub fn new(input: &'i Input) -> Self {
        Self { tokens: input.0.iter().peekable(), events: Vec::new() }
    }

    pub fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            let (kind, text) = self.bump().into_parts(); 
            self.events.push(Event::Token { kind, text });
            true
        } else {
            false 
        }
    }

    pub fn eat_any(&mut self) {   
        let (kind, text) = self.bump().into_parts(); 
        self.events.push(Event::Token { kind, text });
    }

    pub fn bump(&mut self) -> Token {
        self.tokens.next().map(Token::clone)
            .unwrap_or(Token::new(SyntaxKind::EOF, ""))
    }

    pub fn is_eof(&mut self) -> bool {
        self.at(SyntaxKind::EOF)
    }

    pub fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek() == kind
    }

    pub fn peek(&mut self) -> SyntaxKind {
        self.tokens.peek().map(|t| t.kind()).unwrap_or(SyntaxKind::EOF)
    }

    #[must_use]
    pub fn start(&mut self) -> Marker {
        self.events.push(Event::tombstone());
        Marker { pos: self.events.len() - 1 } 
    }

    pub fn abandon(&mut self, m: Marker) {
        if m.pos == self.events.len() - 1 {
            match self.events.pop() {
                Some(Event::Start { kind: SyntaxKind::TOMBSTONE }) => (),
                _ => unreachable!()
            }
        }
    }

    pub fn complete(&mut self, m: Marker, kind: SyntaxKind) -> CompletedMarker {
        self.events.push(Event::Finish);

        let e = &mut self.events[m.pos];

        match e {
            Event::Start { .. } => { 
                *e = Event::Start { kind }; 
            }
            _ => unreachable!()
        }

        CompletedMarker { pos: m.pos, kind }
    }

    pub fn error(&mut self, msg: String, span: (usize, usize)) {
        self.events.push(Event::Error { msg, span })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Marker {
    pos: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct CompletedMarker {
    pos: usize,
    kind: SyntaxKind,
}
