use crate::Error;
use crate::{SyntaxElement, SyntaxKind, SyntaxNode};

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use rowan::{GreenNode, GreenNodeBuilder};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token<'a> {
    kind: SyntaxKind,
    text: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(kind: SyntaxKind, text: &'a str) -> Self {
        Self { kind, text }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    fn from_lex_token(
        lex: pomelo_lex::LexToken,
        src: &'a str,
        offset: usize,
    ) -> (Self, Option<crate::Error>) {
        let text = &src[offset..offset + lex.len()];
        let (kind, opt_err_msg) = SyntaxKind::from_lexed(lex.kind(), text);

        let opt_err = if let Some(msg) = opt_err_msg {
            Some(Error::new(msg, text, offset))
        } else {
            None
        };

        (Self::new(kind, text), opt_err)
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxTree {
    node: GreenNode,
    errors: Vec<Error>,
}

impl SyntaxTree {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.node.clone())
    }

    pub fn errors(&self) -> impl Iterator<Item = &Error> {
        self.errors.iter()
    }

    pub fn has_errors(&self) -> bool {
        self.errors.len() != 0
    }
}

impl fmt::Display for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn pretty_print(
            f: &mut fmt::Formatter<'_>,
            indent: usize,
            element: SyntaxElement,
        ) -> fmt::Result {
            write!(f, "{:indent$}", "", indent = indent)?;
            match element {
                rowan::NodeOrToken::Node(node) => {
                    writeln!(f, "{:?}", node)?;
                    for c in node.children_with_tokens() {
                        pretty_print(f, indent + 2, c)?;
                    }
                    Ok(())
                }
                rowan::NodeOrToken::Token(token) => match token.kind() {
                    // Don't show whitespace text when formatting the tree
                    SyntaxKind::WHITESPACE => {
                        writeln!(f, "{:?}@{:?}", token.kind(), token.text_range())
                    }
                    _ => writeln!(f, "{:?}", token),
                },
            }
        }
        // Ignore errors for now!
        pretty_print(f, 0, SyntaxElement::Node(self.syntax()))
    }
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    current_pos: usize,
    /// Tokens are stored in reverse order
    tokens: Vec<Token<'a>>,
    errors: Vec<Error>,
    builder: Rc<RefCell<GreenNodeBuilder<'static>>>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        let lex_tokens = pomelo_lex::lex(src);

        let mut offset = 0;
        for lex_tok in lex_tokens {
            let lex_tok_len = lex_tok.len();

            let (token, opt_err) = Token::from_lex_token(lex_tok, src, offset);
            tokens.push(token);

            if let Some(err) = opt_err {
                errors.push(err);
            }

            offset += lex_tok_len;
        }
        tokens.reverse(); // Do this so we can pop tokens off the back of the vector efficiently

        let builder = Rc::new(RefCell::new(GreenNodeBuilder::default()));

        Self {
            current_pos: 0,
            tokens,
            errors,
            builder,
        }
    }

    pub fn peek(&self) -> SyntaxKind {
        self.peek_nth(0)
    }

    /// n = 0 is self.peek()
    pub fn peek_nth(&self, n: usize) -> SyntaxKind {
        self.tokens
            .iter()
            .rev()
            .nth(n)
            .map(Token::kind)
            .unwrap_or(SyntaxKind::EOF)
    }

    /// Peeks past `skip` nontrivia tokens, then peeks at the next one.
    pub fn peek_next_nontrivia(&self, skip: usize) -> SyntaxKind {
        self.peek_token_next_nontrivia(skip)
            .map(Token::kind)
            .unwrap_or(SyntaxKind::EOF)
    }

    pub fn peek_token(&self) -> Option<&Token> {
        self.tokens.last()
    }

    pub fn peek_text(&self) -> &str {
        self.peek_token().map(Token::text).unwrap_or("\0")
    }

    /// Peeks past `skip` nontrivia tokens, then peeks at the next one.
    pub fn peek_token_next_nontrivia(&self, skip: usize) -> Option<&Token> {
        self.tokens
            .iter()
            .rev()
            .filter(|t| !t.kind().is_trivia())
            .skip(skip)
            .next()
    }

    pub fn eat(&mut self, kind: SyntaxKind) -> bool {
        if kind == self.peek() {
            let token = self.pop();
            self.push_token(token);
            true
        } else {
            false
        }
    }

    pub fn eat_any(&mut self) -> SyntaxKind {
        let token = self.pop();
        let kind = token.kind();
        self.push_token(token);
        kind
    }

    pub fn eat_trivia(&mut self) -> bool {
        let mut eaten = false;

        while !self.is_eof() {
            if self.peek().is_trivia() {
                self.eat_any();
                eaten = true;
            } else {
                break;
            }
        }
        eaten
    }

    /// Eats the current token if it matches `kind`, returning `true`.
    ///
    /// Otherwise, if the current token is trivia,
    /// peeks through to the next non-trivia token.
    /// If it matches `kind`, eats the trivia and the target token and returns `true`.
    /// If it doesn't match `kind`, eats nothing and returns `false`.
    ///
    /// Else eats nothing and returns `false`.
    pub fn eat_through_trivia(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            true
        } else if self.peek_next_nontrivia(0) == kind {
            self.eat_trivia();
            return self.eat(kind);
        } else {
            false
        }
    }

    pub fn expect(&mut self, kind: SyntaxKind) {
        if !self.eat(kind) {
            self.error(format!("expected {:?}", kind))
        }
    }

    pub fn error(&mut self, msg: impl Into<String> + Clone) {
        let pos = self.current_pos;
        let text = self.peek_token().map(|t| t.text()).unwrap_or("").to_owned();

        self.errors.push(Error::new(msg.clone(), text, pos));

        // Push into syntax tree as well for now
        self.push_token(Token::new(SyntaxKind::ERROR, ""))
    }

    pub fn eat_mapped(&mut self, kind: SyntaxKind) -> SyntaxKind {
        let token = self.pop();
        let mapped = Token::new(kind, token.text());
        self.push_token(mapped);
        token.kind()
    }

    fn pop(&mut self) -> Token<'a> {
        match self.tokens.pop() {
            Some(t) => {
                self.current_pos += t.text().len();
                t
            }
            None => Token::new(SyntaxKind::EOF, ""),
        }
    }

    pub fn is_eof(&self) -> bool {
        self.peek() == SyntaxKind::EOF
    }

    /// Check if the current token is a valid VId.
    ///
    /// With the current lexing strategy, correct
    /// symbolic identifiers are caught at lexing stage.
    /// Thus, the only special one we need to check for is EQ.
    ///
    /// However, not sure if this is a good strategy for being
    /// error-resilient. The lexer obviously has less context than
    /// the parser for determining what to do if there is an error.
    /// This may be generally an issue with gluing together tokens at
    /// lex-time (like "=>" as THICK_ARROW, "..." as ELLIPSIS, etc.)
    pub fn is_vid(&self) -> bool {
        match self.peek() {
            SyntaxKind::IDENT | SyntaxKind::EQ => true,
            _ => false,
        }
    }

    /// Check if the current token is a valid StrId.
    pub fn is_strid(&self) -> bool {
        let t = self.peek_token();

        if let Some(t) = t {
            match t.kind() {
                SyntaxKind::IDENT => t.text().chars().all(char::is_alphanumeric),
                _ => false,
            }
        } else {
            false
        }
    }

    #[must_use]
    pub fn start_node(&mut self, kind: SyntaxKind) -> NodeGuard {
        self.builder.borrow_mut().start_node(kind.into());
        NodeGuard {
            builder: self.builder.clone(),
        }
    }

    #[must_use]
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint(self.builder.borrow().checkpoint())
    }

    #[must_use]
    pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) -> NodeGuard {
        self.builder
            .borrow_mut()
            .start_node_at(checkpoint.0, kind.into());
        NodeGuard {
            builder: self.builder.clone(),
        }
    }

    pub fn push_token(&mut self, token: Token) {
        self.builder
            .borrow_mut()
            .token(token.kind().into(), token.text())
    }

    pub fn parse(self) -> SyntaxTree {
        self.parse_inner(crate::grammar::source_file)
    }

    /// For testing
    pub(crate) fn parse_inner<F>(mut self, mut f: F) -> SyntaxTree
    where
        F: FnMut(&mut Parser),
    {
        f(&mut self);

        SyntaxTree {
            node: self.builder.take().finish(),
            errors: self.errors,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Checkpoint(rowan::Checkpoint);

#[derive(Debug, Clone)]
pub struct NodeGuard {
    builder: Rc<RefCell<GreenNodeBuilder<'static>>>,
}

impl Drop for NodeGuard {
    fn drop(&mut self) {
        self.builder.borrow_mut().finish_node();
    }
}
