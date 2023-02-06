//! Defines the parse tokens, parser interface, and output syntax tree.
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use rowan::{GreenNode, GreenNodeBuilder};

use crate::Error;
use crate::{
    language::{SyntaxElement, SyntaxNode},
    SyntaxKind,
};

/// A parsed token.
///
/// Consists of a [`crate::SyntaxKind`] and a reference to the corresponding
/// span of the source code.
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
        self.text
    }

    fn from_lex_token(
        lex: pomelo_lex::LexToken,
        src: &'a str,
        offset: usize,
    ) -> (Self, Option<crate::Error>) {
        let text = &src[offset..offset + lex.len()];
        let (kind, opt_err_msg) = SyntaxKind::from_lexed(lex.kind(), text);
        let opt_err = opt_err_msg.map(|msg| Error::new(msg, text, offset));
        (Self::new(kind, text), opt_err)
    }
}

/// Output of the parsing stage.
///
/// Holds a [`rowan::GreenNode`] representing the root of the tree,
/// and any errors encountered during parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxTree {
    node: GreenNode,
    errors: Vec<Error>,
}

impl SyntaxTree {
    pub fn new(node: GreenNode, errors: Vec<Error>) -> Self {
        Self { node, errors }
    }

    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.node.clone())
    }

    pub fn errors(&self) -> impl Iterator<Item = &Error> {
        self.errors.iter()
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn node(&self) -> GreenNode {
        self.node.clone()
    }

    pub fn into_parts(self) -> (GreenNode, Vec<Error>) {
        (self.node, self.errors)
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
                    writeln!(f, "{node:?}")?;
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
                    _ => writeln!(f, "{token:?}"),
                },
            }
        }
        // Ignore errors for now!
        pretty_print(f, 0, SyntaxElement::Node(self.syntax()))
    }
}

/// The main interface for manipulating the lexed source.
///
/// This handles iterating over the lex tokens as well as building the syntax tree
/// (see [`rowan::GreenNodeBuilder`](https://docs.rs/rowan/latest/rowan/struct.GreenNodeBuilder.html)
/// for details).
#[derive(Debug, Clone)]
pub struct Parser<'a> {
    current_pos: usize,
    /// Tokens are stored in reverse order
    tokens: Vec<Token<'a>>,
    errors: Vec<Error>,
    builder: NodeBuilder,
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

        let builder = NodeBuilder::new();

        Self {
            current_pos: 0,
            tokens,
            errors,
            builder,
        }
    }

    /// Parse an entire source file.
    pub fn parse(self) -> SyntaxTree {
        self.parse_inner(crate::grammar::source_file)
    }

    /// Parse a single expression.
    pub fn parse_expr(self) -> SyntaxTree {
        self.parse_inner(crate::grammar::expression)
    }

    /// Parse a single pattern.
    pub fn parse_pat(self) -> SyntaxTree {
        self.parse_inner(crate::grammar::pattern)
    }

    /// Parse a single type.
    pub fn parse_type(self) -> SyntaxTree {
        self.parse_inner(crate::grammar::ty)
    }

    /// Parse a single declaration.
    pub fn parse_dec(self) -> SyntaxTree {
        self.parse_inner(crate::grammar::declaration)
    }
}

// Parsing utilities
impl<'a> Parser<'a> {
    /// Parse according to a specified parsing function `f`.
    ///
    /// This is here for testing purposes.
    pub(crate) fn parse_inner<F>(mut self, mut f: F) -> SyntaxTree
    where
        F: FnMut(&mut Parser),
    {
        f(&mut self);

        SyntaxTree {
            node: self.builder.finish(),
            errors: self.errors,
        }
    }

    /// Peek at the kind of the next token.
    pub(crate) fn peek(&self) -> SyntaxKind {
        self.peek_nth(0)
    }

    /// Peek ahead `n` tokens.
    pub(crate) fn peek_nth(&self, n: usize) -> SyntaxKind {
        self.tokens
            .iter()
            .rev()
            .nth(n)
            .map(Token::kind)
            .unwrap_or(SyntaxKind::EOF)
    }

    /// Skips past `skip` nontrivia tokens, then peeks at the kind of the next one.
    pub(crate) fn peek_next_nontrivia(&self, skip: usize) -> SyntaxKind {
        self.peek_token_next_nontrivia(skip)
            .map(Token::kind)
            .unwrap_or(SyntaxKind::EOF)
    }

    /// Peek at the next token.
    pub(crate) fn peek_token(&self) -> Option<&Token> {
        self.tokens.last()
    }

    /// Peek at the text of the next token.
    pub(crate) fn peek_text(&self) -> &str {
        self.peek_token().map(Token::text).unwrap_or("\0")
    }

    /// Peeks past `skip` nontrivia tokens, then peeks at the next token.
    pub(crate) fn peek_token_next_nontrivia(&self, skip: usize) -> Option<&Token> {
        self.tokens
            .iter()
            .rev()
            .filter(|t| !t.kind().is_trivia())
            .nth(skip)
    }

    /// If `kind` matches the next token kind, consumes the token and returns true.
    /// Else, returns false.
    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if kind == self.peek() {
            let token = self.pop();
            self.push_token(token);
            true
        } else {
            false
        }
    }

    /// Consume the next token, regardless of its kind.
    pub(crate) fn eat_any(&mut self) -> SyntaxKind {
        let token = self.pop();
        let kind = token.kind();
        self.push_token(token);
        kind
    }

    /// While the next token is trivia, consume it.
    pub(crate) fn eat_trivia(&mut self) -> bool {
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

    /// If the next nontrivia token matches `kind`, consume it (and any leading trivia) and
    /// return true. Else returns false.
    pub(crate) fn eat_through_trivia(&mut self, kind: SyntaxKind) -> bool {
        if self.peek_next_nontrivia(0) == kind {
            self.eat_trivia();
            assert!(self.eat(kind));
            true
        } else {
            false
        }
    }

    /// Consume the next token if it matches `kind`, else generate an error.
    pub(crate) fn expect(&mut self, kind: SyntaxKind) {
        if !self.eat(kind) {
            self.error(format!("expected {kind:?}"))
        }
    }

    /// Append a new error to the stored list of errors.
    pub(crate) fn error(&mut self, msg: impl Into<String> + Clone) {
        let pos = self.current_pos;
        let text = self.peek_token().map(|t| t.text()).unwrap_or("").to_owned();

        self.errors.push(Error::new(msg, text, pos));

        // Push into syntax tree as well for now
        self.push_token(Token::new(SyntaxKind::ERROR, ""))
    }

    /// Consume the next token but remap its `SyntaxKind` to be `kind`.
    pub(crate) fn eat_mapped(&mut self, kind: SyntaxKind) -> SyntaxKind {
        let token = self.pop();
        let mapped = Token::new(kind, token.text());
        self.push_token(mapped);
        token.kind()
    }

    pub(crate) fn is_eof(&self) -> bool {
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
    pub(crate) fn is_vid(&self) -> bool {
        matches!(self.peek(), SyntaxKind::IDENT | SyntaxKind::EQ)
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
    pub(crate) fn next_nontrivia_is_vid(&self) -> bool {
        matches!(
            self.peek_next_nontrivia(0),
            SyntaxKind::IDENT | SyntaxKind::EQ
        )
    }

    /// Check if the current token is a valid StrId.
    pub(crate) fn is_strid(&self) -> bool {
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

    fn pop(&mut self) -> Token<'a> {
        match self.tokens.pop() {
            Some(t) => {
                self.current_pos += t.text().len();
                t
            }
            None => Token::new(SyntaxKind::EOF, ""),
        }
    }
}

// Syntax tree builder
impl<'a> Parser<'a> {
    /// Start a new node in the syntax tree.
    ///
    /// Note that the returned [`NodeGuard`] will be dropped immediately if not
    /// bound to a variable.
    #[must_use]
    pub(crate) fn start_node(&mut self, kind: SyntaxKind) -> NodeGuard {
        self.builder.start_node(kind)
    }

    /// Set a checkpoint that can be used later to create a node earlier in the tree.
    ///
    /// This is essentially just lookahead. Make an example?
    #[must_use]
    pub(crate) fn checkpoint(&self) -> Checkpoint {
        self.builder.checkpoint()
    }

    /// Use a previously set `checkpoint` to create a new node at its position (higher up in the tree).
    ///
    /// Note that the returned [`NodeGuard`] will be dropped immediately if not
    /// bound to a variable.
    #[must_use]
    pub(crate) fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) -> NodeGuard {
        self.builder.start_node_at(checkpoint, kind)
    }

    /// Add a token to the current node.
    pub fn push_token(&mut self, token: Token) {
        self.builder.push_token(token)
    }
}

/// A wrapper for [`rowan::GreenNodeBuilder`].
#[derive(Debug, Clone)]
pub(crate) struct NodeBuilder(Rc<RefCell<GreenNodeBuilder<'static>>>);

impl NodeBuilder {
    pub(crate) fn new() -> Self {
        Self(Rc::new(RefCell::new(GreenNodeBuilder::default())))
    }

    #[must_use]
    pub(crate) fn start_node(&mut self, kind: SyntaxKind) -> NodeGuard {
        self.0.borrow_mut().start_node(kind.into());
        NodeGuard {
            builder: self.0.clone(),
        }
    }

    #[must_use]
    pub(crate) fn checkpoint(&self) -> Checkpoint {
        Checkpoint(self.0.borrow().checkpoint())
    }

    #[must_use]
    pub(crate) fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) -> NodeGuard {
        self.0.borrow_mut().start_node_at(checkpoint.0, kind.into());
        NodeGuard {
            builder: self.0.clone(),
        }
    }

    pub(crate) fn push_token(&mut self, token: Token) {
        self.0.borrow_mut().token(token.kind().into(), token.text())
    }

    pub(crate) fn finish(self) -> GreenNode {
        self.0.take().finish()
    }
}

/// A wrapper for [`rowan::Checkpoint`].
#[derive(Debug, Clone)]
pub(crate) struct Checkpoint(rowan::Checkpoint);

/// A guard to avoid forgetting to call `builder.finish_node()`.
///
/// This nice RAII trick is copied from
/// [`apollo-parser`](https://docs.rs/apollo-parser/latest/apollo_parser/).
#[derive(Debug, Clone)]
pub(crate) struct NodeGuard {
    builder: Rc<RefCell<GreenNodeBuilder<'static>>>,
}

impl Drop for NodeGuard {
    fn drop(&mut self) {
        self.builder.borrow_mut().finish_node();
    }
}
