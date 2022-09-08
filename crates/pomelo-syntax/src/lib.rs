//! Syntax tree definition for `pomelo`
//!
//! This is a concrete syntax tree based on the design from
//! [rust-analyzer](https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/syntax.md).
//! (Currently, this is the simplified version discussed in the linked docs.)

pub mod kind;
pub use kind::SyntaxKind;

pub mod pretty_print;
pub use pretty_print::pretty_print;

use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GreenElement {
    Node(GreenNode),
    Token(Token),
}

impl GreenElement {
    pub fn len(&self) -> usize {
        match self {
            Self::Node(node) => node.len(),
            Self::Token(token) => token.len(),
        }
    }
}

impl From<GreenNode> for GreenElement {
    fn from(node: GreenNode) -> Self {
        Self::Node(node)
    }
}

impl From<Token> for GreenElement {
    fn from(token: Token) -> Self {
        Self::Token(token)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GreenNode {
    kind: SyntaxKind,
    text_len: usize,
    children: Vec<Rc<GreenElement>>,
}

impl GreenNode {
    pub fn new(kind: SyntaxKind, children: impl IntoIterator<Item = GreenElement>) -> Self {
        let mut text_len = 0;
        let mut cs = vec![];

        for child in children {
            text_len += child.len();
            cs.push(Rc::new(child));
        }

        Self {
            kind,
            text_len,
            children: cs,
        }
    }

    pub fn len(&self) -> usize {
        self.text_len
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn children(&self) -> impl Iterator<Item = &Rc<GreenElement>> {
        self.children.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    kind: SyntaxKind,
    text: String,
}

impl Token {
    pub fn new(kind: SyntaxKind, text: &str) -> Self {
        Self {
            kind,
            text: String::from(text),
        }
    }

    pub fn len(&self) -> usize {
        self.text.len()
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn text(&self) -> &str {
        &self.text
    }
}

/// Builder for a Green tree.
///
/// This is based directly on the implementation from
/// [`rowan`](https://github.com/rust-analyzer/rowan),
/// though no caching is done for simplicity.
#[derive(Debug, Clone, Default)]
pub struct GreenNodeBuilder {
    parents: Vec<(SyntaxKind, usize)>,
    children: Vec<GreenElement>,
}

impl GreenNodeBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Start building a node.
    pub fn start_node(&mut self, kind: SyntaxKind) {
        let pos = self.children.len();
        self.parents.push((kind, pos));
    }

    /// Finish the current node and start along the next branch.
    pub fn finish_node(&mut self) {
        let (kind, first_child) = self.parents.pop().unwrap();
        let children = self.children.drain(first_child..);
        let node = GreenNode::new(kind, children);
        self.children.push(node.into());
    }

    /// Push a token (leaf) onto the tree.
    pub fn token(&mut self, kind: SyntaxKind, text: &str) {
        self.children.push(Token::new(kind, text).into())
    }

    /// Finish the syntax tree.
    ///
    /// Panics: if there was an unequal number of calls
    /// to `start_node` and `finish_node`.
    pub fn finish(mut self) -> GreenNode {
        assert_eq!(self.children.len(), 1);
        match self.children.pop().unwrap() {
            GreenElement::Node(node) => node,
            GreenElement::Token(_) => panic!(),
        }
    }
}
