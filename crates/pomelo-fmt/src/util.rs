//! Utility functions for working with whitespace, comments, etc.
//!
//! Comments need to stay attached to their items.
//!
//! Extra whitespace should be respected up to a newline, but multiple newlines should probably
//! become a single newline.
//!
//! Anything else?

use pomelo_parse::ast;
// use pomelo_parse::{
//     ast,
//     language::{SyntaxNode, SyntaxToken},
//     SyntaxKind,
// };
// use rowan::NodeOrToken;
// use std::mem;

use crate::{printer::Printer, Printable};

impl Printer {
    pub fn match_rules(&mut self, match_: &ast::Match, is_fn: bool) -> Option<()> {
        for (i, rule) in match_.mrules().enumerate() {
            if i > 0 {
                self.linebreak();
            }
            self.igroup(0);
            if i == 0 {
                if !is_fn {
                    self.space();
                }
            } else {
                if is_fn {
                    self.space();
                }
                self.text("|");
            }
            self.space();
            self.pat(&rule.pat()?)?;
            self.space();
            self.text("=>");
            self.space_indent();
            self.expr(&rule.expr()?)?;
            self.endgroup();
        }
        Some(())
    }

    pub(crate) fn list_like<T: Printable>(
        &mut self,
        open_bracket: &'static str,
        close_bracket: &'static str,
        delimiter: &'static str,
        ends_spaced: bool,
        items: impl Iterator<Item = T>,
        item_count: usize,
    ) -> Option<()> {
        let end_breaks = |p: &mut Printer| {
            if ends_spaced {
                p.space();
            } else {
                p.zerobreak();
            }
        };

        self.cgroup(0);
        self.cgroup(crate::INDENT);
        self.text(open_bracket);
        end_breaks(self);
        for (i, item) in items.enumerate() {
            self.cgroup(0);
            item.print(self);
            if i < item_count - 1 {
                self.zerobreak();
                self.text(delimiter);
                self.endgroup();
                self.space();
            } else {
                self.endgroup();
            }
        }
        self.endgroup();
        end_breaks(self);
        self.text(close_bracket);
        self.endgroup();

        Some(())
    }
}

// #[derive(Debug, Clone)]
// pub enum ChildKind {
//     Node(SyntaxNode),
//     Trivia(TriviaKind),
//     NontriviaToken,
// }
//
// #[derive(Debug, Clone)]
// pub enum TriviaKind {
//     Space,
//     Newline,
//     Comment(SyntaxToken),
// }
//
// #[derive(Debug, Clone)]
// pub struct NodeWithTrivia {
//     node: SyntaxNode,
//     trivia: Vec<TriviaKind>,
// }
//
// /// Attach trivia tokens to their owning nodes.
// ///
// /// No idea yet if this is how we want to do it...
// pub fn child_nodes_with_trivia(syntax: &SyntaxNode) -> Vec<NodeWithTrivia> {
//     let mut out = vec![];
//
//     let mut current_node = None;
//     let mut current_trivia = vec![];
//
//     for child in children_no_dedup(syntax) {
//         match child {
//             // Ignore these -- we'll handle them separately
//             ChildKind::NontriviaToken | ChildKind::Trivia(TriviaKind::Space) => {}
//             ChildKind::Node(node) => {
//                 if let Some(n) = current_node.take() {
//                     out.push(NodeWithTrivia {
//                         node: n,
//                         trivia: mem::take(&mut current_trivia),
//                     });
//                 }
//                 current_node = Some(node);
//             }
//             ChildKind::Trivia(TriviaKind::Comment(c)) => {
//                 current_trivia.push(TriviaKind::Comment(c));
//             }
//             ChildKind::Trivia(TriviaKind::Newline) => {
//                 // We will ignore newlines that aren't a part of a current node.
//                 if let Some(n) = current_node.take() {
//                     current_trivia.push(TriviaKind::Newline);
//
//                     out.push(NodeWithTrivia {
//                         node: n,
//                         trivia: mem::take(&mut current_trivia),
//                     });
//                 }
//             }
//         }
//     }
//     out
// }
//
// fn children_no_dedup(syntax: &SyntaxNode) -> impl Iterator<Item = ChildKind> {
//     syntax.children_with_tokens().map(|child| {
//         match child {
//             NodeOrToken::Node(n) => ChildKind::Node(n),
//             NodeOrToken::Token(n) => match n.kind() {
//                 SyntaxKind::WHITESPACE => {
//                     // Currently, lexer makes each whitespace character its own token.
//                     assert_eq!(n.text_range().len(), 1.into());
//                     let c = n.text().chars().next().unwrap();
//                     let kind = match c {
//                         ' ' => TriviaKind::Space,
//                         // TODO: Carriage returns??
//                         '\n' => TriviaKind::Newline,
//                         _ => unreachable!(),
//                     };
//                     ChildKind::Trivia(kind)
//                 }
//                 SyntaxKind::COMMENT => ChildKind::Trivia(TriviaKind::Comment(n)),
//                 _ => ChildKind::NontriviaToken,
//             },
//         }
//     })
// }
