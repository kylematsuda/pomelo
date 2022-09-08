use std::rc::Rc;

use crate::{GreenElement, GreenNode, Token};

pub fn pretty_print(node: Rc<GreenNode>) -> String {
    let mut out = String::new();
    pretty_print_node(&node, 0, &mut out);
    out
}

fn pretty_print_elt<'a>(elt: &GreenElement, level: u32, buf: &mut String) {
    match elt {
        GreenElement::Node(node) => pretty_print_node(node, level, buf),
        GreenElement::Token(token) => pretty_print_token(token, level, buf),
    }
}

fn pretty_print_node<'a>(node: &GreenNode, level: u32, buf: &mut String) {
    for _ in 0..level {
        buf.push_str("  ");
    }
    buf.push_str(&format!("{:?}@{:?}", node.kind(), node.len()));

    for child in node.children() {
        pretty_print_elt(child, level + 1, buf);
    }
}

fn pretty_print_token<'a>(token: &Token, level: u32, buf: &mut String) {
    for _ in 0..level {
        buf.push_str("  ");
    }
    buf.push_str(&format!(
        "{:?}@{:?} \"{}\"",
        token.kind(),
        token.len(),
        token.text()
    ));
}
