//! Code formatting for SML.
//!
//! This is intended for use internally within `pomelo`.
//! Beware, this currently discards comments. Not suitable for real code!
//!
//! This is intended to format the pretty-printed HIR, which is itself valid SML.
//! Currently, only dealing with the Core language, as [`pomelo_parse`](../pomelo_parse/index.html)
//! does not know how to handle modules.
//!
//! # Status
//!
//! - [x] format expressions (mostly done)
//! - [ ] format patterns
//! - [ ] format declarations
//! - [ ] format types
//! - [ ] deal with comments and other trivia.. maybe need to do a better job of attaching trivia to nodes in parsing?
//!
//! # Resources
//!
//! - ["Prettyprinting"](https://dl.acm.org/doi/pdf/10.1145/357114.357115), Derek C. Oppen (1980)
//!
//! - ["A prettier printer"](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf), Phillip Wadler (2003)
//!
//! - ["Pretty Printing with Lazy Dequeues"](https://dl.acm.org/doi/pdf/10.1145/1053468.1053473), Olaf Chitil
//! (2005)
//!
//! - [`prettyplease`](https://github.com/dtolnay/prettyplease), an implementation of Oppen's algorithm in Rust by
//! David Tolnay
//!
//! The code here is based on the imperative algorithm from the Oppen paper.
//! The other papers give pure functional versions of the pretty printing algorithm -- I found
//! those a bit easier to understand than the Oppen paper but the imperative algorithm should be
//! easier to implement efficiently in Rust.
//!
//! David Tolnay's `prettyplease` crate is a nice implementation of Oppen's algorithm in Rust.
//! I mainly worked off of the description in Oppen's paper, but I peeked at `prettyplease`
//! a couple of times to help iron out some of the bugs in my implementation.
mod buffer;
mod printer;
use crate::printer::Printer;

mod dec;
mod expr;
mod pat;
mod ty;

mod util;

#[cfg(test)]
mod tests;

use pomelo_parse::{ast, AstNode, SyntaxTree};

const INDENT: isize = 2;

/// Pretty print the AST.
pub fn print(ast: SyntaxTree) -> String {
    // TODO: handle errors
    let _errors = ast.errors().cloned();
    let node = ast::File::cast(ast.syntax()).unwrap();

    let mut output = String::new();
    for dec in node.declarations() {
        output.push_str(&dec::print_dec(&dec));
    }
    output
}

trait Printable {
    fn print(&self, printer: &mut Printer) -> Option<()>;
}
