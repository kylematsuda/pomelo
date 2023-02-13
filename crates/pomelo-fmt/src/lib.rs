//! Code formatting for SML.
//!
//! Currently, only dealing with the Core language, as [`pomelo_parse`](../pomelo_parse/index.html)
//! does not know how to handle modules.
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
//! dtolnay
//!
//! Not much done yet!

mod buffer;
mod printer;

mod dec;
mod expr;
mod pat;
mod ty;

use pomelo_parse::{ast, SyntaxTree, AstNode};

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
