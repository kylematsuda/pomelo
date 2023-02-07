//! Validation passes to run after the AST is constructed.
//!
//! This doesn't do anything yet...
//!
//! TODO: add some passes for several things
//! - Checking record type labels to be numeric (+ positive), etc.
//! - Checking infix precedence is a single digit
//! - Checking unterminated strs, etc.
use crate::SyntaxTree;

/// Apply validation passes, etc.
///
/// This should output a list of errors or something,
/// but importantly should take `&SyntaxTree` since we probably want to
/// defer any semantic analysis to later
pub fn apply_passes(_tree: &SyntaxTree) {}

#[cfg(test)]
pub(crate) mod tests {
    use crate::{Parser, SyntaxTree};
    use expect_test::Expect;

    pub(crate) fn _check<F>(pass: F, should_error: bool, src: &str, expect_fixed: Expect)
    where
        F: Fn(SyntaxTree) -> SyntaxTree,
    {
        let parser = Parser::new(src);
        let tree = parser.parse();

        let fixed_tree = pass(tree);
        let fixed: String = format!("{}", fixed_tree);
        expect_fixed.assert_eq(&fixed);

        assert_eq!(fixed_tree.has_errors(), should_error);
    }
}
