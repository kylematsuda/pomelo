pub mod infix;

#[cfg(test)]
pub(crate) mod tests {
    use crate::{Parser, SyntaxTree};
    use expect_test::Expect;

    pub(crate) fn check<F>(pass: F, should_error: bool, src: &str, expect_fixed: Expect)
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
