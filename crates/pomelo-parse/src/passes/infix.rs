use crate::{ast, AstNode, SyntaxNode, SyntaxTree};

use std::collections::HashMap;

#[rustfmt::skip]
const BUILTINS: [(&'static str, Fixity); 18] = [
    ("*",		Fixity { val: 7, assoc: Associativity::Left }),
    ("/",		Fixity { val: 7, assoc: Associativity::Left }),
    ("div",		Fixity { val: 7, assoc: Associativity::Left }),
    ("mod",		Fixity { val: 7, assoc: Associativity::Left }),
    ("+",		Fixity { val: 6, assoc: Associativity::Left }),
    ("-",		Fixity { val: 6, assoc: Associativity::Left }),
    ("^",		Fixity { val: 6, assoc: Associativity::Left }),
    ("::",		Fixity { val: 5, assoc: Associativity::Right }),
    ("@",		Fixity { val: 5, assoc: Associativity::Right }),
    ("=",		Fixity { val: 4, assoc: Associativity::Left }),
    ("<>",		Fixity { val: 4, assoc: Associativity::Left }),
    (">",		Fixity { val: 4, assoc: Associativity::Left }),
    (">=",		Fixity { val: 4, assoc: Associativity::Left }),
    ("<",		Fixity { val: 4, assoc: Associativity::Left }),
    ("<=",		Fixity { val: 4, assoc: Associativity::Left }),
    (":=",		Fixity { val: 3, assoc: Associativity::Left }),
    ("o",		Fixity { val: 3, assoc: Associativity::Left }),
    ("before",	Fixity { val: 0, assoc: Associativity::Left }),
];

type Context = HashMap<String, Fixity>;

struct Fixity {
    val: u8,
    assoc: Associativity,
}

impl Fixity {
    fn priority(&self) -> (u8, u8) {
        let base = self.val * 2;
        match self.assoc {
            Associativity::Left => (base, base + 1),
            Associativity::Right => (base + 1, base),
        }
    }
}

enum Associativity {
    Left,
    Right,
}

pub fn resolve_infixed(tree: SyntaxTree) -> SyntaxTree {
    let mut ctx = BUILTINS.into_iter()
        .map(|(s, f)| (String::from(s), f))
        .collect::<Context>();

    let node = tree.syntax();

    fix_infix(node.clone(), &mut ctx);

    todo!()
}

fn find_infix_or_app_expr_parent(node: SyntaxNode) -> Option<SyntaxNode> {
    if ast::InfixOrAppExpr::cast(node.clone()).is_some() {
        Some(node)
    } else {
        for c in node.children() {
            if let Some(n) = find_infix_or_app_expr_parent(c) {
                return Some(n);
            }
        }
        None
    }
}

fn fix_infix(infix_or_app_expr: SyntaxNode, ctx: &mut Context) {
}

#[cfg(test)]
mod tests {
    use crate::{passes::infix::find_infix_or_app_expr_parent, Parser};

    #[test]
    fn basic() {
        let input = "val a = 1 + 2";
        let tree = Parser::new(input).parse();

        let node = tree.syntax();

        eprintln!("{:?}", find_infix_or_app_expr_parent(node).unwrap());
    }
}
