use crate::{ast, AstNode, SyntaxNode, SyntaxTree};

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

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

struct Context(HashMap<String, Fixity>);

impl Context {
    fn new_with_builtins() -> Self {
        Self(
            BUILTINS
                .into_iter()
                .map(|(s, f)| (String::from(s), f))
                .collect(),
        )
    }
}

impl Deref for Context {
    type Target = HashMap<String, Fixity>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Context {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Associativity {
    Left,
    Right,
}

pub fn resolve_infixed(tree: SyntaxTree) -> SyntaxTree {
    let mut ctx = Context::new_with_builtins();

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

fn fix_infix(infix_or_app_expr: SyntaxNode, ctx: &mut Context) {}

#[cfg(test)]
mod tests {
    use crate::{
        ast::AtomicExpr, ast::Expr, ast::InfixOrAppExpr,
        passes::infix::find_infix_or_app_expr_parent, passes::infix::Context, AstNode, Parser,
    };

    #[test]
    fn scratch_infix() {
        let ctx = Context::new_with_builtins();

        let input = "val a = 1 + 2";
        let tree = Parser::new(input).parse();

        let node = tree.syntax();

        // DFS to find unresolved node
        let infix = find_infix_or_app_expr_parent(node.clone()).unwrap();
        // Cast to ast node
        let infix = InfixOrAppExpr::cast(infix).unwrap();

        eprintln!("{:?}", infix);

        // Iter on contained exprs
        // Todo: replace this with Pratt parsing
        for e in infix.exprs() {
            if let Expr::Atomic(AtomicExpr::VId(e)) = e {
                let op = e.syntax().text().to_string();

                if let Some(fixity) = ctx.get(&op) {
                    eprintln!("op: {:?}, {:?}", op, fixity);
                }
            }
        }
    }
}
