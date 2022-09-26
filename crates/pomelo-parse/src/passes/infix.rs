use crate::{ast, AstNode, AstToken, SyntaxNode, SyntaxTree, ast::InfixOrAppExpr, parser::Token};
use crate::parser::NodeBuilder;
use rowan::{GreenNode, NodeOrToken};

use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::iter::Peekable;

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

const FN_APPL: Fixity = Fixity { val: 11, assoc: Associativity::Left };

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

    // If the op is in the map, return its binding power.
    fn get_bp(&self, op: &str) -> Option<(u8, u8)> {
        self.0.get(op).map(Fixity::bp)
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
    fn bp(&self) -> (u8, u8) {
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

fn fix_infix(expr: InfixOrAppExpr, ctx: &Context) -> GreenNode {
    let mut peek = expr.exprs().peekable();
    fix_infix_bp(&mut peek, ctx, 0)
}

fn fix_infix_bp<I>(exprs: &mut Peekable<I>, ctx: &Context, min_bp: u8) -> GreenNode
where
    I: Iterator<Item = ast::Expr>
{
    let mut lhs = exprs.next().unwrap().syntax().green().into_owned(); 

    loop {
        let op = if let Some(op) = exprs.peek() { op } else { break; };
        let op_text = op.syntax().text().to_string();

        // Infix
        if let Some((l_bp, r_bp)) = ctx.get_bp(&op_text) {
            if l_bp < min_bp {
                break;
            }

            let op = exprs.next().unwrap();
            let vid = ast::VIdExpr::cast(op.syntax().clone()).unwrap().longvid().unwrap().vid().unwrap().syntax().green().to_owned();
            let rhs = fix_infix_bp(exprs, ctx, r_bp);

            let mut outer = GreenNode::new(crate::SyntaxKind::INFIX_EXP.into(), [
                NodeOrToken::Node(lhs.clone()),
                NodeOrToken::Token(vid),
                NodeOrToken::Node(rhs),
            ]);

            std::mem::swap(&mut lhs, &mut outer);

        } else { // fn application 
            let (l_bp, _) = FN_APPL.bp();

            if l_bp < min_bp {
                break;
            }

            let receiver = exprs.next().unwrap().syntax().green().into_owned();
            let mut outer = GreenNode::new(crate::SyntaxKind::APP_EXP.into(), [
                NodeOrToken::Node(lhs.clone()),
                NodeOrToken::Node(receiver),
            ]);

            std::mem::swap(&mut lhs, &mut outer);
        }
    }
    lhs
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::AtomicExpr, ast::Expr, ast::InfixOrAppExpr,
        passes::infix::find_infix_or_app_expr_parent, passes::infix::Context, AstNode, Parser,
        passes::infix::fix_infix,
        SyntaxTree, 
    };

    #[test]
    fn scratch_infix() {
        let ctx = Context::new_with_builtins();

        let input = "val a = 1 + f 2 * 3 + 4";
        let tree = Parser::new(input).parse();

        let node = tree.syntax();

        eprintln!("{}", tree);

        // DFS to find unresolved node
        let infix = find_infix_or_app_expr_parent(node.clone()).unwrap();
        // Cast to ast node
        let infix = InfixOrAppExpr::cast(infix).unwrap();

        let green = fix_infix(infix.clone(), &ctx);

        eprintln!("{}", SyntaxTree::new(green, vec![]));
    }
}
