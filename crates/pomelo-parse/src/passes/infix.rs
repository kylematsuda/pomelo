use crate::parser::NodeBuilder;
use crate::{
    ast, ast::InfixOrAppExpr, grammar, parser::Token, AstNode, AstToken, Parser,
    SyntaxElementChildren, SyntaxNode, SyntaxTree,
};
use rowan::ast::SyntaxNodePtr;
use rowan::{GreenNode, GreenToken, NodeOrToken};

use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

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

const FN_APPL: Fixity = Fixity {
    val: 11,
    assoc: Associativity::Left,
};

#[derive(Clone, Debug)]
struct Context(Rc<HashMap<String, Fixity>>);

impl Context {
    fn new_with_builtins() -> Self {
        Self(Rc::new(
            BUILTINS
                .into_iter()
                .map(|(s, f)| (String::from(s), f))
                .collect(),
        ))
    }

    // If the op is in the map, return its binding power.
    fn get_bp(&self, op: &str) -> Option<(u8, u8)> {
        self.0.get(op).map(Fixity::bp)
    }
}

impl Deref for Context {
    type Target = Rc<HashMap<String, Fixity>>;

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

pub fn pass_rearrange_infix(tree: SyntaxTree) -> SyntaxTree {
    let root = tree.syntax();
    let ctx = Context::new_with_builtins();
    rearrange_infix(tree, root, &ctx)
}

fn rearrange_infix(tree: SyntaxTree, node: SyntaxNode, ctx: &Context) -> SyntaxTree {
    let mut tree = tree;
    let mut node = node;

    if ast::InfixOrAppExpr::cast(node.clone()).is_some() {
        let parent_ptr = SyntaxNodePtr::new(
            &node
                .parent()
                .expect("an expression cannot be the root of the tree"),
        );
        let index = node.index();

        tree = fix_infix(&tree, node.clone(), ctx);

        let parent_node = parent_ptr.to_node(&tree.syntax());
        node = match parent_node.children_with_tokens().nth(index) {
            Some(NodeOrToken::Node(node)) => node,
            _ => panic!("number of children hasn't changed"),
        };
    }

    for c in node.children() {
        tree = rearrange_infix(tree, c, ctx);
    }
    tree
}

fn update_context(ctx: Context, dec: &SyntaxNode) -> Context {
    let mut ctx: Context = ctx.clone();
    if let Some(infix) = ast::InfixDec::cast(dec.clone()) {
        let fixity = infix
            .fixity()
            .and_then(|f| f.value())
            .map(|i| i.parse())
            .unwrap_or(0u8);
        for name in infix.vids() {
            let vid = name.syntax().text();
            Rc::get_mut(&mut ctx).unwrap().insert(
                vid.to_owned(),
                Fixity {
                    val: fixity,
                    assoc: Associativity::Left,
                },
            );
        }
    } else if let Some(infixr) = ast::InfixrDec::cast(dec.clone()) {
        let fixity = infixr
            .fixity()
            .and_then(|f| f.value())
            .map(|i| i.parse())
            .unwrap_or(0u8);
        for name in infixr.vids() {
            let vid = name.syntax().text();
            Rc::get_mut(&mut ctx).unwrap().insert(
                vid.to_owned(),
                Fixity {
                    val: fixity,
                    assoc: Associativity::Right,
                },
            );
        }
    } else if let Some(nonfix) = ast::NonfixDec::cast(dec.clone()) {
        for name in nonfix.vids() {
            let vid = name.syntax().text();
            Rc::get_mut(&mut ctx).unwrap().remove(vid);
        }
    }
    ctx
}

fn fix_infix(tree: &SyntaxTree, expr: SyntaxNode, ctx: &Context) -> SyntaxTree {
    let mut peek = expr.children_with_tokens().peekable();
    let new_green = fix_infix_bp_ii(&mut peek, ctx, 0).unwrap();

    let old_parent = expr.parent().unwrap();
    let new_parent = old_parent
        .green()
        .into_owned()
        .replace_child(expr.index(), new_green.into());

    let new_tree = old_parent.replace_with(new_parent);
    tree.replace_node(new_tree)
}

fn intersperse_trivia(
    first: NodeOrToken<GreenNode, GreenToken>,
    mut trivia: Vec<NodeOrToken<GreenNode, GreenToken>>,
    second: NodeOrToken<GreenNode, GreenToken>,
    last: Option<(
        Vec<NodeOrToken<GreenNode, GreenToken>>,
        NodeOrToken<GreenNode, GreenToken>,
    )>,
) -> Vec<NodeOrToken<GreenNode, GreenToken>> {
    let mut out = vec![first];
    out.append(&mut trivia);
    out.push(second);
    if let Some((mut triv, last)) = last {
        out.append(&mut triv);
        out.push(last);
    }
    out
}

fn next_nontrivia(children: &mut Peekable<SyntaxElementChildren>) -> Option<SyntaxNode> {
    let mut first = children.peek()?.clone();
    while let NodeOrToken::Token(_) = first {
        first = first.next_sibling_or_token()?;
    }

    match first {
        NodeOrToken::Node(n) => Some(n),
        _ => unreachable!(),
    }
}

fn fix_infix_bp_ii(
    children: &mut Peekable<SyntaxElementChildren>,
    ctx: &Context,
    min_bp: u8,
) -> Option<GreenNode> {
    let lhs_syntax = match children.next() {
        Some(NodeOrToken::Node(expr)) => expr,
        _ => panic!("FIXME later"),
    };
    let mut lhs = lhs_syntax.green().into_owned();

    loop {
        let next = match next_nontrivia(children) {
            Some(expr) => expr,
            None => break,
        };
        let next_text = next.text().to_string();

        eprintln!("{}, {}", lhs, next);

        if let Some((l_bp, r_bp)) = ctx.get_bp(&next_text) {
            if l_bp < min_bp {
                break;
            }

            let trivia = collect_trivia(children);

            let vid = match children.next() {
                Some(NodeOrToken::Node(expr)) => expr,
                _ => panic!("FIXME"),
            };
            assert_eq!(vid, next);

            let vid = ast::VIdExpr::cast(vid)
                .unwrap()
                .longvid()
                .unwrap()
                .vid()
                .unwrap()
                .syntax()
                .green()
                .to_owned();

            let trivia_2 = collect_trivia(children);

            let rhs = fix_infix_bp_ii(children, ctx, r_bp).unwrap();

            let mut outer = GreenNode::new(
                crate::SyntaxKind::INFIX_EXP.into(),
                intersperse_trivia(
                    NodeOrToken::Node(lhs.clone()),
                    trivia,
                    NodeOrToken::Token(vid),
                    Some((trivia_2, NodeOrToken::Node(rhs))),
                ),
            );

            std::mem::swap(&mut lhs, &mut outer);
        } else {
            // fn application
            let (l_bp, _) = FN_APPL.bp();

            if l_bp < min_bp {
                break;
            }

            let trivia = collect_trivia(children);

            let receiver = match children.next() {
                Some(NodeOrToken::Node(expr)) => expr,
                _ => panic!("FIXME"),
            };
            assert_eq!(receiver, next);
            let receiver = receiver.green().into_owned();

            let mut outer = GreenNode::new(
                crate::SyntaxKind::APP_EXP.into(),
                intersperse_trivia(
                    NodeOrToken::Node(lhs.clone()),
                    trivia,
                    NodeOrToken::Node(receiver),
                    None,
                ),
            );

            std::mem::swap(&mut lhs, &mut outer);
        }
    }

    Some(lhs)
}

fn collect_trivia(
    children: &mut Peekable<SyntaxElementChildren>,
) -> Vec<NodeOrToken<GreenNode, GreenToken>> {
    let mut out = vec![];

    loop {
        match children.peek() {
            Some(NodeOrToken::Token(t)) if t.kind().is_trivia() => {
                out.push(NodeOrToken::Token(t.green().to_owned()));
                children.next();
            }
            _ => break,
        }
    }
    out
}

fn fix_infix_bp<I>(exprs: &mut Peekable<I>, ctx: &Context, min_bp: u8) -> Option<GreenNode>
where
    I: Iterator<Item = ast::Expr>,
{
    let mut lhs = exprs.next()?.syntax().green().into_owned();

    loop {
        let op = if let Some(op) = exprs.peek() {
            op
        } else {
            break;
        };
        let op_text = op.syntax().text().to_string();

        // Infix
        if let Some((l_bp, r_bp)) = ctx.get_bp(&op_text) {
            if l_bp < min_bp {
                break;
            }

            let op = exprs.next().unwrap();
            let vid = ast::VIdExpr::cast(op.syntax().clone())
                .unwrap()
                .longvid()
                .unwrap()
                .vid()
                .unwrap()
                .syntax()
                .green()
                .to_owned();
            let rhs = fix_infix_bp(exprs, ctx, r_bp)?;

            let mut outer = GreenNode::new(
                crate::SyntaxKind::INFIX_EXP.into(),
                [
                    NodeOrToken::Node(lhs.clone()),
                    NodeOrToken::Token(vid),
                    NodeOrToken::Node(rhs),
                ],
            );

            std::mem::swap(&mut lhs, &mut outer);
        } else {
            // fn application
            let (l_bp, _) = FN_APPL.bp();

            if l_bp < min_bp {
                break;
            }

            let receiver = exprs.next().unwrap().syntax().green().into_owned();
            let mut outer = GreenNode::new(
                crate::SyntaxKind::APP_EXP.into(),
                [NodeOrToken::Node(lhs.clone()), NodeOrToken::Node(receiver)],
            );

            std::mem::swap(&mut lhs, &mut outer);
        }
    }
    Some(lhs)
}

#[cfg(test)]
mod tests {
    use crate::{passes::infix::pass_rearrange_infix, Parser};

    #[test]
    fn scratch_infix() {
        let input = "val a = 1 + f 2 * 3 + 4 ";
        let tree = Parser::new(input).parse();
        eprintln!("{}", tree);

        let new_tree = pass_rearrange_infix(tree);
        eprintln!("{}", new_tree);
    }
}
