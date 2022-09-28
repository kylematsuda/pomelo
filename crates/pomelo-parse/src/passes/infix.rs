use crate::{
    ast, AstChildrenTokens, AstNode, AstToken, Error, SyntaxElement, SyntaxElementChildren,
    SyntaxKind, SyntaxNode, SyntaxTree,
};

use rowan::{GreenNode, GreenToken, NodeOrToken, ast::SyntaxNodePtr};

use std::collections::HashMap;
use std::iter::Peekable;

type GreenElement = NodeOrToken<GreenNode, GreenToken>;

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

// Maximum user-defined fixity is 9 
const FN_APPL: Fixity = Fixity {
    val: 10,
    assoc: Associativity::Left,
};

#[derive(Clone, Debug)]
pub(crate) struct Context(HashMap<String, Fixity>);

impl Context {
    pub(crate) fn new_with_builtins() -> Self {
        Self(
            BUILTINS
                .into_iter()
                .map(|(s, f)| (String::from(s), f))
                .collect(),
        )
    }

    // If the op is in the map, return its binding power.
    pub(crate) fn get_bp(&self, op: &SyntaxNode) -> Option<(u8, u8)> {
        self.0.get(&op.text().to_string()).map(Fixity::bp)
    }

    pub(crate) fn update(&mut self, dec: &SyntaxNode) {
        let fixity_to_int =
            |f: Option<ast::Fixity>| f.and_then(|f| f.value()).map(|i| i.parse()).unwrap_or(0);

        if let Some(infix) = ast::InfixDec::cast(dec.clone()) {
            let fixity = fixity_to_int(infix.fixity());
            self.update_vids(infix.vids(), fixity, Associativity::Left);
        } else if let Some(infixr) = ast::InfixrDec::cast(dec.clone()) {
            let fixity = fixity_to_int(infixr.fixity());
            self.update_vids(infixr.vids(), fixity, Associativity::Right);
        } else if let Some(nonfix) = ast::NonfixDec::cast(dec.clone()) {
            self.remove_vids(nonfix.vids());
        }
    }

    fn update_vids(&mut self, vids: AstChildrenTokens<ast::VId>, fixity: u8, assoc: Associativity) {
        for name in vids {
            let vid = name.syntax().text();
            self.0.insert(vid.to_owned(), Fixity { val: fixity, assoc });
        }
    }

    fn remove_vids(&mut self, vids: AstChildrenTokens<ast::VId>) {
        for name in vids {
            let vid = name.syntax().text();
            self.0.remove(vid);
        }
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

pub fn pass_rearrange_infix(tree: SyntaxTree) -> SyntaxTree {
    let root = tree.syntax();
    let mut ctx = Context::new_with_builtins();
    rearrange_infix(tree, root, &mut ctx)
}

fn rearrange_infix(tree: SyntaxTree, node: SyntaxNode, ctx: &mut Context) -> SyntaxTree {
    let mut tree = tree;
    let mut node = node;

    ctx.update(&node);

    if ast::InfixOrAppExpr::cast(node.clone()).is_some() {
        tree = fix_infix(tree, node.clone(), &ctx);
        node = switch_tree(&node, &tree);
    }

    // We have to be careful where we share context.
    // All children at the same level should use the same context,
    // which is updated as statements are iterated through.
    // However, the context should not leak upwards, so we need to clone 
    // into a new context every time we descend a level in the tree.
    let mut child_context = ctx.clone();
    for c in node.children() {
        let c = switch_tree(&c, &tree);
        tree = rearrange_infix(tree, c, &mut child_context);
    }
    tree
}

// Panics if called on the root node.
fn switch_tree(node: &SyntaxNode, new_tree: &SyntaxTree) -> SyntaxNode {
    let parent_ptr = SyntaxNodePtr::new(
        &node
            .parent()
            .expect("an expression cannot be the root of the tree"),
    );
    let index = node.index();
    let parent_node = parent_ptr.to_node(&new_tree.syntax());

    match parent_node.children_with_tokens().nth(index) {
        Some(NodeOrToken::Node(node)) => node,
        _ => panic!("number of children hasn't changed"),
    }
}

fn fix_infix(tree: SyntaxTree, expr: SyntaxNode, ctx: &Context) -> SyntaxTree {
    let mut peek = expr.children_with_tokens().peekable();
    let mut new_errors = vec![];
    let new_green = fix_infix_bp(&mut peek, ctx, &mut new_errors, 0).unwrap();

    let old_parent = expr.parent().unwrap();
    let new_parent = old_parent
        .green()
        .into_owned()
        .replace_child(expr.index(), new_green.into());

    let new_tree = old_parent.replace_with(new_parent);

    let (_, mut errors) = tree.into_parts();
    errors.append(&mut new_errors);

    SyntaxTree::new(new_tree, errors)
}

// Pratt parsing
fn fix_infix_bp(
    children: &mut Peekable<SyntaxElementChildren>,
    ctx: &Context,
    errors: &mut Vec<Error>,
    min_bp: u8,
) -> Option<GreenNode> {
    let lhs_syntax = match children.next() {
        Some(NodeOrToken::Node(expr)) => expr,
        _ => return None,
    };
    let mut lhs = lhs_syntax.green().into_owned();

    loop {
        let next = match next_syntax_node(children) {
            Some(expr) => expr,
            None => break,
        };

        let mut outer = if let Some((l_bp, r_bp)) = ctx.get_bp(&next) {
            if l_bp < min_bp {
                break;
            }

            let mut trivia = collect_trivia(children);

            let vid = children
                .next()
                .map(unwrap_syntax_node)
                .expect("this is the same node as next");
            assert_eq!(vid, next);
            let vid_green = unwrap_vid_or_err_node(vid.clone(), errors);

            let mut trivia_2 = collect_trivia(children);

            let rhs_green = fix_infix_bp(children, ctx, errors, r_bp)
                .map(GreenElement::from)
                .unwrap_or_else(|| {
                    missing_node_or_token(errors, || {
                        let pos = children
                            .peek()
                            .map(|c| c.text_range().start().into())
                            .unwrap_or_else(|| vid.text_range().start().into());
                        Error::new("missing rhs of infix expression", "", pos)
                    })
                });

            // Collate trivia: factor out into a function?
            let mut elts = vec![lhs.clone().into()];
            elts.append(&mut trivia);
            elts.push(vid_green);
            elts.append(&mut trivia_2);
            elts.push(rhs_green.into());

            GreenNode::new(SyntaxKind::INFIX_EXP.into(), elts)
        } else {
            // fn application
            let (l_bp, _) = FN_APPL.bp();

            if l_bp < min_bp {
                break;
            }

            let mut trivia = collect_trivia(children);

            let rhs = children
                .next()
                .map(unwrap_syntax_node)
                .expect("this is the same node as next");
            assert_eq!(rhs, next);
            let rhs_green = rhs.green().into_owned();

            // Collate trivia: factor out into a function?
            let mut elts = vec![lhs.clone().into()];
            elts.append(&mut trivia);
            elts.push(rhs_green.into());

            GreenNode::new(SyntaxKind::APP_EXP.into(), elts)
        };

        std::mem::swap(&mut lhs, &mut outer);
    }

    Some(lhs)
}

fn next_syntax_node(children: &Peekable<SyntaxElementChildren>) -> Option<SyntaxNode> {
    children
        .clone()
        .skip_while(|c| match c {
            NodeOrToken::Token(_) => true,
            _ => false,
        })
        .next()
        .map(unwrap_syntax_node)
}

fn collect_trivia(children: &mut Peekable<SyntaxElementChildren>) -> Vec<GreenElement> {
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

fn missing_node_or_token(errors: &mut Vec<Error>, mut err: impl FnMut() -> Error) -> GreenElement {
    errors.push(err());
    GreenToken::new(SyntaxKind::ERROR.into(), "").into()
}

fn unwrap_vid_or_err_node(node: SyntaxNode, errors: &mut Vec<Error>) -> GreenElement {
    ast::VIdExpr::cast(node.clone())
        .and_then(|v| v.longvid())
        .and_then(|v| v.vid())
        .map(|tok| tok.syntax().green().to_owned())
        .map(GreenElement::from)
        .unwrap_or_else(|| {
            missing_node_or_token(errors, || {
                Error::new("expected vid", "", node.text_range().start().into())
            })
        })
}

fn unwrap_syntax_node(elt: SyntaxElement) -> SyntaxNode {
    match elt {
        NodeOrToken::Node(n) => n,
        _ => panic!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::{passes::infix::pass_rearrange_infix, passes::tests::check};
    use expect_test::expect;

    #[test]
    fn scratch_infix() {
        check(
            pass_rearrange_infix,
            false,
            "val a = 1 + f 2 * 3 + 4 ",
            expect![[r#"
             FILE@0..24
               VAL_DEC@0..23
                 VAL_KW@0..3 "val"
                 WHITESPACE@3..4
                 VAL_BIND@4..23
                   VID_PAT@4..5
                     LONG_VID@4..5
                       VID@4..5 "a"
                   WHITESPACE@5..6
                   EQ@6..7 "="
                   WHITESPACE@7..8
                   INFIX_EXP@8..23
                     INFIX_EXP@8..19
                       SCON_EXP@8..9
                         INT@8..9 "1"
                       WHITESPACE@9..10
                       VID@10..11 "+"
                       WHITESPACE@11..12
                       INFIX_EXP@12..19
                         APP_EXP@12..15
                           VID_EXP@12..13
                             LONG_VID@12..13
                               VID@12..13 "f"
                           WHITESPACE@13..14
                           SCON_EXP@14..15
                             INT@14..15 "2"
                         WHITESPACE@15..16
                         VID@16..17 "*"
                         WHITESPACE@17..18
                         SCON_EXP@18..19
                           INT@18..19 "3"
                     WHITESPACE@19..20
                     VID@20..21 "+"
                     WHITESPACE@21..22
                     SCON_EXP@22..23
                       INT@22..23 "4"
               WHITESPACE@23..24
        "#]],
        )
    }

    #[test]
    fn op_infix() {
        check(
            pass_rearrange_infix,
            false,
            "val a = op +(1, 2) + 3",
            expect![[r#"
                FILE@0..22
                  VAL_DEC@0..22
                    VAL_KW@0..3 "val"
                    WHITESPACE@3..4
                    VAL_BIND@4..22
                      VID_PAT@4..5
                        LONG_VID@4..5
                          VID@4..5 "a"
                      WHITESPACE@5..6
                      EQ@6..7 "="
                      WHITESPACE@7..8
                      INFIX_EXP@8..22
                        APP_EXP@8..18
                          VID_EXP@8..12
                            OP_KW@8..10 "op"
                            WHITESPACE@10..11
                            LONG_VID@11..12
                              VID@11..12 "+"
                          TUPLE_EXP@12..18
                            L_PAREN@12..13 "("
                            SCON_EXP@13..14
                              INT@13..14 "1"
                            COMMA@14..15 ","
                            WHITESPACE@15..16
                            SCON_EXP@16..17
                              INT@16..17 "2"
                            R_PAREN@17..18 ")"
                        WHITESPACE@18..19
                        VID@19..20 "+"
                        WHITESPACE@20..21
                        SCON_EXP@21..22
                          INT@21..22 "3"
            "#]],
        )
    }

    #[test]
    fn two_infix_decs() {
        check(
            pass_rearrange_infix,
            false,
            "val a = 1 + f 2 * 3 + 4
            val b = 3 div 4 + 5",
            expect![[r#"
                FILE@0..55
                  SEQ_DEC@0..55
                    VAL_DEC@0..23
                      VAL_KW@0..3 "val"
                      WHITESPACE@3..4
                      VAL_BIND@4..23
                        VID_PAT@4..5
                          LONG_VID@4..5
                            VID@4..5 "a"
                        WHITESPACE@5..6
                        EQ@6..7 "="
                        WHITESPACE@7..8
                        INFIX_EXP@8..23
                          INFIX_EXP@8..19
                            SCON_EXP@8..9
                              INT@8..9 "1"
                            WHITESPACE@9..10
                            VID@10..11 "+"
                            WHITESPACE@11..12
                            INFIX_EXP@12..19
                              APP_EXP@12..15
                                VID_EXP@12..13
                                  LONG_VID@12..13
                                    VID@12..13 "f"
                                WHITESPACE@13..14
                                SCON_EXP@14..15
                                  INT@14..15 "2"
                              WHITESPACE@15..16
                              VID@16..17 "*"
                              WHITESPACE@17..18
                              SCON_EXP@18..19
                                INT@18..19 "3"
                          WHITESPACE@19..20
                          VID@20..21 "+"
                          WHITESPACE@21..22
                          SCON_EXP@22..23
                            INT@22..23 "4"
                    WHITESPACE@23..36
                    VAL_DEC@36..55
                      VAL_KW@36..39 "val"
                      WHITESPACE@39..40
                      VAL_BIND@40..55
                        VID_PAT@40..41
                          LONG_VID@40..41
                            VID@40..41 "b"
                        WHITESPACE@41..42
                        EQ@42..43 "="
                        WHITESPACE@43..44
                        INFIX_EXP@44..55
                          INFIX_EXP@44..51
                            SCON_EXP@44..45
                              INT@44..45 "3"
                            WHITESPACE@45..46
                            VID@46..49 "div"
                            WHITESPACE@49..50
                            SCON_EXP@50..51
                              INT@50..51 "4"
                          WHITESPACE@51..52
                          VID@52..53 "+"
                          WHITESPACE@53..54
                          SCON_EXP@54..55
                            INT@54..55 "5"
            "#]],
        )
    }

    #[test]
    fn user_defined() {
        check(
            pass_rearrange_infix,
            false,
            "infix 9 f; val a = b f c + 1",
            expect![[r#"
                FILE@0..28
                  SEQ_DEC@0..28
                    INFIX_DEC@0..9
                      INFIX_KW@0..5 "infix"
                      WHITESPACE@5..6
                      FIXITY@6..7
                        INT@6..7 "9"
                      WHITESPACE@7..8
                      VID@8..9 "f"
                    SEMICOLON@9..10 ";"
                    WHITESPACE@10..11
                    VAL_DEC@11..28
                      VAL_KW@11..14 "val"
                      WHITESPACE@14..15
                      VAL_BIND@15..28
                        VID_PAT@15..16
                          LONG_VID@15..16
                            VID@15..16 "a"
                        WHITESPACE@16..17
                        EQ@17..18 "="
                        WHITESPACE@18..19
                        INFIX_EXP@19..28
                          INFIX_EXP@19..24
                            VID_EXP@19..20
                              LONG_VID@19..20
                                VID@19..20 "b"
                            WHITESPACE@20..21
                            VID@21..22 "f"
                            WHITESPACE@22..23
                            VID_EXP@23..24
                              LONG_VID@23..24
                                VID@23..24 "c"
                          WHITESPACE@24..25
                          VID@25..26 "+"
                          WHITESPACE@26..27
                          SCON_EXP@27..28
                            INT@27..28 "1"
            "#]],
        )
    }

    #[test]
    fn user_defined_and_removed() {
        check(
            pass_rearrange_infix,
            false,
            "
infix 9 f 
val a = x f y + 1
nonfix f 
val b = x f y + 1",
            expect![[r#"
                FILE@0..57
                  WHITESPACE@0..1
                  SEQ_DEC@1..57
                    INFIX_DEC@1..10
                      INFIX_KW@1..6 "infix"
                      WHITESPACE@6..7
                      FIXITY@7..8
                        INT@7..8 "9"
                      WHITESPACE@8..9
                      VID@9..10 "f"
                    WHITESPACE@10..12
                    VAL_DEC@12..29
                      VAL_KW@12..15 "val"
                      WHITESPACE@15..16
                      VAL_BIND@16..29
                        VID_PAT@16..17
                          LONG_VID@16..17
                            VID@16..17 "a"
                        WHITESPACE@17..18
                        EQ@18..19 "="
                        WHITESPACE@19..20
                        INFIX_EXP@20..29
                          INFIX_EXP@20..25
                            VID_EXP@20..21
                              LONG_VID@20..21
                                VID@20..21 "x"
                            WHITESPACE@21..22
                            VID@22..23 "f"
                            WHITESPACE@23..24
                            VID_EXP@24..25
                              LONG_VID@24..25
                                VID@24..25 "y"
                          WHITESPACE@25..26
                          VID@26..27 "+"
                          WHITESPACE@27..28
                          SCON_EXP@28..29
                            INT@28..29 "1"
                    WHITESPACE@29..30
                    NONFIX_DEC@30..38
                      NONFIX_KW@30..36 "nonfix"
                      WHITESPACE@36..37
                      VID@37..38 "f"
                    WHITESPACE@38..40
                    VAL_DEC@40..57
                      VAL_KW@40..43 "val"
                      WHITESPACE@43..44
                      VAL_BIND@44..57
                        VID_PAT@44..45
                          LONG_VID@44..45
                            VID@44..45 "b"
                        WHITESPACE@45..46
                        EQ@46..47 "="
                        WHITESPACE@47..48
                        INFIX_EXP@48..57
                          APP_EXP@48..53
                            APP_EXP@48..51
                              VID_EXP@48..49
                                LONG_VID@48..49
                                  VID@48..49 "x"
                              WHITESPACE@49..50
                              VID_EXP@50..51
                                LONG_VID@50..51
                                  VID@50..51 "f"
                            WHITESPACE@51..52
                            VID_EXP@52..53
                              LONG_VID@52..53
                                VID@52..53 "y"
                          WHITESPACE@53..54
                          VID@54..55 "+"
                          WHITESPACE@55..56
                          SCON_EXP@56..57
                            INT@56..57 "1"
            "#]],
        )
    }

    #[test]
    fn scoped_infix() {
        check(
            pass_rearrange_infix,
            false,
            "val a =
  let 
    infix 9 f
  in 
    x f y + 1 
  end

val b = x f y + 1",
            expect![[r#"
                FILE@0..74
                  SEQ_DEC@0..74
                    VAL_DEC@0..55
                      VAL_KW@0..3 "val"
                      WHITESPACE@3..4
                      VAL_BIND@4..55
                        VID_PAT@4..5
                          LONG_VID@4..5
                            VID@4..5 "a"
                        WHITESPACE@5..6
                        EQ@6..7 "="
                        WHITESPACE@7..10
                        LET_EXP@10..55
                          LET_KW@10..13 "let"
                          WHITESPACE@13..19
                          INFIX_DEC@19..28
                            INFIX_KW@19..24 "infix"
                            WHITESPACE@24..25
                            FIXITY@25..26
                              INT@25..26 "9"
                            WHITESPACE@26..27
                            VID@27..28 "f"
                          WHITESPACE@28..31
                          IN_KW@31..33 "in"
                          WHITESPACE@33..39
                          INFIX_EXP@39..48
                            INFIX_EXP@39..44
                              VID_EXP@39..40
                                LONG_VID@39..40
                                  VID@39..40 "x"
                              WHITESPACE@40..41
                              VID@41..42 "f"
                              WHITESPACE@42..43
                              VID_EXP@43..44
                                LONG_VID@43..44
                                  VID@43..44 "y"
                            WHITESPACE@44..45
                            VID@45..46 "+"
                            WHITESPACE@46..47
                            SCON_EXP@47..48
                              INT@47..48 "1"
                          WHITESPACE@48..52
                          END_KW@52..55 "end"
                    WHITESPACE@55..57
                    VAL_DEC@57..74
                      VAL_KW@57..60 "val"
                      WHITESPACE@60..61
                      VAL_BIND@61..74
                        VID_PAT@61..62
                          LONG_VID@61..62
                            VID@61..62 "b"
                        WHITESPACE@62..63
                        EQ@63..64 "="
                        WHITESPACE@64..65
                        INFIX_EXP@65..74
                          APP_EXP@65..70
                            APP_EXP@65..68
                              VID_EXP@65..66
                                LONG_VID@65..66
                                  VID@65..66 "x"
                              WHITESPACE@66..67
                              VID_EXP@67..68
                                LONG_VID@67..68
                                  VID@67..68 "f"
                            WHITESPACE@68..69
                            VID_EXP@69..70
                              LONG_VID@69..70
                                VID@69..70 "y"
                          WHITESPACE@70..71
                          VID@71..72 "+"
                          WHITESPACE@72..73
                          SCON_EXP@73..74
                            INT@73..74 "1"
            "#]],
        )
    }
}
