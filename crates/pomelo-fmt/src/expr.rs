use pomelo_parse::{ast, AstNode};

use crate::{printer::Printer, Printable};

impl Printable for ast::Expr {
    fn print(&self, printer: &mut Printer) -> Option<()> {
        printer.expr(self)
    }
}

impl Printer {
    pub fn expr(&mut self, expr: &ast::Expr) -> Option<()> {
        match expr {
            ast::Expr::Atomic(e) => self.atomic_expr(e),
            ast::Expr::Typed(e) => self.typed_expr(e),
            ast::Expr::InfixOrApp(e) => self.infix_or_app_expr(e),
            ast::Expr::Fn(e) => self.fn_expr(e),
            ast::Expr::Case(e) => self.case_expr(e),
            ast::Expr::If(e) => self.if_expr(e),
            ast::Expr::OrElse(e) => self.or_else_expr(e),
            ast::Expr::AndAlso(e) => self.and_also_expr(e),
            ast::Expr::While(e) => self.while_expr(e),
            ast::Expr::Raise(e) => self.raise_expr(e),
            ast::Expr::Handle(e) => self.handle_expr(e),
        }
    }

    fn atomic_expr(&mut self, e: &ast::AtomicExpr) -> Option<()> {
        match e {
            ast::AtomicExpr::VId(e) => self.vid_expr(e),
            ast::AtomicExpr::SCon(e) => self.scon_expr(e),
            ast::AtomicExpr::Let(e) => self.let_expr(e),
            ast::AtomicExpr::Seq(e) => {
                self.list_like("(", ")", ";", false, e.exprs(), e.exprs().count())
            }
            ast::AtomicExpr::Unit(e) => self.unit_expr(e),
            ast::AtomicExpr::List(e) => {
                self.list_like("[", "]", ",", false, e.exprs(), e.exprs().count())
            }
            ast::AtomicExpr::Tuple(e) => {
                self.list_like("(", ")", ",", false, e.exprs(), e.exprs().count())
            }
            ast::AtomicExpr::Paren(e) => self.paren_expr(e),
            ast::AtomicExpr::RecSel(e) => self.recsel_expr(e),
            ast::AtomicExpr::Record(e) => {
                self.list_like("{", "}", ",", true, e.exprows(), e.exprows().count())
            }
        }
    }

    fn vid_expr(&mut self, e: &ast::VIdExpr) -> Option<()> {
        self.text(e.syntax().to_string());
        Some(())
    }

    fn scon_expr(&mut self, e: &ast::SConExpr) -> Option<()> {
        self.text(e.syntax().to_string());
        Some(())
    }

    fn let_expr(&mut self, e: &ast::LetExpr) -> Option<()> {
        let dec = e.dec()?;
        let expr = e.expr()?;

        self.cgroup(0);
        {
            self.cgroup(crate::INDENT);
            self.text("let");
            self.linebreak();
            self.dec(&dec)?;
            self.endgroup();
        }
        self.linebreak();
        {
            self.cgroup(crate::INDENT);
            self.text("in");
            self.linebreak();
            self.expr(&expr)?;
            self.endgroup();
        }
        self.linebreak();
        self.text("end");
        self.endgroup();

        Some(())
    }

    fn unit_expr(&mut self, _e: &ast::UnitExpr) -> Option<()> {
        self.cgroup(0);
        self.text("()");
        self.endgroup();
        Some(())
    }

    fn paren_expr(&mut self, e: &ast::ParenExpr) -> Option<()> {
        self.cgroup(0);
        {
            self.cgroup(crate::INDENT);
            self.text("(");
            self.zerobreak();
            self.expr(&e.expr()?)?;
            self.endgroup();
        }
        self.zerobreak();
        self.text(")");
        self.endgroup();
        Some(())
    }

    fn recsel_expr(&mut self, e: &ast::RecSelExpr) -> Option<()> {
        self.cgroup(crate::INDENT);
        self.text("#");
        self.zerobreak();
        self.text(e.label()?.to_string());
        self.endgroup();
        Some(())
    }

    fn typed_expr(&mut self, _e: &ast::TypedExpr) -> Option<()> {
        todo!()
    }

    fn infix_or_app_expr(&mut self, e: &ast::InfixOrAppExpr) -> Option<()> {
        // This will just treat them all the same...
        // But probably at least want to do something nice for the builtin infix operators?
        self.igroup(crate::INDENT);
        for (i, e) in e.exprs().enumerate() {
            if i != 0 {
                self.space();
            }
            self.expr(&e)?;
        }
        self.endgroup();

        Some(())
    }

    fn fn_expr(&mut self, e: &ast::FnExpr) -> Option<()> {
        let match_ = e.match_expr()?;
        self.igroup(0);
        self.text("fn");
        self.match_rules(&match_, true)?;
        self.endgroup();
        Some(())
    }

    fn case_expr(&mut self, e: &ast::CaseExpr) -> Option<()> {
        let test = e.expr()?;
        let match_ = e.match_expr()?;

        self.igroup(0);
        {
            self.cgroup(0);
            self.text("case");
            self.space_indent();
            self.expr(&test)?;
            self.space();
            self.text("of");
            self.endgroup();
        }
        self.linebreak();
        self.match_rules(&match_, false)?;
        self.endgroup();

        Some(())
    }

    fn if_expr(&mut self, e: &ast::IfExpr) -> Option<()> {
        let condition = e.expr_1()?;
        let then_branch = e.expr_2()?;
        let else_branch = e.expr_3()?;

        self.cgroup(0);
        {
            self.igroup(0);
            self.text("if");
            self.space_indent();
            self.expr(&condition)?;
            self.space();
            self.text("then");
            self.space_indent();
            self.expr(&then_branch)?;
            self.endgroup();
        }
        self.space();
        {
            self.igroup(0);
            self.text("else");
            self.space_indent();
            self.expr(&else_branch)?;
            self.endgroup();
        }
        self.endgroup();

        Some(())
    }

    fn or_else_expr(&mut self, e: &ast::OrElseExpr) -> Option<()> {
        let e1 = e.expr_1()?;
        let e2 = e.expr_2()?;

        self.igroup(crate::INDENT);
        self.expr(&e1)?;
        self.space();
        {
            self.cgroup(0);
            self.text("orelse");
            self.space();
            self.expr(&e2)?;
            self.endgroup();
        }
        self.endgroup();

        Some(())
    }

    fn and_also_expr(&mut self, e: &ast::AndAlsoExpr) -> Option<()> {
        let e1 = e.expr_1()?;
        let e2 = e.expr_2()?;

        self.igroup(crate::INDENT);
        self.expr(&e1)?;
        self.space();
        {
            self.cgroup(0);
            self.text("andalso");
            self.space();
            self.expr(&e2)?;
            self.endgroup();
        }
        self.endgroup();

        Some(())
    }

    fn while_expr(&mut self, e: &ast::WhileExpr) -> Option<()> {
        self.cgroup(0);
        {
            self.igroup(0);
            self.text("while");
            self.space_indent();
            self.expr(&e.expr_1()?)?;
            self.endgroup();
        }
        self.space();
        {
            self.igroup(0);
            self.text("do");
            self.space_indent();
            self.expr(&e.expr_2()?)?;
            self.endgroup();
        }
        self.endgroup();

        Some(())
    }

    fn raise_expr(&mut self, _e: &ast::RaiseExpr) -> Option<()> {
        todo!()
    }

    fn handle_expr(&mut self, _e: &ast::HandleExpr) -> Option<()> {
        todo!()
    }
}

impl Printable for ast::ExprRow {
    fn print(&self, printer: &mut Printer) -> Option<()> {
        printer.expr_row(self)
    }
}

impl Printer {
    fn expr_row(&mut self, e: &ast::ExprRow) -> Option<()> {
        self.igroup(crate::INDENT);
        self.text(e.label()?.to_string());
        self.space();
        self.text("=");
        self.space();
        self.expr(&e.expr()?)?;
        self.endgroup();

        Some(())
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::check_expr;
    use expect_test::expect;

    #[test]
    fn if_expr() {
        check_expr(
            "if true then true else false",
            10,
            expect![[r#"
                if true
                then true
                else false"#]],
        );

        check_expr(
            "if true then true else false",
            12,
            expect![[r#"
                if true then
                  true
                else false"#]],
        );

        check_expr(
            "if true then true else false",
            20,
            expect![[r#"
                if true then true
                else false"#]],
        );

        check_expr(
            "if true then true else false",
            40,
            expect![[r#"
                if true then true else false"#]],
        );

        check_expr(
            "if true then if true then true else if false then false else true else true",
            40,
            expect![[r#"
                if true then
                  if true then true
                  else if false then false else true
                else true"#]],
        );
    }

    #[test]
    fn orelse_expr() {
        check_expr(
            "true orelse false",
            14,
            expect![[r#"
                true
                  orelse false"#]],
        );

        check_expr(
            "true orelse false",
            10,
            expect![[r#"
                true
                  orelse
                  false"#]],
        )
    }

    #[test]
    fn andalso_expr() {
        check_expr(
            "true andalso false",
            20,
            expect![[r#"
                true andalso false"#]],
        );

        check_expr(
            "true andalso false",
            15,
            expect![[r#"
                true
                  andalso false"#]],
        );

        check_expr(
            "true andalso false",
            10,
            expect![[r#"
                true
                  andalso
                  false"#]],
        )
    }

    #[test]
    fn case_expr() {
        check_expr(
            "case x of 0 => 0 | 1 => 1",
            40,
            expect![[r#"
                case x of
                  0 => 0
                | 1 => 1"#]],
        );

        check_expr(
            "case abcdefgh of 0 => 0 | 1 => 1",
            10,
            expect![[r#"
                case
                  abcdefgh
                of
                  0 => 0
                | 1 => 1"#]],
        );

        check_expr(
            "case abcd of 0 => 0 | 1 => 1",
            10,
            expect![[r#"
                case
                  abcd
                of
                  0 => 0
                | 1 => 1"#]],
        );
    }

    #[test]
    fn fn_expr() {
        check_expr(
            "fn 1 => 1 | _ => 0",
            10,
            expect![[r#"
                fn 1 => 1
                 | _ => 0"#]],
        )
    }

    #[test]
    fn let_expr() {
        check_expr(
            "let val x = 1 in x end",
            80,
            expect![[r#"
                let
                  val x = 1
                in
                  x
                end"#]],
        )
    }

    #[test]
    fn seq_expr() {
        check_expr("(0; 1)", 80, expect![[r#"(0; 1)"#]]);

        check_expr(
            "(0; 1; 2; 3; 4; 5; 6; 7; 8)",
            80,
            expect![[r#"
                (0; 1; 2; 3; 4; 5; 6; 7; 8)"#]],
        );

        check_expr(
            "(0; 1; 2; 3; 4; 5; 6; 7; 8)",
            10,
            expect![[r#"
                (
                  0;
                  1;
                  2;
                  3;
                  4;
                  5;
                  6;
                  7;
                  8
                )"#]],
        )
    }

    #[test]
    fn list_expr() {
        check_expr("[0, 1, 2, 3, 4, 5]", 80, expect![[r#"[0, 1, 2, 3, 4, 5]"#]]);

        check_expr(
            "[0, 1, 2, 3, 4, 5]",
            10,
            expect![[r#"
            [
              0,
              1,
              2,
              3,
              4,
              5
            ]"#]],
        );
    }

    #[test]
    fn tuple_expr() {
        check_expr("(0, 1, 2, 3, 4, 5)", 80, expect![[r#"(0, 1, 2, 3, 4, 5)"#]]);

        check_expr(
            "(0, 1, 2, 3, 4, 5)",
            10,
            expect![[r#"
            (
              0,
              1,
              2,
              3,
              4,
              5
            )"#]],
        );
    }

    #[test]
    fn paren() {
        check_expr("(42)", 80, expect![[r#"(42)"#]]);

        check_expr(
            "(if abcdefgh then true else false)",
            20,
            expect![[r#"
            (
              if abcdefgh then
                true
              else false
            )"#]],
        );
    }

    #[test]
    fn recsel_expr() {
        check_expr("#1 (x,y)", 80, expect![[r#"#1 (x, y)"#]]);

        check_expr(
            "#1 (x,y)",
            8,
            expect![[r#"
            #1
              (x, y)"#]],
        );
    }

    #[test]
    fn record_expr() {
        check_expr("{ 1=x, 2=y }", 80, expect![[r#"{ 1 = x, 2 = y }"#]]);

        check_expr(
            "{ 1 = x, 2 = y }",
            8,
            expect![[r#"
            {
              1 = x,
              2 = y
            }"#]],
        );
    }

    #[test]
    fn while_expr() {
        check_expr(
            "while abcdefg do hijklmnop",
            80,
            expect![[r#"while abcdefg do hijklmnop"#]],
        );

        check_expr(
            "while abcdefg do hijklmnop",
            15,
            expect![[r#"
            while abcdefg
            do hijklmnop"#]],
        );

        check_expr(
            "while abcdefg do hijklmnop",
            12,
            expect![[r#"
            while
              abcdefg
            do hijklmnop"#]],
        );

        check_expr(
            "while abcde do hijklm",
            8,
            expect![[r#"
            while
              abcde
            do
              hijklm"#]],
        );
    }
}
