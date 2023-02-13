use pomelo_parse::ast;
use crate::printer::Printer;

impl Printer {
    pub fn expr(&mut self, expr: &ast::Expr) {
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

    fn atomic_expr(&mut self, _e: &ast::AtomicExpr) {
        todo!()
    }

    fn typed_expr(&mut self, _e: &ast::TypedExpr) {
        todo!()
    }

    fn infix_or_app_expr(&mut self, _e: &ast::InfixOrAppExpr) {
        todo!()
    }

    fn fn_expr(&mut self, _e: &ast::FnExpr) {
        todo!()
    }

    fn case_expr(&mut self, _e: &ast::CaseExpr) {
        todo!()
    }

    fn if_expr(&mut self, _e: &ast::IfExpr) {
        todo!()
    }

    fn or_else_expr(&mut self, _e: &ast::OrElseExpr) {
        todo!()
    }

    fn and_also_expr(&mut self, _e: &ast::AndAlsoExpr) {
        todo!()
    }

    fn while_expr(&mut self, _e: &ast::WhileExpr) {
        todo!()
    }

    fn raise_expr(&mut self, _e: &ast::RaiseExpr) {
        todo!()
    }

    fn handle_expr(&mut self, _e: &ast::HandleExpr) {
        todo!()
    }
}
