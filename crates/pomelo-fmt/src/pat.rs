use pomelo_parse::ast;
use crate::printer::Printer;

impl Printer {
    pub fn pat(&mut self, pat: &ast::Pat) {
        match pat {
            ast::Pat::Atomic(p) => self.atomic_pat(p),
            ast::Pat::Typed(p) => self.typed_pat(p),
            ast::Pat::Layered(p) => self.layered_pat(p),
            ast::Pat::ConsOrInfix(p) => self.cons_or_infix_pat(p),
        }
    }

    fn atomic_pat(&mut self, p: &ast::AtomicPat) {
        todo!()
    }

    fn typed_pat(&mut self, p: &ast::TypedPat) {
        todo!()
    }

    fn layered_pat(&mut self, p: &ast::LayeredPat) {
        todo!()
    }

    fn cons_or_infix_pat(&mut self, p: &ast::ConsOrInfixPat) {
        todo!()
    }
}
