use crate::{printer::Printer, Printable};
use pomelo_parse::{ast, AstNode};

impl Printable for ast::Pat {
    fn print(&self, printer: &mut Printer) -> Option<()> {
        printer.pat(self)
    }
}

impl Printer {
    pub fn pat(&mut self, pat: &ast::Pat) -> Option<()> {
        match pat {
            ast::Pat::Atomic(p) => self.atomic_pat(p),
            ast::Pat::Typed(p) => self.typed_pat(p),
            ast::Pat::Layered(p) => self.layered_pat(p),
            ast::Pat::ConsOrInfix(p) => self.cons_or_infix_pat(p),
        }
    }

    fn atomic_pat(&mut self, p: &ast::AtomicPat) -> Option<()> {
        match p {
            ast::AtomicPat::VId(p) => self.vid_pat(p),
            ast::AtomicPat::SCon(p) => self.scon_pat(p),
            ast::AtomicPat::Wildcard(p) => self.wildcard_pat(p),
            _ => todo!(),
        }
    }

    fn vid_pat(&mut self, p: &ast::VIdPat) -> Option<()> {
        self.text(p.syntax().to_string());
        Some(())
    }

    fn scon_pat(&mut self, p: &ast::SConPat) -> Option<()> {
        self.text(p.syntax().to_string());
        Some(())
    }

    fn wildcard_pat(&mut self, _p: &ast::WildcardPat) -> Option<()> {
        self.text("_");
        Some(())
    }

    fn typed_pat(&mut self, _p: &ast::TypedPat) -> Option<()> {
        todo!()
    }

    fn layered_pat(&mut self, _p: &ast::LayeredPat) -> Option<()> {
        todo!()
    }

    fn cons_or_infix_pat(&mut self, _p: &ast::ConsOrInfixPat) -> Option<()> {
        todo!()
    }
}
