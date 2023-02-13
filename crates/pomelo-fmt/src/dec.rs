use pomelo_parse::ast;
use crate::printer::{Printer, LINE_WIDTH};

pub fn print_dec(dec: &ast::Dec) -> String {
    let mut printer = Printer::new(LINE_WIDTH);
    printer.dec(dec);
    printer.output()
}

impl Printer {
    pub fn dec(&mut self, dec: &ast::Dec) {
        match dec {
            ast::Dec::Seq(d) => self.seq_dec(d),
            ast::Dec::Val(d) => self.val(d),
            ast::Dec::Fun(d) => self.fun(d),
            ast::Dec::Type(d) => self.tydec(d),
            ast::Dec::Datatype(d) => self.datatype(d),
            ast::Dec::DatatypeRep(d) => self.datarep(d),
            ast::Dec::Abstype(d) => self.abstype(d),
            ast::Dec::Exception(d) => self.exception(d),
            ast::Dec::Local(d) => self.local(d),
            ast::Dec::Open(d) => self.open(d),
            ast::Dec::Infix(d) => self.infix(d),
            ast::Dec::Infixr(d) => self.infixr(d),
            ast::Dec::Nonfix(d) => self.nonfix(d),
        }
    }

    fn seq_dec(&mut self, _d: &ast::SeqDec) {
        todo!()
    }

    fn val(&mut self, _d: &ast::ValDec) {
        todo!()
    }

    fn fun(&mut self, _d: &ast::FunDec) {
        todo!()
    }

    fn tydec(&mut self, _d: &ast::TypeDec) {
        todo!()
    }

    fn datatype(&mut self, _d: &ast::DatatypeDec) {
        todo!()
    }

    fn datarep(&mut self, _d: &ast::DatatypeRepDec) {
        todo!()
    }

    fn abstype(&mut self, _d: &ast::AbstypeDec) {
        todo!()
    }

    fn exception(&mut self, _d: &ast::ExceptionDec) {
        todo!()
    }

    fn local(&mut self, _d: &ast::LocalDec) {
        todo!()
    }

    fn open(&mut self, _d: &ast::OpenDec) {
        todo!()
    }

    fn infix(&mut self, _d: &ast::InfixDec) {
        todo!()
    }
    
    fn infixr(&mut self, _d: &ast::InfixrDec) {
        todo!()
    }

    fn nonfix(&mut self, _d: &ast::NonfixDec) {
        todo!()
    }
}
