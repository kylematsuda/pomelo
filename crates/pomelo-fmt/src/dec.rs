use crate::{
    printer::{Printer, LINE_WIDTH},
    Printable,
};
use pomelo_parse::ast;

pub fn print_dec(dec: &ast::Dec) -> String {
    let mut printer = Printer::new(LINE_WIDTH);
    printer.dec(dec);
    printer.output()
}

impl Printable for ast::Dec {
    fn print(&self, printer: &mut Printer) -> Option<()> {
        printer.dec(self)
    }
}

impl Printer {
    pub fn dec(&mut self, dec: &ast::Dec) -> Option<()> {
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

    fn seq_dec(&mut self, d: &ast::SeqDec) -> Option<()> {
        self.igroup(0);
        for (i, d) in d.declarations().enumerate() {
            if i != 0 {
                self.linebreak();
            }
            self.dec(&d)?;
        }
        self.endgroup();

        Some(())
    }

    fn val(&mut self, d: &ast::ValDec) -> Option<()> {
        let tyvars = d.tyvarseq();
        let mut bindings = d.bindings().enumerate();
        let n_bindings = d.bindings().count();

        self.igroup(0);

        self.igroup(crate::INDENT);
        self.text("val");
        self.space();
        // TODO: factor this out
        for t in tyvars {
            self.text(t.to_string());
            self.space();
        }

        let (_, b) = bindings.next()?;
        self.pat(&b.pat()?)?;
        self.space();
        self.text("=");
        self.space();
        self.expr(&b.expr()?)?;
        self.endgroup();

        if n_bindings > 1 {
            self.space();
            self.igroup(0);
            for (i, b) in bindings {
                self.igroup(crate::INDENT);
                self.text("and");
                self.space();

                self.pat(&b.pat()?)?;
                self.space();
                self.text("=");

                self.space();
                self.expr(&b.expr()?)?;
                self.endgroup();

                if i < n_bindings - 1 {
                    self.linebreak();
                }
            }
            self.endgroup();
        }
        self.endgroup();
        Some(())
    }

    fn fun(&mut self, _d: &ast::FunDec) -> Option<()> {
        todo!()
    }

    fn tydec(&mut self, _d: &ast::TypeDec) -> Option<()> {
        todo!()
    }

    fn datatype(&mut self, _d: &ast::DatatypeDec) -> Option<()> {
        todo!()
    }

    fn datarep(&mut self, _d: &ast::DatatypeRepDec) -> Option<()> {
        todo!()
    }

    fn abstype(&mut self, _d: &ast::AbstypeDec) -> Option<()> {
        todo!()
    }

    fn exception(&mut self, _d: &ast::ExceptionDec) -> Option<()> {
        todo!()
    }

    fn local(&mut self, _d: &ast::LocalDec) -> Option<()> {
        todo!()
    }

    fn open(&mut self, _d: &ast::OpenDec) -> Option<()> {
        todo!()
    }

    fn infix(&mut self, _d: &ast::InfixDec) -> Option<()> {
        todo!()
    }

    fn infixr(&mut self, _d: &ast::InfixrDec) -> Option<()> {
        todo!()
    }

    fn nonfix(&mut self, _d: &ast::NonfixDec) -> Option<()> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::check_dec;
    use expect_test::expect;

    #[test]
    fn valdec_let_expr() {
        // TODO: figure out whether this output is too weird...
        check_dec(
            "val x = let val y = 1 in y end and z = let val a = b in c end and d = 1",
            80,
            expect![[r#"
                val x =
                  let
                    val y = 1
                  in
                    y
                  end
                and z =
                  let
                    val a = b
                  in
                    c
                  end
                and d = 1"#]],
        );
    }
}
