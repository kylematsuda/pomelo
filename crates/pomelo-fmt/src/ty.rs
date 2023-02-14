use crate::{printer::Printer, Printable};
use pomelo_parse::ast;

impl Printable for ast::Ty {
    fn print(&self, printer: &mut Printer) -> Option<()> {
        printer.ty(self)
    }
}

impl Printer {
    pub fn ty(&mut self, ty: &ast::Ty) -> Option<()> {
        match ty {
            ast::Ty::Fun(t) => self.fun_ty(t),
            ast::Ty::Tuple(t) => self.tuple_ty(t),
            ast::Ty::Cons(t) => self.cons_ty(t),
            ast::Ty::Record(t) => self.record_ty(t),
            ast::Ty::TyVar(t) => self.tyvar_ty(t),
        }
    }

    fn fun_ty(&mut self, _t: &ast::FunTy) -> Option<()> {
        todo!()
    }

    fn tuple_ty(&mut self, _t: &ast::TupleTy) -> Option<()> {
        todo!()
    }

    fn cons_ty(&mut self, _t: &ast::ConsTy) -> Option<()> {
        todo!()
    }

    fn record_ty(&mut self, _t: &ast::RecordTy) -> Option<()> {
        todo!()
    }

    fn tyvar_ty(&mut self, _t: &ast::TyVarTy) -> Option<()> {
        todo!()
    }
}
