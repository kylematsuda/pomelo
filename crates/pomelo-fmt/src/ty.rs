use pomelo_parse::ast;
use crate::printer::Printer;

impl Printer {
    pub fn ty(&mut self, ty: &ast::Ty) {
        match ty {
            ast::Ty::Fun(t) => self.fun_ty(t),
            ast::Ty::Tuple(t) => self.tuple_ty(t),
            ast::Ty::Cons(t) => self.cons_ty(t),
            ast::Ty::Record(t) => self.record_ty(t),
            ast::Ty::TyVar(t) => self.tyvar_ty(t),
        }
    }

    fn fun_ty(&mut self, t: &ast::FunTy) {
        todo!()
    }

    fn tuple_ty(&mut self, t: &ast::TupleTy) {
        todo!()
    }

    fn cons_ty(&mut self, t: &ast::ConsTy) {
        todo!()
    }

    fn record_ty(&mut self, t: &ast::RecordTy) {
        todo!()
    }

    fn tyvar_ty(&mut self, t: &ast::TyVarTy) {
        todo!()
    }
}
