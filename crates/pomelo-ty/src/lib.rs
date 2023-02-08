//! Type inference and type checking for `pomelo`
//!
//! (Also any supporting bits of semantic analysis.)
//!
//! Not much done yet!
pub mod usages;

use pomelo_hir as hir;
use pomelo_hir::arena::Idx;

pub trait HirVisitor {
    fn visit_dec(&mut self, dec: Idx<hir::Dec>);
    fn visit_expr(&mut self, expr: Idx<hir::Expr>);
    fn visit_pat(&mut self, pat: Idx<hir::Pat>);
    fn visit_ty(&mut self, ty: Idx<hir::Ty>);
}
