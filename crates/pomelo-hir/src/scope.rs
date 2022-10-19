use crate::arena::Idx;
use crate::core::{Dec, Expr, Pat};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BodyRefs {
    def: Option<Idx<Dec>>,
    refs: Vec<HirRef>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirRef {
    Pat(Idx<Pat>),
    Expr(Idx<Expr>),
}
