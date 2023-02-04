//! Scopes for expression bodies
//!
//! This is very basic scaffolding, needs to be fleshed out.
//! See r-a/crates/hir-def/src/body/scope.rs

use crate::arena::{Arena, Idx};
use crate::body::Body;
use crate::identifiers::LongVId;
use crate::{DecKind, Expr, Pat};

use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct ExprScopes {
    pub scopes: Arena<Scope>,
    pub scope_by_expr: HashMap<Idx<Expr>, Idx<Scope>>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub parent: Option<Idx<Scope>>,
    pub entries: Vec<ScopeEntry>,
}

#[derive(Debug, Clone)]
pub struct ScopeEntry {
    name: LongVId,
    pat: Idx<Pat>,
}

impl ScopeEntry {
    pub fn name(&self) -> &LongVId {
        &self.name
    }

    pub fn pat(&self) -> Idx<Pat> {
        self.pat
    }
}

impl ExprScopes {
    pub fn new(body: &Body) -> Self {
        let mut scopes = ExprScopes::default();

        // FIXME: this is going to act like each body has only 1 top-most dec.
        // This is true syntactically, but not semantically, because of constructs like
        // "val a = 3 and b = 4", etc., which are desugared to a [`DecKind::Seq`].
        //
        // Dunno what to do.......
        let _root = scopes.root_scope();

        match &body.topdec().kind {
            DecKind::Val { .. } => {}
            DecKind::Local { .. } => {}
            DecKind::Abstype { .. } => {}
            DecKind::Open { .. } => todo!(),
            DecKind::Seq { .. } => todo!(), // Don't know how to handle this yet
            // The following don't contain an inner Expr
            DecKind::Missing
            | DecKind::Ty { .. }
            | DecKind::Fixity { .. }
            | DecKind::Datatype { .. }
            | DecKind::Replication { .. }
            | DecKind::Exception { .. } => {}
        }
        scopes
    }

    fn root_scope(&mut self) -> Idx<Scope> {
        self.scopes.alloc(Scope {
            parent: None,
            entries: vec![],
        })
    }

    fn _new_scope(&mut self, parent: Idx<Scope>) -> Idx<Scope> {
        self.scopes.alloc(Scope {
            parent: Some(parent),
            entries: vec![],
        })
    }

    fn _set_scope(&mut self, node: Idx<Expr>, scope: Idx<Scope>) {
        self.scope_by_expr.insert(node, scope);
    }
}

fn _compute_expr_scopes(
    _expr: Idx<Expr>,
    _body: &Body,
    _scopes: &mut ExprScopes,
    _scope: Idx<Scope>,
) {
    todo!()
}
