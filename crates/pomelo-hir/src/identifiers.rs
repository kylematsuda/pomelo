//! Representation of identifiers in the HIR.
//!
//! TODO: this is not very DRY, especially repetition between `VId`, `StrId`, `TyCon` and their
//! `Long-` forms.
use crate::arena::{Arena, Idx};
use crate::builtins::Builtin;
use crate::NameInterner;
use std::collections::HashMap;

/// Holds information about identifiers in the HIR.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct NameInternerImpl {
    names: Arena<String>,
    mapping: HashMap<String, Idx<String>>,
    generated: u32,
}

impl NameInterner for NameInternerImpl {
    fn fresh(&mut self) -> u32 {
        let out = self.generated;
        self.generated += 1;
        out
    }

    fn alloc(&mut self, s: &str) -> Idx<String> {
        if let Some(idx) = self.mapping.get(s) {
            *idx
        } else {
            let idx = self.names.alloc(s.to_owned());
            self.mapping.insert(s.to_owned(), idx);
            idx
        }
    }

    fn get(&self, index: Idx<String>) -> &str {
        self.names.get(index).as_str()
    }
}

/// An interned name in the HIR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Name {
    BuiltIn(Builtin),
    String(Idx<String>),
    Generated(u32),
}

impl Name {
    pub fn from_string<I: NameInterner>(s: &str, interner: &mut I) -> Self {
        Self::String(interner.alloc(s))
    }

    pub fn from_builtin(b: Builtin) -> Self {
        Self::BuiltIn(b)
    }

    pub fn try_builtin<I: NameInterner>(s: &str, interner: &mut I) -> Self {
        match Builtin::from_string(s) {
            Some(b) => Self::BuiltIn(b),
            None => Self::from_string(s, interner),
        }
    }
}

/// A long (structure-qualified) value identifier.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LongVId {
    pub strids: Box<[StrId]>,
    pub vid: VId,
}

impl LongVId {
    pub fn missing() -> Self {
        Self {
            strids: Box::new([]),
            vid: VId::missing(),
        }
    }

    pub fn from_vid(vid: VId) -> Self {
        Self {
            strids: Box::new([]),
            vid,
        }
    }

    pub fn is_builtin(&self) -> bool {
        self.vid.is_builtin() && self.strids.is_empty()
    }

    pub fn try_into_vid(self) -> Option<VId> {
        if self.strids.is_empty() {
            Some(self.vid)
        } else {
            None
        }
    }
}

/// A value identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VId {
    Missing,
    Name(Name),
}

impl VId {
    pub fn from_string<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_string(name, interner))
    }

    pub fn from_builtin(name: Builtin) -> Self {
        Self::Name(Name::from_builtin(name))
    }

    pub fn missing() -> Self {
        Self::Missing
    }

    pub fn try_into_name(self) -> Option<Name> {
        match self {
            Self::Missing => None,
            Self::Name(n) => Some(n),
        }
    }

    pub(crate) fn try_builtin<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        match Builtin::from_string(name) {
            Some(b) => Self::from_builtin(b),
            None => Self::from_string(name, interner),
        }
    }

    pub fn is_builtin(&self) -> bool {
        matches!(self, VId::Name(Name::BuiltIn(_)))
    }
}

/// A long (structure-qualified) structure identifier.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongStrId {
    pub strid_path: Box<[StrId]>,
    pub strid: StrId,
}

/// A structure identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StrId {
    Missing,
    Name(Name),
}

impl StrId {
    pub fn from_string<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_string(name, interner))
    }

    pub fn from_builtin(name: Builtin) -> Self {
        Self::Name(Name::from_builtin(name))
    }

    pub fn missing() -> Self {
        Self::Missing
    }

    pub fn try_into_name(self) -> Option<Name> {
        match self {
            Self::Missing => None,
            Self::Name(n) => Some(n),
        }
    }

    pub(crate) fn try_builtin<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        match Builtin::from_string(name) {
            Some(b) => Self::from_builtin(b),
            None => Self::from_string(name, interner),
        }
    }
}

/// A type variable.
///
/// This must start with `'`.
/// TODO: check if this is enforce during parsing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyVar {
    Missing,
    Name(Name),
}

impl TyVar {
    pub fn from_string<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_string(name, interner))
    }

    pub fn missing() -> Self {
        Self::Missing
    }
}

/// A long (structure-qualified) type constructor.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LongTyCon {
    pub strids: Box<[StrId]>,
    pub tycon: TyCon,
}

impl LongTyCon {
    pub fn missing() -> Self {
        Self {
            strids: Box::new([]),
            tycon: TyCon::missing(),
        }
    }

    pub fn is_builtin(&self) -> bool {
        self.tycon.is_builtin() && self.strids.is_empty()
    }
}

impl From<TyCon> for LongTyCon {
    fn from(value: TyCon) -> Self {
        Self {
            strids: Box::new([]),
            tycon: value,
        }
    }
}

/// A type constructor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyCon {
    Missing,
    Name(Name),
}

impl TyCon {
    pub fn from_string<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_string(name, interner))
    }

    pub fn from_builtin(name: Builtin) -> Self {
        Self::Name(Name::from_builtin(name))
    }

    pub fn missing() -> Self {
        Self::Missing
    }

    pub fn try_into_name(self) -> Option<Name> {
        match self {
            Self::Missing => None,
            Self::Name(n) => Some(n),
        }
    }

    pub(crate) fn try_builtin<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        match Builtin::from_string(name) {
            Some(b) => Self::from_builtin(b),
            None => Self::from_string(name, interner),
        }
    }

    pub fn is_builtin(&self) -> bool {
        matches!(self, Self::Name(Name::BuiltIn(_)))
    }
}

/// A label in a record type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Label {
    Missing,
    Numeric(u32),
    Named(Name),
}

impl Label {
    pub fn numeric(n: u32) -> Self {
        Self::Numeric(n)
    }

    pub fn missing() -> Self {
        Self::Missing
    }
}
