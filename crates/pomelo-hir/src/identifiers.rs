//! Representation of identifiers in the HIR.
//!
//! TODO: this is not very DRY, especially repetition between `VId`, `StrId`, `TyCon` and their
//! `Long-` forms.
use crate::arena::{Arena, Idx};

use std::collections::HashMap;

pub trait NameInterner {
    fn fresh(&mut self) -> u32;
    fn alloc(&mut self, s: &str) -> Idx<String>;
    fn get(&self, index: Idx<String>) -> &str;

    fn fresh_vid(&mut self) -> VId {
        VId::Name(Name::Generated(self.fresh()))
    }

    fn fresh_strid(&mut self) -> StrId {
        StrId::Name(Name::Generated(self.fresh()))
    }

    fn fresh_tyvar(&mut self) -> TyVar {
        TyVar::Name(Name::Generated(self.fresh()))
    }

    fn fresh_tycon(&mut self) -> TyCon {
        TyCon::Name(Name::Generated(self.fresh()))
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct NameInternerImpl {
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

/// Intern built-in identifiers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltIn {
    True,
    False,
    Cons,
    Nil,
}

impl BuiltIn {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::True => "true",
            Self::False => "false",
            Self::Cons => "::",
            Self::Nil => "nil",
        }
    }

    pub fn from_string(s: &str) -> Option<Self> {
        match s {
            "true" => Some(Self::True),
            "false" => Some(Self::False),
            "::" => Some(Self::Cons),
            "nil" => Some(Self::Nil),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Name {
    BuiltIn(BuiltIn),
    String(Idx<String>),
    Generated(u32),
}

impl Name {
    pub fn from_string<I: NameInterner>(s: &str, interner: &mut I) -> Self {
        Self::String(interner.alloc(s))
    }

    pub fn from_builtin(b: BuiltIn) -> Self {
        Self::BuiltIn(b)
    }

    pub fn try_builtin<I: NameInterner>(s: &str, interner: &mut I) -> Self {
        match BuiltIn::from_string(s) {
            Some(b) => Self::BuiltIn(b),
            None => Self::from_string(s, interner),
        }
    }
}

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VId {
    Missing,
    Name(Name),
}

impl VId {
    pub fn from_string<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_string(name, interner))
    }

    pub fn from_builtin(name: BuiltIn) -> Self {
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
        match BuiltIn::from_string(name) {
            Some(b) => Self::from_builtin(b),
            None => Self::from_string(name, interner),
        }
    }

    pub fn is_builtin(&self) -> bool {
        matches!(self, VId::Name(Name::BuiltIn(_)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongStrId {
    pub strid_path: Box<[StrId]>,
    pub strid: StrId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StrId {
    Missing,
    Name(Name),
}

impl StrId {
    pub fn from_string<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_string(name, interner))
    }

    pub fn from_builtin(name: BuiltIn) -> Self {
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
        match BuiltIn::from_string(name) {
            Some(b) => Self::from_builtin(b),
            None => Self::from_string(name, interner),
        }
    }
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyCon {
    Missing,
    Name(Name),
}

impl TyCon {
    pub fn from_string<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_string(name, interner))
    }

    pub fn from_builtin(name: BuiltIn) -> Self {
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
        match BuiltIn::from_string(name) {
            Some(b) => Self::from_builtin(b),
            None => Self::from_string(name, interner),
        }
    }

    pub fn is_builtin(&self) -> bool {
        matches!(self, Self::Name(Name::BuiltIn(_)))
    }
}

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

/// Utility trait for swapping from one interner to another.
pub trait SwitchInterner: Sized {
    fn switch_interner<O, N>(&self, old_interner: &O, new_interner: &mut N) -> Option<Self>
    where
        O: NameInterner,
        N: NameInterner;
}

impl SwitchInterner for Name {
    fn switch_interner<O, N>(&self, old_interner: &O, new_interner: &mut N) -> Option<Self>
    where
        O: NameInterner,
        N: NameInterner,
    {
        if let Name::String(idx) = self {
            let s = old_interner.get(*idx);
            let new_idx = new_interner.alloc(s);
            Some(Name::String(new_idx))
        } else {
            Some(*self)
        }
    }
}

impl SwitchInterner for VId {
    fn switch_interner<O, N>(&self, old_interner: &O, new_interner: &mut N) -> Option<Self>
    where
        O: NameInterner,
        N: NameInterner,
    {
        match self {
            VId::Missing => None,
            VId::Name(n) => n.switch_interner(old_interner, new_interner).map(VId::Name),
        }
    }
}

impl SwitchInterner for StrId {
    fn switch_interner<O, N>(&self, old_interner: &O, new_interner: &mut N) -> Option<Self>
    where
        O: NameInterner,
        N: NameInterner,
    {
        match self {
            StrId::Missing => None,
            StrId::Name(n) => n
                .switch_interner(old_interner, new_interner)
                .map(StrId::Name),
        }
    }
}

impl SwitchInterner for TyCon {
    fn switch_interner<O, N>(&self, old_interner: &O, new_interner: &mut N) -> Option<Self>
    where
        O: NameInterner,
        N: NameInterner,
    {
        match self {
            TyCon::Missing => None,
            TyCon::Name(n) => n
                .switch_interner(old_interner, new_interner)
                .map(TyCon::Name),
        }
    }
}

impl SwitchInterner for LongVId {
    fn switch_interner<O, N>(&self, old_interner: &O, new_interner: &mut N) -> Option<Self>
    where
        O: NameInterner,
        N: NameInterner,
    {
        let vid = self.vid.switch_interner(old_interner, new_interner)?;

        let mut strids = vec![];
        for strid in self.strids.iter() {
            let strid = strid.switch_interner(old_interner, new_interner)?;
            strids.push(strid);
        }

        Some(LongVId {
            strids: strids.into_boxed_slice(),
            vid,
        })
    }
}

impl SwitchInterner for LongTyCon {
    fn switch_interner<O, N>(&self, old_interner: &O, new_interner: &mut N) -> Option<Self>
    where
        O: NameInterner,
        N: NameInterner,
    {
        let tycon = self.tycon.switch_interner(old_interner, new_interner)?;

        let mut strids = vec![];
        for strid in self.strids.iter() {
            let strid = strid.switch_interner(old_interner, new_interner)?;
            strids.push(strid);
        }

        Some(LongTyCon {
            strids: strids.into_boxed_slice(),
            tycon,
        })
    }
}
