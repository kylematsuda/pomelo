use crate::arena::{Arena, Idx};

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(String);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct NameData {
    vids: Arena<VId>,
    strids: Arena<StrId>,
    tycons: Arena<TyCon>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongVId {
    pub strids: Box<[Idx<StrId>]>,
    pub vid: Idx<VId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VId {
    Missing,
    Name(Name),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongStrId {
    pub strid_path: Box<[Idx<StrId>]>,
    pub strid: Idx<StrId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StrId {
    Missing,
    Name(Name),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyVar {
    Missing,
    Name(Name),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongTyCon {
    pub strids: Box<[Idx<StrId>]>,
    pub tycon: Idx<TyCon>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyCon {
    Missing,
    Name(Name),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Label {
    Missing,
    Numeric(u32),
    Named(Name),
}
