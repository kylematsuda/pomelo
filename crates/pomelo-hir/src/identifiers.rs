use crate::arena::Idx;
use crate::topdecs::FileArena;
use pomelo_parse::{ast, AstToken};

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(String);

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongVId {
    pub strids: Box<[Idx<StrId>]>,
    pub vid: Idx<VId>,
}

impl LongVId {
    pub fn new_from_node<A: FileArena>(node: &ast::LongVId, arena: &mut A) -> Self {
        let strids = node
            .strids()
            .map(|s| StrId::from_token(Some(s)))
            .map(|s| arena.alloc_strid(s))
            .collect();
        let vid = arena.alloc_vid(VId::from_token(node.vid()));

        Self { strids, vid }
    }

    pub fn new_from_opt_node<A: FileArena>(opt_node: Option<&ast::LongVId>, arena: &mut A) -> Self {
        match opt_node {
            Some(vid) => Self::new_from_node(vid, arena),
            None => Self::missing(arena),
        }
    }

    pub fn missing<A: FileArena>(arena: &mut A) -> Self {
        Self {
            strids: Box::new([]),
            vid: arena.alloc_vid(VId::missing()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VId {
    Missing,
    Name(Name),
}

impl VId {
    pub fn from_token(opt_vid: Option<ast::VId>) -> Self {
        match opt_vid {
            Some(vid) => Self::Name(Name(vid.to_string())),
            None => Self::Missing,
        }
    }

    pub fn from_string(name: String) -> Self {
        Self::Name(Name(name))
    }

    pub fn missing() -> Self {
        Self::Missing
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongStrId {
    pub strid_path: Box<[Idx<StrId>]>,
    pub strid: Idx<StrId>,
}

impl LongStrId {
    pub fn new_from_node<A: FileArena>(node: &ast::LongVId, arena: &mut A) -> Self {
        let mut strids = node
            .strids()
            .map(|s| StrId::from_token(Some(s)))
            .map(|s| arena.alloc_strid(s))
            .collect::<Vec<_>>();
        let strid = strids
            .pop()
            .unwrap_or_else(|| arena.alloc_strid(StrId::missing()));

        Self {
            strid_path: strids.into_boxed_slice(),
            strid,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StrId {
    Missing,
    Name(Name),
}

impl StrId {
    pub fn from_token(opt_strid: Option<ast::StrId>) -> Self {
        match opt_strid {
            Some(strid) => Self::Name(Name(strid.to_string())),
            None => Self::Missing,
        }
    }

    pub fn missing() -> Self {
        Self::Missing
    }
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

impl Label {
    pub fn from_token(opt_label: Option<ast::Label>) -> Self {
        if let Some(label) = opt_label {
            let s = label.syntax().text();

            if let Ok(n) = s.parse::<u32>() {
                Self::Numeric(n)
            } else {
                Self::Named(Name(s.to_owned()))
            }
        } else {
            Self::Missing
        }
    }
}
