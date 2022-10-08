use crate::arena::Idx;
use crate::core::BodyArena;
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
    pub fn from_node<A: FileArena>(node: &ast::LongVId, arena: &mut A) -> Self {
        let strids = node
            .strids()
            .map(|s| StrId::from_token(Some(s), arena))
            .collect();
        let vid = VId::from_token(node.vid(), arena);

        Self { strids, vid }
    }

    pub fn from_opt_node<A: FileArena>(opt_node: Option<&ast::LongVId>, arena: &mut A) -> Self {
        match opt_node {
            Some(vid) => Self::from_node(vid, arena),
            None => Self::missing(arena),
        }
    }

    pub fn missing<A: FileArena>(arena: &mut A) -> Self {
        Self {
            strids: Box::new([]),
            vid: VId::missing(arena),
        }
    }

    pub fn from_vid(vid: Idx<VId>) -> Self {
        Self {
            strids: Box::new([]),
            vid,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VId {
    Missing,
    Name(Name),
}

impl VId {
    pub fn from_token<A: FileArena>(opt_vid: Option<ast::VId>, arena: &mut A) -> Idx<Self> {
        match opt_vid {
            Some(vid) => Self::from_str(vid.syntax().text(), arena),
            None => Self::missing(arena),
        }
    }

    pub fn from_str<A: FileArena>(name: &str, arena: &mut A) -> Idx<Self> {
        arena.alloc_vid(Self::Name(Name(name.to_owned())))
    }

    pub fn missing<A: FileArena>(arena: &mut A) -> Idx<Self> {
        arena.alloc_vid(Self::Missing)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongStrId {
    pub strid_path: Box<[Idx<StrId>]>,
    pub strid: Idx<StrId>,
}

impl LongStrId {
    pub fn from_node<A: FileArena>(node: &ast::LongStrId, arena: &mut A) -> Self {
        let mut strids = node
            .strids()
            .map(|s| StrId::from_token(Some(s), arena))
            .collect::<Vec<_>>();
        let strid = strids.pop().unwrap_or_else(|| StrId::missing(arena));

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
    pub fn from_token<A: FileArena>(opt_strid: Option<ast::StrId>, arena: &mut A) -> Idx<Self> {
        match opt_strid {
            Some(strid) => Self::from_str(strid.syntax().text(), arena),
            None => Self::missing(arena),
        }
    }

    pub fn from_str<A: FileArena>(name: &str, arena: &mut A) -> Idx<Self> {
        arena.alloc_strid(Self::Name(Name(name.to_owned())))
    }

    pub fn missing<A: FileArena>(arena: &mut A) -> Idx<Self> {
        arena.alloc_strid(Self::Missing)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyVar {
    Missing,
    Name(Name),
}

impl TyVar {
    pub fn from_token<A: BodyArena>(opt_tyvar: Option<ast::TyVar>, arena: &mut A) -> Idx<Self> {
        match opt_tyvar {
            None => Self::missing(arena),
            Some(t) => Self::from_str(t.syntax().text(), arena),
        }
    }

    pub fn from_str<A: BodyArena>(name: &str, arena: &mut A) -> Idx<Self> {
        arena.alloc_tyvar(Self::Name(Name(name.to_owned())))
    }

    pub fn missing<A: BodyArena>(arena: &mut A) -> Idx<Self> {
        arena.alloc_tyvar(Self::Missing)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongTyCon {
    pub strids: Box<[Idx<StrId>]>,
    pub tycon: Idx<TyCon>,
}

impl LongTyCon {
    pub fn from_node<A: FileArena>(node: &ast::LongTyCon, arena: &mut A) -> Self {
        let strids = node
            .strids()
            .map(|s| StrId::from_token(Some(s), arena))
            .collect();
        let tycon = TyCon::from_token(node.tycon(), arena);

        Self { strids, tycon }
    }

    pub fn from_opt_node<A: FileArena>(opt_node: Option<&ast::LongTyCon>, arena: &mut A) -> Self {
        match opt_node {
            None => Self::missing(arena),
            Some(t) => Self::from_node(t, arena),
        }
    }

    pub fn missing<A: FileArena>(arena: &mut A) -> Self {
        let tycon = arena.alloc_tycon(TyCon::Missing);
        Self {
            strids: Box::new([]),
            tycon,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyCon {
    Missing,
    Name(Name),
}

impl TyCon {
    pub fn from_token<A: FileArena>(opt_tycon: Option<ast::TyCon>, arena: &mut A) -> Idx<Self> {
        match opt_tycon {
            None => Self::missing(arena),
            Some(t) => Self::from_str(t.syntax().text(), arena),
        }
    }

    pub fn from_str<A: FileArena>(name: &str, arena: &mut A) -> Idx<Self> {
        arena.alloc_tycon(Self::Name(Name(name.to_owned())))
    }

    pub fn missing<A: FileArena>(arena: &mut A) -> Idx<Self> {
        arena.alloc_tycon(Self::Missing)
    }
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
