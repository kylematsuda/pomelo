use crate::arena::{Arena, Idx};
use pomelo_parse::{ast, AstToken};

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
    generated: u32,
}

impl NameInterner for NameInternerImpl {
    fn fresh(&mut self) -> u32 {
        let out = self.generated;
        self.generated += 1;
        out
    }

    fn alloc(&mut self, s: &str) -> Idx<String> {
        self.names.alloc(s.to_owned())
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

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "true" => Some(Self::True),
            "false" => Some(Self::False),
            "::" => Some(Self::Cons),
            "nil" => Some(Self::Nil),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Name {
    BuiltIn(BuiltIn),
    String(Idx<String>),
    Generated(u32),
}

impl Name {
    pub fn from_str<I: NameInterner>(s: &str, interner: &mut I) -> Self {
        Self::String(interner.alloc(s))
    }

    pub fn from_builtin(b: BuiltIn) -> Self {
        Self::BuiltIn(b)
    }

    pub fn try_builtin<I: NameInterner>(s: &str, interner: &mut I) -> Self {
        match BuiltIn::from_str(s) {
            Some(b) => Self::BuiltIn(b),
            None => Self::from_str(s, interner),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongVId {
    pub strids: Box<[StrId]>,
    pub vid: VId,
}

impl LongVId {
    pub fn from_node<I: NameInterner>(node: &ast::LongVId, arena: &mut I) -> Self {
        let strids = node
            .strids()
            .map(|s| StrId::from_token(Some(s), arena))
            .collect();
        let vid = VId::from_token(node.vid(), arena);

        Self { strids, vid }
    }

    pub fn from_opt_node<I: NameInterner>(opt_node: Option<&ast::LongVId>, arena: &mut I) -> Self {
        match opt_node {
            Some(vid) => Self::from_node(vid, arena),
            None => Self::missing(),
        }
    }

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VId {
    Missing,
    Name(Name),
}

impl VId {
    pub fn from_token<I: NameInterner>(opt_vid: Option<ast::VId>, interner: &mut I) -> Self {
        match opt_vid {
            Some(vid) => Self::try_builtin(vid.syntax().text(), interner),
            None => Self::missing(),
        }
    }

    pub fn from_str<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_str(name, interner))
    }

    pub fn from_builtin(name: BuiltIn) -> Self {
        Self::Name(Name::from_builtin(name))
    }

    pub fn missing() -> Self {
        Self::Missing
    }

    fn try_builtin<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        match BuiltIn::from_str(name) {
            Some(b) => Self::from_builtin(b),
            None => Self::from_str(name, interner),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongStrId {
    pub strid_path: Box<[StrId]>,
    pub strid: StrId,
}

impl LongStrId {
    pub fn from_node<A: NameInterner>(node: &ast::LongStrId, arena: &mut A) -> Self {
        let mut strids = node
            .strids()
            .map(|s| StrId::from_token(Some(s), arena))
            .collect::<Vec<_>>();
        let strid = strids.pop().unwrap_or_else(|| StrId::missing());

        Self {
            strid_path: strids.into_boxed_slice(),
            strid,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StrId {
    Missing,
    Name(Name),
}

impl StrId {
    pub fn from_token<I: NameInterner>(opt_strid: Option<ast::StrId>, interner: &mut I) -> Self {
        match opt_strid {
            Some(strid) => Self::try_builtin(strid.syntax().text(), interner),
            None => Self::missing(),
        }
    }

    pub fn from_str<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_str(name, interner))
    }

    pub fn from_builtin(name: BuiltIn) -> Self {
        Self::Name(Name::from_builtin(name))
    }

    pub fn missing() -> Self {
        Self::Missing
    }

    fn try_builtin<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        match BuiltIn::from_str(name) {
            Some(b) => Self::from_builtin(b),
            None => Self::from_str(name, interner),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyVar {
    Missing,
    Name(Name),
}

impl TyVar {
    pub fn from_token<I: NameInterner>(opt_tyvar: Option<ast::TyVar>, interner: &mut I) -> Self {
        match opt_tyvar {
            None => Self::missing(),
            Some(t) => Self::from_str(t.syntax().text(), interner),
        }
    }

    pub fn from_str<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_str(name, interner))
    }

    pub fn missing() -> Self {
        Self::Missing
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LongTyCon {
    pub strids: Box<[StrId]>,
    pub tycon: TyCon,
}

impl LongTyCon {
    pub fn from_node<I: NameInterner>(node: &ast::LongTyCon, arena: &mut I) -> Self {
        let strids = node
            .strids()
            .map(|s| StrId::from_token(Some(s), arena))
            .collect();
        let tycon = TyCon::from_token(node.tycon(), arena);

        Self { strids, tycon }
    }

    pub fn from_opt_node<I: NameInterner>(
        opt_node: Option<&ast::LongTyCon>,
        arena: &mut I,
    ) -> Self {
        match opt_node {
            None => Self::missing(),
            Some(t) => Self::from_node(t, arena),
        }
    }

    pub fn missing() -> Self {
        Self {
            strids: Box::new([]),
            tycon: TyCon::missing(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyCon {
    Missing,
    Name(Name),
}

impl TyCon {
    pub fn from_token<I: NameInterner>(opt_tycon: Option<ast::TyCon>, interner: &mut I) -> Self {
        match opt_tycon {
            None => Self::missing(),
            Some(t) => Self::try_builtin(t.syntax().text(), interner),
        }
    }

    pub fn from_str<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        Self::Name(Name::from_str(name, interner))
    }

    pub fn from_builtin(name: BuiltIn) -> Self {
        Self::Name(Name::from_builtin(name))
    }

    pub fn missing() -> Self {
        Self::Missing
    }

    fn try_builtin<I: NameInterner>(name: &str, interner: &mut I) -> Self {
        match BuiltIn::from_str(name) {
            Some(b) => Self::from_builtin(b),
            None => Self::from_str(name, interner),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Label {
    Missing,
    Numeric(u32),
    Named(Name),
}

impl Label {
    pub fn from_token<I: NameInterner>(opt_label: Option<ast::Label>, interner: &mut I) -> Self {
        if let Some(label) = opt_label {
            let s = label.syntax().text();

            if let Ok(n) = s.parse::<u32>() {
                Self::Numeric(n)
            } else {
                Self::Named(Name::from_str(s, interner))
            }
        } else {
            Self::Missing
        }
    }

    pub fn numeric(n: u32) -> Self {
        Self::Numeric(n)
    }

    pub fn missing() -> Self {
        Self::Missing
    }
}
