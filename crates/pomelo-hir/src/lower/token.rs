use pomelo_parse::{ast, AstToken};

use crate::identifiers::{Label, Name, StrId};
use crate::lower::LoweringCtxt;
use crate::{LongStrId, LongTyCon, LongVId, TyCon, TyVar, VId};

impl VId {
    pub fn from_token(ctx: &mut LoweringCtxt, opt_vid: Option<ast::VId>) -> Self {
        match opt_vid {
            Some(vid) => Self::try_builtin(vid.syntax.text(), ctx.interner_mut()),
            None => Self::missing(),
        }
    }
}

impl TyVar {
    pub fn from_token(ctx: &mut LoweringCtxt, opt_tyvar: Option<ast::TyVar>) -> Self {
        match opt_tyvar {
            None => Self::missing(),
            Some(t) => Self::from_string(t.syntax().text(), ctx.interner_mut()),
        }
    }
}

impl LongVId {
    pub fn from_node(ctx: &mut LoweringCtxt, node: &ast::LongVId) -> Self {
        let strids = node
            .strids()
            .map(|s| StrId::from_token(ctx, Some(s)))
            .collect();
        let vid = VId::from_token(ctx, node.vid());

        Self { strids, vid }
    }

    pub fn from_opt_node(ctx: &mut LoweringCtxt, opt_node: Option<&ast::LongVId>) -> Self {
        match opt_node {
            Some(vid) => Self::from_node(ctx, vid),
            None => Self::missing(),
        }
    }
}

impl StrId {
    pub fn from_token(ctx: &mut LoweringCtxt, opt_strid: Option<ast::StrId>) -> Self {
        match opt_strid {
            Some(strid) => Self::try_builtin(strid.syntax().text(), ctx.interner_mut()),
            None => Self::missing(),
        }
    }
}

impl LongStrId {
    pub fn from_node(ctx: &mut LoweringCtxt, node: &ast::LongStrId) -> Self {
        let mut strids = node
            .strids()
            .map(|s| StrId::from_token(ctx, Some(s)))
            .collect::<Vec<_>>();
        let strid = strids.pop().unwrap_or_else(StrId::missing);

        Self {
            strid_path: strids.into_boxed_slice(),
            strid,
        }
    }
}

impl TyCon {
    pub fn from_token(ctx: &mut LoweringCtxt, opt_tycon: Option<ast::TyCon>) -> Self {
        match opt_tycon {
            None => Self::missing(),
            Some(t) => Self::try_builtin(t.syntax().text(), ctx.interner_mut()),
        }
    }
}

impl LongTyCon {
    pub fn from_node(ctx: &mut LoweringCtxt, node: &ast::LongTyCon) -> Self {
        let strids = node
            .strids()
            .map(|s| StrId::from_token(ctx, Some(s)))
            .collect();
        let tycon = TyCon::from_token(ctx, node.tycon());

        Self { strids, tycon }
    }

    pub fn from_opt_node(ctx: &mut LoweringCtxt, opt_node: Option<&ast::LongTyCon>) -> Self {
        match opt_node {
            None => Self::missing(),
            Some(t) => Self::from_node(ctx, t),
        }
    }
}

impl Label {
    pub fn from_token(ctx: &mut LoweringCtxt, opt_label: Option<ast::Label>) -> Self {
        if let Some(label) = opt_label {
            let s = label.syntax().text();

            if let Ok(n) = s.parse::<u32>() {
                Self::Numeric(n)
            } else {
                Self::Named(Name::from_string(s, ctx.interner_mut()))
            }
        } else {
            Self::Missing
        }
    }
}
