//! Resolve infix vs applicative expressions and patterns.

use crate::{Builtin, Fixity};

#[rustfmt::skip]
const BUILTINS: [(Builtin, Fixity); 18] = [
    (Builtin::Star,	    Fixity::Left(Some(7))),
    (Builtin::Slash,	Fixity::Left(Some(7))),
    (Builtin::Div,		Fixity::Left(Some(7))),
    (Builtin::Mod,		Fixity::Left(Some(7))),
    (Builtin::Plus,		Fixity::Left(Some(6))),
    (Builtin::Minus,	Fixity::Left(Some(6))),
    (Builtin::Carat,	Fixity::Left(Some(6))),
    (Builtin::Cons,		Fixity::Right(Some(5))),
    (Builtin::At,		Fixity::Right(Some(5))),
    (Builtin::Eq,		Fixity::Left(Some(4))),
    (Builtin::Ineq,		Fixity::Left(Some(4))),
    (Builtin::Gtr,		Fixity::Left(Some(4))),
    (Builtin::GtrEq,    Fixity::Left(Some(4))),
    (Builtin::Less,		Fixity::Left(Some(4))),
    (Builtin::LessEq,	Fixity::Left(Some(4))),
    (Builtin::RefAssign,Fixity::Left(Some(3))),
    (Builtin::O,		Fixity::Left(Some(3))),
    (Builtin::Before,	Fixity::Left(Some(0))),
];

/// Maximum user-defined fixity is 9
const FN_APPL: Fixity = Fixity::Left(Some(10));
