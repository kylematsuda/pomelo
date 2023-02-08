//! Builtin names and symbols.
use crate::Fixity;

/// Built-in identifiers in the HIR.
///
/// Check if this is complete...
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Builtin {
    // Type constructors
    // Appendix C of Defn
    Unit,
    Bool,
    Int,
    Word,
    Real,
    String,
    Char,
    List,
    Ref,
    Exn,
    // Values
    True,
    False,
    Nil,
    // Operators
    Star,
    Slash,
    Div,
    Mod,
    Plus,
    Minus,
    Carat,
    Cons,
    At,
    Eq,
    Ineq,
    Gtr,
    GtrEq,
    Less,
    LessEq,
    RefAssign,
    O,
    Before,
}

impl Builtin {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Unit => "()",
            Self::Bool => "bool",
            Self::Int => "int",
            Self::Word => "word",
            Self::Real => "real",
            Self::String => "string",
            Self::Char => "char",
            Self::List => "list",
            Self::Ref => "ref",
            Self::Exn => "exn",
            Self::True => "true",
            Self::False => "false",
            Self::Nil => "nil",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Div => "div",
            Self::Mod => "mod",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Carat => "^",
            Self::Cons => "::",
            Self::At => "@",
            Self::Eq => "=",
            Self::Ineq => "<>",
            Self::Gtr => ">",
            Self::GtrEq => ">=",
            Self::Less => "<",
            Self::LessEq => "<=",
            Self::RefAssign => ":=",
            Self::O => "o",
            Self::Before => "before",
        }
    }

    pub fn from_string(s: &str) -> Option<Self> {
        Some(match s {
            "()" | "{}" => Self::Unit,
            "bool" => Self::Bool,
            "int" => Self::Int,
            "word" => Self::Word,
            "real" => Self::Real,
            "string" => Self::String,
            "char" => Self::Char,
            "list" => Self::List,
            "ref" => Self::Ref,
            "exn" => Self::Exn,
            "true" => Self::True,
            "false" => Self::False,
            "nil" => Self::Nil,
            "*" => Self::Star,
            "/" => Self::Slash,
            "div" => Self::Div,
            "mod" => Self::Mod,
            "+" => Self::Plus,
            "-" => Self::Minus,
            "^" => Self::Carat,
            "::" => Self::Cons,
            "@" => Self::At,
            "=" => Self::Eq,
            "<>" => Self::Ineq,
            ">" => Self::Gtr,
            ">=" => Self::GtrEq,
            "<" => Self::Less,
            "<=" => Self::LessEq,
            ":=" => Self::RefAssign,
            "o" => Self::O,
            "before" => Self::Before,
            _ => return None,
        })
    }
}

pub const BUILTIN_TYCONS: [Builtin; 10] = [
    Builtin::Unit,
    Builtin::Bool,
    Builtin::Int,
    Builtin::Word,
    Builtin::Real,
    Builtin::String,
    Builtin::Char,
    Builtin::List,
    Builtin::Ref,
    Builtin::Exn,
];

pub const BUILTIN_VALUES: [Builtin; 3] = [Builtin::True, Builtin::False, Builtin::Nil];

#[rustfmt::skip]
pub const BUILTIN_INFIX: [(Builtin, Fixity); 18] = [
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
