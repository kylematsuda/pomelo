//!  
//!
//! Inspired by rust-analyzer/crates/parser/src/syntax_kind/generated.rs

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kind {
    Whitespace,
    Comment,
    Eq,
    Colon,
    LParen,
    RParen,
    Pipe,
    ValStmt,
    ValKw,
    LetStmt,
    LetKw,
    InKw,
    EndKw,
    IfStmt,
    IfKw,
    ThenKw,
    ElseKw,
    Fun,
    FunKw,
    Fn,
    FnKw,
    Ident,
    Int,
    Real,
    Char,
    String,
    Unknown,
}

impl Kind {
    pub fn is_keyword(self) -> bool {
        use Kind::*;

        matches!(
            self,
            ValKw | LetKw | InKw | EndKw | IfKw | ThenKw | ElseKw | FunKw | FnKw
        )
    }

    pub fn from_keyword(s: &str) -> Option<Self> {
        use Kind::*;

        let token = match s {
            "val" => ValKw,
            "let" => LetKw,
            "in" => InKw,
            "end" => EndKw,
            "if" => IfKw,
            "then" => ThenKw,
            "else" => ElseKw,
            "fun" => FunKw,
            "fn" => FnKw,
            _ => return None,
        };
        Some(token)
    }

    pub fn from_char(c: char) -> Option<Self> {
        use Kind::*;

        let token = match c {
            '=' => Eq,
            ':' => Colon,
            '(' => LParen,
            ')' => RParen,
            '|' => Pipe,
            _ => return None,
        };
        Some(token)
    }
}

#[macro_export]
macro_rules! T {
    [=] => { $crate::Kind::Eq };
    [:] => { $crate::Kind::Colon };
    ['('] => { $crate::Kind::LParen};
    [')'] => { $crate::Kind::RParen};
    [|] => { $crate::Kind::Pipe};
    [val] => { $crate::Kind::ValKw };
    [let] => { $crate::Kind::LetKw };
    [in] => { $crate::Kind::InKw };
    [end] => { $crate::Kind::EndKw };
    [if] => { $crate::Kind::IfKw };
    [then] => { $crate::Kind::ThenKw };
    [else] => { $crate::Kind::ElseKw };
    [fun] => { $crate::Kind::FunKw };
    [fn] => { $crate::Kind::FunKw };
}
