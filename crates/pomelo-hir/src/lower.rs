use crate::topdecs::{CoreDec, File, FileData, TopDec};
use pomelo_parse::ast;

enum OneOrMany<T> {
    One(T),
    Many(Vec<T>),
}

impl File {
    pub fn from_syntax(node: ast::File) -> Self {
        let mut decs = vec![];
        let mut data = FileData::default();

        for dec in node.declarations() {
            match CoreDec::from_syntax(dec, &mut data) {
                OneOrMany::One(d) => decs.push(TopDec::Core(d)),
                OneOrMany::Many(d) => decs.extend(d.into_iter().map(TopDec::Core)),
            }
        }

        Self {
            decs,
            data: Box::new(data),
        }
    }
}

impl CoreDec {
    fn from_syntax(node: ast::Dec, data: &mut FileData) -> OneOrMany<CoreDec> {
        match node {
            ast::Dec::Val(v) => Self::val(v, data),
            ast::Dec::Fun(f) => Self::fun(f, data),
            ast::Dec::Type(t) => Self::ty(t, data),
            ast::Dec::Datatype(d) => Self::datatype(d, data),
            ast::Dec::DatatypeRep(r) => Self::replication(r, data),
            ast::Dec::Abstype(a) => Self::abstype(a, data),
            ast::Dec::Exception(e) => Self::exception(e, data),
            ast::Dec::Local(l) => Self::local(l, data),
            ast::Dec::Open(o) => Self::open(o, data),
            ast::Dec::Infix(i) => Self::infix(i, data),
            ast::Dec::Infixr(i) => Self::infixr(i, data),
            ast::Dec::Nonfix(n) => Self::nonfix(n, data),
            ast::Dec::Seq(_) => todo!(),
        }
    }

    fn val(node: ast::ValDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn fun(node: ast::FunDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn ty(node: ast::TypeDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn datatype(node: ast::DatatypeDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn replication(node: ast::DatatypeRepDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn abstype(node: ast::AbstypeDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn exception(node: ast::ExceptionDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn local(node: ast::LocalDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn open(node: ast::OpenDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn infix(node: ast::InfixDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn infixr(node: ast::InfixrDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }

    fn nonfix(node: ast::NonfixDec, data: &mut FileData) -> OneOrMany<Self> {
        todo!()
    }
}
