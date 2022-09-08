#![allow(bad_style)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxKind {
    // Sentinel variants
    TOMBSTONE,
    EOF,

    // Unevaluated
    WHITESPACE,
    COMMENT,

    // Reserved words
    ABSTYPE_KW,
    AND_KW,
    ANDALSO_KW,
    AS_KW,
    CASE_KW,
    DATATYPE_KW,
    DO_KW,
    ELSE_KW,
    END_KW,
    EXCEPTION_KW,
    FN_KW,
    FUN_KW,
    HANDLE_KW,
    IF_KW,
    IN_KW,
    INFIX_KW,
    INFIXR_KW,
    LET_KW,
    LOCAL_KW,
    NONFIX_KW,
    OF_KW,
    OP_KW,
    OPEN_KW,
    ORELSE_KW,
    RAISE_KW,
    REC_KW,
    THEN_KW,
    TYPE_KW,
    VAL_KW,
    WITH_KW,
    WITHTYPE_KW,
    WHILE_KW,
    L_PAREN,
    R_PAREN,
    L_BRACKET,
    R_BRACKET,
    L_BRACE,
    R_BRACE,
    COMMA,
    COLON,
    SEMICOLON,
    ELLIPSIS,
    UNDERSCORE,
    PIPE,
    EQ,
    THICK_ARROW,
    THIN_ARROW,
    HASH,

    // Core Grammar: Expressions, Matches, Declarations, and Bindings
    AT_EXP,
    SCON_EXP,
    SCON,
    VID_EXP,
    LONGVID,
    RECORD_EXP,
    LET_DEC,
    PAREN_EXP,

    EXP_ROW,

    EXP,
    APP_EXP,
    INFIX_EXP,
    TY_EXP,
    HANDLE_EXP,
    RAISE_EXP,
    FN_EXP,

    MATCH,
    MRULE,

    DEC,
    VAL_DEC,
    TY_DEC,
    DATATYPE_DEC,
    DATATYPE_REP,
    ABSTYPE_DEC,
    EXCEPT_DEC,
    LOCAL_DEC,
    OPEN_DEC,
    SEQ_DEC,
    INFIX_DEC,
    INFIXR_DEC,
    NONFIX_DEC,

    VAL_BIND,
    VAL_BIND_NO_REC,
    VAL_BIND_REC,

    TY_BIND,
    DAT_BIND,
    CON_BIND,

    EX_BIND,
    EX_BIND_NOEQ,
    EX_BIND_EQ,

    // Core Grammar: Patterns and Type expressions
    AT_PAT,
    WILDCARD_PAT,
    SCON_PAT,
    VID_PAT,
    RECORD_PAT,
    PAREN_PAT,

    PAT_ROW,
    PAT_ROW_PAT,

    PAT,
    CONS_PAT,
    INFIX_VAL_CONS_PAT,
    TY_PAT,
    LAYERED_PAT,

    TY,
    TY_VAR,
    RECORD_TY_EXP,
    TY_CONS,
    FUN_TY_EXPR,
    PAREN_TY,

    TY_ROW,

    // Derived expressions
    DERIV_EXP,
    UNIT_EXP,
    TUPLE_EXP,
    ACCESSOR_EXP,
    CASE_MATCH_EXP,
    IF_EXP,
    ORELSE_EXP,
    ANDALSO_EXP,
    CASE_SEQ_EXP,
    LET_SEQ_EXP,
    WHILE_EXP,
    LIST_EXP,

    // Derived patterns and type expressions
    DERIV_PAT,
    UNIT_PAT,
    TUPLE_PAT,
    LIST_PAT,

    DERIV_PAT_ROW,
    DERIV_PAT_ROW_PAT,

    DERIV_TY_EXP,
    TY_PROD_EXP,

    // Derived function-value bindings and declarations
    DERIV_FUN_BIND,
    FVAL_BIND,

    DERIV_DEC,
    FUN_DEC,
    DATATYPE_WITHTYPE_DEC,
    ABSTYPE_WITHTYPE_DEC,
}

impl SyntaxKind {
    pub fn from_keyword(s: &str) -> Option<Self> {
        use SyntaxKind::*;

        let kw = match s {
            "abstype" => ABSTYPE_KW,
            "and" => AND_KW,
            "andalso" => ANDALSO_KW,
            "as" => AS_KW,
            "case" => CASE_KW,
            "datatype" => DATATYPE_KW,
            "do" => DO_KW,
            "else" => ELSE_KW,
            "end" => END_KW,
            "exception" => EXCEPTION_KW,
            "fn" => FN_KW,
            "fun" => FUN_KW,
            "handle" => HANDLE_KW,
            "if" => IF_KW,
            "in" => IN_KW,
            "infix" => INFIX_KW,
            "infixr" => INFIXR_KW,
            "let" => LET_KW,
            "local" => LOCAL_KW,
            "nonfix" => NONFIX_KW,
            "of" => OF_KW,
            "op" => OP_KW,
            "open" => OPEN_KW,
            "orelse" => ORELSE_KW,
            "raise" => RAISE_KW,
            "rec" => REC_KW,
            "then" => THEN_KW,
            "type" => TYPE_KW,
            "val" => VAL_KW,
            "with" => WITH_KW,
            "withtype" => WITHTYPE_KW,
            "while" => WHILE_KW,
            _ => return None,
        };
        Some(kw)
    }

    pub fn from_symbol(s: &str) -> Option<Self> {
        use SyntaxKind::*;

        let symb = match s {
            "(" => L_PAREN,
            ")" => R_PAREN,
            "[" => L_BRACKET,
            "]" => R_BRACKET,
            "{" => L_BRACE,
            "}" => R_BRACE,
            "," => COMMA,
            ":" => COLON,
            ";" => SEMICOLON,
            "..." => ELLIPSIS,
            "_" => UNDERSCORE,
            "|" => PIPE,
            "=" => EQ,
            "=>" => THICK_ARROW,
            "->" => THIN_ARROW,
            "#" => HASH,
            _ => return None,
        };
        Some(symb)
    }
}
