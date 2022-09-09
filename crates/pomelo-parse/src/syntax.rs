use pomelo_lex::LexKind;

#[allow(bad_style)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // Sentinel variants
    TOMBSTONE = 0,
    EOF,
    UNKNOWN,
    ERROR,

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

    // Needed punctuation
    DOT,
    STAR, // Only used in TUPLE_TY_EXP

    // Core Grammar: Expressions, Matches, Declarations, and Bindings
    AT_EXP,
    SCON_EXP,
    VID_EXP,
    RECORD_EXP,
    RECORD_SEL_EXP,
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

    PAT_ROW,
    PAT_ROW_PAT,

    PAT,
    CONS_PAT,
    INFIX_VAL_CONS_PAT,
    TY_PAT,
    LAYERED_PAT,

    TY,
    RECORD_TY_EXP,
    TY_CON_EXP,
    FUN_TY_EXP,

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
    SEQ_EXP,
    WHILE_EXP,
    LIST_EXP,

    // Derived patterns and type expressions
    DERIV_PAT,
    UNIT_PAT,
    TUPLE_PAT,
    LIST_PAT,

    LAB_AS_VAR_PAT,
    LAB_AS_VAR_TY,
    LAB_AS_VAR_AS_PAT,

    TUPLE_TY_EXP,

    // Derived function-value bindings and declarations
    DERIV_FUN_BIND,
    FVAL_BIND,
    FVAL_BIND_ROW,

    DERIV_DEC,
    FUN_DEC,
    DATATYPE_WITHTYPE_DEC,
    ABSTYPE_WITHTYPE_DEC,

    // Special constant types
    INT,
    REAL,
    WORD,
    CHAR,
    STRING,

    // Identifier types
    IDENT, // Generic identifier that hasn't been resolved
    VID,
    LONG_VID,
    TY_VAR,
    TY_VAR_SEQ,
    TY_CON,
    LONG_TY_CON,
    LAB,
    STR_ID,
    LONG_STR_ID,

    // Program level
    FILE,
}

impl SyntaxKind {
    pub fn is_special_constant(&self) -> bool {
        use SyntaxKind::*;
        matches!(self, INT | REAL | WORD | CHAR | STRING)
    }

    pub fn is_trivia(&self) -> bool {
        use SyntaxKind::*;
        matches!(self, WHITESPACE | COMMENT)
    }

    pub fn is_dec_kw(&self) -> bool {
        use SyntaxKind::*;
        matches!(
            self,
            VAL_KW
                | FUN_KW
                | TYPE_KW
                | DATATYPE_KW
                | ABSTYPE_KW
                | EXCEPTION_KW
                | LOCAL_KW
                | OPEN_KW
                | INFIX_KW
                | INFIXR_KW
                | NONFIX_KW
        )
    }

    pub fn is_atomic_exp_start(&self) -> bool {
        use SyntaxKind::*;

        self.is_special_constant()
            || matches!(
                self,
                OP_KW | IDENT | L_BRACE | HASH | L_PAREN | L_BRACKET | LET_KW
            )
    }

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
            "." => DOT,
            _ => return None,
        };
        Some(symb)
    }

    pub fn from_lexed(lexkind: LexKind, text: &str) -> (Self, Option<&'static str>) {
        use LexKind::*;
        use SyntaxKind::*;

        let mut err = None;

        let kind = match lexkind {
            Whitespace => WHITESPACE,
            Comment { terminated } => {
                if !terminated {
                    err = Some("unterminated comment");
                }
                COMMENT
            }
            Eq => EQ,
            Colon => COLON,
            Semicolon => SEMICOLON,
            LParen => L_PAREN,
            RParen => R_PAREN,
            LBracket => L_BRACKET,
            RBracket => R_BRACKET,
            LBrace => L_BRACE,
            RBrace => R_BRACE,
            Pipe => PIPE,
            Hash => HASH,
            Comma => COMMA,
            Dot => DOT,
            Ellipsis => ELLIPSIS,
            Underscore => UNDERSCORE,
            ThickArrow => THICK_ARROW,
            ThinArrow => THIN_ARROW,
            Int => INT,
            Word => WORD,
            Real => REAL,
            Char { terminated } => {
                if !terminated {
                    err = Some("unterminated character constant");
                }
                CHAR
            }
            String { terminated } => {
                if !terminated {
                    err = Some("unterminated string constant");
                }
                STRING
            }
            Unknown => {
                err = Some("unknown input character");
                UNKNOWN
            }
            Ident => match Self::from_keyword(text) {
                Some(k) => k,
                None => match text.chars().next() {
                    Some(c) if c == '\'' => TY_VAR,
                    _ => IDENT,
                },
            },
        };
        (kind, err)
    }
}