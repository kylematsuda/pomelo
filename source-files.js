var sourcesIndex = JSON.parse('{\
"pomelo":["",[],["lib.rs"]],\
"pomelo_fmt":["",[],["buffer.rs","dec.rs","expr.rs","lib.rs","pat.rs","printer.rs","ty.rs","util.rs"]],\
"pomelo_hir":["",[["lower",[],["context.rs","dec.rs","expr.rs","infix.rs","pat.rs","token.rs","ty.rs","util.rs"]]],["arena.rs","builtins.rs","hir.rs","identifiers.rs","lib.rs","lower.rs","pretty.rs"]],\
"pomelo_lex":["",[],["cursor.rs","lib.rs","token.rs"]],\
"pomelo_parse":["",[["ast",[],["bindings.rs","constants.rs","declarations.rs","expressions.rs","identifiers.rs","matches.rs","patterns.rs","type_expressions.rs"]],["grammar",[],["bindings.rs","combinators.rs","declarations.rs","expressions.rs","identifiers.rs","matches.rs","patterns.rs","type_expressions.rs"]]],["ast.rs","grammar.rs","language.rs","lib.rs","parser.rs","passes.rs","syntax.rs"]],\
"pomelo_ty":["",[],["lib.rs","usages.rs"]]\
}');
createSourceSidebar();
