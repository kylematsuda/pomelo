# pomelo-parse

Please see the [docs](https://kylematsuda.github.io/pomelo/pomelo_parse/index.html) for more details.

- [x] Fix labels (can take numeric values)
- [x] Correctly restrict TyCon to have no stars
- [x] Remove Real constants from patterns
- [x] Make sure all legal vid work 
    - [x] add method to parser to peek correctly
    - [x] cons :: operator
    - [x] special identifiers (e.g., "=") work (only in expressions)
- [x] Reduce tree nesting
    - [x] Compress VID nodes (remove inner IDENT)
    - [x] Improve LONG_VID nodes (sequence of STRID followed by VID?) 
    - [x] Labels 
    - [x] TY_CON? 
    - [x] Remove nesting on EXP, TY, PAT, DEC etc.?
- [ ] Fix infix operators vs function applications (Note: this sort of works but is not currently being used, as it seems like it belongs either in semantic analysis or HIR lowering)
    - [ ] Write more tests!!!
        - [ ] Precedence
        - [ ] L vs R association
        - [ ] Error logging
    - [x] Single pass? Simultaneously:
        - [x] Rearrange the syntax tree
        - [x] Keep track of which infix operators are in scope
    - [ ] Add for patterns too: CONS_PAT vs INFIX_CONS_PAT 
- [x] AST
    - [x] Stub out types
    - [x] Define getters for subparts
    - [x] Switch to Rowan-defined AstNode trait
- [ ] Validation passes
    - [ ] Infix binding strength must be a single digit
    - [ ] Malformed numbers (may need to be more permissive/resilient at lexing stage)
    - [ ] ?? Read Standard to find other constraints
- [ ] Refactor lexer and parser to be more error resilient 
    - [ ] Smarter strategy for lexing numbers?
    - [ ] Don't glue compound special symbols at lexing stage?
    - [ ] ???
- [ ] Stub out parsing for Modules to provide useful error messages while failing
