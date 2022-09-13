# pomelo

## Plan

- [ ] Lexer and parser
    - [ ] Don't glue tokens at lex stage to improve error messages?
    - [ ] Don't reject invalid number forms (like "1e5") at lex time for better error messages?
- [ ] Concrete syntax tree: resolve infix vs applications
    - [ ] Context: hold names that are infix + fixity + span (scope) of infix decl
    - [ ] Remap nodes to resolve based on fixity and precedence rules 
- [ ] AST: define AST similar to r-a and apollo-rs 
    - [x] Stub out types
    - [ ] Define getters for subparts
- [ ] HIR:
    - [ ] Figure out a good representation...
    - [ ] Desugar derived forms 
- [ ] Type inference
- [ ] Type check 
- [ ] LSP (once the above are finished)
- [ ] Figure out runtime
    - [ ] GC
    - [ ] Bytecode or treewalk interp? Need something to test
- [ ] Codegen
    - [ ] Native? (LLVM?)
    - [ ] WASM? (Cranelift?)

