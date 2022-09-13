# pomelo-parse

- [x] Fix labels (can take numeric values)
- [x] Correctly restrict TyCon to have no stars
- [x] Remove Real constants from patterns
- [ ] Make sure all legal vid work 
    - [x] add method to parser to peek correctly
    - [x] cons :: operator
    - [x] special identifiers (e.g., "=") work (only in expressions)
- [ ] Reduce tree nesting
    - [x] Compress VID nodes (remove inner IDENT)
    - [x] Improve LONG_VID nodes (sequence of STRID followed by VID?) 
    - [x] Labels 
    - [x] TY_CON? 
    - [x] Remove nesting on EXP, TY, PAT, DEC etc.?
    -     - [x] Can hold the inner variants in an enum when going to AST
    - [ ] Look for other places where we can do this...
- [ ] AST
