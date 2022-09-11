# pomelo-parse

[x] Fix labels (can take numeric values)
[ ] Correctly restrict TyCon to have no stars
[x] Remove Real constants from patterns
[ ] Make sure all legal vid work 
    [ ] add method to parser to peek correctly
    [ ] cons :: operator
    [ ] special identifiers (e.g., "=") work
[ ] Reduce tree nesting
    [x] Compress VID nodes (remove inner IDENT)
    [x] Improve LONG_VID nodes (sequence of STRID followed by VID?) 
    [x] Labels 
    [x] TY_CON? 
    [ ] Look for other places where we can do this...
