# pomelo-parse

[ ] Fix labels (can take numeric values)
[ ] Correctly restrict TyCon to have no stars
[ ] Remove Real constants from patterns
[ ] Make sure all legal vid work 
    [ ] add method to parser to peek correctly
    [ ] cons :: operator
    [ ] special identifiers (e.g., "=") work
[ ] Reduce tree nesting
    [ ] Compress VID and LONG_VID nodes (remove inner IDENT)
    [ ] Labels 
    [ ] VID_PAT? VID_EXP?
    [ ] LONG_TY_CON? TY_VAR?
    [ ] (Then these will be treated as tokens instead of nodes)
    [ ] Look for other places where we can do this...
