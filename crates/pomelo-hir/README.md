# pomelo-hir

- [ ] Handle generated nodes, probably by wrapping the current `ast_id` field with (several) enums
- [ ] Handle generated temporary names created by lowering; may need to add this to the `FileArena` trait?
    - [ ] Can impl with a counter in the arena struct
