# pomelo-hir

- [x] Handle generated nodes, probably by wrapping the current `ast_id` field with (several) enums
- [x] Handle generated temporary names created by lowering; may need to add this to the `FileArena` trait?
- [ ] Make a `LoweringCtxt` type or something to track infix vs app during lowering?
- [ ] Document/understand the difference between topdecs and bodies
- [ ] Clean up crate structure?
