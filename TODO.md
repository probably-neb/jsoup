- [x] Replacing collections
  - [x] consolidate code paths for splicing and updating ranges
- [x] `insert`
  - [x] array values
  - [x] object keys
  - [x] fuzz
  - [x] unify index updates
- [x] `remove` values
  - [x] just takes index to remove
  - [x] if key remove value as well
  - [x] if value for key, then set to null?
  - [x] fuzz
- [x] Add error type for replace
- [x] use `usize::signed_diff` instead of `+ positive - negative`
- [x] Improve container tok range
  - start will always be index + 1, store `term` as last index of subtree starting at index
    * recursing on end to find complete end
    * iterating over all items to find last item index
  - therefore
    * complete subtree range will be `container_index..tok_term[container_index].end_complete`
    * first item index will be `container_index + 1`
    * to check if container is empty, check if `last_item_index == 0`
    * when container is empty, `tok_term[container_index] == container_index` so removal does not require any additional logic
- [ ] Builder API for simultaneous creation of formatted JSON as well as tree
  - useful for replacement APIs
- [ ] Proc macro like `serde_json_lenient::json!` that lowers to builder
- [ ] Move from taking `serde_json::Value` to `JsonAst` based on builder pattern
- [ ] maintain formatting in updates
  - [ ] `format_style_from` that takes an index, and returns a `FormatStyle`
      * useful for limiting scope of CRUD ops, where args can be created with correct formatting before insertion
      * probably only makes sense for containers.
- [ ] Add `tok_cmma` to store index of comma after `index`
    * will avoid issues found in Zed where we have to parse comma location without knowing if it's in comment
- [ ] Add benchmarks
  - before unifying update logic
  - options
    * insert 1000 values into array
    * insert values into very deeply nested value
- [ ] Cursor API for traversal
- [ ] explore more efficient updating: splitting into gather and apply steps to amoritize actual full updates with expensive splice calls
- [ ] improve update efficiency
    * avoiding allocating new values as much as possible
      * convert `serde::Value` to tree directly (tied to maintaining formatting in updates)
    * JsonAstRef type with `&'tree []` types?
- [ ] Deeper serde interop
  - `From<JsonAst> for serde_json::Value`
  - `From<serde_json::Value> for JsonAst`
  - can then simplify insert, replace, remove, etc to take trees instead
  - if have `format_style_from` then these should take `FormatStyle`
- [ ] Error recovering parsing
  - goal is to parse as much as possible while marking sub regions as errors
  - return type becomes `(tree, List<Error>)` always
- [ ] JSON5 parsing
- ? maintaining comments in objects when replacing object with very similar object
