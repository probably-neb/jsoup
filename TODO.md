- [x] Replacing collections
  - [x] consolidate code paths for splicing and updating ranges
- [*] `insert`
  - [x] array values
  - [x] object keys
  - [x] fuzz
  - [ ] unify index updates
- [ ] Add error type for replace
- [ ] `remove` values
  - just takes index to remove
  - [ ] if key remove value as well
  - [ ] if value for key, then set to null?
- [ ] use `usize::signed_diff` instead of `+ positive - negative`
- [ ] consider refactoring insert into separate functions for before, after, prepend, append instead of parameterizing
- [ ] Improve `tok_span`
  - [ ] default to `tok_index..tok_index`
  - [ ] make updating loops just add instead of checking for `EMPTY_RANGE`
  - [ ] replace instances of `usize::max(tree.tok_desc[i].end, i + 1)` with `tree.tok_desc[i].end`
- [ ] Improve container tok range
  - store last item index, and complete end of last item in tok_desc instead of start and end
  - start will always be index + 1, and storing both ends avoids:
    * recursing on end to find complete end
    * iterating over all items to find last item index
  - therefore
    * complete subtree range will be `container_index..tok_desc[container_index].end_complete`
    * first item index will be `container_index + 1`
    * last item index will be `tok_desc[container_index].last_item_index`
    * to check if container is empty, check if `last_item_index == 0`
    * when container is empty, `tok_desc[container_index].end_complete == container_index + 1` so removal does not require any additional logic
- [?] Custom range type
    * `offset_by(usize)`
    * `impl Copy`
    * `Range<u32>`?
    * `beg` + `end` for same len
- [ ] maintain formatting in updates
  - [ ] `format_style_from` that takes an index, and returns a `FormatStyle`
      * useful for limiting scope of CRUD ops, where args can be created with correct formatting before insertion
- [ ] Add benchmarks
  - before unifying update logic
  - options
    * insert 1000 values into array
    * insert values into very deeply nested value
- [ ] Builder API for simultaneous creation of formatted JSON as well as tree
  - useful for replacement APIs
- [ ] expore more efficient updating: splitting into gather and apply steps to amoritize actual full updates with expensive splice calls
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
