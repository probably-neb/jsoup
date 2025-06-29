- [x] Replacing collections
  - [x] consolidate code paths for splicing and updating ranges
- [ ] expore more efficient updating: splitting into gather and apply steps to amoritize actual full updates with expensive splice calls
- [ ] maintain formatting in updates
- [ ] improve update efficiency by not stringifying then reparsing when inserting collection, convert `serde::Value` to tree directly (tied to maintaining formatting in updates)
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

- [ ] Deeper serde interop
  - `From<JsonAst> for serde_json::Value`
  - `From<serde_json::Value> for JsonAst`
- [ ] Error recovering parsing
  - goal is to parse as much as possible while marking sub regions as errors
  - return type becomes `(tree, List<Error>)` always
- [ ] JSON5 parsing
- ? maintaining comments in objects when replacing object with very similar object
