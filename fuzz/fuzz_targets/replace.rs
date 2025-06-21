#![no_main]

use common::{random_json, random_serde_json_value, random_value_index};
use jsoup::serde_json;
use libfuzzer_sys::{
    arbitrary::{self, Arbitrary, Unstructured},
    fuzz_target,
};

#[derive(Debug)]
struct ReplaceDef {
    contents: jsoup::JsonAst,
    index: usize,
    value: serde_json::Value,
}

impl<'a> Arbitrary<'a> for ReplaceDef {
    fn arbitrary(rng: &mut Unstructured) -> arbitrary::Result<Self> {
        let contents = random_json(rng)?;
        let value = random_serde_json_value(rng)?;
        let index = random_value_index(&contents, rng)?;
        Ok(ReplaceDef {
            contents,
            index,
            value,
        })
    }
}

fuzz_target!(|data: ReplaceDef| {
    let ReplaceDef {
        contents,
        index,
        value,
    } = data;

    let contents_hash = contents.hash_default();

    let mut tree = contents;

    let did_replace = jsoup::replace_index(&mut tree, index, &value);
    // should never make a valid tree invalid
    jsoup::assert_tree_valid(&tree);
    if !did_replace {
        // contents should not have changed if replace failed
        assert_eq!(
            contents_hash,
            tree.hash_default(),
            "tree modified during update even though update failed"
        );
    }
});
