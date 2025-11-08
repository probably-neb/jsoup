#![no_main]

use common::{
    AnnotatedJSON, annotated_json, random_json_ast, random_value_index,
};
use libfuzzer_sys::{
    arbitrary::{self, Arbitrary, Unstructured},
    fuzz_target,
};

#[derive(Debug)]
struct RemoveDef {
    contents: jsoup::JsonAst,
    index: usize,
    _annotated: AnnotatedJSON,
}

impl<'a> Arbitrary<'a> for RemoveDef {
    fn arbitrary(rng: &mut Unstructured) -> arbitrary::Result<Self> {
        let contents = random_json_ast(rng)?;
        let index = random_value_index(&contents, rng)?;
        let _annotated = annotated_json(&contents, index);
        Ok(RemoveDef {
            contents,
            index,
            _annotated,
        })
    }
}

fuzz_target!(|data: RemoveDef| {
    let RemoveDef {
        contents, index, ..
    } = data;

    let contents_hash = contents.hash_default();

    let mut tree = contents;

    let did_replace = jsoup::remove_index(&mut tree, index).is_ok();
    if !did_replace {
        // contents should not have changed if replace failed
        assert_eq!(
            contents_hash,
            tree.hash_default(),
            "tree modified during update even though update failed"
        );
    }
    // should never make a valid tree invalid
    jsoup::assert_tree_valid(&tree);

    let result_tree = jsoup::parse(
        std::str::from_utf8(&tree.contents).expect("Failed to parse contents as UTF-8 bytes"),
    )
    .expect("tree contents valid");
    assert_eq!(tree, result_tree);
});
