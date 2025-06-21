#![no_main]

use common::{random_json, random_path, random_serde_json_value};
use json_inc::serde_json;
use libfuzzer_sys::{
    arbitrary::{self, Arbitrary, Unstructured},
    fuzz_target,
};

#[derive(Debug)]
struct ReplaceDef {
    contents: json_inc::JsonAst,
    path: json_inc::Path,
    target: json_inc::ReplaceTarget,
    value: serde_json::Value,
}

impl<'a> Arbitrary<'a> for ReplaceDef {
    fn arbitrary(u: &mut Unstructured) -> arbitrary::Result<Self> {
        let contents = random_json(u)?;
        let (path, target) = random_path(&contents, u)?;
        let value = random_serde_json_value(u)?;
        Ok(ReplaceDef {
            contents,
            path,
            target,
            value,
        })
    }
}

fuzz_target!(|data: ReplaceDef| {
    let ReplaceDef {
        contents,
        path,
        value,
        target,
    } = data;

    let mut tree = contents;

    if json_inc::replace_path(&mut tree, &path, &value, target) {
        json_inc::assert_tree_valid(&tree);
    }
});
