#![no_main]

use common::{random_json_ast, random_value_index};
use libfuzzer_sys::{
    arbitrary::{self, Arbitrary, Unstructured},
    fuzz_target,
};

#[derive(Debug)]
struct ReplaceDef {
    contents: jsoup::JsonAst,
    index: usize,
    value: jsoup::JsonAst,
}

impl<'a> Arbitrary<'a> for ReplaceDef {
    fn arbitrary(rng: &mut Unstructured) -> arbitrary::Result<Self> {
        let contents = random_json_ast(rng)?;
        let value = random_json_ast(rng)?;
        let index = random_value_index(&contents, rng)?;
        Ok(ReplaceDef {
            contents,
            index,
            value,
        })
    }
}

fn debug_print_as_test(data: &ReplaceDef) {
    let ReplaceDef {
        contents,
        index,
        value,
    } = data;

    eprintln!("check!(");
    eprintln!("    failing_fuzz_target,");
    let index_range = contents.tok_span[*index].clone();
    eprint!(
        r##"    r#"{}<"##,
        std::str::from_utf8(&contents.contents[..index_range.start]).expect("serialization failed")
    );
    eprint!(
        "{}",
        std::str::from_utf8(&contents.contents[index_range.start..index_range.end])
            .expect("serialization failed")
    );
    eprintln!(
        r##">{}"#,"##,
        std::str::from_utf8(&contents.contents[index_range.end..]).expect("serialization failed")
    );
    eprintln!(
        r##"    parse(r#"{}"#).expect("valid json"),"##,
        std::str::from_utf8(&value.contents).expect("serialization failed")
    );
    eprintln!(
        r##"    r#"{}"#"##,
        std::str::from_utf8(&contents.contents).expect("serialization failed")
    );
    eprintln!(");");
}

fuzz_target!(|data: ReplaceDef| {
    if option_env!("FUZZ_TARGET_DBG").is_some() {
        debug_print_as_test(&data);
    }
    let ReplaceDef {
        contents,
        index,
        value,
    } = data;

    let contents_hash = contents.hash_default();

    let mut tree = contents;

    let did_replace = jsoup::replace_index(&mut tree, index, value).is_ok();
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
