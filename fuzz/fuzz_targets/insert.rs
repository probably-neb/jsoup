#![no_main]

use common::{random_json_ast, random_serde_json_value, random_value_index};
use jsoup::{InsertionMethod, InsertionValue, serde_json};
use libfuzzer_sys::{
    arbitrary::{self, Arbitrary, Unstructured},
    fuzz_target,
};

#[derive(Debug)]
struct InsertDef {
    contents: jsoup::JsonAst,
    index: usize,
    key: Option<String>,
    value: serde_json::Value,
    method: InsertionMethod,
}

impl<'a> Arbitrary<'a> for InsertDef {
    fn arbitrary(rng: &mut Unstructured) -> arbitrary::Result<Self> {
        let contents = random_json_ast(rng)?;
        let value = random_serde_json_value(rng)?;
        let index = random_value_index(&contents, rng)?;
        let method = *rng.choose(&[
            InsertionMethod::After,
            InsertionMethod::Append,
            InsertionMethod::Before,
            InsertionMethod::Prepend,
        ])?;
        let key = rng.arbitrary()?;
        Ok(InsertDef {
            contents,
            index,
            key,
            value,
            method,
        })
    }
}

fn debug_print_as_test(data: &InsertDef) {
    let InsertDef {
        contents,
        index,
        key,
        value,
        method,
    } = data;

    eprintln!("check!(");
    eprintln!("    failing_fuzz_target,");
    let index_range = contents.tok_span[*index].clone();
    eprint!(r##"    r#"{}<"##, unsafe {
        std::str::from_utf8_unchecked(&contents.contents[..index_range.start])
    });
    eprint!("{}", unsafe {
        std::str::from_utf8_unchecked(&contents.contents[index_range.start..index_range.end])
    });
    eprintln!(r##">{}"#,"##, unsafe {
        std::str::from_utf8_unchecked(&contents.contents[index_range.end..])
    });

    let method_str = match method {
        InsertionMethod::Before => "Before",
        InsertionMethod::After => "After",
        InsertionMethod::Append => "Append",
        InsertionMethod::Prepend => "Prepend",
    };
    eprintln!("    {},", method_str);

    match key {
        Some(k) => {
            eprintln!(
                r#"    Obj(("{}", serde_json::from_str("{}").expect("valid json"))),"#,
                k,
                serde_json::to_string(&value).expect("serialization failed")
            );
        }
        None => {
            eprintln!(
                r#"    Arr(serde_json::from_str("{}").expect("valid json")),"#,
                serde_json::to_string(&value).expect("serialization failed")
            );
        }
    }

    eprintln!(r##"    r#"{}"#"##, unsafe {
        std::str::from_utf8_unchecked(&contents.contents)
    });
    eprintln!(");");
}

fuzz_target!(|data: InsertDef| {
    if option_env!("FUZZ_TARGET_DBG").is_some() {
        debug_print_as_test(&data);
    }
    let InsertDef {
        contents,
        index,
        value,
        key,
        method,
    } = data;

    let contents_hash = contents.hash_default();

    let mut tree = contents;

    let key_str = key.clone().unwrap_or_default();
    let value = match key {
        Some(_) => InsertionValue::Obj((&key_str, value)),
        None => InsertionValue::Arr(value),
    };

    let did_replace = jsoup::insert_index(&mut tree, value, method, index).is_ok();
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

    let result_tree = match jsoup::parse(
        std::str::from_utf8(&tree.contents).expect("Failed to parse contents as UTF-8 bytes"),
    ) {
        Ok(tree) => tree,
        Err(err) => panic!(
            "Tree parsing failed: {}. json = r#\"{}\"",
            err,
            String::from_utf8_lossy(&tree.contents)
        ),
    };
    pretty_assertions::assert_eq!(tree, result_tree);
});
