const SETTINGS_JSON: &'static str = include_str!("./settings.jsonc");

const JSON_SMOL: &'static str = r#"
// 1
{ // 2
    "name": /* 3 */ "John Doe" /* 4 */,
    "age": 30,
    "pi": 3.14,
    "pets": [ /* 5 */
       "dog" /* 6**/,
       /*7 */"cat",
    ], //8
} /* 9 */
"#;

fn main() -> Result<(), json_inc::ParseError> {
    let tree = json_inc::parse(SETTINGS_JSON)?;
    for (index, ((kind, range), children)) in tree
        .tok_types
        .iter()
        .zip(tree.tok_ranges.iter())
        .zip(tree.tok_children.iter())
        .enumerate()
    {
        println!(
            "Index: {}, Kind: {:?}, Range: {:?}, Children: {:?} Value: `{}`",
            index,
            kind,
            range.clone(),
            children,
            tree.value_at(index),
        );
    }

    for comment in tree.comments.iter() {
        println!("Comment: {:?}", comment);
    }

    Ok(())
}
