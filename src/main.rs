const JSON_SETTINGS: &'static str = include_str!("./settings.jsonc");
const JSON_KEYMAP: &'static str = include_str!("./keymap.jsonc");

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

fn main() -> Result<(), jsoup::ParseError> {
    let tree = jsoup::parse(JSON_KEYMAP)?;
    for (index, ((kind, range), children)) in tree
        .tok_kind
        .iter()
        .zip(tree.tok_span.iter())
        .zip(tree.tok_desc.iter())
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
