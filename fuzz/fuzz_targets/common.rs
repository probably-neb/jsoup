use jsoup::*;
use libfuzzer_sys::arbitrary::{self, Unstructured};

pub fn random_value_index(
    tree: &JsonAst,
    rng: &mut Unstructured,
) -> Result<usize, arbitrary::Error> {
    rng.choose_index(tree.tok_kind.len())
}

pub fn random_serde_json_value(
    rng: &mut Unstructured,
) -> Result<serde_json::Value, arbitrary::Error> {
    random_serde_json_value_depth(rng, 0)
}

pub fn random_serde_json_value_depth(
    rng: &mut Unstructured,
    depth: usize,
) -> Result<serde_json::Value, arbitrary::Error> {
    // TODO: replace usages of rng.int_in_range for sizes with rng.arbitrary_len
    const MAX_DEPTH: usize = 4;

    // Limit choices based on depth to prevent infinite recursion
    let type_choice: u8 = if depth >= MAX_DEPTH {
        rng.int_in_range(0..=3)? // Only simple types
    } else {
        rng.int_in_range(0..=5)? // All types
    };

    match type_choice {
        0 => Ok(serde_json::Value::Null),
        1 => Ok(serde_json::Value::Bool(rng.arbitrary()?)),
        2 => {
            let kind: u32 = rng.int_in_range(0..=2)?;
            Ok(match kind {
                0 => serde_json::Value::from(rng.arbitrary::<u64>()?),
                1 => serde_json::Value::from(rng.arbitrary::<i64>()?),
                2 => serde_json::Value::from(rng.arbitrary::<f64>()?),
                _ => unreachable!(),
            })
        }
        3 => Ok(serde_json::Value::String(rng.arbitrary()?)),
        4 => {
            // Array
            let len = rng.int_in_range(0..=5)?;
            let mut arr = Vec::with_capacity(len);
            for _ in 0..len {
                match random_serde_json_value_depth(rng, depth + 1) {
                    Ok(v) => arr.push(v),
                    Err(arbitrary::Error::NotEnoughData) => break,
                    Err(e) => return Err(e),
                }
            }
            Ok(serde_json::Value::Array(arr))
        }
        5 => {
            // Object
            let len = rng.int_in_range(0..=5)?;
            let mut map = serde_json::Map::new();
            for _ in 0..len {
                let key: String = match rng.arbitrary() {
                    Ok(k) => k,
                    Err(arbitrary::Error::NotEnoughData) => break,
                    Err(e) => return Err(e),
                };
                let value = match random_serde_json_value_depth(rng, depth + 1) {
                    Ok(v) => v,
                    Err(arbitrary::Error::NotEnoughData) => break,
                    Err(e) => return Err(e),
                };
                map.insert(key, value);
            }
            Ok(serde_json::Value::Object(map))
        }
        _ => unreachable!(),
    }
}

pub fn random_json_ast(rng: &mut Unstructured) -> Result<JsonAst, arbitrary::Error> {
    let mut builder = JsonAstBuilder::new();
    random_json_ast_depth(rng, &mut builder, 0)?;
    Ok(builder.build())
}

pub fn random_json_ast_depth(
    rng: &mut Unstructured,
    builder: &mut JsonAstBuilder,
    depth: usize,
) -> Result<(), arbitrary::Error> {
    // TODO: replace usages of rng.int_in_range for sizes with rng.arbitrary_len
    const MAX_DEPTH: usize = 4;

    // Limit choices based on depth to prevent infinite recursion
    let type_choice: u8 = if depth == 0 {
        rng.int_in_range(5..=6)? // Only containers
    } else if depth >= MAX_DEPTH {
        rng.int_in_range(0..=4)? // Only simple types
    } else {
        rng.int_in_range(0..=6)? // All types
    };

    let _json: &str = &builder.json;

    if depth > 0 {
        if rng.ratio(1, 10)? {
            builder.line_comment(rng.arbitrary()?);
        }
        if rng.ratio(1, 10)? {
            // todo: should be block comment
            builder.line_comment(rng.arbitrary()?);
        }
    }

    let _json: &str = &builder.json;

    match type_choice {
        0 => builder.null(),
        1 => builder.bool(rng.arbitrary()?),
        2 => {
            let mut float: f64 = rng.arbitrary()?;
            while !float.is_finite() {
                float = rng.arbitrary()?;
            }
            builder.float(float)
        }
        3 => builder.int(rng.arbitrary()?),
        4 => builder.string(rng.arbitrary()?),
        5 => {
            // Array
            let len = rng.arbitrary_len::<u64>()?;
            builder.begin_array();
            for _ in 0..len {
                match random_json_ast_depth(rng, builder, depth + 1) {
                    Ok(_) => continue,
                    Err(arbitrary::Error::NotEnoughData) => break,
                    Err(e) => return Err(e),
                }
            }
            builder.end_array();
        }
        6 => {
            // Object
            let len = rng.arbitrary_len::<u64>()?;
            builder.begin_object();
            for _ in 0..len {
                let key: String = match rng.arbitrary() {
                    Ok(k) => k,
                    Err(arbitrary::Error::NotEnoughData) => break,
                    Err(e) => return Err(e),
                };
                builder.key(&key);
                random_json_ast_depth(rng, builder, depth + 1)?;
            }
            builder.end_object();
        }
        _ => unreachable!(),
    };

    if depth > 0 {
        if rng.ratio(1, 10)? {
            builder.line_comment(rng.arbitrary()?);
        }
        if rng.ratio(1, 10)? {
            builder.line_comment(rng.arbitrary()?);
        }
    }
    Ok(())
}

pub struct AnnotatedJSON(String);

fn write_raw_string(f: &mut std::fmt::Formatter<'_>, s: &str) -> std::fmt::Result {
    f.write_str("r#\"")?;
    f.write_str(s)?;
    f.write_str("\"#")?;
    Ok(())
}

// impl ToString for AnnotatedJSON {
//     fn to_string(&self) -> String {
//         format!("{}", self)
//     }
// }

impl std::fmt::Display for AnnotatedJSON {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_raw_string(f, &self.0)
    }
}

impl std::fmt::Debug for AnnotatedJSON {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_raw_string(f, &self.0)
    }
}

pub fn annotated_json(tree: &JsonAst, index: usize) -> AnnotatedJSON {
    let span = tree.tok_span[index].clone();
    let mut contents = std::str::from_utf8(&tree.contents)
        .expect("contents valid")
        .to_string();

    contents.insert(span.start, '<');
    contents.insert(span.end + 1, '>');
    AnnotatedJSON(contents)
}
