use jsoup::*;
use libfuzzer_sys::arbitrary::{self, Unstructured};

pub fn random_json(rng: &mut Unstructured) -> Result<JsonAst, arbitrary::Error> {
    let value = random_serde_json_value(rng)?;
    let json_contents =
        serde_json::to_string(&value).map_err(|_| arbitrary::Error::IncorrectFormat)?;
    return jsoup::parse(&json_contents).map_err(|_| arbitrary::Error::IncorrectFormat);
}

pub fn random_value_index(
    tree: &JsonAst,
    rng: &mut Unstructured,
) -> Result<usize, arbitrary::Error> {
    return rng.choose_index(tree.tok_kind.len());
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
    return AnnotatedJSON(contents);
}
