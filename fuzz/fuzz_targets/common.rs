use json_inc::*;
use libfuzzer_sys::arbitrary::{self, Unstructured};

pub fn random_json(rng: &mut Unstructured) -> Result<JsonAst, arbitrary::Error> {
    let value = random_serde_json_value(rng)?;
    let json_contents =
        serde_json::to_string(&value).map_err(|_| arbitrary::Error::IncorrectFormat)?;
    return json_inc::parse(&json_contents).map_err(|_| arbitrary::Error::IncorrectFormat);
}

pub fn random_value_index(
    tree: &JsonAst,
    rng: &mut Unstructured,
) -> Result<usize, arbitrary::Error> {
    return rng.choose_index(tree.tok_kind.len());
}

pub fn random_path(
    tree: &JsonAst,
    rng: &mut Unstructured,
) -> Result<(json_inc::Path, json_inc::ReplaceTarget), arbitrary::Error> {
    let index = random_value_index(tree, rng)?;
    use json_inc::{PathEntry, ReplaceTarget};
    let mut path = vec![];
    let mut cur = 0;
    let mut target = ReplaceTarget::Value;

    'outer: while cur != index {
        assert!(tree.tok_desc[cur].contains(&index));
        match tree.tok_kind[cur] {
            json_inc::Token::Array => {
                for (i, val_idx) in json_inc::ArrayItemIter::new(tree, cur).enumerate() {
                    if val_idx == index {
                        path.push(PathEntry::Idx(i));
                        break 'outer;
                    }
                    if tree.tok_desc[val_idx].contains(&index) {
                        cur = val_idx;
                        continue 'outer;
                    }
                }
                unreachable!();
            }
            json_inc::Token::Object => {
                for (key_idx, val_idx) in json_inc::ObjectItemIter::new(tree, cur) {
                    if key_idx == index {
                        path.push(PathEntry::Str(tree.value_at(key_idx).to_string()));
                        target = ReplaceTarget::Key;
                        break 'outer;
                    }
                    if val_idx == index {
                        path.push(PathEntry::Str(tree.value_at(key_idx).to_string()));
                        target = ReplaceTarget::Value;
                        break 'outer;
                    }
                    if tree.tok_desc[val_idx].contains(&index) {
                        path.push(PathEntry::Str(tree.value_at(key_idx).to_string()));
                        continue 'outer;
                    }
                }
                unreachable!();
            }
            _ => unreachable!(),
        }
    }

    return Ok((json_inc::Path(path), target));
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
