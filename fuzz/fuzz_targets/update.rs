#![no_main]

extern crate json_inc;
use json_inc::{JsonAst, serde_json};
use libfuzzer_sys::{
    Corpus,
    arbitrary::{self, Arbitrary, Unstructured},
    fuzz_target,
};

fn random_json(rng: &mut Unstructured) -> Result<JsonAst, arbitrary::Error> {
    let json_contents: &str = Arbitrary::arbitrary(rng)?;
    return json_inc::parse(&json_contents).map_err(|_| arbitrary::Error::IncorrectFormat);
}

fn random_path(
    tree: &JsonAst,
    rng: &mut Unstructured,
) -> Result<(json_inc::Path, json_inc::UpdateTarget), arbitrary::Error> {
    let index = rng.choose_index(tree.tok_types.len())?;
    use json_inc::{PathEntry, UpdateTarget};
    let mut path = vec![];
    let mut cur = 0;
    let mut target: UpdateTarget = UpdateTarget::Value;

    'outer: while cur != index {
        assert!(tree.tok_children[cur].contains(&index));
        match tree.tok_types[cur] {
            json_inc::TokenType::Array => {
                for (i, val_idx) in json_inc::ArrayItemIter::new(tree, cur).enumerate() {
                    if val_idx == index {
                        path.push(PathEntry::Idx(i));
                        break 'outer;
                    }
                    if tree.tok_children[val_idx].contains(&index) {
                        cur = val_idx;
                        continue 'outer;
                    }
                }
                unreachable!();
            }
            json_inc::TokenType::Object => {
                for (key_idx, val_idx) in json_inc::ObjectItemIter::new(tree, cur) {
                    if key_idx == index {
                        path.push(PathEntry::Str(tree.value_at(key_idx).to_string()));
                        target = UpdateTarget::Key;
                        break 'outer;
                    }
                    if val_idx == index {
                        path.push(PathEntry::Str(tree.value_at(key_idx).to_string()));
                        target = UpdateTarget::Value;
                        break 'outer;
                    }
                    if tree.tok_children[val_idx].contains(&index) {
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

fn random_serde_json_value(rng: &mut Unstructured) -> Result<serde_json::Value, arbitrary::Error> {
    let contents: &str = Arbitrary::arbitrary(rng)?;
    serde_json::from_str(contents).map_err(|_| arbitrary::Error::IncorrectFormat)
}

fuzz_target!(|data: &[u8]| -> Corpus {
    let mut rng = Unstructured::new(data);

    let Ok(mut tree) = random_json(&mut rng) else {
        return Corpus::Reject;
    };
    let Ok((path, target)) = random_path(&tree, &mut rng) else {
        return Corpus::Reject;
    };
    let Ok(value) = random_serde_json_value(&mut rng) else {
        return Corpus::Reject;
    };

    if json_inc::update(&mut tree, &path, &value, target) {
        json_inc::assert_tree_valid(&tree);
    }

    Corpus::Keep
});
