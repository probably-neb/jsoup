use std::fmt::Write as _;

use crate::*;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum State {
    Start,
    ObjectValue,
    Object,
    Array,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum NextPunctuation {
    Beginning,
    None,
    Colon,
    Comma,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsonAstBuilder {
    pub json: String,
    state: Vec<State>,
    next_punctuation: NextPunctuation,
}

impl JsonAstBuilder {
    pub fn new() -> JsonAstBuilder {
        JsonAstBuilder {
            json: String::new(),
            state: vec![State::Start],
            next_punctuation: NextPunctuation::Beginning,
        }
    }

    pub fn build(self) -> JsonAst {
        match crate::parse(&self.json) {
            Ok(tree) => tree,
            Err(err) => panic!("Failed to parse JSON: {}. \n{}", err, self.json),
        }
    }

    fn assert_value(&self) {
        assert!(matches!(
            self.state.last(),
            Some(State::Start | State::ObjectValue | State::Array)
        ));
    }

    fn write_punctuation(&mut self) {
        match self.next_punctuation {
            NextPunctuation::Beginning => {}
            NextPunctuation::None => {}
            NextPunctuation::Colon => {
                self.json.push(':');
            }
            NextPunctuation::Comma => {
                self.json.push(',');
            }
        }
    }

    fn value_start(&mut self) {
        self.assert_value();
        self.write_punctuation();
    }

    fn value_end(&mut self) {
        self.next_punctuation = NextPunctuation::Comma;
        // todo assert state
        self.state.pop();
    }

    pub fn begin_object(&mut self) {
        self.value_start();
        self.json.push('{');

        self.next_punctuation = NextPunctuation::None;
        self.state.push(State::Object);
    }

    pub fn end_object(&mut self) {
        let Some(State::Object) = self.state.pop() else {
            unreachable!("Trying to end an object without starting it");
        };
        self.json.push('}');

        self.value_end();
    }

    pub fn begin_array(&mut self) {
        self.value_start();
        self.json.push('[');

        self.next_punctuation = NextPunctuation::None;
        self.state.push(State::Array);
    }

    pub fn end_array(&mut self) {
        self.json.push(']');
        self.value_end();
    }

    pub fn key(&mut self, key: &str) {
        assert!(matches!(self.state.last(), Some(State::Object)));
        if self.next_punctuation == NextPunctuation::Comma {
            self.json.push(',');
        }
        self.json.push('"');
        self.json.push_str(key);
        self.json.push('"');
        self.state.push(State::ObjectValue);
        self.next_punctuation = NextPunctuation::Colon;
    }

    pub fn string(&mut self, value: &str) {
        self.value_start();
        self.json.push('"');
        self.json.push_str(value);
        self.json.push('"');
        self.value_end();
    }

    // todo: i128?
    pub fn int(&mut self, arg: i64) {
        self.value_start();
        write!(&mut self.json, "{}", arg).unwrap();
        self.value_end();
    }

    // todo: precision/rounding
    pub fn float(&mut self, arg: f64) {
        self.value_start();
        write!(&mut self.json, "{}", arg).unwrap();
        self.value_end();
    }

    pub fn bool(&mut self, arg: bool) {
        self.value_start();
        write!(&mut self.json, "{}", arg).unwrap();
        self.value_end();
    }

    pub fn null(&mut self) {
        self.value_start();
        self.json.push_str("null");
        self.value_end();
    }

    pub fn line_comment(&mut self, comment: &str) {
        self.write_punctuation();
        self.next_punctuation = NextPunctuation::None;
        assert!(!comment.starts_with("//"));
        self.json.push_str("// ");
        self.json.push_str(comment);
        self.json.push('\n');
    }

    pub fn block_comment(&mut self, comment: &str) {
        self.write_punctuation();
        self.next_punctuation = NextPunctuation::None;
        assert!(!comment.starts_with("/*"));
        self.json.push_str("/* ");
        self.json.push_str(comment);
        self.json.push_str(" */");
    }
}

#[cfg(test)]
mod tests {
    use crate::{JsonAst, assert_tree_valid, builder::JsonAstBuilder, parse};

    fn check(input: &JsonAst, expected: impl Into<String>) {
        let expected = parse(&expected.into()).expect("expected valid JSON");
        pretty_assertions::assert_eq!(input, &expected);
        assert_tree_valid(input);
    }

    macro_rules! check {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let input = $input;
                let expected = $expected;
                check(&input, expected);
            }
        };
    }

    macro_rules! check_fail {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            #[should_panic]
            fn $name() {
                let input = $input;
                let expected = $expected;
                check(&input, expected);
            }
        };
    }

    check!(
        obj_empty,
        {
            let mut builder = JsonAstBuilder::new();
            builder.begin_object();
            builder.end_object();
            builder.build()
        },
        r#"{}"#
    );

    check!(
        obj_key_val,
        {
            let mut builder = JsonAstBuilder::new();
            builder.begin_object();
            builder.key("foo");
            builder.string("bar");
            builder.end_object();
            builder.build()
        },
        r#"{"foo":"bar"}"#
    );

    check!(
        obj_med_depth,
        {
            let mut b = JsonAstBuilder::new();
            b.begin_object();
            {
                b.key("name");
                b.string("John");

                b.key("age");
                b.int(30);

                b.key("is_student");
                b.bool(false);

                b.key("grades");
                b.begin_array();
                {
                    b.begin_object();
                    {
                        b.key("math");
                        b.int(85);
                        b.key("science");
                        b.int(90);
                    }
                    b.end_object();
                }
                b.end_array();
            }
            b.end_object();
            b.build()
        },
        r#"{"name":"John","age":30,"is_student":false,"grades":[{"math":85,"science":90}]}"#
    );

    check!(
        arr_with_comments,
        {
            let mut builder = JsonAstBuilder::new();
            builder.begin_array();
            {
                builder.begin_object();
                {
                    builder.key("id");
                    builder.line_comment("Item ID");
                    builder.float(1.2);

                    builder.key("location");
                    builder.null();

                    builder.key("name");
                    builder.block_comment("Item Name");
                    builder.string("Item 1");
                }
                builder.end_object();
            }
            builder.end_array();
            builder.build()
        },
        r#"[{"id":// Item ID
1.2,"location":null,"name":/* Item Name */"Item 1"}]"#
    );
}
