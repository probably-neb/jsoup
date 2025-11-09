use std::io::Write as _;

use crate::*;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Container {
    Start,
    ObjectValue,
    Object,
    Array,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct State {
    pub container_kind: Container,
    pub container_index: usize,
    pub prev_item_index: Option<usize>,
}

impl State {
    pub fn start() -> State {
        State {
            container_kind: Container::Start,
            container_index: 0,
            prev_item_index: None,
        }
    }

    pub fn object_value(key_index: usize) -> State {
        State {
            container_kind: Container::ObjectValue,
            container_index: key_index,
            prev_item_index: None,
        }
    }

    pub fn object(object_index: usize) -> State {
        State {
            container_kind: Container::Object,
            container_index: object_index,
            prev_item_index: None,
        }
    }

    pub fn array(array_index: usize) -> State {
        State {
            container_kind: Container::Array,
            container_index: array_index,
            prev_item_index: None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum NextPunctuation {
    Beginning,
    None,
    Colon,
    Comma,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsonAstBuilder {
    pub json: JsonAst,
    pub state: Vec<State>,
    pub next_punctuation: NextPunctuation,
}

impl JsonAstBuilder {
    pub fn new() -> JsonAstBuilder {
        JsonAstBuilder {
            json: JsonAst::empty(),
            state: vec![State::start()],
            next_punctuation: NextPunctuation::Beginning,
        }
    }

    pub fn build(self) -> JsonAst {
        if option_env!("BUILDER_DBG").is_some() {
            let json: &str =
                std::str::from_utf8(&self.json.contents).expect("contents are valid UTF-8");
            println!("json = r#\"\n{}\n\"#;", json);
        }
        self.json
    }

    fn assert_value(&self) {
        assert!(matches!(
            self.state.last().as_ref().unwrap().container_kind,
            Container::Start | Container::ObjectValue | Container::Array
        ));
    }

    fn write_punctuation(&mut self) {
        match self.next_punctuation {
            NextPunctuation::Beginning => {}
            NextPunctuation::None => {}
            NextPunctuation::Colon => {
                self.json.contents.push(b':');
            }
            NextPunctuation::Comma => {
                self.json.contents.push(b',');
            }
        }
    }

    fn value_start(&mut self) {
        self.assert_value();
        self.write_punctuation();
    }

    fn update_parent_tok_term(&mut self) {
        let tok_term = self.json.next_index() as u32 - 1;

        // Update all parent containers' tok_term
        for state in &self.state {
            self.json.tok_term[state.container_index] = tok_term;
        }
    }

    fn value_end(&mut self, index: usize) {
        self.next_punctuation = NextPunctuation::Comma;

        self.update_parent_tok_term();
        let Some(mut last_state) = self.state.pop() else {
            unreachable!("Unexpected end of state stack");
        };

        // If we're at the root level (Start container), don't update parent metadata
        if last_state.container_kind == Container::Start {
            return;
        }

        let container;
        let prev_item;
        let value_index;

        if last_state.container_kind == Container::ObjectValue {
            let Some(object_state) = self.state.last_mut() else {
                unreachable!("Unexpected end of state stack");
            };
            assert_eq!(object_state.container_kind, Container::Object);
            self.json.tok_chld[last_state.container_index] = index as u32;
            container = object_state.container_index;
            prev_item = &mut object_state.prev_item_index;
            value_index = last_state.container_index;
        } else {
            container = last_state.container_index;
            prev_item = &mut last_state.prev_item_index;
            value_index = index;
        }
        if let Some(prev_item) = prev_item.as_mut() {
            self.json.tok_next[*prev_item] = value_index as u32;
            *prev_item = value_index;
        } else if
        // HACK: don't overwrite tok_chld when doing key-value pair in `insert_index`
        self.json.tok_kind[container] != Token::String {
            self.json.tok_chld[container] = value_index as u32;
            *prev_item = Some(value_index);
        }
        self.json.tok_meta[container] += 1;

        if last_state.container_kind != Container::ObjectValue {
            self.state.push(last_state);
        }
    }

    pub fn begin_object(&mut self) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.begin_object();");
        }
        self.value_start();
        self.next_punctuation = NextPunctuation::None;
        let object_index = self.json.create_object();
        self.state.push(State::object(object_index));
    }

    pub fn end_object(&mut self) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.end_object();");
        }
        let Some(State {
            container_kind: Container::Object,
            container_index: object_index,
            ..
        }) = self.state.pop()
        else {
            unreachable!("Trying to end an object without starting it");
        };

        self.json.create_object_end(object_index);
        self.value_end(object_index);
    }

    pub fn begin_array(&mut self) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.begin_array();");
        }

        self.value_start();
        self.next_punctuation = NextPunctuation::None;
        let array_index = self.json.create_array();
        self.state.push(State::array(array_index));
    }

    pub fn end_array(&mut self) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.end_array();");
        }
        let Some(State {
            container_kind: Container::Array,
            container_index: array_index,
            ..
        }) = self.state.pop()
        else {
            unreachable!("Trying to end an array without starting it");
        };
        self.json.create_array_end(array_index);
        self.value_end(array_index);
    }

    pub fn key(&mut self, key: &str) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.key(\"{}\");", key);
        }
        self.write_punctuation();
        assert_eq!(
            self.state.last().unwrap().container_kind,
            Container::Object,
            "Expected an object container"
        );
        let key_index = self.json.create_string(key);

        self.state.push(State::object_value(key_index));
        self.next_punctuation = NextPunctuation::Colon;
    }

    pub fn string(&mut self, value: &str) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.string(\"{}\");", value);
        }
        self.value_start();
        let index = self.json.create_string(value);
        self.value_end(index);
    }

    // todo: i128?
    pub fn int(&mut self, arg: i64) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.int({});", arg);
        }
        self.value_start();
        let index = self.json.create_int(arg);
        self.value_end(index);
    }

    // todo: precision/rounding
    pub fn float(&mut self, arg: f64) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.float({});", arg);
        }
        assert!(arg.is_finite(), "Float is not finite");
        self.value_start();
        let index = self.json.create_float(arg);
        self.value_end(index);
    }

    pub fn bool(&mut self, arg: bool) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.bool({});", arg);
        }
        self.value_start();
        let index = self.json.create_bool(arg);
        self.value_end(index);
    }

    pub fn null(&mut self) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.null();");
        }
        self.value_start();
        let index = self.json.create_null();
        self.value_end(index);
    }

    pub fn line_comment(&mut self, comment: &str) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.line_comment(r#\"{}\"#);", comment);
        }
        self.json.create_line_comment(comment);
        self.update_parent_tok_term();
    }

    pub fn block_comment(&mut self, comment: &str) {
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.block_comment(r#\"{}\"#);", comment);
        }
        self.json.create_block_comment(comment);
        self.update_parent_tok_term();
    }

    pub fn tree(&mut self, tree: &JsonAst) {
        let contents_str =
            std::str::from_utf8(&tree.contents).expect("tree contents are valid utf8");
        if option_env!("BUILDER_DBG").is_some() {
            println!("builder.tree(r#\"{}\"#);", contents_str);
        }
        let first_non_comment_token = first_non_comment_token(tree);
        if first_non_comment_token.is_some() {
            self.value_start();
        }

        let offset_content = self.json.contents.len();
        let offset_token = self.json.next_index();

        self.json.contents.extend_from_slice(&tree.contents);

        self.json.tok_span.extend_from_slice(&tree.tok_span);
        self.json.tok_kind.extend_from_slice(&tree.tok_kind);
        self.json.tok_meta.extend_from_slice(&tree.tok_meta);
        self.json.tok_next.extend_from_slice(&tree.tok_next);
        self.json.tok_term.extend_from_slice(&tree.tok_term);
        self.json.tok_chld.extend_from_slice(&tree.tok_chld);

        for tok_next in &mut self.json.tok_next[offset_token..] {
            if *tok_next != 0 {
                *tok_next += offset_token as u32;
            }
        }
        for tok_span in &mut self.json.tok_span[offset_token..] {
            tok_span.start += offset_content;
            tok_span.end += offset_content;
        }
        for tok_term in &mut self.json.tok_term[offset_token..] {
            *tok_term += offset_token as u32;
        }
        for tok_chld in &mut self.json.tok_chld[offset_token..] {
            if *tok_chld != 0 {
                *tok_chld += offset_token as u32;
            }
        }
        if let Some(first_non_comment_token) = first_non_comment_token {
            self.value_end(offset_token + first_non_comment_token);
        }
    }
}

impl JsonAst {
    pub fn create_object(&mut self) -> usize {
        let start = self.contents.len();
        self.contents.extend_from_slice(b"{");
        self.push_object(start..self.contents.len())
    }

    pub fn create_object_end(&mut self, object_index: usize) {
        assert_eq!(self.tok_kind[object_index], Token::Object);
        self.contents.push(b'}');
        self.tok_span[object_index].end = self.contents.len();
        self.tok_term[object_index] = self.next_index() as u32 - 1;
    }

    pub fn create_array(&mut self) -> usize {
        let start = self.contents.len();
        self.contents.extend_from_slice(b"[");
        self.push_array(start..self.contents.len())
    }

    pub fn create_array_end(&mut self, array_index: usize) {
        assert_eq!(self.tok_kind[array_index], Token::Array);
        self.contents.push(b']');
        self.tok_span[array_index].end = self.contents.len();
        self.tok_term[array_index] = self.next_index() as u32 - 1;
    }

    pub fn create_null(&mut self) -> usize {
        let start = self.contents.len();
        self.contents.extend_from_slice(b"null");
        self.push_null(start..self.contents.len())
    }

    pub fn create_bool(&mut self, value: bool) -> usize {
        let start = self.contents.len();
        self.contents
            .extend_from_slice(if value { b"true" } else { b"false" });
        self.push_boolean(start..self.contents.len())
    }

    pub fn create_int(&mut self, value: i64) -> usize {
        let start = self.contents.len();
        write!(&mut self.contents, "{}", value).expect("format of int failed");
        self.push_int(start..self.contents.len(), value < 0)
    }

    pub fn create_float(&mut self, value: f64) -> usize {
        let start = self.contents.len();
        write!(&mut self.contents, "{}", value).expect("format of float failed");
        self.push_float(start..self.contents.len(), value < 0.0)
    }

    pub fn create_string(&mut self, value: &str) -> usize {
        let start = self.contents.len();
        write_str_escaped(&mut self.contents, value);
        self.push_string(start..self.contents.len())
    }

    pub fn create_line_comment(&mut self, comment: &str) -> usize {
        let start = self.contents.len();
        let mut lines = comment.lines();
        if let Some(first_line) = lines.next() {
            self.contents.extend_from_slice(b"//");
            if !first_line.is_empty() {
                self.contents.push(b' ');
            }
            self.contents.extend_from_slice(first_line.as_bytes());
            self.contents.push(b'\n');
            for line in lines {
                self.contents.extend_from_slice(b"//");
                if !line.is_empty() {
                    self.contents.push(b' ');
                }
                self.contents.extend_from_slice(line.as_bytes());
                self.contents.push(b'\n');
            }
        } else {
            self.contents.extend_from_slice(b"//\n");
        }
        self.push_comment(start..self.contents.len(), false)
    }

    pub fn create_block_comment(&mut self, comment: &str) -> usize {
        let start = self.contents.len();
        self.contents.extend_from_slice(b"/* ");
        self.contents.extend_from_slice(comment.as_bytes());
        self.contents.extend_from_slice(b" */");
        self.push_comment(start..self.contents.len(), true)
    }
}

/// Writes a JSON-escaped string to the writer.
/// Escapes the following characters according to JSON spec:
/// - " (quotation mark) -> \"
/// - \ (backslash) -> \\
/// - / (forward slash) -> \/ (optional but supported)
/// - \b (backspace) -> \b
/// - \f (form feed) -> \f
/// - \n (newline) -> \n
/// - \r (carriage return) -> \r
/// - \t (tab) -> \t
/// - Control characters (U+0000 to U+001F) -> \uXXXX
fn write_str_escaped(w: &mut Vec<u8>, s: &str) {
    w.push(b'"');
    for ch in s.chars() {
        match ch {
            '"' => w.extend_from_slice(b"\\\""),
            '\\' => w.extend_from_slice(b"\\\\"),
            '/' => w.extend_from_slice(b"\\/"),
            '\x08' => w.extend_from_slice(b"\\b"),
            '\x0C' => w.extend_from_slice(b"\\f"),
            '\n' => w.extend_from_slice(b"\\n"),
            '\r' => w.extend_from_slice(b"\\r"),
            '\t' => w.extend_from_slice(b"\\t"),
            // Escape control characters (U+0000 to U+001F)
            c if c < '\x20' => {
                write!(w, "\\u{:04x}", c as u32).expect("fmt failed");
            }
            // All other characters can be written as-is
            c => w.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes()),
        }
    }
    w.push(b'"');
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

    #[allow(unused)]
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
        r#"[{"id"// Item ID
:1.2,"location":null,"name"/* Item Name */:"Item 1"}]"#
    );

    check!(
        arr_with_newline_line_comment,
        {
            let mut builder = JsonAstBuilder::new();
            builder.begin_array();
            builder.line_comment(
                r#"F
"#,
            );
            builder.bool(true);
            builder.end_array();
            builder.build()
        },
        r#"[// F
true]"#
    );

    check!(
        obj_with_unescaped_string_key,
        {
            let mut builder = JsonAstBuilder::new();
            builder.begin_array();
            builder.begin_object();
            builder.key("\"2\"2");
            builder.null();
            builder.line_comment(r#""#);
            builder.end_object();
            builder.line_comment(r#""#);
            builder.line_comment(r#""#);
            builder.line_comment(r#""#);
            builder.line_comment(r#""#);
            builder.null();
            builder.line_comment(r#""#);
            builder.line_comment(r#""#);
            builder.end_array();
            builder.build()
        },
        r#"[{"\"2\"2":null//
}//
//
//
//
,null//
//
]"#
    );
}
