#![feature(iter_array_chunks)]
#![feature(unsigned_signed_diff)]

mod builder;
pub use builder::*;
pub use serde_json;
use std::{fmt::Display, hash::Hasher, ops::Range};

const META_NUM_NEGATIVE: u32 = 1 << 0;
const META_NUM_FLOAT: u32 = 1 << 1;
const META_COMMENT_LINE: u32 = 1 << 2;
const META_COMMENT_BLOCK: u32 = 1 << 3;

const EMPTY_RANGE: Range<usize> = 0..0;

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct JsonAst {
    pub contents: Vec<u8>,
    pub tok_span: Vec<Range<usize>>,
    pub tok_kind: Vec<Token>,
    /// short for tok_termination:
    /// the index of the last item in the subtree starting at this node
    pub tok_term: Vec<u32>,
    /// token metadata:
    /// additional information about the token
    pub tok_meta: Vec<u32>,
    /// token next:
    /// the index of the next token in the sequence
    /// For keys this is the next key in the object,
    /// for array values this is the next value in the array
    /// 0 for no next token
    pub tok_next: Vec<u32>,
    /// short for tok_child:
    /// the index of the child of this node
    /// For containers, this is the first (non comment) child element
    /// For obj keys, this is their corresponding value element
    /// 0 for no child
    pub tok_chld: Vec<u32>,
}

impl std::fmt::Debug for JsonAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JsonAst")
            .field("contents", &std::str::from_utf8(&self.contents))
            .field(
                "tok_span",
                &self
                    .tok_span
                    .iter()
                    .map(|range| {
                        (
                            range.clone(),
                            self.contents.get(range.clone()).map(std::str::from_utf8),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
            .field("tok_kind", &self.tok_kind)
            .field("tok_meta", &self.tok_meta)
            .field("tok_next", &self.tok_next)
            .field("tok_term", &self.tok_term)
            .field("tok_chld", &self.tok_chld)
            .finish()
    }
}

impl JsonAst {
    pub fn empty() -> Self {
        Self {
            contents: Vec::new(),
            tok_span: Vec::new(),
            tok_kind: Vec::new(),
            tok_meta: Vec::new(),
            tok_next: Vec::new(),
            tok_term: Vec::new(),
            tok_chld: Vec::new(),
        }
    }

    pub fn value_at(&self, index: usize) -> &str {
        let mut range = self.tok_span[index].clone();
        if self.tok_kind[index] == Token::String {
            #[cfg(debug_assertions)]
            assert_string_valid(self, index);
            range.start += 1;
            range.end -= 1;
        }
        unsafe { std::str::from_utf8_unchecked(&self.contents[range]) }
    }

    pub fn value_for_char_range(&self, range: &Range<usize>) -> &str {
        return str_range_adjusted(&self.contents, range.clone());
    }

    pub fn next_index(&self) -> usize {
        self.assert_lengths();
        return self.tok_kind.len();
    }

    fn assert_lengths(&self) {
        assert_eq!(self.tok_span.len(), self.tok_kind.len());
        assert_eq!(self.tok_span.len(), self.tok_meta.len());
        assert_eq!(self.tok_span.len(), self.tok_next.len());
        assert_eq!(self.tok_span.len(), self.tok_chld.len());
        assert_eq!(self.tok_span.len(), self.tok_term.len());
    }

    pub fn hash_default(&self) -> u64 {
        use std::hash::{DefaultHasher, Hash as _};
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }

    fn reserve(&mut self, tok: Token) -> usize {
        let index = self.next_index();
        self.tok_term.push(index as u32);
        self.tok_span.push(EMPTY_RANGE);
        self.tok_kind.push(tok);
        self.tok_meta.push(0);
        self.tok_next.push(0);
        self.tok_chld.push(0);
        self.assert_lengths();
        index
    }

    pub fn push_string(&mut self, span: Range<usize>) -> usize {
        assert!(
            span.len() >= 2,
            "String span must be at least 2 bytes for quotes"
        );
        assert_eq!(
            self.contents[span.start], b'"',
            "String must start with quote"
        );
        assert_eq!(
            self.contents[span.end - 1],
            b'"',
            "String must end with quote"
        );
        let index = self.reserve(Token::String);
        self.tok_span[index] = span;
        index
    }

    pub fn push_comment(&mut self, span: Range<usize>, is_block: bool) -> usize {
        if is_block {
            assert!(
                span.len() >= 4,
                "Block comment must be at least 4 bytes for /**/"
            );
            assert_eq!(
                &self.contents[span.start..span.start + 2],
                b"/*",
                "Block comment must start with /*"
            );
            assert_eq!(
                &self.contents[span.end - 2..span.end],
                b"*/",
                "Block comment must end with */"
            );
        } else {
            assert!(
                span.len() >= 2,
                "Line comment must be at least 2 bytes for //"
            );
            assert_eq!(
                &self.contents[span.start..span.start + 2],
                b"//",
                "Line comment must start with //"
            );
        }
        let index = self.reserve(Token::Comment);
        self.tok_span[index] = span;
        self.tok_meta[index] = if is_block {
            META_COMMENT_BLOCK
        } else {
            META_COMMENT_LINE
        };
        index
    }

    pub fn push_null(&mut self, span: Range<usize>) -> usize {
        assert_eq!(span.len(), 4, "Null span must be exactly 4 bytes");
        assert_eq!(
            &self.contents[span.clone()],
            b"null",
            "Null token must contain 'null'"
        );
        let index = self.reserve(Token::Null);
        self.tok_span[index] = span;
        index
    }

    pub fn push_boolean(&mut self, span: Range<usize>) -> usize {
        let content = &self.contents[span.clone()];
        assert!(
            content == b"true" || content == b"false",
            "Boolean token must contain 'true' or 'false'"
        );
        assert!(
            span.len() == 4 || span.len() == 5,
            "Boolean span must be 4 bytes for 'true' or 5 bytes for 'false'"
        );
        let index = self.reserve(Token::Boolean);
        self.tok_span[index] = span;
        index
    }

    pub fn push_int(&mut self, span: Range<usize>, is_negative: bool) -> usize {
        assert!(!span.is_empty(), "Integer span cannot be empty");
        let has_minus = self.contents[span.start] == b'-';
        assert_eq!(
            is_negative, has_minus,
            "is_negative argument must match whether content starts with '-'"
        );
        let index = self.reserve(Token::Number);
        self.tok_span[index] = span;
        let mut meta = 0;
        if is_negative {
            meta |= META_NUM_NEGATIVE;
        }
        self.tok_meta[index] = meta;
        index
    }

    pub fn push_float(&mut self, span: Range<usize>, is_negative: bool) -> usize {
        assert!(!span.is_empty(), "Float span cannot be empty");
        let has_minus = self.contents[span.start] == b'-';
        assert_eq!(
            is_negative, has_minus,
            "is_negative argument must match whether content starts with '-'"
        );
        let content = &self.contents[span.clone()];
        let has_float_indicator =
            content.contains(&b'.') || content.contains(&b'e') || content.contains(&b'E');
        assert!(has_float_indicator, "Float must contain '.', 'e', or 'E'");
        let index = self.reserve(Token::Number);
        self.tok_span[index] = span;
        let mut meta = META_NUM_FLOAT;
        if is_negative {
            meta |= META_NUM_NEGATIVE;
        }
        self.tok_meta[index] = meta;
        index
    }

    pub fn push_object(&mut self, span: Range<usize>) -> usize {
        // Objects are initially created with empty spans, then updated later
        if !span.is_empty() {
            assert_eq!(
                self.contents[span.start], b'{',
                "Object span must start with '{{' if non-empty"
            );
        }
        let index = self.reserve(Token::Object);
        self.tok_span[index] = span;
        index
    }

    pub fn push_array(&mut self, span: Range<usize>) -> usize {
        // Arrays are initially created with empty spans, then updated later
        if !span.is_empty() {
            assert_eq!(
                self.contents[span.start], b'[',
                "Array span must start with '[' if non-empty"
            );
        }
        let index = self.reserve(Token::Array);
        self.tok_span[index] = span;
        index
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    Array,
    Object,
    String,
    Number,
    Boolean,
    Null,
    Comment,
}

impl Token {
    pub fn from_value(value: &serde_json::Value) -> Self {
        use serde_json::Value;
        match value {
            Value::Array(_) => Token::Array,
            Value::Object(_) => Token::Object,
            Value::String(_) => Token::String,
            Value::Number(_) => Token::Number,
            Value::Bool(_) => Token::Boolean,
            Value::Null => Token::Null,
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(char),
    UnexpectedEndOfInput,
    InvalidNumber,
    InvalidBoolean,
    InvalidNull,
    TODOCannotParseNonContainerRoot,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::TODOCannotParseNonContainerRoot => {
                write!(f, "TODO: Parsing non container root")
            }
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token '{}'", token),
            ParseError::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            ParseError::InvalidNumber => write!(f, "Invalid number"),
            ParseError::InvalidBoolean => write!(f, "Invalid boolean"),
            ParseError::InvalidNull => write!(f, "Invalid null"),
        }
    }
}

impl std::error::Error for ParseError {
    fn description(&self) -> &str {
        match self {
            ParseError::TODOCannotParseNonContainerRoot => "Not implemented",
            ParseError::UnexpectedToken(_) => "Unexpected token",
            ParseError::UnexpectedEndOfInput => "Unexpected end of input",
            ParseError::InvalidNumber => "Invalid number",
            ParseError::InvalidBoolean => "Invalid boolean",
            ParseError::InvalidNull => "Invalid null",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PathEntry {
    Str(String),
    Idx(usize),
}

#[derive(Debug)]
pub struct Path(pub Vec<PathEntry>);

impl Path {
    pub fn from_str(s: &str) -> Path {
        if s.is_empty() {
            return Self(Vec::new());
        }
        let mut path = Vec::new();
        for item in s.split('.') {
            if let Ok(idx) = item.parse::<usize>() {
                path.push(PathEntry::Idx(idx));
            } else {
                path.push(PathEntry::Str(item.to_string()));
            }
        }
        return Self(path);
    }
}

pub fn parse(input: &str) -> Result<JsonAst, ParseError> {
    let mut tree = JsonAst::empty();
    // todo! don't clone if not necessary
    tree.contents = input.as_bytes().to_vec();

    let mut cursor = 0;

    let eof = parse_whitespace_or_comment(&mut tree, &mut cursor)?;
    if !eof {
        let res = parse_value(&mut tree, &mut cursor);

        tree.assert_lengths();

        let Ok(_) = res else {
            // let context_start = cursor.saturating_sub(3);
            // let context_end = usize::min(cursor + 4, tree.contents.len());
            // eprintln!(
            //     "Error at position {}: `{}`",
            //     context_start,
            //     tree.value_for_char_range(&(context_start..context_end)),
            // );
            return res.map(|_| tree);
        };
    }
    let eof = parse_whitespace_or_comment(&mut tree, &mut cursor)?;
    if !eof {
        return Err(ParseError::UnexpectedToken(tree.contents[cursor] as char));
    }
    assert_tree_valid(&tree);
    return Ok(tree);
}

fn parse_whitespace_or_comment(tree: &mut JsonAst, cursor: &mut usize) -> Result<bool, ParseError> {
    tree.assert_lengths();
    let eof = parse_whitespace(tree, cursor);
    if eof {
        return Ok(eof);
    }
    parse_any_comments(tree, cursor)?;
    let eof = parse_whitespace(tree, cursor);
    Ok(eof)
}

fn parse_whitespace(tree: &mut JsonAst, cursor: &mut usize) -> bool {
    tree.assert_lengths();
    assert!(*cursor <= tree.contents.len());
    while *cursor < tree.contents.len() && tree.contents[*cursor].is_ascii_whitespace() {
        *cursor += 1;
    }
    assert!(*cursor <= tree.contents.len());
    assert!(*cursor == tree.contents.len() || !tree.contents[*cursor].is_ascii_whitespace());
    return *cursor == tree.contents.len();
}

fn parse_any_comments(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    if *cursor >= tree.contents.len() {
        return Ok(());
    }
    loop {
        if *cursor + 2 > tree.contents.len() {
            return Ok(());
        }
        if tree.contents[*cursor] != b'/' {
            return Ok(());
        }
        let start = *cursor;
        match tree.contents[*cursor + 1] {
            b'/' => {
                assert_eq!(&tree.contents[*cursor..*cursor + 2], [b'/', b'/']);
                *cursor += 2;
                while *cursor <= tree.contents.len() {
                    if *cursor == tree.contents.len() || tree.contents[*cursor] == b'\n' {
                        let range = start..usize::min(*cursor + 1, tree.contents.len());
                        tree.push_comment(range, false);
                        break;
                    }

                    *cursor += 1;
                }
                parse_whitespace(tree, cursor);
            }
            b'*' => {
                assert_eq!(&tree.contents[*cursor..*cursor + 2], [b'/', b'*']);
                *cursor += 2;
                let mut found = false;
                while *cursor < tree.contents.len() {
                    if tree.contents[*cursor] == b'*' {
                        if *cursor + 1 >= tree.contents.len() {
                            return Err(ParseError::UnexpectedEndOfInput);
                        }
                        if tree.contents[*cursor + 1] == b'/' {
                            assert_eq!(&tree.contents[*cursor..*cursor + 2], [b'*', b'/']);
                            *cursor += 2;
                            tree.push_comment(start..*cursor, true);
                            found = true;
                            break;
                        }
                    }
                    *cursor += 1;
                }
                if !found {
                    return Err(ParseError::UnexpectedEndOfInput);
                }
                parse_whitespace(tree, cursor);
            }
            _ => {
                return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
            }
        }
    }
}

fn parse_null(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    assert_eq!(tree.contents[*cursor], b'n');
    if *cursor + 4 > tree.contents.len() {
        return Err(ParseError::UnexpectedEndOfInput);
    }
    if &tree.contents[*cursor..*cursor + 4] != [b'n', b'u', b'l', b'l'] {
        return Err(ParseError::InvalidNull);
    }
    tree.push_null(*cursor..*cursor + 4);
    *cursor += 4;
    Ok(())
}

fn parse_true(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    if *cursor + 4 > tree.contents.len() {
        return Err(ParseError::UnexpectedEndOfInput);
    }
    if &tree.contents[*cursor..*cursor + 4] != [b't', b'r', b'u', b'e'] {
        return Err(ParseError::InvalidBoolean);
    }
    tree.push_boolean(*cursor..*cursor + 4);
    *cursor += 4;
    Ok(())
}

fn parse_false(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    assert_eq!(tree.contents[*cursor], b'f');
    if *cursor + 5 > tree.contents.len() {
        return Err(ParseError::UnexpectedEndOfInput);
    }
    if &tree.contents[*cursor..*cursor + 5] != [b'f', b'a', b'l', b's', b'e'] {
        return Err(ParseError::InvalidBoolean);
    }
    tree.push_boolean(*cursor..*cursor + 5);
    *cursor += 5;
    Ok(())
}

fn parse_string(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    assert_eq!(tree.contents[*cursor], b'"');
    if *cursor + 1 > tree.contents.len() {
        return Err(ParseError::UnexpectedEndOfInput);
    }
    let start = *cursor;
    *cursor += 1;
    while *cursor < tree.contents.len() {
        let ch = tree.contents[*cursor];
        match ch {
            b'\\' => {
                if *cursor + 1 >= tree.contents.len() {
                    return Err(ParseError::UnexpectedEndOfInput);
                }
                *cursor += 2;
            }
            b'"' => {
                *cursor += 1;
                let range = start..*cursor;
                assert_eq!(tree.contents[range.start], b'"');
                assert_eq!(tree.contents[range.end - 1], b'"');
                tree.push_string(range);
                return Ok(());
            }
            _ => *cursor += 1,
        }
    }
    Err(ParseError::UnexpectedEndOfInput)
}

fn parse_number(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    assert!(is_start_of_number(tree.contents[*cursor]));
    if *cursor + 1 > tree.contents.len() {
        return Err(ParseError::UnexpectedEndOfInput);
    }
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum State {
        Int,
        Frac,
        ExpSign,
        Exp,
    }
    let start = *cursor;
    *cursor += 1;
    let mut state = State::Int;

    let mut is_float = false;

    while *cursor < tree.contents.len() {
        match tree.contents[*cursor] {
            b'0'..=b'9' => *cursor += 1,
            b'.' => {
                if state == State::Int {
                    state = State::Frac;
                    is_float = true;
                    *cursor += 1
                } else {
                    return Err(ParseError::InvalidNumber);
                }
            }
            b'e' | b'E' => {
                if state == State::Frac || state == State::Int {
                    state = State::ExpSign;
                    is_float = true;
                    *cursor += 1
                } else {
                    return Err(ParseError::InvalidNumber);
                }
            }
            b'-' => {
                if state == State::ExpSign {
                    state = State::Exp;
                    *cursor += 1;
                } else {
                    return Err(ParseError::InvalidNumber);
                }
            }
            _ => break,
        }
    }
    let is_negative = tree.contents[start] == b'-';
    if *cursor == start + 1 && is_negative {
        return Err(ParseError::InvalidNumber);
    }
    let value_start = start + is_negative as usize;
    let is_int_with_leading_0 =
        !is_float && *cursor - value_start > 1 && tree.contents[value_start] == b'0';
    if is_int_with_leading_0 {
        return Err(ParseError::InvalidNumber);
    }

    if is_float {
        tree.push_float(start..*cursor, is_negative);
    } else {
        tree.push_int(start..*cursor, is_negative);
    }
    Ok(())
}

fn is_start_of_number(c: u8) -> bool {
    c.is_ascii_digit() || c == b'-'
}

fn parse_value(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    let res = match tree.contents[*cursor] {
        b'"' => parse_string(tree, cursor),
        b'n' => parse_null(tree, cursor),
        b't' => parse_true(tree, cursor),
        b'f' => parse_false(tree, cursor),
        b'{' => parse_object(tree, cursor),
        b'[' => parse_array(tree, cursor),
        _ if is_start_of_number(tree.contents[*cursor]) => parse_number(tree, cursor),
        _ => Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char)),
    };
    tree.assert_lengths();
    return res;
}

fn parse_object(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    assert_eq!(tree.contents[*cursor], b'{');
    if *cursor + 1 > tree.contents.len() {
        return Err(ParseError::UnexpectedEndOfInput);
    }
    let obj_index = tree.push_object(*cursor..*cursor);

    *cursor += 1;

    let mut key_index_prev = obj_index;
    let mut key_count = 0;

    loop {
        let eof = parse_whitespace_or_comment(tree, cursor)?;
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        if tree.contents[*cursor] == b'}' {
            *cursor += 1;
            break;
        }

        let key_index = parse_key_value(tree, cursor)?;
        tree.tok_next[key_index_prev] = key_index as u32;
        key_index_prev = key_index;
        key_count += 1;
        if tree.tok_chld[obj_index] == 0 {
            tree.tok_chld[obj_index] = key_index as u32;
        }
        let eof = parse_whitespace_or_comment(tree, cursor)?;
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }

        if tree.contents[*cursor] == b',' {
            *cursor += 1;
        } else if tree.contents[*cursor] != b'}' {
            return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
        }
    }

    tree.tok_term[obj_index] = tree.next_index() as u32 - 1;
    // clear because we set without checking in loop
    tree.tok_next[obj_index] = 0;

    tree.tok_meta[obj_index] = key_count;

    tree.tok_span[obj_index].end = *cursor;
    return Ok(());
}

fn parse_key_value(tree: &mut JsonAst, cursor: &mut usize) -> Result<usize, ParseError> {
    if tree.contents[*cursor] != b'"' {
        return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
    }
    let key_index = tree.next_index();
    parse_string(tree, cursor)?;

    let eof = parse_whitespace_or_comment(tree, cursor)?;
    if eof {
        return Err(ParseError::UnexpectedEndOfInput);
    }

    if tree.contents[*cursor] != b':' {
        return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
    }
    *cursor += 1;

    let eof = parse_whitespace_or_comment(tree, cursor)?;
    if eof {
        return Err(ParseError::UnexpectedEndOfInput);
    }

    let value_index = tree.next_index();
    parse_value(tree, cursor)?;
    // key descendant range is the value range
    tree.tok_chld[key_index] = value_index as u32;
    let value_term = tree.tok_term[value_index];
    tree.tok_term[key_index] = value_term;

    return Ok(key_index);
}

fn parse_array(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    assert_eq!(tree.contents[*cursor], b'[');
    let array_index = tree.push_array(*cursor..*cursor);

    *cursor += 1;

    let mut value_index_prev = array_index;
    let mut value_count = 0;

    loop {
        let eof = parse_whitespace_or_comment(tree, cursor)?;
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        if tree.contents[*cursor] == b']' {
            *cursor += 1;
            break;
        }
        let value_index = tree.next_index();
        parse_value(tree, cursor)?;
        tree.tok_next[value_index_prev] = value_index as u32;
        value_index_prev = value_index;
        value_count += 1;
        if tree.tok_chld[array_index] == 0 {
            tree.tok_chld[array_index] = value_index as u32;
        }

        let eof = parse_whitespace_or_comment(tree, cursor)?;
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        if tree.contents[*cursor] == b',' {
            *cursor += 1;
        } else if tree.contents[*cursor] != b']' {
            return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
        }
    }
    tree.tok_term[array_index] = tree.next_index() as u32 - 1;
    // clear because we set without checking in loop
    tree.tok_next[array_index] = 0;

    tree.tok_meta[array_index] = value_count;
    tree.tok_span[array_index].end = *cursor;
    return Ok(());
}

fn assert_number_valid(tree: &JsonAst, i: usize) {
    let range = &tree.tok_span[i];
    assert!(
        is_start_of_number(tree.contents[range.start]),
        "number must start with a digit or '-', found '{}'",
        tree.contents[range.start] as char
    );

    let is_negative_sign = tree.contents[range.start] == b'-';
    let is_negative_extra = tree.tok_meta[i] & META_NUM_NEGATIVE != 0;
    assert_eq!(
        is_negative_sign, is_negative_extra,
        "Expected negative sign on negative number, found is_negative={} and first_char={}",
        is_negative_extra, tree.contents[range.start] as char
    );

    let value = &tree.contents[range.clone()];

    fn count(bytes: &[u8], char: u8) -> u32 {
        let mut count = 0;
        for &byte in bytes {
            count += (byte == char) as u32;
        }
        return count;
    }
    assert!(
        count(value, b'.') <= 1,
        "number can have at most one decimal point, found {}",
        count(value, b'.')
    );
    assert!(
        count(value, b'E') + count(value, b'e') <= 1,
        "number can have at most one 'E' or 'e' for exponent, found {}",
        count(value, b'E') + count(value, b'e')
    );
    assert!(
        count(value, b'-') <= 2,
        "number can have at most two '-' signs (one for negative, one for exponent), found {}",
        count(value, b'-')
    );
    assert!(
        count(value, b'+') == 0,
        "number should not contain '+' sign, found {}",
        count(value, b'+')
    );

    let is_float_scientific = (count(value, b'e') + count(value, b'E')) == 1;
    let is_float_frac = count(value, b'.') != 0;
    let is_float = is_float_scientific || is_float_frac;
    let is_float_extra = tree.tok_meta[i] & META_NUM_FLOAT != 0;
    assert!(
        is_float_extra == is_float,
        "float metadata flag {} should match actual float status {}",
        is_float_extra,
        is_float
    );
}

fn assert_string_valid(tree: &JsonAst, i: usize) {
    let range = &tree.tok_span[i];
    assert!(
        range.len() >= 2,
        "string token must be at least 2 characters (opening and closing quotes)"
    );
    assert_eq!(
        tree.contents[range.start], b'"',
        "expected `\"` at start of string, found `{}`",
        tree.contents[range.start] as char
    );
    assert_eq!(
        tree.contents[range.end - 1],
        b'"',
        "expected `\"` at end of string, found `{}`",
        tree.contents[range.end - 1] as char
    );
    assert!(
        std::str::from_utf8(&tree.contents[range.start + 1..range.end - 1]).is_ok(),
        "string content must be valid UTF-8"
    );
    assert_ne!(
        tree.contents[range.end - 1],
        b'\\',
        "string must not end with an unescaped backslash"
    );
}

fn assert_object_valid(tree: &JsonAst, i: usize) {
    let range = &tree.tok_span[i];
    assert!(range.len() >= 2);
    assert_eq!(
        tree.contents[range.start], b'{',
        "expected `{{` at start of obj. Found `{}`",
        tree.contents[range.start] as char
    );
    assert_eq!(
        tree.contents[range.end - 1],
        b'}',
        "expected `}}` at end of obj. Found `{}`",
        tree.contents[range.end - 1] as char
    );
    assert!(
        std::str::from_utf8(&tree.contents[range.clone()]).is_ok(),
        "object content must be valid UTF-8"
    );

    let expected_count = tree.tok_meta[i];
    let mut found_count = 0;
    let mut key_index = container_first_item_index(tree, i);
    while key_index != 0 {
        assert_eq!(
            Token::String,
            tree.tok_kind[key_index],
            "key of object should be string",
        );
        assert_eq!(
            0, tree.tok_meta[key_index],
            "object key should have metadata value of 0"
        );
        assert!(
            tree.tok_chld[key_index] > 0,
            "key index {} must have a child index greater than 0",
            key_index
        );
        assert!(
            key_index as u32 <= tree.tok_term[i],
            "key index {} must not exceed object termination index {}",
            key_index,
            tree.tok_term[i]
        );
        if tree.tok_next[key_index] == 0 {
            assert!(
                tree.tok_term[key_index] <= tree.tok_term[i],
                "last key's termination index {} should not exceed object's termination index {}",
                tree.tok_term[key_index],
                tree.tok_term[i]
            );
        }
        key_index = tree.tok_next[key_index] as usize;
        found_count += 1;
    }
    assert_eq!(
        expected_count, found_count,
        "object has correct number of keys"
    );
}

fn assert_array_valid(tree: &JsonAst, i: usize) {
    let range = &tree.tok_span[i];
    assert!(
        range.len() >= 2,
        "array token must be at least 2 characters (opening and closing brackets)"
    );
    assert_eq!(
        tree.contents[range.start], b'[',
        "expected '[' at start of array, found '{}'",
        tree.contents[range.start] as char
    );
    assert_eq!(
        tree.contents[range.end - 1],
        b']',
        "expected ']' at end of array, found '{}' `{}`",
        tree.contents[range.end - 1] as char,
        tree.value_at(i)
    );
    assert!(
        std::str::from_utf8(&tree.contents[range.clone()]).is_ok(),
        "array content must be valid UTF-8"
    );

    let expected_count = tree.tok_meta[i];
    let mut found_count = 0;
    let mut value_index = container_first_item_index(tree, i);
    while value_index != 0 {
        let next_value_index = tree.tok_next[value_index] as usize;
        assert!(
            value_index != next_value_index,
            "value index should not be equal to next value index"
        );
        assert!(
            value_index < next_value_index || next_value_index == 0,
            "value index should be less than next value index"
        );
        assert!(
            value_index as u32 <= tree.tok_term[i],
            "value index {} must not exceed array termination index {}",
            value_index,
            tree.tok_term[i]
        );
        if next_value_index == 0 {
            assert!(
                tree.tok_term[value_index] <= tree.tok_term[i],
                "last value's termination index {} should match array's termination index {}",
                tree.tok_term[value_index],
                tree.tok_term[i]
            );
        }
        value_index = next_value_index;
        found_count += 1;
    }
    assert_eq!(
        expected_count,
        found_count,
        "array `{}` has correct number of values",
        tree.value_at(i)
    );
}

fn assert_bool_valid(tree: &JsonAst, i: usize) {
    assert!(
        ["true", "false"].contains(&tree.value_at(i)),
        "boolean value must be either 'true' or 'false', found '{}'",
        tree.value_at(i)
    );
}

fn assert_null_valid(tree: &JsonAst, i: usize) {
    assert_eq!(
        "null",
        tree.value_at(i),
        "null token must have value 'null', found '{}'",
        tree.value_at(i)
    );
}

fn assert_comment_valid(tree: &JsonAst, i: usize) {
    if tree.tok_meta[i] & META_COMMENT_LINE != 0 {
        assert!(
            tree.value_at(i).starts_with("//"),
            "line comment value must start with '//', found '{}'",
            tree.value_at(i)
        );
    } else if tree.tok_meta[i] & META_COMMENT_BLOCK != 0 {
        assert!(
            tree.value_at(i).starts_with("/*") && tree.value_at(i).ends_with("*/"),
            "block comment value must start with '/*' and end with '*/', found '{}'",
            tree.value_at(i)
        );
    } else {
        unreachable!("unexpected token type for comment");
    }
}

pub fn assert_tree_valid(tree: &JsonAst) {
    tree.assert_lengths();
    // todo! assert all comments are
    // - not intersecting with any token span
    // - actually correspond to a comment (i.e. were updated correctly)

    for (i, &tok_type) in tree.tok_kind.iter().enumerate() {
        match tok_type {
            Token::String => assert_string_valid(tree, i),
            Token::Number => assert_number_valid(tree, i),
            Token::Object => assert_object_valid(tree, i),
            Token::Array => assert_array_valid(tree, i),
            Token::Boolean => assert_bool_valid(tree, i),
            Token::Null => assert_null_valid(tree, i),
            Token::Comment => assert_comment_valid(tree, i),
        }
    }
    // todo! assert reparsing contents succeeds, and that the parsed tree is identical to the original tree

    for i in 0..tree.next_index() {
        for j in 0..tree.next_index() {
            if i == j {
                continue;
            }
            assert_ne!(
                tree.tok_span[i], tree.tok_span[j],
                "tok range {i} and {j} are the same"
            );
            if tree.tok_next[i] != 0 {
                assert_ne!(
                    tree.tok_next[i], tree.tok_next[j],
                    "tok next {i} and {j} are the same"
                );
            }
        }
    }
}

#[derive(Debug)]
pub struct Node<'a> {
    pub kind: Token,
    pub meta: u32,
    pub span: Range<usize>,
    pub value: &'a str,
}

impl<'a> Node<'a> {
    pub fn from_tree(tree: &'a JsonAst, index: usize) -> Self {
        return Self {
            kind: tree.tok_kind[index],
            meta: tree.tok_meta[index],
            span: tree.tok_span[index].clone(),
            value: tree.value_for_char_range(&tree.tok_span[index]),
        };
    }
}

pub struct ObjectItemIter<'a> {
    tree: &'a JsonAst,
    key_index: usize,
}

impl<'a> ObjectItemIter<'a> {
    pub fn new(tree: &'a JsonAst, obj_index: usize) -> Self {
        assert_eq!(tree.tok_kind[obj_index], Token::Object);
        let key_index = container_first_item_index(tree, obj_index);
        ObjectItemIter { tree, key_index }
    }
}

impl<'a> Iterator for ObjectItemIter<'a> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.key_index == 0 {
            return None;
        }

        let value_index = container_first_item_index(self.tree, self.key_index);
        let key_index = self.key_index;
        self.key_index = self.tree.tok_next[self.key_index] as usize;
        Some((key_index, value_index))
    }
}

pub struct ArrayItemIter<'a> {
    tree: &'a JsonAst,
    next_index: usize,
}

impl<'a> ArrayItemIter<'a> {
    pub fn new(tree: &'a JsonAst, array_index: usize) -> Self {
        assert_eq!(tree.tok_kind[array_index], Token::Array);
        let next_index = container_first_item_index(tree, array_index);
        ArrayItemIter { tree, next_index }
    }
}

impl<'a> Iterator for ArrayItemIter<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_index == 0 {
            return None;
        }

        let value_index = self.next_index;
        self.next_index = self.tree.tok_next[self.next_index] as usize;

        Some(value_index)
    }
}

/// Will be zero if container is empty
fn container_first_item_index(tree: &JsonAst, index: usize) -> usize {
    assert!(
        matches!(tree.tok_kind[index], Token::Array | Token::Object) || is_object_key(tree, index)
    );
    tree.tok_chld[index] as usize
}

/// Will be zero if array is empty
fn container_last_item_index(tree: &JsonAst, container_index: usize) -> usize {
    assert!(matches!(
        tree.tok_kind[container_index],
        Token::Array | Token::Object
    ));
    let mut item_index = container_first_item_index(tree, container_index);
    while tree.tok_next[item_index] != 0 {
        item_index = tree.tok_next[item_index] as usize;
    }
    item_index
}

/// Upsert a containers first child index
fn upsert_container_first_child_index(
    tree: &mut JsonAst,
    container_index: usize,
    child_index: usize,
) {
    assert!(matches!(
        tree.tok_kind[container_index],
        Token::Array | Token::Object
    ));
    let container_child_index = tree.tok_chld[container_index];
    if container_child_index == 0 || container_child_index > child_index as u32 {
        tree.tok_chld[container_index] = child_index as u32;
    }
}

/// Adjusts a byte range to ensure it falls on UTF-8 character boundaries
pub fn str_range_adjusted<'a>(bytes: &'a [u8], range: Range<usize>) -> &'a str {
    let contents_str = unsafe { std::str::from_utf8_unchecked(&bytes) };

    // Find valid start position (move forward if needed)
    let valid_start =
        if range.start < contents_str.len() && !contents_str.is_char_boundary(range.start) {
            // Find the next character boundary
            let mut pos = range.start;
            while pos < contents_str.len() && !contents_str.is_char_boundary(pos) {
                pos += 1;
            }
            pos
        } else {
            range.start
        };

    // Find valid end position (move backward if needed)
    let valid_end = if range.end <= contents_str.len() && !contents_str.is_char_boundary(range.end)
    {
        // Find the previous character boundary
        let mut pos = range.end;
        while pos > valid_start && !contents_str.is_char_boundary(pos) {
            pos -= 1;
        }
        pos
    } else {
        range.end
    };

    let adjusted_range = valid_start..valid_end;
    return &contents_str[adjusted_range];
}

pub fn is_object_key(tree: &JsonAst, target_index: usize) -> bool {
    tree.tok_kind[target_index] == Token::String
        && tree.tok_term[target_index] > target_index as u32
        && tree.tok_chld[target_index] > 0
}

pub fn tok_meta_from_value(value: &serde_json::Value) -> u32 {
    match value {
        serde_json::Value::Null => 0,
        serde_json::Value::Bool(_) => 0,
        serde_json::Value::Number(n) => {
            let mut meta = 0;
            if n.is_f64() {
                meta |= META_NUM_FLOAT;
                if n.as_f64().unwrap().is_sign_negative() {
                    meta |= META_NUM_NEGATIVE;
                }
            } else if n.is_i64() && !n.is_u64() {
                meta |= META_NUM_NEGATIVE;
                meta &= !META_NUM_FLOAT;
            }
            meta
        }
        serde_json::Value::String(_) => 0,
        serde_json::Value::Array(arr) => arr.len() as u32,
        serde_json::Value::Object(obj) => obj.len() as u32,
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ReplaceTarget {
    Key,
    Value,
}

#[derive(Debug, PartialEq)]
pub enum ReplaceError {
    KeyMustBeString,
    NewValueSerializationFailure,
    InvalidPath,
}

pub fn replace_index(
    tree: &mut JsonAst,
    target_index: usize,
    mut source_tree: JsonAst,
) -> Result<(), ReplaceError> {
    let target_is_key = is_object_key(tree, target_index);

    let source_token_index = 'blk: {
        for (index, &tok) in source_tree.tok_kind.iter().enumerate() {
            if tok != Token::Comment {
                break 'blk index;
            }
        }
        unimplemented!("comment only replacement");
    };

    if target_is_key && source_tree.tok_kind[source_token_index] != Token::String {
        return Err(ReplaceError::KeyMustBeString);
    }

    let target_replacement_range = if target_is_key {
        target_index..target_index + 1
    } else {
        target_index..tree.tok_term[target_index] as usize + 1
    };
    let source_insertion_range = target_index..target_index + source_tree.next_index();
    let target_content_range = tree.tok_span[target_index].clone();
    let source_content_len = source_tree.contents.len();

    let offset_content = tree.tok_span[target_index].start;
    let offset_token = target_index;

    for tok_next in &mut source_tree.tok_next {
        if *tok_next != 0 {
            *tok_next += offset_token as u32;
        }
    }
    for range in &mut source_tree.tok_span {
        range.start += offset_content;
        range.end += offset_content;
    }
    for tok_term in &mut source_tree.tok_term {
        *tok_term += offset_token as u32;
    }
    for tok_chld in &mut source_tree.tok_chld {
        if *tok_chld != 0 {
            *tok_chld += offset_token as u32;
        }
    }
    if target_is_key {
        source_tree.tok_term[source_token_index] = tree.tok_term[target_index];
        source_tree.tok_chld[source_token_index] = tree.tok_chld[target_index];
    }

    let tok_next_prev = tree.tok_next[target_index];

    tree.tok_kind
        .splice(target_replacement_range.clone(), source_tree.tok_kind);
    tree.tok_span
        .splice(target_replacement_range.clone(), source_tree.tok_span);
    tree.tok_meta
        .splice(target_replacement_range.clone(), source_tree.tok_meta);
    tree.tok_next
        .splice(target_replacement_range.clone(), source_tree.tok_next);
    tree.tok_term
        .splice(target_replacement_range.clone(), source_tree.tok_term);
    tree.tok_chld
        .splice(target_replacement_range.clone(), source_tree.tok_chld);
    tree.contents
        .splice(target_content_range.clone(), source_tree.contents);

    if tok_next_prev != 0 {
        tree.tok_next[target_index] = tok_next_prev;
    }

    let tok_diff =
        usize::checked_signed_diff(source_insertion_range.end, target_replacement_range.end)
            .unwrap();

    for tok_next in &mut tree.tok_next[0..=source_insertion_range.start] {
        if *tok_next >= target_replacement_range.end as u32 {
            add_signed_u32(tok_next, tok_diff);
        }
    }
    for tok_next in &mut tree.tok_next[source_insertion_range.end..] {
        if *tok_next > target_replacement_range.end as u32 {
            add_signed_u32(tok_next, tok_diff);
        }
    }

    for tok_term in &mut tree.tok_term[0..source_insertion_range.start] {
        if *tok_term >= source_insertion_range.start as u32 {
            add_signed_u32(tok_term, tok_diff);
        }
    }
    for tok_term in &mut tree.tok_term[source_insertion_range.end..] {
        add_signed_u32(tok_term, tok_diff);
    }

    for tok_child in &mut tree.tok_chld[0..source_insertion_range.start] {
        if *tok_child >= target_replacement_range.end as u32 {
            add_signed_u32(tok_child, tok_diff);
        }
    }
    for tok_child in &mut tree.tok_chld[source_insertion_range.end..] {
        if *tok_child >= target_replacement_range.end as u32 {
            add_signed_u32(tok_child, tok_diff);
        }
    }

    let original_end = target_content_range.end;
    let updated_end = target_content_range.start + source_content_len;
    let end_diff = usize::checked_signed_diff(updated_end, original_end).unwrap();

    for range in &mut tree.tok_span[0..source_insertion_range.start] {
        if range.end > target_content_range.end {
            add_signed(&mut range.end, end_diff);
        }
    }
    for range in &mut tree.tok_span[source_insertion_range.end..] {
        add_signed_range(range, end_diff);
    }

    Ok(())
}

pub fn replace_path(
    tree: &mut JsonAst,
    path: &Path,
    source_value: JsonAst,
    target: ReplaceTarget,
) -> Result<(), ReplaceError> {
    let Some(target_index) = index_for_path(tree, path, target) else {
        return Err(ReplaceError::InvalidPath);
    };

    return replace_index(tree, target_index, source_value);
}

fn index_for_path(tree: &JsonAst, path: &Path, target: ReplaceTarget) -> Option<usize> {
    let mut index = 0;
    let mut value_index = 0;

    let path = &path.0;
    if path.is_empty() || path.len() == 1 && matches!(&path[0], PathEntry::Str(s) if s.is_empty()) {
        return Some(0);
    }
    'segments: for segment in path {
        match (segment, tree.tok_kind[index]) {
            (&PathEntry::Idx(idx), Token::Array) => {
                value_index = 0;
                let mut iter = ArrayItemIter::new(&tree, index);
                for _ in 0..idx {
                    iter.next()?;
                }
                index = iter.next()?;
            }
            (PathEntry::Str(key), Token::Object) => {
                let iter = ObjectItemIter::new(&tree, index);
                for (key_i, val_i) in iter {
                    let key_str = tree.value_at(key_i);
                    if key_str == key {
                        index = key_i;
                        value_index = val_i;
                        continue 'segments;
                    }
                }
                return None;
            }
            _ => {
                return None;
            }
        }
    }

    if target == ReplaceTarget::Value && tree.tok_kind[index] == Token::String && value_index != 0 {
        index = value_index;
    }
    Some(index)
}

#[derive(Debug, Clone, Copy)]
pub enum InsertionMethod {
    /// index is of item in array or key in object
    Before,
    /// index is of item in array or key in object
    After,
    /// index is of array or object
    Append,
    /// index is of array or object
    Prepend,
}

pub enum InsertionValue<'a> {
    Arr(JsonAst),
    Obj((&'a str, JsonAst)),
}

#[derive(Debug, PartialEq, Eq)]
pub enum InsertionError {
    IncorrectContainerType,
    FailedToSerializeValue,
    TargetIsNotItem,
    TargetIsNotContainer,
}

impl std::fmt::Display for InsertionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for InsertionError {}

/// `target_index` should be index of item in container (element in array or key in object) based on `method`
/// See documentation of ![`InsertionMethod`] for details
pub fn insert_index(
    tree: &mut JsonAst,
    source_value: InsertionValue<'_>,
    method: InsertionMethod,
    target_index: usize,
) -> Result<(), InsertionError> {
    let is_method_relative_to_item =
        matches!(method, InsertionMethod::After | InsertionMethod::Before);

    let target_tok_kind = tree.tok_kind[target_index];
    // todo! remove, this is misleading when inserting after a container
    let is_target_container = matches!(target_tok_kind, Token::Array | Token::Object);
    let target_container_index = if is_method_relative_to_item {
        // if target is container, and method is relative to item,
        // or vice versa, the api has been broken
        item_container_index(tree, target_index).ok_or(InsertionError::TargetIsNotItem)? // todo! better error
    } else {
        if !is_target_container {
            return Err(InsertionError::TargetIsNotContainer);
        }
        target_index
    };

    let is_value_obj_and_target_arr = matches!(source_value, InsertionValue::Obj(_))
        && ((is_method_relative_to_item && !is_object_key(tree, target_index))
            || (!is_method_relative_to_item && target_tok_kind != Token::Object));

    let is_value_arr_and_target_obj = matches!(source_value, InsertionValue::Arr(_))
        && ((is_method_relative_to_item && is_object_key(tree, target_index))
            || (!is_method_relative_to_item && target_tok_kind != Token::Array));

    if is_value_obj_and_target_arr || is_value_arr_and_target_obj {
        // cannot insert key, value pair into array
        return Err(InsertionError::IncorrectContainerType);
    }

    let source_tree = match source_value {
        InsertionValue::Arr(source_tree) => source_tree,
        InsertionValue::Obj((key, value)) => {
            let mut builder = JsonAstBuilder::new();
            builder.state.push(builder::State::Object);
            builder.key(key);
            builder.tree(&value);

            let mut tree = JsonAst::empty();
            tree.contents = builder.json.into_bytes();
            parse_key_value(&mut tree, &mut 0).expect("Failed to parse key-value pair");
            tree
        }
    };

    let (method, target_index) = match method {
        InsertionMethod::Before | InsertionMethod::After => (method, target_index),
        InsertionMethod::Prepend => match tree.tok_term[target_index] == target_index as u32 {
            true => (InsertionMethod::Prepend, target_index),
            false => (
                InsertionMethod::Before,
                match target_tok_kind {
                    Token::Object | Token::Array => container_first_item_index(tree, target_index),
                    _ => unreachable!(),
                },
            ),
        },
        InsertionMethod::Append => match tree.tok_term[target_index] == target_index as u32 {
            true => (InsertionMethod::Append, target_index),
            false => (
                InsertionMethod::After,
                match target_tok_kind {
                    Token::Object | Token::Array => container_last_item_index(tree, target_index),
                    _ => unreachable!(),
                },
            ),
        },
    };

    // the index in the target tree's token data lists
    // to insert the source trees data
    let target_tok_insertion_index;
    // the index in the target tree's content to insert
    // the content slices
    let target_content_insertion_index;
    // the content to insert at target_content_insertion_index
    let content_slices;
    let comma = *b",";
    // The next item in the container after the newly inserted item is
    // context dependent in append, after, and prepend/append.
    let source_tok_next;
    // The offset of the content within the source tree. I.e. when inserting after an item,
    // this is the length of the ", ".
    // Needed to correctly adjust the content ranges within the source tree
    let source_content_offset;

    match method {
        InsertionMethod::After => {
            target_tok_insertion_index = tree.tok_term[target_index] as usize + 1;
            target_content_insertion_index = if is_object_key(tree, target_index) {
                let value_index = tree.tok_chld[target_index] as usize;
                tree.tok_span[value_index].end
            } else {
                tree.tok_span[target_index].end
            };
            source_content_offset = comma.len();

            content_slices = [&comma, source_tree.contents.as_slice()];

            let tok_next_prev = tree.tok_next[target_index];
            if tok_next_prev != 0 {
                source_tok_next = tok_next_prev + source_tree.next_index() as u32;
            } else {
                source_tok_next = 0;
            }
            tree.tok_next[target_index] = target_tok_insertion_index as u32;
        }
        InsertionMethod::Before => {
            target_tok_insertion_index = target_index;
            target_content_insertion_index = tree.tok_span[target_index].start;
            content_slices = [source_tree.contents.as_slice(), &comma];
            source_content_offset = 0;
            source_tok_next = (target_tok_insertion_index + source_tree.next_index()) as u32;
        }
        // because of transformation above, this is actually just adding the first element
        // to the container
        InsertionMethod::Prepend | InsertionMethod::Append => {
            target_tok_insertion_index = target_index + 1;

            target_content_insertion_index = tree.tok_span[target_index].start + 1;
            content_slices = [source_tree.contents.as_slice(), b""];
            source_content_offset = 0;
            source_tok_next = 0;
        }
    };

    let diff_content = content_slices.iter().map(|s| s.len()).sum::<usize>();
    let diff_token = source_tree.next_index();

    let source_insertion_range =
        target_tok_insertion_index..target_tok_insertion_index + source_tree.next_index();

    tree.tok_kind.splice(
        target_tok_insertion_index..target_tok_insertion_index,
        source_tree.tok_kind,
    );
    tree.tok_meta.splice(
        target_tok_insertion_index..target_tok_insertion_index,
        source_tree.tok_meta,
    );
    tree.tok_next.splice(
        target_tok_insertion_index..target_tok_insertion_index,
        source_tree.tok_next,
    );
    tree.tok_span.splice(
        target_tok_insertion_index..target_tok_insertion_index,
        source_tree.tok_span,
    );
    tree.tok_term.splice(
        target_tok_insertion_index..target_tok_insertion_index,
        source_tree.tok_term,
    );
    tree.tok_chld.splice(
        target_tok_insertion_index..target_tok_insertion_index,
        source_tree.tok_chld,
    );
    tree.contents.splice(
        target_content_insertion_index..target_content_insertion_index,
        content_slices.into_iter().flatten().copied(),
    );

    // meta on containers is the length, this will always have increased by one
    // after the insertion
    tree.tok_meta[target_container_index] += 1;

    // up to the start of the container we are inserting into, if a tok_next
    // is >= the start of the insertion range, we are in a container of depth > 1
    // and need to adjust it by the difference in tokens to point at the same token
    for tok_next in &mut tree.tok_next[..=target_container_index] {
        // >= because if the container was empty before the insertion, the
        // tok_next of the container pointing to it's next sibling would be the
        // start of the insertion range
        assert_ne!(
            source_insertion_range.start, 0,
            "must be inserting into a container, which by construction must be at a lower index than it's children"
        );
        if *tok_next >= source_insertion_range.start as u32 {
            *tok_next += diff_token as u32;
        }
    }

    tree.tok_next[source_insertion_range.start] = source_tok_next;
    // except for the first token, which we set explicitly, all tokens within the insertion range
    // should be offset by the start of the insertion range
    for tok_next in &mut tree.tok_next[source_insertion_range.start + 1..source_insertion_range.end]
    {
        if *tok_next != 0 {
            *tok_next += source_insertion_range.start as u32;
        }
    }

    // for tokens after the insertion range, we need to offset them by the difference in tokens
    for tok_next in &mut tree.tok_next[source_insertion_range.end..] {
        if *tok_next != 0 {
            *tok_next += diff_token as u32
        }
    }

    upsert_container_first_child_index(tree, target_container_index, source_insertion_range.start);
    // the only tokens before the insertion range that need to have their
    // descendant and content ranges updated are the ancestors of the container
    // we are inserting into
    let mut container_index = Some(target_container_index);
    while let Some(outer_container_index) = container_index {
        tree.tok_term[outer_container_index] += diff_token as u32;
        if !is_object_key(tree, outer_container_index) {
            tree.tok_span[outer_container_index].end += diff_content;
        }
        container_index = item_parent_index(tree, outer_container_index);
    }

    for tok_term in &mut tree.tok_term[source_insertion_range.clone()] {
        *tok_term += source_insertion_range.start as u32;
    }

    for tok_term in &mut tree.tok_term[source_insertion_range.end..] {
        *tok_term += diff_token as u32;
    }

    for tok_child in &mut tree.tok_chld[source_insertion_range.clone()] {
        if *tok_child > 0 {
            *tok_child += source_insertion_range.start as u32;
        }
    }

    for tok_child in &mut tree.tok_chld[source_insertion_range.end..] {
        if *tok_child > 0 {
            *tok_child += diff_token as u32;
        }
    }

    // all tokens within the insertion range need to have their content ranges
    // offset by the start of the insertion range, as well as the source_content_offset
    // which describes the offset within the source content that the source_tree actually
    // starts at (e.g. 2 when inserting a ", " before the first token in the source tree)
    for tok_span in &mut tree.tok_span[source_insertion_range.clone()] {
        tok_span.start += target_content_insertion_index + source_content_offset;
        tok_span.end += target_content_insertion_index + source_content_offset;
    }

    // all tokens after the insertion range need to have their content ranges
    // offset by the difference in content length
    for tok_span in &mut tree.tok_span[source_insertion_range.end..] {
        tok_span.start += diff_content;
        tok_span.end += diff_content;
    }

    // todo! return index of new item
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RemoveError {
    CannotRemoveRoot,
    InvalidIndex,
    InvalidTree,
}

pub fn remove_index(tree: &mut JsonAst, index: usize) -> Result<(), RemoveError> {
    if index == 0 {
        return Err(RemoveError::CannotRemoveRoot);
    }
    if index >= tree.next_index() {
        return Err(RemoveError::InvalidIndex);
    }

    let parent_index = item_parent_index(tree, index);

    if let Some(_key_index) = parent_index.filter(|&i| is_object_key(tree, i)) {
        if replace_index(tree, index, parse("null").unwrap()).is_ok() {
            return Ok(());
        } else {
            // todo! better error
            return Err(RemoveError::InvalidTree);
        }
    }

    let token_removal_range = index..tree.tok_term[index] as usize + 1;
    let mut contents_removal_range = tree.tok_span[index].start
        ..usize::max(
            tree.tok_span[index].end,
            tree.tok_span[tree.tok_term[index] as usize].end,
        );

    let prev_sibling_index = tok_prev(tree, index);

    if let Some(tok_next) = Some(tree.tok_next[index]).filter(|&n| n != 0) {
        contents_removal_range.end = tree.tok_span[tok_next as usize].start;
    } else if let Some(tok_prev) = prev_sibling_index {
        let prev_end = usize::max(
            tree.tok_span[tok_prev].end,
            tree.tok_span[tree.tok_term[tok_prev] as usize].end,
        );
        let str = std::str::from_utf8(&tree.contents[prev_end..])
            .ok()
            .ok_or(RemoveError::InvalidTree)?;
        contents_removal_range.start = prev_end + str.find(',').ok_or(RemoveError::InvalidTree)?;
        tree.tok_next[tok_prev] = tree.tok_next[index];
    }

    tree.tok_kind.drain(token_removal_range.clone());
    tree.tok_span.drain(token_removal_range.clone());
    tree.tok_next.drain(token_removal_range.clone());
    tree.tok_meta.drain(token_removal_range.clone());
    tree.tok_term.drain(token_removal_range.clone());
    tree.tok_chld.drain(token_removal_range.clone());
    tree.contents.drain(contents_removal_range.clone());

    if let Some(parent_index) = parent_index.filter(|&i| !is_object_key(tree, i)) {
        tree.tok_meta[parent_index] -= 1;
        if tree.tok_meta[parent_index] == 0 {
            tree.tok_chld[parent_index] = 0;
        }
    }

    let diff_token = token_removal_range.len();
    let diff_contents = contents_removal_range.len();

    let mut ancestor_index = parent_index;
    while let Some(parent_index) = ancestor_index {
        tree.tok_term[parent_index] -= diff_token as u32;
        if !is_object_key(tree, parent_index) {
            tree.tok_span[parent_index].end -= diff_contents;
        }
        ancestor_index = item_parent_index(tree, parent_index);
    }

    for tok_term in &mut tree.tok_term[token_removal_range.start..] {
        *tok_term -= diff_token as u32;
    }

    for tok_child in &mut tree.tok_chld[token_removal_range.start..] {
        if *tok_child > 0 {
            *tok_child -= diff_token as u32;
        }
    }

    for tok_span in &mut tree.tok_span[token_removal_range.start..] {
        tok_span.start -= diff_contents;
        tok_span.end -= diff_contents;
    }

    for tok_next in &mut tree.tok_next[..token_removal_range.start] {
        if *tok_next > token_removal_range.start as u32 {
            *tok_next -= diff_token as u32;
        }
    }

    for tok_next in &mut tree.tok_next[token_removal_range.start..] {
        if *tok_next != 0 {
            *tok_next -= diff_token as u32;
        }
    }

    Ok(())
}

fn item_container_index(tree: &JsonAst, item_index: usize) -> Option<usize> {
    return item_parent_index(tree, item_index)
        .filter(|&parent_index| !is_object_key(tree, parent_index));
}

fn item_parent_index(tree: &JsonAst, mut item_index: usize) -> Option<usize> {
    let mut cur_index = item_index;
    while cur_index > 0 {
        if tree.tok_next[cur_index] as usize == item_index {
            item_index = cur_index;
        } else if tree.tok_chld[cur_index] as usize == item_index
            && tree.tok_term[cur_index] >= item_index as u32
        {
            return Some(cur_index);
        }
        cur_index -= 1;
    }
    if tree.tok_chld[cur_index] as usize == item_index && item_index > 0 {
        return Some(cur_index);
    }

    None
}

fn tok_prev(tree: &JsonAst, item_index: usize) -> Option<usize> {
    if item_index == 0 {
        return None;
    }
    let mut index = item_index;
    while index > 0 {
        index -= 1;
        if tree.tok_next[index] as usize == item_index {
            return Some(index);
        }
    }
    return None;
}

#[inline(always)]
fn add_signed(val: &mut usize, diff: isize) {
    *val = usize::checked_add_signed(*val, diff).expect("overflow");
}

#[inline(always)]
fn add_signed_u32(val: &mut u32, diff: isize) {
    *val = u32::checked_add_signed(*val, diff as i32).expect("overflow");
}

fn add_signed_range(val: &mut Range<usize>, diff: isize) {
    add_signed(&mut val.start, diff);
    add_signed(&mut val.end, diff);
}

#[cfg(test)]
mod test {
    use super::*;
    use std::ops::Range;

    // todo! parameterize on delimiters
    fn extract_delimited(target: &str) -> (String, Range<usize>) {
        let span_begin = target.find('<').expect("span start defined");
        let span_terminate = target.find('>').expect("span end defined");
        let mut target_str = String::with_capacity(target.len());
        target_str.push_str(&target[..span_begin]);
        target_str.push_str(&target[span_begin + 1..span_terminate]);
        target_str.push_str(&target[span_terminate + 1..]);

        return (target_str, span_begin..span_terminate - 1);
    }

    #[test]
    fn find_item_container_index() {
        #[track_caller]
        fn check(target: &str) {
            let (mut target, mut container_range) = extract_delimited(target);

            let item_begin = target.find('$').expect("item start delimiter present");
            target.remove(item_begin);
            let item_end = target.find('$').expect("item end delimiter present");
            target.remove(item_end);
            container_range.end -= 2;

            let item_range = item_begin..item_end;
            let tree = parse(&target).expect("parse succeeded");

            assert_tree_valid(&tree);
            let container_index = tree
                .tok_span
                .iter()
                .position(|range| range == &container_range)
                .expect("index found");
            let item_index = tree
                .tok_span
                .iter()
                .position(|range| range == &item_range)
                .expect("index found");
            let index = item_container_index(&tree, item_index).expect("container index found");
            assert_eq!(container_index, index);
        }

        check("<[$3$]>");
        check("<[1,2,$3$,4,5]>");
        check("[[[<[1,2,$3$,4,5]>]]]");
        check("[<[$[]$, 1]>, 2]");
        check("<[$[[], 1]$, 2]>");
    }

    mod iter {
        use super::*;

        fn check(target: &str) {
            let (mut target, mut item_range) = extract_delimited(target);

            let mut items = vec![];

            while let Some(begin) = target.find('$') {
                target.remove(begin);
                let end = target.find('$').expect("item has # on either end");
                target.remove(end);
                item_range.end -= 2;
                items.push(begin..end);
            }

            let tree = parse(&target).expect("parse succeeded");
            assert_tree_valid(&tree);

            let index = tree
                .tok_span
                .iter()
                .position(|range| range == &item_range)
                .expect("index found");

            let tok_type = tree.tok_kind[index];
            if tok_type == Token::Array {
                let mut iter = ArrayItemIter::new(&tree, index);
                for (i, item_range) in items.into_iter().enumerate() {
                    let iter_item_index = iter.next().expect(&format!(
                        "missing item [{i}]: `{}`",
                        tree.value_for_char_range(&item_range)
                    ));

                    assert_eq!(
                        item_range,
                        tree.tok_span[iter_item_index],
                        "item mismatch, expected:\n{}\nfound:\n{}\n",
                        tree.value_for_char_range(&item_range),
                        tree.value_at(iter_item_index)
                    );
                }
                let last = iter.next();
                match last {
                    None => {}
                    Some(index) => {
                        panic!("found extra item in array iter:\n{}", tree.value_at(index));
                    }
                }
            } else if tok_type == Token::Object {
                let mut iter = ObjectItemIter::new(&tree, index);
                assert_eq!(
                    items.len() % 2,
                    0,
                    "odd number of expected items for object: {}",
                    items.len()
                );
                for (i, [item_key_range, item_val_range]) in
                    items.into_iter().array_chunks().enumerate()
                {
                    let (iter_key_index, iter_val_index) = iter.next().expect(&format!(
                        "missing item key [{i}]: `{}`",
                        tree.value_for_char_range(&item_key_range)
                    ));

                    assert_eq!(
                        item_key_range,
                        tree.tok_span[iter_key_index],
                        "item key mismatch, expected:\n{}\nfound:\n{}\n",
                        tree.value_for_char_range(&item_key_range),
                        tree.value_at(iter_key_index)
                    );

                    assert_eq!(
                        item_val_range,
                        tree.tok_span[iter_val_index],
                        "item value mismatch, expected:\n{}\nfound:\n{}\n",
                        tree.value_for_char_range(&item_val_range),
                        tree.value_at(iter_val_index)
                    );
                }
                let last = iter.next();
                match last {
                    None => {}
                    Some((key_index, val_index)) => {
                        panic!(
                            "found extra item in array iter:\n{}: {}",
                            tree.value_at(key_index),
                            tree.value_at(val_index)
                        );
                    }
                }
            }
        }

        #[test]
        fn object() {
            check(r#"<{ $"key"$: $"value"$ }>"#);
            check(r#"<{}>"#);
            check(r#"<{ $"key"$: $"value"$, $"key2"$: $"value2"$ }>"#);
            check(r#"{"parent": true, "child": <{ $"key"$: $"value"$, $"key2"$: $"value2"$ }>}"#);
            check(r#"{"parent": true, "child": <{}>}"#);
            check(r#"[<{}>]"#);
            check(r#"[1, 2, 3, 4, 5, <{$"a"$: $1$, $"b"$: $2$ }>, 6, 7, 8]"#);
            // todo! LLM generate more tests
        }

        #[test]
        fn array() {
            check(r#"<[]>"#);
            check(r#"<[$1$]>"#);
            check(r#"<[${}$, ${}$,     ${}$]>"#);
            // todo! LLM generate more tests
        }
    }

    mod replace {
        use super::*;

        #[track_caller]
        fn check(target: &str, source: JsonAst, expected: &str) {
            let (target, item_range) = extract_delimited(target);

            let mut tree = parse(&target).expect("parse succeeded");
            assert_tree_valid(&tree);

            let index = tree
                .tok_span
                .iter()
                .position(|range| range == &item_range)
                .expect("index found");

            replace_index(&mut tree, index, source).expect("replace failed");

            // todo! parsing non container root
            match parse(&expected) {
                Ok(expected_tree) => pretty_assertions::assert_eq!(&tree, &expected_tree),
                Err(ParseError::TODOCannotParseNonContainerRoot) => {}
                Err(err) => panic!("{err}"),
            }

            assert_tree_valid(&tree);
        }

        macro_rules! check {
            ($name:ident, $target:expr, $source:expr, $expected:expr) => {
                #[test]
                fn $name() {
                    check($target, $source, $expected);
                }
            };
        }

        #[track_caller]
        fn check_fail(target: &str, source: JsonAst, expected_err: ReplaceError) {
            let (target, item_range) = extract_delimited(target);

            let mut tree = parse(&target).expect("parse succeeded");
            assert_tree_valid(&tree);

            let index = tree
                .tok_span
                .iter()
                .position(|range| range == &item_range)
                .expect("index found");

            let err =
                replace_index(&mut tree, index, source).expect_err("expected replace to fail");
            assert_eq!(err, expected_err);
            assert_tree_valid(&tree);

            let new_contents =
                std::str::from_utf8(&tree.contents).expect("tree contents is valid utf8");

            assert_eq!(new_contents, &target);
        }

        macro_rules! check_fail {
            ($name:ident, $target:expr, $source:expr, $expected:expr) => {
                #[test]
                fn $name() {
                    check_fail($target, $source, $expected);
                }
            };
        }

        check!(
            obj_value_string,
            r#"{"key":<"value">}"#,
            parse(r#""new_value""#).unwrap(),
            r#"{"key":"new_value"}"#
        );

        check!(
            obj_value_number,
            r#"{"key":<"value">}"#,
            parse(r#"3.1459"#).unwrap(),
            r#"{"key":3.1459}"#
        );

        check!(
            obj_value_negative_zero,
            r#"{"key":<"value">}"#,
            parse(r#"-0.0"#).unwrap(),
            r#"{"key":-0.0}"#
        );

        check!(
            obj_value_array,
            r#"{"key":<"value">}"#,
            parse(r#"[true]"#).unwrap(),
            r#"{"key":[true]}"#
        );

        check!(
            obj_value_nested_object,
            r#"{"key":<"value">,"key2":"value2"}"#,
            parse(r#"{"sub_key":"sub_value"}"#).unwrap(),
            r#"{"key":{"sub_key":"sub_value"},"key2":"value2"}"#
        );

        check!(
            obj_key_simple,
            r#"{<"key">:"value"}"#,
            parse(r#""new_key""#).unwrap(),
            r#"{"new_key":"value"}"#
        );

        check!(
            obj_key_middle,
            r#"{"a":1,<"b">:2,"c":3}"#,
            parse(r#""d""#).unwrap(),
            r#"{"a":1,"d":2,"c":3}"#
        );

        check!(
            obj_key_to_empty_string,
            r#"{"a":<{"a":null,"b":null}>,"b":{"":""}}"#,
            parse(r#""""#).unwrap(),
            r#"{"a":"","b":{"":""}}"#
        );

        check!(
            obj_root,
            r#"<{"key":"value","key2":"value2"}>"#,
            parse(r#"true"#).unwrap(),
            "true"
        );

        check!(
            arr_value_object_to_array,
            r#"[1,<{"key":"value"}>,3,4,5]"#,
            parse(r#"[2]"#).unwrap(),
            r#"[1,[2],3,4,5]"#
        );

        check!(
            arr_value_object_to_number,
            r#"[1,<{"key":"value"}>,3,4,5]"#,
            parse(r#"2"#).unwrap(),
            r#"[1,2,3,4,5]"#
        );

        check!(
            arr_value_null_to_array,
            r#"[<null>,{"":null},null]"#,
            parse(r#"[{}]"#).unwrap(),
            r#"[[{}],{"":null},null]"#
        );

        check!(
            arr_replace_root_with_comment,
            r#"<[false,//
]>"#,
            parse(r#"null"#).unwrap(),
            "null"
        );

        check!(
            obj_replace_root_with_comment,
            r#"<{"key":false,//
}>"#,
            parse(r#"null"#).unwrap(),
            "null"
        );

        check_fail!(
            obj_key_non_string_fail,
            r#"{<"key">:"value"}"#,
            parse(r#"123"#).unwrap(),
            ReplaceError::KeyMustBeString
        );

        check_fail!(
            obj_key_array_fail,
            r#"{"a":1,<"b">:2,"c":3}"#,
            parse(r#"[1,2,3]"#).unwrap(),
            ReplaceError::KeyMustBeString
        );

        check_fail!(
            obj_key_object_fail,
            r#"{<"old_key">:"value"}"#,
            parse(r#"{"new":"key"}"#).unwrap(),
            ReplaceError::KeyMustBeString
        );

        check_fail!(
            obj_key_null_fail,
            r#"{<"key">:"value"}"#,
            parse(r#"null"#).unwrap(),
            ReplaceError::KeyMustBeString
        );

        check_fail!(
            obj_key_boolean_fail,
            r#"{"a":1,<"key">:2}"#,
            parse(r#"true"#).unwrap(),
            ReplaceError::KeyMustBeString
        );
    }

    mod insert {
        use super::*;
        use InsertionMethod::*;
        use InsertionValue::*;

        #[track_caller]
        fn check(
            target: &str,
            insertion_method: InsertionMethod,
            source: InsertionValue,
            expected: &str,
        ) {
            let (target, item_range) = extract_delimited(target);

            let mut tree = parse(&target).expect("parse succeeded");
            assert_tree_valid(&tree);

            let index = tree
                .tok_span
                .iter()
                .position(|range| range == &item_range)
                .expect("index found");

            insert_index(&mut tree, source, insertion_method, index).expect("insert failed");
            // assert_tree_valid(&tree);

            let expected_tree = parse(expected).expect("expected is not valid json");

            pretty_assertions::assert_eq!(tree, expected_tree);
        }

        #[track_caller]
        fn check_fail(
            target: &str,
            insertion_method: InsertionMethod,
            source: InsertionValue,
            expected_err: InsertionError,
        ) {
            let (target, item_range) = extract_delimited(target);

            let mut tree = parse(&target).expect("parse succeeded");
            assert_tree_valid(&tree);

            let index = tree
                .tok_span
                .iter()
                .position(|range| range == &item_range)
                .expect("index found");

            let err = insert_index(&mut tree, source, insertion_method, index)
                .expect_err("expected insert to fail");
            assert_eq!(err, expected_err);
            assert_tree_valid(&tree);

            let new_contents =
                std::str::from_utf8(&tree.contents).expect("tree contents is valid utf8");

            assert_eq!(new_contents, &target);
        }

        macro_rules! check {
            ($name:ident, $target:expr, $method:expr, $source:expr, $expected:expr) => {
                #[test]
                fn $name() {
                    check($target, $method, $source, $expected);
                }
            };
        }

        macro_rules! check_fail {
            ($name:ident, $target:expr, $method:expr, $source:expr, $expected:expr) => {
                #[test]
                fn $name() {
                    check_fail($target, $method, $source, $expected);
                }
            };
        }

        check!(
            arr_nested_obj,
            r#"[[<[]>,1],2]"#,
            Prepend,
            Arr(parse(r#"{"foo":"bar"}"#).unwrap()),
            r#"[[[{"foo":"bar"}],1],2]"#
        );

        check!(
            arr_after_middle,
            r#"[1,<2>,4]"#,
            After,
            Arr(parse("3").unwrap()),
            r#"[1,2,3,4]"#
        );

        check!(
            arr_after_single,
            r#"[<1>]"#,
            After,
            Arr(parse("2").unwrap()),
            r#"[1,2]"#
        );

        check_fail!(
            arr_after_empty_fail,
            r#"<[]>"#,
            After,
            Arr(parse("2").unwrap()),
            InsertionError::TargetIsNotItem
        );

        check!(
            arr_after_object,
            r#"[<{"foo":"bar"}>]"#,
            After,
            Arr(parse(r#"{"baz":"qux"}"#).unwrap()),
            r#"[{"foo":"bar"},{"baz":"qux"}]"#
        );

        check_fail!(
            arr_append_to_null_fail,
            r#"[<null>]"#,
            Append,
            Arr(parse("null").unwrap()),
            InsertionError::TargetIsNotContainer
        );

        check!(
            arr_before_object,
            r#"[<{"foo":"bar"}>]"#,
            Before,
            Arr(parse(r#"{"baz":"qux"}"#).unwrap()),
            r#"[{"baz":"qux"},{"foo":"bar"}]"#
        );

        check!(
            arr_before_nested_array,
            r#"[<{"foo":"bar"}>,{"baz":"qux"}]"#,
            Before,
            Arr(parse("[1,2]").unwrap()),
            r#"[[1,2],{"foo":"bar"},{"baz":"qux"}]"#
        );

        check!(
            arr_before_end,
            r#"[1,2,<4>]"#,
            Before,
            Arr(parse("3").unwrap()),
            r#"[1,2,3,4]"#
        );

        check!(
            arr_before_start,
            r#"[<1>,2,3]"#,
            Before,
            Arr(parse("0").unwrap()),
            r#"[0,1,2,3]"#
        );

        check!(
            arr_before_single,
            r#"[<1>]"#,
            Before,
            Arr(parse("0").unwrap()),
            r#"[0,1]"#
        );

        check!(
            arr_before_middle,
            r#"[0,<2>,3]"#,
            Before,
            Arr(parse("1").unwrap()),
            r#"[0,1,2,3]"#
        );

        check!(
            arr_prepend,
            r#"<[1,2,3]>"#,
            Prepend,
            Arr(parse("0").unwrap()),
            r#"[0,1,2,3]"#
        );

        check!(
            arr_append,
            r#"<[1,2,3]>"#,
            Append,
            Arr(parse("4").unwrap()),
            r#"[1,2,3,4]"#
        );

        check!(
            arr_prepend_empty,
            r#"<[]>"#,
            Prepend,
            Arr(parse("0").unwrap()),
            r#"[0]"#
        );

        check!(
            arr_prepend_object_to_empty,
            r#"<[]>"#,
            Prepend,
            Arr(parse(r#"{"foo":"bar"}"#).unwrap()),
            r#"[{"foo":"bar"}]"#
        );

        check!(
            arr_prepend_null_to_nested,
            r#"[<[]>,null,null,[null]]"#,
            Prepend,
            Arr(parse("null").unwrap()),
            r#"[[null],null,null,[null]]"#
        );

        check_fail!(
            obj_array_value_fail,
            r#"{<"foo">: "bar"}"#,
            After,
            Arr(parse(r#"{"baz":"qux"}"#).unwrap()),
            InsertionError::IncorrectContainerType
        );

        check!(
            obj_after_single,
            r#"{<"foo">:"bar"}"#,
            After,
            Obj(("baz", parse(r#""qux""#).unwrap())),
            r#"{"foo":"bar","baz":"qux"}"#
        );

        check!(
            obj_after_first,
            r#"{<"foo">:"bar","quz":"qua"}"#,
            After,
            Obj(("baz", parse(r#""qux""#).unwrap())),
            r#"{"foo":"bar","baz":"qux","quz":"qua"}"#
        );

        check!(
            obj_after_first_duplicate,
            r#"{<"foo">:"bar","quz":"qua"}"#,
            After,
            Obj(("baz", parse(r#""qux""#).unwrap())),
            r#"{"foo":"bar","baz":"qux","quz":"qua"}"#
        );

        check_fail!(
            obj_after_value_fail,
            r#"{"": <null>}"#,
            After,
            Arr(parse("null").unwrap()),
            InsertionError::TargetIsNotItem
        );

        check!(
            obj_before_first,
            r#"{<"foo">:"bar","quz":"qua"}"#,
            Before,
            Obj(("baz", parse(r#""qux""#).unwrap())),
            r#"{"baz":"qux","foo":"bar","quz":"qua"}"#
        );

        check!(
            obj_before_second,
            r#"{"foo":"bar",<"quz">:"qua"}"#,
            Before,
            Obj(("baz", parse(r#""qux""#).unwrap())),
            r#"{"foo":"bar","baz":"qux","quz":"qua"}"#
        );

        check!(
            obj_append_empty,
            r#"<{}>"#,
            Append,
            Obj(("baz", parse(r#""qux""#).unwrap())),
            r#"{"baz":"qux"}"#
        );

        check!(
            obj_prepend_empty,
            r#"<{}>"#,
            Prepend,
            Obj(("baz", parse(r#""qux""#).unwrap())),
            r#"{"baz":"qux"}"#
        );

        check!(
            obj_append_special_chars,
            r#"<{}>"#,
            Append,
            Obj(("y\0c", parse("null").unwrap())),
            r#"{"y\u0000c":null}"#
        );

        check!(
            obj_append_empty_key,
            r#"{"\n":false,"0":<{}>}"#,
            Append,
            Obj(("", parse("{}").unwrap())),
            r#"{"\n":false,"0":{"":{}}}"#
        );

        check_fail!(
            arr_insert_at_comment,
            r#"[<// F
>            true]"#,
            After,
            Arr(parse("null").unwrap()),
            InsertionError::TargetIsNotItem
        );

        check!(
            obj_insert_after_key_with_nested_value,
            r#"{"`":// hhhhhe
        [false],<"">:[[]]}"#,
            After,
            Obj(("", parse("true").unwrap())),
            r#"{"`":// hhhhhe
        [false],"":[[]],"":true}"#
        );
    }

    mod remove {
        use crate::{RemoveError, assert_tree_valid, parse, remove_index, test::extract_delimited};

        #[track_caller]
        fn check(input: impl Into<String>, expected: impl Into<String>) {
            let (input, range) = extract_delimited(&input.into());
            let mut tree = parse(&input).expect("parse failed");
            let index = tree
                .tok_span
                .iter()
                .position(|r| r == &range)
                .expect("range found");

            remove_index(&mut tree, index).expect("Remove succeeded");
            let expected_tree = parse(&expected.into()).expect("contents valid after remove");

            pretty_assertions::assert_eq!(&tree, &expected_tree);

            assert_tree_valid(&tree);
        }

        #[track_caller]
        fn check_fail(input: impl Into<String>, expected_err: RemoveError) {
            let (input, range) = extract_delimited(&input.into());
            let mut tree = parse(&input).expect("parse failed");
            let index = tree
                .tok_span
                .iter()
                .position(|r| r == &range)
                .expect("range found");

            let err = remove_index(&mut tree, index).expect_err("expected remove to fail");
            assert_eq!(err, expected_err);
            assert_tree_valid(&tree);

            let new_contents =
                std::str::from_utf8(&tree.contents).expect("tree contents is valid utf8");

            assert_eq!(new_contents, &input);
        }

        macro_rules! check {
            ($name:ident, $input:expr, $expected:expr) => {
                #[test]
                fn $name() {
                    check($input, $expected);
                }
            };
        }

        macro_rules! check_fail {
            ($name:ident, $input:expr, $expected:expr) => {
                #[test]
                fn $name() {
                    check_fail($input, $expected);
                }
            };
        }

        check!(arr_value_single, "[<1>]", "[]");

        check!(arr_value_middle, "[1, <0>, 2]", "[1, 2]");

        check!(
            arr_value_nested_trailing_comma,
            r#"{"foo": [1,2,<3>,]}"#,
            r#"{"foo": [1,2,]}"#
        );

        check!(arr_value_first, "[<0>, 1, 2]", "[1, 2]");

        check!(arr_value_nested_array, r#"[<[""]>]"#, r#"[]"#);

        check!(arr_value_deeply_nested, r#"[0,[<1>],3]"#, r#"[0,[],3]"#);

        check!(obj_key_single, r#"{<"key">: "value"}"#, "{}");

        check!(
            obj_key_middle,
            r#"{"foo": "bar", <"baz">: "qux", "qal": "qud"}"#,
            r#"{"foo": "bar", "qal": "qud"}"#
        );

        check!(
            obj_key_nested,
            r#"{"foo": {"bar": {<"qux">: "qal"}}}"#,
            r#"{"foo": {"bar": {}}}"#
        );

        check!(
            obj_key_with_array_value,
            r#"{"a":[1,2,3],<"b">:""}"#,
            r#"{"a":[1,2,3]}"#
        );

        check!(
            obj_value_nested_object,
            r#"{"foo": {"bar": <{"qux": "qal"}>}}"#,
            r#"{"foo": {"bar": null}}"#
        );

        check!(
            obj_value_string,
            r#"{"foo": <"bar">, "baz": "quz"}"#,
            r#"{"foo": null, "baz": "quz"}"#
        );

        check_fail!(arr_root_fail, "<[1, 2, 3]>", RemoveError::CannotRemoveRoot);

        check_fail!(
            obj_root_fail,
            r#"<{"key": "value"}>"#,
            RemoveError::CannotRemoveRoot
        );
    }
}
