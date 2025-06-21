#![feature(iter_array_chunks)]

pub use serde_json;
use std::{fmt::Display, hash::Hasher, ops::Range};

const NUM_NEGATIVE: u32 = 1 << 0;
const NUM_FLOAT: u32 = 1 << 1;
const EMPTY_RANGE: Range<usize> = 0..0;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    Array,
    Object,
    String,
    Number,
    Boolean,
    Null,
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct JsonAst {
    pub contents: Vec<u8>,
    pub tok_span: Vec<Range<usize>>,
    pub tok_kind: Vec<Token>,
    /// short for tok_descendants
    pub tok_desc: Vec<Range<usize>>,
    pub tok_meta: Vec<u32>,
    pub tok_next: Vec<u32>,
    pub comments: Vec<Range<usize>>,
}

impl std::fmt::Debug for JsonAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JsonAst")
            .field("contents", &std::str::from_utf8(&self.contents))
            .field(
                "tok_range",
                &self
                    .tok_span
                    .iter()
                    .map(|range| {
                        (
                            range.clone(),
                            std::str::from_utf8(&self.contents[range.clone()]),
                        )
                    })
                    .collect::<Vec<_>>(),
            )
            .field("tok_type", &self.tok_kind)
            .field("tok_children", &self.tok_desc)
            .field("tok_meta", &self.tok_meta)
            .field("tok_next", &self.tok_next)
            .field("comments", &self.comments)
            .finish()
    }
}

impl JsonAst {
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
        assert_eq!(self.tok_span.len(), self.tok_desc.len());
        assert_eq!(self.tok_span.len(), self.tok_meta.len());
        assert_eq!(self.tok_span.len(), self.tok_next.len());
    }

    pub fn hash_default(&self) -> u64 {
        use std::hash::{DefaultHasher, Hash as _};
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(char),
    UnexpectedEndOfInput,
    InvalidNumber,
    InvalidBoolean,
    InvalidNull,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
    fn from_str(s: &str) -> Path {
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
    let contents = input.as_bytes().to_vec();
    let mut tree = JsonAst {
        contents,
        tok_span: Vec::new(),
        tok_kind: Vec::new(),
        tok_desc: Vec::new(),
        tok_meta: Vec::new(),
        tok_next: Vec::new(),
        comments: Vec::new(),
    };

    let mut cursor = 0;

    let eof = parse_any_ignore_maybe(&mut tree, &mut cursor)?;
    if !eof {
        let res = match tree.contents[cursor] {
            b'[' => parse_array(&mut tree, &mut cursor),
            b'{' => parse_object(&mut tree, &mut cursor),
            _ => Err(ParseError::UnexpectedToken(tree.contents[cursor] as char)),
        };

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
    let eof = parse_any_ignore_maybe(&mut tree, &mut cursor)?;
    if !eof {
        return Err(ParseError::UnexpectedToken(tree.contents[cursor] as char));
    }
    assert_tree_valid(&tree);
    return Ok(tree);
}

fn parse_any_ignore_maybe(tree: &mut JsonAst, cursor: &mut usize) -> Result<bool, ParseError> {
    tree.assert_lengths();
    let eof = parse_whitespace_maybe(tree, cursor);
    if eof {
        return Ok(eof);
    }
    parse_comment_maybe(tree, cursor)?;
    let eof = parse_whitespace_maybe(tree, cursor);
    Ok(eof)
}

fn parse_whitespace_maybe(tree: &mut JsonAst, cursor: &mut usize) -> bool {
    tree.assert_lengths();
    assert!(*cursor <= tree.contents.len());
    while *cursor < tree.contents.len() && tree.contents[*cursor].is_ascii_whitespace() {
        *cursor += 1;
    }
    assert!(*cursor <= tree.contents.len());
    assert!(*cursor == tree.contents.len() || !tree.contents[*cursor].is_ascii_whitespace());
    return *cursor == tree.contents.len();
}

fn parse_comment_maybe(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
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
                while *cursor < tree.contents.len() {
                    if tree.contents[*cursor] == b'\n' {
                        tree.comments.push(start..*cursor);
                        break;
                    }

                    *cursor += 1;
                }
                parse_whitespace_maybe(tree, cursor);
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
                            tree.comments.push(start..*cursor + 1);
                            found = true;
                            break;
                        }
                    }
                    *cursor += 1;
                }
                if !found {
                    return Err(ParseError::UnexpectedEndOfInput);
                }
                parse_whitespace_maybe(tree, cursor);
            }
            _ => {}
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
    tree.tok_span.push(*cursor..*cursor + 4);
    *cursor += 4;
    tree.tok_kind.push(Token::Null);
    tree.tok_desc.push(EMPTY_RANGE);
    tree.tok_meta.push(0);
    tree.tok_next.push(0);
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
    tree.tok_span.push(*cursor..*cursor + 4);
    *cursor += 4;
    tree.tok_kind.push(Token::Boolean);
    tree.tok_desc.push(EMPTY_RANGE);
    tree.tok_meta.push(0);
    tree.tok_next.push(0);
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
    tree.tok_span.push(*cursor..*cursor + 5);
    *cursor += 5;
    tree.tok_kind.push(Token::Boolean);
    tree.tok_desc.push(EMPTY_RANGE);
    tree.tok_meta.push(0);
    tree.tok_next.push(0);
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
        match tree.contents[*cursor] {
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
                tree.tok_span.push(range);
                tree.tok_kind.push(Token::String);
                tree.tok_desc.push(EMPTY_RANGE);
                tree.tok_meta.push(0);
                tree.tok_next.push(0);
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

    let mut meta = 0;
    if is_negative {
        meta |= NUM_NEGATIVE;
    }
    if is_float {
        meta |= NUM_FLOAT;
    }

    tree.tok_span.push(start..*cursor);
    tree.tok_kind.push(Token::Number);
    tree.tok_desc.push(EMPTY_RANGE);
    tree.tok_meta.push(meta);
    tree.tok_next.push(0);
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
    let obj_index = tree.next_index();
    tree.tok_span.push(*cursor..*cursor);
    let children_start = obj_index + 1;
    tree.tok_desc.push(EMPTY_RANGE);
    tree.tok_kind.push(Token::Object);
    tree.tok_meta.push(0);
    tree.tok_next.push(0);

    *cursor += 1;

    let mut key_index_prev = obj_index;
    let mut key_count = 0;

    loop {
        let eof = parse_any_ignore_maybe(tree, cursor)?;
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        if tree.contents[*cursor] == b'}' {
            *cursor += 1;
            break;
        }
        if tree.contents[*cursor] != b'"' {
            return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
        }
        let key_index = tree.next_index();
        parse_string(tree, cursor)?;
        tree.tok_next[key_index_prev] = key_index as u32;
        key_index_prev = key_index;
        key_count += 1;

        let eof = parse_any_ignore_maybe(tree, cursor)?;
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }

        if tree.contents[*cursor] != b':' {
            return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
        }
        *cursor += 1;

        let eof = parse_any_ignore_maybe(tree, cursor)?;
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }

        let value_index = tree.next_index();
        tree.tok_desc[key_index] = value_index..value_index + 1;
        parse_value(tree, cursor)?;

        let eof = parse_any_ignore_maybe(tree, cursor)?;
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }

        if tree.contents[*cursor] == b',' {
            *cursor += 1;
        } else if tree.contents[*cursor] != b'}' {
            return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
        }
    }
    // clear because we set without checking in loop
    tree.tok_next[obj_index] = 0;

    tree.tok_meta[obj_index] = key_count;

    if tree.next_index() > children_start {
        tree.tok_desc[obj_index] = children_start..tree.next_index();
    }
    tree.tok_span[obj_index].end = *cursor;
    return Ok(());
}

fn parse_array(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    assert_eq!(tree.contents[*cursor], b'[');
    let array_index = tree.tok_kind.len();
    tree.tok_kind.push(Token::Array);
    tree.tok_span.push(*cursor..*cursor);
    let children_start = array_index + 1;
    tree.tok_desc.push(EMPTY_RANGE);
    tree.tok_next.push(0);
    tree.tok_meta.push(0);

    *cursor += 1;

    let mut value_index_prev = array_index;
    let mut value_count = 0;

    loop {
        let eof = parse_any_ignore_maybe(tree, cursor)?;
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

        let eof = parse_any_ignore_maybe(tree, cursor)?;
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        if tree.contents[*cursor] == b',' {
            *cursor += 1;
        } else if tree.contents[*cursor] != b']' {
            return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
        }
    }
    // clear because we set without checking in loop
    tree.tok_next[array_index] = 0;

    tree.tok_meta[array_index] = value_count;
    if tree.next_index() > children_start {
        tree.tok_desc[array_index] = children_start..tree.next_index();
    }
    tree.tok_span[array_index].end = *cursor;
    return Ok(());
}

fn assert_number_valid(tree: &JsonAst, i: usize) {
    let range = &tree.tok_span[i];
    assert!(is_start_of_number(tree.contents[range.start]));
    assert_eq!(tree.tok_desc[i], EMPTY_RANGE);

    let is_negative_sign = tree.contents[range.start] == b'-';
    let is_negative_extra = tree.tok_meta[i] & NUM_NEGATIVE != 0;
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
    assert!(count(value, b'.') <= 1);
    assert!(count(value, b'e') <= 1);
    assert!(count(value, b'E') <= 1);
    assert!(count(value, b'-') <= 2);
    assert!(count(value, b'+') == 0);

    let is_float_scientific = u32::max(count(value, b'e'), count(value, b'E')) != 0;
    let is_float_frac = count(value, b'.') != 0;
    let is_float = is_float_scientific || is_float_frac;
    let is_float_extra = tree.tok_meta[i] & NUM_FLOAT != 0;
    assert!(is_float_extra == is_float);
}

fn assert_string_valid(tree: &JsonAst, i: usize) {
    let range = &tree.tok_span[i];
    assert!(range.len() >= 2);
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
    assert!(std::str::from_utf8(&tree.contents[range.start + 1..range.end - 1]).is_ok());
    assert_ne!(tree.contents[range.end - 1], b'\\');
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
    assert!(std::str::from_utf8(&tree.contents[range.clone()]).is_ok());

    let expected_count = tree.tok_meta[i];
    let mut found_count = 0;
    let mut key_index = tree.tok_desc[i].start;
    while key_index != 0 {
        assert_eq!(
            Token::String,
            tree.tok_kind[key_index],
            "key of object should be string",
        );
        assert_eq!(0, tree.tok_meta[key_index]);
        assert_eq!(1, tree.tok_desc[key_index].len());
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
    assert!(range.len() >= 2);
    assert_eq!(tree.contents[range.start], b'[');
    assert_eq!(tree.contents[range.end - 1], b']');
    assert!(std::str::from_utf8(&tree.contents[range.clone()]).is_ok());

    let expected_count = tree.tok_meta[i];
    let mut found_count = 0;
    let mut value_index = tree.tok_desc[i].start;
    while value_index != 0 {
        value_index = tree.tok_next[value_index] as usize;
        found_count += 1;
    }
    assert_eq!(
        expected_count, found_count,
        "array has correct number of values"
    );
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
            Token::Boolean => {}
            Token::Null => {}
        }
    }

    for i in 0..tree.tok_span.len() {
        for j in 0..tree.tok_span.len() {
            if i == j {
                continue;
            }
            assert_ne!(
                tree.tok_span[i], tree.tok_span[j],
                "tok range {i} and {j} are the same"
            );
        }
    }
}

pub struct ObjectItemIter<'a> {
    tree: &'a JsonAst,
    key_index: usize,
}

impl<'a> ObjectItemIter<'a> {
    pub fn new(tree: &'a JsonAst, obj_index: usize) -> Self {
        assert_eq!(tree.tok_kind[obj_index], Token::Object);
        let key_index = tree.tok_desc[obj_index].start;
        ObjectItemIter { tree, key_index }
    }
}

impl<'a> Iterator for ObjectItemIter<'a> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.key_index == 0 {
            return None;
        }

        let value_range = &self.tree.tok_desc[self.key_index];
        assert_eq!(value_range.len(), 1);
        let value_index = value_range.start;
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
        let next_index = tree.tok_desc[array_index].start;
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ReplaceTarget {
    Key,
    Value,
}

pub fn replace_index(
    tree: &mut JsonAst,
    target_index: usize,
    source_value: &serde_json::Value,
) -> bool {
    let target_token_type = tree.tok_kind[target_index];
    let source_token_type = match source_value {
        serde_json::Value::Bool(_) => Token::Boolean,
        serde_json::Value::Null => Token::Null,
        serde_json::Value::Number(_) => Token::Number,
        serde_json::Value::String(_) => Token::String,
        serde_json::Value::Object(_) => Token::Object,
        serde_json::Value::Array(_) => Token::Array,
    };

    let target_is_container =
        target_token_type == Token::Object || target_token_type == Token::Array;
    let source_is_container =
        source_token_type == Token::Object || source_token_type == Token::Array;
    let target_is_obj_key =
        target_token_type == Token::String && tree.tok_desc[target_index].len() > 0;

    // if replacing key, make sure replacement is string as well
    if target_is_obj_key && source_token_type != Token::String {
        return false;
    }

    let Ok(source_contents) = serde_json::to_string(source_value) else {
        // todo! error here?
        return false;
    };

    let target_replacement_range;
    let source_insertion_range;
    let target_content_range;

    if !source_is_container {
        target_replacement_range = tree.tok_desc[target_index].clone();

        source_insertion_range = target_index..target_index + 1;
        target_content_range = tree.tok_span[target_index].clone();

        tree.tok_meta.drain(target_replacement_range.clone());
        tree.tok_kind.drain(target_replacement_range.clone());
        tree.tok_next.drain(target_replacement_range.clone());
        tree.tok_span.drain(target_replacement_range.clone());
        tree.tok_desc.drain(target_replacement_range.clone());

        tree.tok_kind[target_index] = source_token_type;
        tree.tok_meta[target_index] = 0;
        if !target_is_obj_key {
            tree.tok_desc[target_index] = EMPTY_RANGE;
        }

        tree.tok_span[target_index].end = target_content_range.start + source_contents.len();
        tree.contents.splice(
            target_content_range.clone(),
            source_contents.as_bytes().into_iter().cloned(),
        );

        if let serde_json::Value::Number(n) = source_value {
            let mut meta = 0;
            if n.is_f64() {
                meta |= NUM_FLOAT;
                if n.as_f64().unwrap().is_sign_negative() {
                    meta |= NUM_NEGATIVE;
                }
            } else if n.is_i64() && !n.is_u64() {
                meta |= NUM_NEGATIVE;
                meta &= !NUM_FLOAT;
            }
            tree.tok_meta[target_index] = meta;
        }
    } else {
        let mut source_tree = parse(&source_contents).expect("sub_tree valid json");
        let offset_content = tree.tok_span[target_index].start;
        let offset_token = target_index;

        target_replacement_range = if target_is_container {
            target_index..usize::max(target_index + 1, tree.tok_desc[target_index].end)
        } else {
            target_index..target_index + 1
        };
        source_insertion_range = target_index..target_index + source_tree.next_index();
        target_content_range = tree.tok_span[target_index].clone();

        for tok_next in &mut source_tree.tok_next {
            if *tok_next != 0 {
                *tok_next += offset_token as u32;
            }
        }
        for range in &mut source_tree.tok_span {
            range.start += offset_content;
            range.end += offset_content;
        }
        for child_range in &mut source_tree.tok_desc {
            if child_range.start == 0 && child_range.end == 0 {
                continue;
            }
            child_range.start += offset_token;
            child_range.end += offset_token;
        }
        for comment in &mut source_tree.comments {
            comment.start += offset_content;
            comment.end += offset_content;
        }

        let tok_next_prev = tree.tok_next[target_index];

        tree.comments.append(&mut source_tree.comments);
        tree.comments.sort_by_key(|r| (r.start, r.end));
        tree.tok_desc
            .splice(target_replacement_range.clone(), source_tree.tok_desc);
        tree.tok_kind
            .splice(target_replacement_range.clone(), source_tree.tok_kind);
        tree.tok_span
            .splice(target_replacement_range.clone(), source_tree.tok_span);
        tree.tok_meta
            .splice(target_replacement_range.clone(), source_tree.tok_meta);
        tree.tok_next
            .splice(target_replacement_range.clone(), source_tree.tok_next);
        tree.contents
            .splice(target_content_range.clone(), source_tree.contents);

        if tok_next_prev != 0 {
            tree.tok_next[target_index] = tok_next_prev;
        }
    }

    // update tok_next and tok_desc
    if target_replacement_range.len() > 0 {
        let source_insertion_range = source_insertion_range.clone();
        let tok_diff = u32::abs_diff(
            source_insertion_range.end as u32,
            target_replacement_range.end as u32,
        );

        let tok_diff_positive;
        let tok_diff_negative;

        if source_insertion_range.end < target_replacement_range.end {
            tok_diff_negative = tok_diff;
            tok_diff_positive = 0;
        } else {
            tok_diff_negative = 0;
            tok_diff_positive = tok_diff;
        }

        for tok_next in &mut tree.tok_next[0..=source_insertion_range.start] {
            if *tok_next >= target_replacement_range.end as u32 {
                *tok_next += tok_diff_positive;
                *tok_next -= tok_diff_negative;
            }
        }
        for tok_next in &mut tree.tok_next[source_insertion_range.end..] {
            if *tok_next > target_replacement_range.end as u32 {
                *tok_next += tok_diff_positive;
                *tok_next -= tok_diff_negative;
            }
        }

        for tok_desc in &mut tree.tok_desc[0..source_insertion_range.start] {
            if tok_desc.start >= target_replacement_range.end {
                tok_desc.start += tok_diff_positive as usize;
                tok_desc.start -= tok_diff_negative as usize;
                tok_desc.end += tok_diff_positive as usize;
                tok_desc.end -= tok_diff_negative as usize;
            }
        }
        for tok_desc in &mut tree.tok_desc[source_insertion_range.end..] {
            if tok_desc.start >= target_replacement_range.end {
                tok_desc.start += tok_diff_positive as usize;
                tok_desc.start -= tok_diff_negative as usize;
                tok_desc.end += tok_diff_positive as usize;
                tok_desc.end -= tok_diff_negative as usize;
            }
        }
    }

    // update tok_span
    {
        let end_diff = usize::abs_diff(
            target_content_range.start + source_contents.len(),
            target_content_range.end,
        );

        let end_diff_positive;
        let end_diff_negative;
        if target_content_range.end > target_content_range.start + source_contents.len() {
            end_diff_positive = 0;
            end_diff_negative = end_diff;
        } else {
            end_diff_positive = end_diff;
            end_diff_negative = 0;
        }

        for range in &mut (&mut tree.tok_span)[0..source_insertion_range.start] {
            if range.end > target_content_range.end {
                range.end -= end_diff_negative;
                range.end += end_diff_positive;
            }
        }
        for range in &mut (&mut tree.tok_span)[source_insertion_range.end..] {
            range.start -= end_diff_negative;
            range.start += end_diff_positive;
            range.end -= end_diff_negative;
            range.end += end_diff_positive;
        }
    };

    return true;
}

pub fn replace_path(
    tree: &mut JsonAst,
    path: &Path,
    source_value: &serde_json::Value,
    target: ReplaceTarget,
) -> bool {
    if target == ReplaceTarget::Key && !source_value.is_string() {
        return false;
    }
    let Some(target_index) = index_for_path(tree, path, target) else {
        return false;
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

enum InsertionDirection {
    Before,
    After,
}

pub enum InsertionValue<'a> {
    Arr(serde_json::Value),
    Obj((&'a str, serde_json::Value)),
}

/// `target_index` should be index of item in container (element in array or key in object) based on `method`
/// See documentation of ![`InsertionMethod`] for details
pub fn insert_index(
    tree: &mut JsonAst,
    source_value: InsertionValue<'_>,
    method: InsertionMethod,
    target_index: usize,
) -> bool {
    if matches!(source_value, InsertionValue::Obj(_)) {
        todo!();
    }

    let is_method_relative_to_item =
        matches!(method, InsertionMethod::After | InsertionMethod::Before);
    let is_target_container = matches!(tree.tok_kind[target_index], Token::Array | Token::Object);

    if is_target_container == is_method_relative_to_item {
        // if target is container, and method is relative to item,
        // or vice versa, the api has been broken
        return false;
    }

    if is_method_relative_to_item && tree.tok_next[target_index] == 0 {
        // if method relative to item, and target is not item, api has been broken
        return false;
    }

    let is_target_obj_key =
        tree.tok_kind[target_index] == Token::String || tree.tok_desc[target_index].len() == 0;
    if is_method_relative_to_item
        && matches!(source_value, InsertionValue::Obj(_))
        && !is_target_obj_key
    {
        // cannot insert key, value pair into object
        return false;
    }

    if !is_method_relative_to_item {
        todo!()
    }

    let target_replacement_range;
    let source_insertion_range;
    let target_content_range;

    let source_contents = match source_value {
        InsertionValue::Arr(value) => {
            let Ok(mut source_contents) = serde_json::to_string(&value) else {
                // todo! error
                return false;
            };
            source_contents.push_str(", ");
            source_contents
        }
        InsertionValue::Obj(_) => todo!(),
    };

    match method {
        InsertionMethod::After => {
            let prev_index = 'prev: {
                for (index, &tok_next) in tree.tok_next[0..target_index].iter().enumerate().rev() {
                    if tok_next as usize == target_index {
                        break 'prev index;
                    }
                }
                todo!()
            };

            let mut source_tree = parse(&source_contents).expect("sub_tree valid json");
            let offset_content = tree.tok_span[target_index].start;
            let offset_token = target_index;

            target_replacement_range =
                target_index..usize::max(target_index + 1, tree.tok_desc[target_index].end);
            source_insertion_range = target_index..target_index + source_tree.next_index();
            target_content_range = tree.tok_span[target_index].clone();

            for tok_next in &mut source_tree.tok_next {
                if *tok_next != 0 {
                    *tok_next += offset_token as u32;
                }
            }
            for range in &mut source_tree.tok_span {
                range.start += offset_content;
                range.end += offset_content;
            }
            for child_range in &mut source_tree.tok_desc {
                if child_range.start == 0 && child_range.end == 0 {
                    continue;
                }
                child_range.start += offset_token;
                child_range.end += offset_token;
            }
            for comment in &mut source_tree.comments {
                comment.start += offset_content;
                comment.end += offset_content;
            }

            let tok_next_prev = tree.tok_next[target_index];

            tree.comments.append(&mut source_tree.comments);
            tree.comments.sort_by_key(|r| (r.start, r.end));
            tree.tok_desc
                .splice(target_replacement_range.clone(), source_tree.tok_desc);
            tree.tok_kind
                .splice(target_replacement_range.clone(), source_tree.tok_kind);
            tree.tok_span
                .splice(target_replacement_range.clone(), source_tree.tok_span);
            tree.tok_meta
                .splice(target_replacement_range.clone(), source_tree.tok_meta);
            tree.tok_next
                .splice(target_replacement_range.clone(), source_tree.tok_next);
            tree.contents
                .splice(target_content_range.clone(), source_tree.contents);

            if tok_next_prev != 0 {
                tree.tok_next[target_index] = tok_next_prev;
            }
        }
        InsertionMethod::Before => todo!(),
        InsertionMethod::Append => todo!(),
        InsertionMethod::Prepend => todo!(),
    };

    // todo! extract following into function

    // update tok_next and tok_desc
    let source_insertion_range = source_insertion_range.clone();
    let tok_diff = u32::abs_diff(
        source_insertion_range.end as u32,
        target_replacement_range.end as u32,
    );

    let tok_diff_positive;
    let tok_diff_negative;

    if source_insertion_range.end < target_replacement_range.end {
        tok_diff_negative = tok_diff;
        tok_diff_positive = 0;
    } else {
        tok_diff_negative = 0;
        tok_diff_positive = tok_diff;
    }

    for tok_next in &mut tree.tok_next[0..=source_insertion_range.start] {
        if *tok_next >= target_replacement_range.end as u32 {
            *tok_next += tok_diff_positive;
            *tok_next -= tok_diff_negative;
        }
    }
    for tok_next in &mut tree.tok_next[source_insertion_range.end..] {
        if *tok_next > target_replacement_range.end as u32 {
            *tok_next += tok_diff_positive;
            *tok_next -= tok_diff_negative;
        }
    }

    for tok_desc in &mut tree.tok_desc[0..source_insertion_range.start] {
        if tok_desc.start >= target_replacement_range.end {
            tok_desc.start += tok_diff_positive as usize;
            tok_desc.start -= tok_diff_negative as usize;
            tok_desc.end += tok_diff_positive as usize;
            tok_desc.end -= tok_diff_negative as usize;
        }
    }
    for tok_desc in &mut tree.tok_desc[source_insertion_range.end..] {
        if tok_desc.start >= target_replacement_range.end {
            tok_desc.start += tok_diff_positive as usize;
            tok_desc.start -= tok_diff_negative as usize;
            tok_desc.end += tok_diff_positive as usize;
            tok_desc.end -= tok_diff_negative as usize;
        }
    }

    // update tok_span
    {
        let end_diff = usize::abs_diff(
            target_content_range.start + source_contents.len(),
            target_content_range.end,
        );

        let end_diff_positive;
        let end_diff_negative;
        if target_content_range.end > target_content_range.start + source_contents.len() {
            end_diff_positive = 0;
            end_diff_negative = end_diff;
        } else {
            end_diff_positive = end_diff;
            end_diff_negative = 0;
        }

        for range in &mut (&mut tree.tok_span)[0..source_insertion_range.start] {
            if range.end > target_content_range.end {
                range.end -= end_diff_negative;
                range.end += end_diff_positive;
            }
        }
        for range in &mut (&mut tree.tok_span)[source_insertion_range.end..] {
            range.start -= end_diff_negative;
            range.start += end_diff_positive;
            range.end -= end_diff_negative;
            range.end += end_diff_positive;
        }
    };

    true
}

#[cfg(test)]
mod test {
    use super::*;
    use std::ops::Range;

    fn extract_delimited(target: &str) -> (String, Range<usize>) {
        let span_begin = target.find('<').expect("span start defined");
        let span_terminate = target.find('>').expect("span end defined");
        let mut target_str = String::with_capacity(target.len());
        target_str.push_str(&target[..span_begin]);
        target_str.push_str(&target[span_begin + 1..span_terminate]);
        target_str.push_str(&target[span_terminate + 1..]);

        return (target_str, span_begin..span_terminate - 1);
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
            // todo! llm generate more tests
        }

        #[test]
        fn array() {
            check(r#"<[]>"#);
            check(r#"<[$1$]>"#);
            check(r#"<[${}$, ${}$,     ${}$]>"#);
            // todo! llm generate more tests
        }
    }

    mod replace {
        use super::*;
        use serde_json::json;

        fn check(target: &str, source: serde_json::Value, expected: &str) {
            let (target, item_range) = extract_delimited(target);

            let mut tree = parse(&target).expect("parse succeeded");
            assert_tree_valid(&tree);

            let index = tree
                .tok_span
                .iter()
                .position(|range| range == &item_range)
                .expect("index found");

            assert!(replace_index(&mut tree, index, &source), "replace failed");
            assert_tree_valid(&tree);

            let new_contents =
                std::str::from_utf8(&tree.contents).expect("tree contents is valid utf8");

            assert_eq!(new_contents, expected);
        }

        #[test]
        fn obj_value() {
            check(
                r#"{ "key": <"value"> }"#,
                json!("new_value"),
                r#"{ "key": "new_value" }"#,
            );
            check(
                r#"{ "key": <"value"> }"#,
                json!(3.1459),
                r#"{ "key": 3.1459 }"#,
            );
            check(r#"{ "key": <"value"> }"#, json!(-0.0), r#"{ "key": -0.0 }"#);
            check(
                r#"{ "key": <"value"> }"#,
                json!([true]),
                r#"{ "key": [true] }"#,
            );
            check(
                r#"{ "key": <"value">, "key2": "value2" }"#,
                json!({"sub_key": "sub_value"}),
                r#"{ "key": {"sub_key":"sub_value"}, "key2": "value2" }"#,
            );
        }

        #[test]
        fn obj_replace_root() {
            check(
                r#"<{ "key": "value", "key2": "value2" }>"#,
                json!(true),
                "true",
            );
        }

        #[test]
        fn arr_value() {
            check(
                r#"[1, <{"key": "value"}>, 3, 4, 5]"#,
                json!([2]),
                r#"[1, [2], 3, 4, 5]"#,
            );
            check(
                r#"[1, <{"key": "value"}>, 3, 4, 5]"#,
                json!(2),
                r#"[1, 2, 3, 4, 5]"#,
            );
            check(
                r#"[<null>,{"": null},null]"#,
                json!([{}]),
                r#"[[{}],{"": null},null]"#,
            );
        }
    }
}
