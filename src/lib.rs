pub use serde_json;
use std::{fmt::Display, ops::Range};

const NUM_NEGATIVE: u32 = 1 << 0;
const NUM_FLOAT: u32 = 1 << 1;
const EMPTY_RANGE: Range<usize> = 0..0;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Array,
    Object,
    String,
    Number,
    Boolean,
    Null,
}

#[derive(Debug)]
pub struct JsonAst {
    pub contents: Vec<u8>,
    // todo: rename to tok_range
    pub tok_range: Vec<Range<usize>>,
    // todo: rename to tok_type
    pub tok_type: Vec<TokenType>,
    pub tok_children: Vec<Range<usize>>,
    // todo: rename to tok_meta
    pub tok_meta: Vec<u32>,
    pub tok_next: Vec<u32>,
    pub comments: Vec<Range<usize>>,
}

impl JsonAst {
    pub fn value_at(&self, index: usize) -> &str {
        let mut range = self.tok_range[index].clone();
        if self.tok_type[index] == TokenType::String {
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
        return self.tok_type.len();
    }

    fn assert_lengths(&self) {
        assert_eq!(self.tok_range.len(), self.tok_type.len());
        assert_eq!(self.tok_range.len(), self.tok_children.len());
        assert_eq!(self.tok_range.len(), self.tok_meta.len());
        assert_eq!(self.tok_range.len(), self.tok_next.len());
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
        tok_range: Vec::new(),
        tok_type: Vec::new(),
        tok_children: Vec::new(),
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
    tree.tok_range.push(*cursor..*cursor + 4);
    *cursor += 4;
    tree.tok_type.push(TokenType::Null);
    tree.tok_children.push(EMPTY_RANGE);
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
    tree.tok_range.push(*cursor..*cursor + 4);
    *cursor += 4;
    tree.tok_type.push(TokenType::Boolean);
    tree.tok_children.push(EMPTY_RANGE);
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
    tree.tok_range.push(*cursor..*cursor + 5);
    *cursor += 5;
    tree.tok_type.push(TokenType::Boolean);
    tree.tok_children.push(EMPTY_RANGE);
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
                tree.tok_range.push(range);
                tree.tok_type.push(TokenType::String);
                tree.tok_children.push(EMPTY_RANGE);
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
        !is_float && *cursor - value_start >= 1 && tree.contents[value_start] == b'0';
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

    tree.tok_range.push(start..*cursor);
    tree.tok_type.push(TokenType::Number);
    tree.tok_children.push(EMPTY_RANGE);
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
    tree.tok_range.push(*cursor..*cursor);
    let children_start = obj_index + 1;
    tree.tok_children.push(EMPTY_RANGE);
    tree.tok_type.push(TokenType::Object);
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
        tree.tok_children[key_index] = value_index..value_index + 1;
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
        tree.tok_children[obj_index] = children_start..tree.next_index();
    }
    tree.tok_range[obj_index].end = *cursor;
    return Ok(());
}

fn parse_array(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    assert_eq!(tree.contents[*cursor], b'[');
    let array_index = tree.tok_type.len();
    tree.tok_type.push(TokenType::Array);
    tree.tok_range.push(*cursor..*cursor);
    let children_start = array_index + 1;
    tree.tok_children.push(EMPTY_RANGE);
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
        tree.tok_children[array_index] = children_start..tree.next_index();
    }
    tree.tok_range[array_index].end = *cursor;
    return Ok(());
}

fn assert_number_valid(tree: &JsonAst, i: usize) {
    let range = &tree.tok_range[i];
    assert!(is_start_of_number(tree.contents[range.start]));
    assert_eq!(tree.tok_children[i], EMPTY_RANGE);

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
    let range = &tree.tok_range[i];
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
    let range = &tree.tok_range[i];
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
    let mut key_index = tree.tok_children[i].start;
    while key_index != 0 {
        assert_eq!(tree.tok_type[key_index], TokenType::String);
        assert_eq!(tree.tok_meta[key_index], 0);
        assert_eq!(tree.tok_children[key_index].len(), 1);
        key_index = tree.tok_next[key_index] as usize;
        found_count += 1;
    }
    assert_eq!(
        expected_count, found_count,
        "object has correct number of keys"
    );
}

fn assert_array_valid(tree: &JsonAst, i: usize) {
    let range = &tree.tok_range[i];
    assert!(range.len() >= 2);
    assert_eq!(tree.contents[range.start], b'[');
    assert_eq!(tree.contents[range.end - 1], b']');
    assert!(std::str::from_utf8(&tree.contents[range.clone()]).is_ok());

    let expected_count = tree.tok_meta[i];
    let mut found_count = 0;
    let mut value_index = tree.tok_children[i].start;
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

    // strings
    for (i, &tok_type) in tree.tok_type.iter().enumerate() {
        match tok_type {
            TokenType::String => assert_string_valid(tree, i),
            TokenType::Number => assert_number_valid(tree, i),
            TokenType::Object => assert_object_valid(tree, i),
            TokenType::Array => assert_array_valid(tree, i),
            TokenType::Boolean => {}
            TokenType::Null => {}
        }
    }

    for i in 0..tree.tok_range.len() {
        for j in 0..tree.tok_range.len() {
            if i == j {
                continue;
            }
            assert_ne!(
                tree.tok_range[i], tree.tok_range[j],
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
        assert_eq!(tree.tok_type[obj_index], TokenType::Object);
        let key_index = tree.tok_children[obj_index].start;
        ObjectItemIter { tree, key_index }
    }
}

impl<'a> Iterator for ObjectItemIter<'a> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.key_index == 0 {
            return None;
        }

        let value_range = &self.tree.tok_children[self.key_index];
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
        assert_eq!(tree.tok_type[array_index], TokenType::Array);
        let next_index = tree.tok_children[array_index].start;
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
pub enum UpdateTarget {
    Key,
    Value,
}

pub fn update(
    tree: &mut JsonAst,
    path: &Path,
    value: &serde_json::Value,
    target: UpdateTarget,
) -> bool {
    // TODO: separate this into two functions, update_path and update_index
    //       if combined with storing target in path, this removes the need for target entirely
    let Some(index) = index_for_path(tree, path, target) else {
        return false;
    };
    if matches!(
        value,
        serde_json::Value::Array(_) | serde_json::Value::Object(_)
    ) {
        todo!("objects and array updates")
    }

    let Ok(str) = serde_json::to_string(value) else {
        // todo! error here?
        return false;
    };

    let mut num_is_neg = false;
    let mut num_is_float = false;

    let tok_type_prev = tree.tok_type[index];
    let tok_type_new = match value {
        serde_json::Value::Bool(_) => TokenType::Boolean,
        serde_json::Value::Null => TokenType::Null,
        serde_json::Value::Number(n) => {
            if !n.is_u64() && !n.is_i64() {
                num_is_float = true;
                num_is_neg = n.as_f64().unwrap() < 0.0;
            } else if n.is_i64() && !n.is_u64() {
                num_is_neg = true;
                num_is_float = false;
            } else if n.is_u64() {
                num_is_neg = false;
                num_is_float = false;
            }
            TokenType::Number
        }
        serde_json::Value::String(_) => TokenType::String,
        serde_json::Value::Object(_) => todo!(stringify!(TokenType::Object)),
        serde_json::Value::Array(_) => todo!(stringify!(TokenType::Array)),
    };
    tree.tok_type[index] = tok_type_new;
    // update extra
    {
        // clear extra
        match tok_type_prev {
            TokenType::Array => todo!(),
            TokenType::Object => todo!(),
            TokenType::Number => tree.tok_meta[index] = 0,
            // no extra
            TokenType::Null | TokenType::String | TokenType::Boolean => {}
        }

        // update extra
        match tok_type_new {
            TokenType::Number => {
                if num_is_float {
                    tree.tok_meta[index] |= NUM_FLOAT;
                }
                if num_is_neg {
                    tree.tok_meta[index] |= NUM_NEGATIVE;
                }
            }
            TokenType::Object => todo!(),
            TokenType::Array => todo!(),
            // no extra
            TokenType::Null | TokenType::Boolean | TokenType::String => {}
        }
    }

    let range = tree.tok_range[index].clone();
    let start_idx = range.start;
    let end_idx = range.end;
    let end_idx_new = start_idx + str.len();
    let neg = end_idx > end_idx_new;
    let end_diff = usize::abs_diff(end_idx_new, end_idx);
    tree.tok_range[index].end = end_idx_new;
    eprintln!(
        "replacing `{}` with `{}`",
        std::str::from_utf8(&tree.contents[range.clone()]).unwrap(),
        &str,
    );
    tree.contents
        .splice(range, str.as_bytes().into_iter().cloned());
    if neg {
        // PERF: only update parent containers
        for i in 0..index {
            let range = &mut tree.tok_range[i];
            if range.end > end_idx {
                range.end -= end_diff;
            }
        }
        for i in index + 1..tree.tok_range.len() {
            let range = &mut tree.tok_range[i];
            range.start -= end_diff;
            range.end -= end_diff;
        }
    } else {
        for i in 0..index {
            let range = &mut tree.tok_range[i];
            if range.end > end_idx {
                range.end += end_diff;
            }
        }
        for i in index + 1..tree.tok_range.len() {
            let range = &mut tree.tok_range[i];
            range.start += end_diff;
            range.end += end_diff;
        }
    }

    return true;
}

fn index_for_path(tree: &JsonAst, path: &Path, target: UpdateTarget) -> Option<usize> {
    let mut index = 0;
    let mut value_index = 0;

    let path = &path.0;
    if path.is_empty() {
        return Some(0);
    }
    'segments: for segment in path {
        match (segment, tree.tok_type[index]) {
            (&PathEntry::Idx(idx), TokenType::Array) => {
                value_index = 0;
                let mut iter = ArrayItemIter::new(&tree, index);
                for _ in 0..idx {
                    iter.next()?;
                }
                index = iter.next()?;
            }
            (PathEntry::Str(key), TokenType::Object) => {
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

    if target == UpdateTarget::Value
        && tree.tok_type[index] == TokenType::String
        && value_index != 0
    {
        index = value_index;
    }
    Some(index)
}

#[cfg(test)]
mod iter_tests {
    use super::*;

    #[test]
    fn iter_object_key() {
        let json = r#"{ "key": "value" }"#;
        let tree = parse(json).unwrap();
        let mut iter = ObjectItemIter::new(&tree, 0);
        let (key, value) = iter.next().expect("iter valid");
        assert_ne!(key, 0);
        assert_ne!(value, 0);
        assert!(iter.next().is_none());
        assert_eq!(tree.value_at(key), "key");
        assert_eq!(tree.value_at(value), "value");
    }

    #[test]
    fn iter_object_keys() {
        let json = r#"{ "key": "value", "key2": "value2" }"#;
        let tree = parse(json).unwrap();
        let mut iter = ObjectItemIter::new(&tree, 0);
        let (key, value) = iter.next().expect("iter has one item");
        assert_ne!(key, 0);
        assert_ne!(value, 0);
        let (key, value) = iter.next().expect("iter has two items");
        assert_ne!(key, 0);
        assert_ne!(value, 0);
        assert_eq!(tree.value_at(key), "key2");
        assert_eq!(tree.value_at(value), "value2");
        assert!(iter.next().is_none(), "iter only has two items");
    }
}

#[cfg(test)]
mod update_tests {
    use super::*;

    #[test]
    fn obj_string_value_to_string() {
        let json = r#"{ "key": "value" }"#;
        let mut tree = parse(json).unwrap();
        assert!(update(
            &mut tree,
            &Path::from_str("key"),
            &serde_json::Value::String("new_value".to_string()),
            UpdateTarget::Value,
        ));

        assert_tree_valid(&tree);
        let new_contents = std::str::from_utf8(&tree.contents).unwrap();
        let new_contents_expected = r#"{ "key": "new_value" }"#;
        assert_eq!(new_contents, new_contents_expected);
    }

    #[test]
    fn obj_string_value_to_float() {
        let json = r#"{ "key": "value" }"#;
        let mut tree = parse(json).unwrap();
        assert!(update(
            &mut tree,
            &Path::from_str("key"),
            &serde_json::Value::from(3.1459),
            UpdateTarget::Value,
        ));
        assert_tree_valid(&tree);
        let new_contents = std::str::from_utf8(&tree.contents).unwrap();
        let new_contents_expected = r#"{ "key": 3.1459 }"#;
        assert_eq!(new_contents, new_contents_expected);
    }

    #[test]
    fn obj_string_value_to_negative_float() {
        let json = r#"{ "key": "value" }"#;
        let mut tree = parse(json).unwrap();
        assert!(update(
            &mut tree,
            &Path::from_str("key"),
            &serde_json::Value::from(-3.1459),
            UpdateTarget::Value,
        ));
        assert_tree_valid(&tree);
        let new_contents = std::str::from_utf8(&tree.contents).unwrap();
        let new_contents_expected = r#"{ "key": -3.1459 }"#;
        assert_eq!(new_contents, new_contents_expected);
    }
}
