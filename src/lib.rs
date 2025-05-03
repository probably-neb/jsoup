use std::{fmt::Display, ops::Range};

const NUM_NEGATIVE: u32 = 1 << 0;
const NUM_FLOAT: u32 = 1 << 1;

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
    pub tok_ranges: Vec<Range<usize>>,
    pub tok_types: Vec<TokenType>,
    pub tok_children: Vec<Range<usize>>,
    pub tok_extra: Vec<u32>,
    pub extra: Vec<u32>,
    pub comments: Vec<Range<usize>>,
}

impl JsonAst {
    pub fn value_at(&self, index: usize) -> &str {
        let range = &self.tok_ranges[index];
        unsafe { std::str::from_utf8_unchecked(&self.contents[range.clone()]) }
    }

    pub fn value_for_char_range(&self, range: &Range<usize>) -> &str {
        return str_range_adjusted(&self.contents, range.clone());
    }

    pub fn next_index(&self) -> usize {
        self.assert_lengths();
        return self.tok_types.len();
    }

    fn assert_lengths(&self) {
        assert_eq!(self.tok_ranges.len(), self.tok_types.len());
        assert_eq!(self.tok_ranges.len(), self.tok_children.len());
        assert_eq!(self.tok_ranges.len(), self.tok_extra.len());
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

const EMPTY_RANGE: Range<usize> = 0..0;

pub fn parse(input: &str) -> Result<JsonAst, ParseError> {
    let contents = input.as_bytes().to_vec();
    let mut tree = JsonAst {
        contents,
        tok_ranges: Vec::new(),
        tok_types: Vec::new(),
        tok_children: Vec::new(),
        tok_extra: Vec::new(),
        extra: Vec::new(),
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
            let context_start = cursor.saturating_sub(3);
            let context_end = usize::min(cursor + 4, tree.contents.len());
            eprintln!(
                "Error at position {}: `{}`",
                context_start,
                tree.value_for_char_range(&(context_start..context_end)),
            );
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
                    return Ok(());
                }

                *cursor += 1;
            }
            return Ok(());
        }
        b'*' => {
            assert_eq!(&tree.contents[*cursor..*cursor + 2], [b'/', b'*']);
            *cursor += 2;
            while *cursor < tree.contents.len() {
                if tree.contents[*cursor] == b'*' {
                    if *cursor + 1 >= tree.contents.len() {
                        break;
                    }
                    if tree.contents[*cursor + 1] == b'/' {
                        assert_eq!(&tree.contents[*cursor..*cursor + 2], [b'*', b'/']);
                        *cursor += 2;
                        tree.comments.push(start..*cursor + 1);
                        return Ok(());
                    }
                }
                *cursor += 1;
            }
        }
        _ => {}
    }
    return Err(ParseError::UnexpectedEndOfInput);
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
    tree.tok_ranges.push(*cursor..*cursor + 4);
    *cursor += 4;
    tree.tok_types.push(TokenType::Null);
    tree.tok_children.push(EMPTY_RANGE);
    tree.tok_extra.push(0);
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
    tree.tok_ranges.push(*cursor..*cursor + 4);
    *cursor += 4;
    tree.tok_types.push(TokenType::Boolean);
    tree.tok_children.push(EMPTY_RANGE);
    tree.tok_extra.push(0);
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
    tree.tok_ranges.push(*cursor..*cursor + 5);
    *cursor += 5;
    tree.tok_types.push(TokenType::Boolean);
    tree.tok_children.push(EMPTY_RANGE);
    tree.tok_extra.push(0);
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
                tree.tok_ranges.push(range);
                tree.tok_types.push(TokenType::String);
                tree.tok_children.push(EMPTY_RANGE);
                tree.tok_extra.push(0);
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

    let mut extra = 0;
    if is_negative {
        extra |= NUM_NEGATIVE;
    }
    if is_float {
        extra |= NUM_FLOAT;
    }

    tree.tok_ranges.push(start..*cursor);
    tree.tok_types.push(TokenType::Number);
    tree.tok_children.push(EMPTY_RANGE);
    tree.tok_extra.push(extra);
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
    let obj_start = tree.next_index();
    tree.tok_ranges.push(*cursor..*cursor);
    let children_start = obj_start + 1;
    tree.tok_children.push(children_start..children_start);
    tree.tok_types.push(TokenType::Object);
    tree.tok_extra.push(0);

    *cursor += 1;

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
        parse_string(tree, cursor)?;
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

        parse_value(tree, cursor)?;

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

    tree.tok_children[obj_start].end = tree.tok_types.len();
    tree.tok_ranges[obj_start].end = *cursor;
    return Ok(());
}

fn parse_array(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    assert_eq!(tree.contents[*cursor], b'[');
    *cursor += 1;
    let array_start = tree.tok_types.len();
    tree.tok_types.push(TokenType::Array);
    tree.tok_ranges.push(*cursor..*cursor);
    let children_start = array_start + 1;
    tree.tok_children.push(children_start..children_start);
    tree.tok_extra.push(0);

    loop {
        let eof = parse_any_ignore_maybe(tree, cursor)?;
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        if tree.contents[*cursor] == b']' {
            *cursor += 1;
            break;
        }
        parse_value(tree, cursor)?;
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

    tree.tok_children[array_start].end = tree.tok_types.len();
    tree.tok_ranges[array_start].end = *cursor;
    return Ok(());
}

fn assert_number_valid(tree: &JsonAst, i: usize) {
    let range = &tree.tok_ranges[i];
    assert!(is_start_of_number(tree.contents[range.start]));
    assert_eq!(tree.tok_children[i], EMPTY_RANGE);

    let is_negative_sign = tree.contents[range.start] == b'-';
    let is_negative_extra = tree.tok_extra[i] & NUM_NEGATIVE != 0;
    assert_eq!(is_negative_sign, is_negative_extra);

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
    let is_float_extra = tree.tok_extra[i] & NUM_FLOAT != 0;
    assert!(is_float_extra == is_float);
}

fn assert_string_valid(tree: &JsonAst, i: usize) {
    let range = &tree.tok_ranges[i];
    assert!(range.len() >= 2);
    assert_eq!(tree.contents[range.start], b'"');
    assert_eq!(tree.contents[range.end - 1], b'"');
    assert_eq!(tree.tok_extra[i], 0);
    assert_eq!(tree.tok_children[i], EMPTY_RANGE);
    assert!(std::str::from_utf8(&tree.contents[range.start + 1..range.end - 1]).is_ok());
}

fn assert_tree_valid(tree: &JsonAst) {
    tree.assert_lengths();

    // strings
    for (i, &tok_type) in tree.tok_types.iter().enumerate() {
        match tok_type {
            TokenType::String => assert_string_valid(tree, i),
            TokenType::Number => assert_number_valid(tree, i),
            _ => {}
        }
    }
}
