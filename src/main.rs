use std::{fmt::Display, ops::Range};

const SETTINGS_JSON: &'static str = include_str!("./settings.jsonc");

const JSON_SMOL: &'static str = r#"
{
    "name": "John Doe",
    "age": 30,
    "pets": [
       "dog",
       "cat",
    ],
}
"#;

fn main() -> Result<(), ParseError> {
    let tree = parse(JSON_SMOL)?;
    for (index, ((kind, range), children)) in tree
        .tok_types
        .iter()
        .zip(tree.tok_ranges.iter())
        .zip(tree.tok_children.iter())
        .enumerate()
    {
        println!(
            "Index: {}, Kind: {:?}, Range: {:?}, Children: {:?} Value: `{}`",
            index,
            kind,
            range.clone(),
            children,
            tree.value_at(index),
        );
    }
    Ok(())
}

#[derive(Debug)]
enum TokenType {
    Array,
    Object,
    String,
    Number,
    Boolean,
    Null,
}

#[derive(Debug)]
struct JsonAst {
    contents: Vec<u8>,
    tok_ranges: Vec<Range<usize>>,
    tok_types: Vec<TokenType>,
    tok_children: Vec<Range<usize>>,
}

impl JsonAst {
    fn value_at(&self, index: usize) -> &str {
        let range = &self.tok_ranges[index];
        unsafe { std::str::from_utf8_unchecked(&self.contents[range.clone()]) }
    }
    fn value_for_char_range(&self, range: &Range<usize>) -> &str {
        let contents_str = unsafe { std::str::from_utf8_unchecked(&self.contents) };
        return &contents_str[range.clone()];
    }
}

#[derive(Debug)]
enum ParseError {
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

fn parse(input: &str) -> Result<JsonAst, ParseError> {
    let contents = input.as_bytes().to_vec();
    let mut tree = JsonAst {
        contents,
        tok_ranges: Vec::new(),
        tok_types: Vec::new(),
        tok_children: Vec::new(),
    };

    let mut cursor = 0;

    let eof = skip_whitespace(&mut tree, &mut cursor);
    if !eof {
        let res = match tree.contents[cursor] {
            b'[' => parse_array(&mut tree, &mut cursor),
            b'{' => parse_object(&mut tree, &mut cursor),
            _ => Err(ParseError::UnexpectedToken(tree.contents[cursor] as char)),
        };
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
    let eof = skip_whitespace(&mut tree, &mut cursor);
    assert!(eof);
    return Ok(tree);
}

fn skip_whitespace(tree: &mut JsonAst, cursor: &mut usize) -> bool {
    while *cursor < tree.contents.len() && tree.contents[*cursor].is_ascii_whitespace() {
        *cursor += 1;
    }
    return *cursor >= tree.contents.len();
}

fn parse_null(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
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
    Ok(())
}

fn parse_true(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
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
    Ok(())
}

fn parse_false(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
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
    Ok(())
}

fn parse_string(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
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
                tree.tok_ranges.push(start..*cursor + 1);
                *cursor += 1;
                tree.tok_types.push(TokenType::String);
                tree.tok_children.push(EMPTY_RANGE);
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
    let start = *cursor;
    *cursor += 1;
    while *cursor < tree.contents.len() {
        match tree.contents[*cursor] {
            // FIXME: floats
            b'0'..=b'9' => *cursor += 1,
            _ => break,
        }
    }
    if *cursor == start + 1 && tree.contents[start] == b'-' {
        return Err(ParseError::InvalidNumber);
    }
    tree.tok_ranges.push(start..*cursor);
    tree.tok_types.push(TokenType::Number);
    tree.tok_children.push(EMPTY_RANGE);
    Ok(())
}

fn is_start_of_number(c: u8) -> bool {
    c.is_ascii_digit() || c == b'-'
}

fn parse_value(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    match tree.contents[*cursor] {
        b'"' => parse_string(tree, cursor),
        b'n' => parse_null(tree, cursor),
        b't' => parse_true(tree, cursor),
        b'f' => parse_false(tree, cursor),
        b'{' => parse_object(tree, cursor),
        b'[' => parse_array(tree, cursor),
        _ if is_start_of_number(tree.contents[*cursor]) => parse_number(tree, cursor),
        _ => Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char)),
    }
}

fn parse_object(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    assert_eq!(tree.contents[*cursor], b'{');
    if *cursor + 1 > tree.contents.len() {
        return Err(ParseError::UnexpectedEndOfInput);
    }
    let obj_start = tree.tok_types.len();
    tree.tok_ranges.push(*cursor..*cursor);
    let children_start = obj_start + 1;
    tree.tok_children.push(children_start..children_start);
    tree.tok_types.push(TokenType::Object);

    *cursor += 1;

    loop {
        let eof = skip_whitespace(tree, cursor);
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
        let eof = skip_whitespace(tree, cursor);
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }

        if tree.contents[*cursor] != b':' {
            return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
        }
        *cursor += 1;
        let eof = skip_whitespace(tree, cursor);
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }

        parse_value(tree, cursor)?;

        let eof = skip_whitespace(tree, cursor);
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }

        if tree.contents[*cursor] == b',' {
            *cursor += 1;
            let eof = skip_whitespace(tree, cursor);
            if eof {
                return Err(ParseError::UnexpectedEndOfInput);
            }
        } else if tree.contents[*cursor] != b']' {
            return Err(ParseError::UnexpectedToken(tree.contents[*cursor] as char));
        }
    }

    tree.tok_children[obj_start].end = tree.tok_types.len();
    tree.tok_ranges[obj_start].end = *cursor;
    return Ok(());
}

fn parse_array(tree: &mut JsonAst, cursor: &mut usize) -> Result<(), ParseError> {
    assert_eq!(tree.contents[*cursor], b'[');
    *cursor += 1;
    let array_start = tree.tok_types.len();
    tree.tok_types.push(TokenType::Array);
    tree.tok_ranges.push(*cursor..*cursor);
    let children_start = tree.tok_children.len();
    tree.tok_children.push(children_start..children_start);

    loop {
        let eof = skip_whitespace(tree, cursor);
        if eof {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        if tree.contents[*cursor] == b']' {
            *cursor += 1;
            break;
        }
        parse_value(tree, cursor)?;
        let eof = skip_whitespace(tree, cursor);
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
    tree.tok_ranges[array_start].end = *cursor + 1;
    return Ok(());
}
