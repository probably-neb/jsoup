use crate::{Tree, assert_tree_valid};

pub fn parse(input: &str) -> Result<Tree, ParseError> {
    let mut tree = Tree::empty();
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

fn parse_whitespace_or_comment(tree: &mut Tree, cursor: &mut usize) -> Result<bool, ParseError> {
    tree.assert_lengths();
    let eof = parse_whitespace(tree, cursor);
    if eof {
        return Ok(eof);
    }
    parse_any_comments(tree, cursor)?;
    let eof = parse_whitespace(tree, cursor);
    Ok(eof)
}

fn parse_whitespace(tree: &mut Tree, cursor: &mut usize) -> bool {
    tree.assert_lengths();
    assert!(*cursor <= tree.contents.len());
    while *cursor < tree.contents.len() && tree.contents[*cursor].is_ascii_whitespace() {
        *cursor += 1;
    }
    assert!(*cursor <= tree.contents.len());
    assert!(*cursor == tree.contents.len() || !tree.contents[*cursor].is_ascii_whitespace());
    return *cursor == tree.contents.len();
}

fn parse_any_comments(tree: &mut Tree, cursor: &mut usize) -> Result<(), ParseError> {
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
                        tree.push_comment(range);
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
                            tree.push_comment(start..*cursor);
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

fn parse_null(tree: &mut Tree, cursor: &mut usize) -> Result<(), ParseError> {
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

fn parse_true(tree: &mut Tree, cursor: &mut usize) -> Result<(), ParseError> {
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

fn parse_false(tree: &mut Tree, cursor: &mut usize) -> Result<(), ParseError> {
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

fn parse_string(tree: &mut Tree, cursor: &mut usize) -> Result<(), ParseError> {
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

fn parse_number(tree: &mut Tree, cursor: &mut usize) -> Result<(), ParseError> {
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
        tree.push_float(start..*cursor);
    } else {
        tree.push_int(start..*cursor);
    }
    Ok(())
}

pub fn is_start_of_number(c: u8) -> bool {
    c.is_ascii_digit() || c == b'-'
}

fn parse_value(tree: &mut Tree, cursor: &mut usize) -> Result<(), ParseError> {
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

fn parse_object(tree: &mut Tree, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    assert_eq!(tree.contents[*cursor], b'{');
    if *cursor + 1 > tree.contents.len() {
        return Err(ParseError::UnexpectedEndOfInput);
    }
    let obj_index = tree.push_object(*cursor..*cursor);

    *cursor += 1;

    let mut key_index_prev = obj_index;

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

    tree.tok_span[obj_index].end = *cursor;
    return Ok(());
}

fn parse_key_value(tree: &mut Tree, cursor: &mut usize) -> Result<usize, ParseError> {
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

fn parse_array(tree: &mut Tree, cursor: &mut usize) -> Result<(), ParseError> {
    tree.assert_lengths();
    assert_eq!(tree.contents[*cursor], b'[');
    let array_index = tree.push_array(*cursor..*cursor);

    *cursor += 1;

    let mut value_index_prev = array_index;

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

    tree.tok_span[array_index].end = *cursor;
    return Ok(());
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

impl std::fmt::Display for ParseError {
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
