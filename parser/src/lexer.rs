use lox_ast::Spanned;
use thiserror::Error;

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Star,
    Slash,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    True,
    False,
    Nil,
    This,
    Super,
    And,
    Or,
    If,
    Else,
    While,
    For,
    Print,
    Return,
    Class,
    Fun,
    Var,
    Identifier(&'a str),
    String(String),
    Number(f64),
    Eof,
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("unexpected character: {0} at {1:?}")]
    UnexpectedCharacter(char, std::ops::Range<usize>),
    #[error("unexpected newline in string: {0:?}")]
    UnexpectedNewlineInString(std::ops::Range<usize>),
    #[error("unexpected escape sequence: {0:?}")]
    UnexpectedEscapeSequence(std::ops::Range<usize>, char),
    #[error("unexpected EOF: {0:?}")]
    UnexpectedEOF(std::ops::Range<usize>),
}

fn is_identifier_start(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_identifier_char(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9')
}

pub fn lex(source: &str) -> Result<Vec<Spanned<Token>>, LexerError> {
    let mut chars = source.chars().enumerate().peekable();
    let mut tokens = vec![];

    while let Some((start, char)) = chars.next() {
        match char {
            // Tabs are not allowed
            ' ' | '\n' => continue,
            '(' => tokens.push((start..start + 1, Token::LeftParen)),
            ')' => tokens.push((start..start + 1, Token::RightParen)),
            '{' => tokens.push((start..start + 1, Token::LeftBrace)),
            '}' => tokens.push((start..start + 1, Token::RightBrace)),
            ',' => tokens.push((start..start + 1, Token::Comma)),
            '.' => tokens.push((start..start + 1, Token::Dot)),
            ';' => tokens.push((start..start + 1, Token::Semicolon)),
            '-' => tokens.push((start..start + 1, Token::Minus)),
            '+' => tokens.push((start..start + 1, Token::Plus)),
            '*' => tokens.push((start..start + 1, Token::Star)),
            // slash or comment
            '/' => {
                if chars.peek().is_some_and(|(_, c)| *c == '/') {
                    for (_, c) in chars.by_ref() {
                        if c == '\n' {
                            break;
                        }
                    }
                } else {
                    tokens.push((start..start + 1, Token::Slash));
                }
            }
            '!' => {
                if chars.peek().is_some_and(|(_, c)| *c == '=') {
                    tokens.push((start..start + 2, Token::BangEqual));
                    chars.next();
                } else {
                    tokens.push((start..start + 1, Token::Bang));
                }
            }
            '=' => {
                if chars.peek().is_some_and(|(_, c)| *c == '=') {
                    tokens.push((start..start + 2, Token::EqualEqual));
                    chars.next();
                } else {
                    tokens.push((start..start + 1, Token::Equal));
                }
            }
            '>' => {
                if chars.peek().is_some_and(|(_, c)| *c == '=') {
                    tokens.push((start..start + 2, Token::GreaterEqual));
                    chars.next();
                } else {
                    tokens.push((start..start + 1, Token::Greater));
                }
            }
            '<' => {
                if chars.peek().is_some_and(|(_, c)| *c == '=') {
                    tokens.push((start..start + 2, Token::LessEqual));
                    chars.next();
                } else {
                    tokens.push((start..start + 1, Token::Less));
                }
            }
            c if is_identifier_start(c) => tokens.push(lex_identifier(source, &mut chars, start)),
            '"' => tokens.push(lex_string(&mut chars, start, '"')?),
            c if c.is_ascii_digit() => tokens.extend(lex_number(source, &mut chars, start)),
            _ => return Err(LexerError::UnexpectedCharacter(char, start..start + 1)),
        };
    }

    tokens.push((source.len()..source.len() + 1, Token::Eof));

    Ok(tokens)
}

fn lex_identifier<'a>(
    source: &'a str,
    chars: &mut std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'a>>>,
    start: usize,
) -> Spanned<Token<'a>> {
    let mut end = start + 1;
    while let Some((_, c)) = chars.peek() {
        if is_identifier_char(*c) {
            end += 1;
            chars.next();
        } else {
            break;
        }
    }
    match &source[start..end] {
        "true" => (start..end, Token::True),
        "false" => (start..end, Token::False),
        "nil" => (start..end, Token::Nil),
        "and" => (start..end, Token::And),
        "or" => (start..end, Token::Or),
        "super" => (start..end, Token::Super),
        "this" => (start..end, Token::This),
        "if" => (start..end, Token::If),
        "else" => (start..end, Token::Else),
        "while" => (start..end, Token::While),
        "for" => (start..end, Token::For),
        "print" => (start..end, Token::Print),
        "return" => (start..end, Token::Return),
        "class" => (start..end, Token::Class),
        "fun" => (start..end, Token::Fun),
        "var" => (start..end, Token::Var),
        _ => (start..end, Token::Identifier(&source[start..end])),
    }
}

fn lex_string<'a>(
    chars: &mut std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'a>>>,
    start: usize,
    delimiter: char,
) -> Result<Spanned<Token<'a>>, LexerError> {
    let mut end = start + 1;
    let mut string = String::new();
    while let Some((i, c)) = chars.peek() {
        if *c == delimiter {
            end = *i + 1;
            chars.next();

            break;
        } else if *c == '\n' || *c == '\r' {
            return Err(LexerError::UnexpectedNewlineInString(start..end));
        } else if *c == '\\' {
            end = *i + 2;
            chars.next();
            match chars.next() {
                Some((_, 'n')) => string.push('\n'),
                Some((_, 't')) => string.push('\t'),
                Some((_, 'r')) => string.push('\r'),
                Some((_, '\\')) => string.push('\\'),
                Some((_, c)) if c == delimiter => string.push(delimiter),
                Some((_, c)) => {
                    return Err(LexerError::UnexpectedEscapeSequence(start..end, c));
                }
                None => return Err(LexerError::UnexpectedEOF(start..end)),
            }
        } else {
            string.push(*c);
            end = *i + 1;
            chars.next();
        }
    }
    Ok((start..end, Token::String(string)))
}

fn lex_number<'a>(
    source: &'a str,
    chars: &mut std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'_>>>,
    start: usize,
) -> Vec<Spanned<Token<'a>>> {
    let mut end = start + 1;
    let mut has_dot = false;
    while let Some((i, c)) = chars.peek() {
        if c.is_ascii_digit() {
            end = *i + 1;
            chars.next();
        } else if *c == '.' && !has_dot {
            has_dot = true;
            end = *i + 1;
            chars.next();

            if let Some((_, c)) = chars.peek() {
                if !c.is_ascii_digit() {
                    return vec![
                        (
                            start..end,
                            Token::Number(source[start..end - 1].parse::<i64>().unwrap() as f64),
                        ),
                        (end..end + 1, Token::Dot),
                    ];
                }
            }
        } else {
            break;
        }
    }

    if has_dot {
        vec![(
            start..end,
            Token::Number(source[start..end].parse::<f64>().unwrap()),
        )]
    } else {
        vec![(
            start..end,
            Token::Number(source[start..end].parse::<i64>().unwrap() as f64),
        )]
    }
}
