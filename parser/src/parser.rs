use lox_ast::*;
use thiserror::Error;

use crate::lexer::Token;

type TokenStream<'a, I> = multipeek::MultiPeek<I>;

#[derive(Debug, Error, PartialEq)]
pub enum ParseError<'a> {
    #[error("unexpected token: {0:?}")]
    UnexpectedToken(Spanned<Token<'a>>),
    #[error("unexpected end of file")]
    UnexpectedEof,
}

pub fn parse<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
) -> Result<Program<'a>, ParseError<'a>> {
    let mut tokens = multipeek::multipeek(tokens);
    let stmts = parse_stmts(&mut tokens)?;

    parse_token(&mut tokens, Token::Eof)?;

    Ok(Program::new(stmts))
}

fn parse_token<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
    expected: Token<'a>,
) -> Result<Span, ParseError<'a>> {
    match tokens.next() {
        Some((span, token)) if *token == expected => Ok(span.clone()),
        Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
        None => Err(ParseError::UnexpectedEof),
    }
}

fn parse_stmts<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
) -> Result<Vec<Spanned<Stmt<'a>>>, ParseError<'a>> {
    let mut stmts = vec![];

    while let Some((_span, token)) = tokens.peek() {
        match token {
            Token::Eof => break,
            Token::RightBrace => break,
            _ => stmts.push(parse_stmt(tokens)?),
        }
    }

    Ok(stmts)
}

fn parse_stmt<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
) -> Result<Spanned<Stmt<'a>>, ParseError<'a>> {
    let (start, token) = tokens.peek().ok_or(ParseError::UnexpectedEof)?;

    let stmt = match token {
        Token::Class => todo!("class stmt"),
        Token::Var => {
            let stmt = parse_var_stmt(tokens)?;
            (start.end..stmt.0.end, Stmt::Var(stmt))
        }
        Token::Fun => {
            let stmt = parse_fun_stmt(tokens)?;
            (start.end..stmt.0.end, Stmt::Fun(stmt))
        }
        Token::Print => {
            tokens.next();
            let expr = parse_expr(tokens)?;
            let end = parse_token(tokens, Token::Semicolon)?;
            (start.start..end.end, Stmt::Print(expr))
        }
        Token::LeftBrace => {
            let block = parse_block(tokens)?;
            (start.start..block.0.end, Stmt::Block(block.1))
        }
        Token::If => {
            tokens.next();
            parse_token(tokens, Token::LeftParen)?;
            let cond = parse_expr(tokens)?;
            parse_token(tokens, Token::RightParen)?;
            let body = Box::new(parse_stmt(tokens)?);
            let else_body = if let Some((_, Token::Else)) = tokens.peek() {
                tokens.next();
                Some(Box::new(parse_stmt(tokens)?))
            } else {
                None
            };
            let span_end = else_body
                .clone()
                .map_or(body.0.end, |else_body| else_body.0.end);
            (
                start.start..span_end,
                Stmt::If((
                    start.start..span_end,
                    If {
                        cond,
                        body,
                        else_body,
                    },
                )),
            )
        }
        Token::While => {
            tokens.next();
            let cond = parse_expr(tokens)?;
            let body = Box::new(parse_stmt(tokens)?);
            (
                start.start..body.0.end,
                Stmt::While((start.start..body.0.end, While { cond, body })),
            )
        }
        Token::For => {
            todo!("for stmt")
        }
        Token::Return => {
            tokens.next();

            if let Some((_, Token::Semicolon)) = tokens.peek() {
                tokens.next();
                (start.start..start.end, Stmt::Return(None))
            } else {
                let expr = parse_expr(tokens)?;
                let end = parse_token(tokens, Token::Semicolon)?;
                (start.start..end.end, Stmt::Return(Some(expr)))
            }
        }
        _ => {
            let expr = parse_expr(tokens)?;
            let end = parse_token(tokens, Token::Semicolon)?;
            (start.start..end.end, Stmt::Expr(expr))
        }
    };

    Ok(stmt)
}

fn parse_var_stmt<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
) -> Result<Spanned<VarStmt<'a>>, ParseError<'a>> {
    let start = parse_token(tokens, Token::Var)?;
    let name = match tokens.next() {
        Some((span, Token::Identifier(name))) => (span.clone(), *name),
        Some(token) => return Err(ParseError::UnexpectedToken(token.clone())),
        _ => return Err(ParseError::UnexpectedEof),
    };

    let expr = if let Some((_, Token::Equal)) = tokens.peek() {
        tokens.next();
        Some(parse_expr(tokens)?)
    } else {
        None
    };

    let end = parse_token(tokens, Token::Semicolon)?;

    Ok((
        start.start..end.end,
        VarStmt {
            lhs: name,
            rhs: expr,
        },
    ))
}

fn parse_ident<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
) -> Result<Spanned<&'a str>, ParseError<'a>> {
    match tokens.next() {
        Some((span, Token::Identifier(name))) => Ok((span.clone(), *name)),
        Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
        _ => Err(ParseError::UnexpectedEof),
    }
}

// Identifiers separated by commas
fn parse_params<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
) -> Result<Vec<Spanned<&'a str>>, ParseError<'a>> {
    let mut params = vec![];

    while let Some((_, token)) = tokens.peek() {
        if *token == Token::RightParen {
            break;
        }

        if !params.is_empty() {
            parse_token(tokens, Token::Comma)?;
        }

        params.push(parse_ident(tokens)?);
    }

    Ok(params)
}

fn parse_block<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
) -> Result<Spanned<Vec<Spanned<Stmt<'a>>>>, ParseError<'a>> {
    let start = parse_token(tokens, Token::LeftBrace)?;
    let stmts = parse_stmts(tokens)?;
    let end = parse_token(tokens, Token::RightBrace)?;
    Ok((start.start..end.end, stmts))
}

fn parse_fun_stmt<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
) -> Result<Spanned<FunStmt<'a>>, ParseError<'a>> {
    let start = parse_token(tokens, Token::Fun)?;
    let name = parse_ident(tokens)?;
    parse_token(tokens, Token::LeftParen)?;
    let params = parse_params(tokens)?;
    parse_token(tokens, Token::RightParen)?;

    let body = parse_block(tokens)?;

    Ok((start.start..body.0.end, FunStmt { name, params, body }))
}

fn parse_args<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
) -> Result<Vec<Spanned<Expr<'a>>>, ParseError<'a>> {
    let mut args = vec![];

    while let Some((_, token)) = tokens.peek() {
        if *token == Token::RightParen {
            break;
        }

        if !args.is_empty() {
            parse_token(tokens, Token::Comma)?;
        }

        args.push(parse_expr(tokens)?);
    }

    Ok(args)
}

fn parse_expr<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
) -> Result<Spanned<Expr<'a>>, ParseError<'a>> {
    parse_expr_bp(tokens, 0)
}

fn parse_expr_bp<'a, I: Iterator<Item = &'a Spanned<Token<'a>>>>(
    tokens: &mut TokenStream<'a, I>,
    min_bp: u8,
) -> Result<Spanned<Expr<'a>>, ParseError<'a>> {
    let (span, token) = tokens.next().ok_or(ParseError::UnexpectedEof)?;

    let mut span = span.clone();

    let mut lhs = match token {
        Token::Nil => Expr::Primary((span.clone(), Primary::Nil)),
        Token::True => Expr::Primary((span.clone(), Primary::Bool(true))),
        Token::False => Expr::Primary((span.clone(), Primary::Bool(false))),
        Token::Number(num) => Expr::Primary((span.clone(), Primary::Number(*num))),
        Token::String(str) => Expr::Primary((span.clone(), Primary::String(str))),
        Token::Super => {
            parse_token(tokens, Token::Dot)?;
            let name = match tokens.next() {
                Some((_, Token::Identifier(name))) => *name,
                _ => return Err(ParseError::UnexpectedEof),
            };
            Expr::Primary((span.clone(), Primary::Super(name)))
        }
        Token::LeftParen => {
            let expr = parse_expr_bp(tokens, 0)?;
            let end = parse_token(tokens, Token::RightParen)?;
            Expr::Grouping(Box::new((span.start..end.end, expr.1)))
        }
        Token::Identifier(ident) => Expr::Primary((span.clone(), Primary::Identifier(ident))),
        lhs if unary_op(lhs).is_some() => {
            let op = unary_op(lhs).unwrap();
            let rhs = parse_expr_bp(tokens, 13)?;
            span.end = rhs.0.end;
            Expr::Unary((
                span.clone(),
                Unary {
                    op: (span.clone(), op),
                    rhs: Box::new(rhs),
                },
            ))
        }
        _ => return Err(ParseError::UnexpectedToken((span.clone(), token.clone()))),
    };

    loop {
        match tokens.peek() {
            Some((_, Token::LeftParen)) => {
                tokens.next();
                let args = parse_args(tokens)?;
                let end = parse_token(tokens, Token::RightParen)?;
                span.end = end.end;
                lhs = Expr::Call((
                    span.clone(),
                    Call {
                        callee: Box::new((span.clone(), lhs)),
                        args,
                    },
                ));
            }
            Some((_, Token::Dot)) => {
                todo!("property access");
                // tokens.next();
                // let prop = parse_ident(tokens)?;
                // span.end = prop.0.end;
                // lhs = Expr::Call((
                //     span.clone(),
                //     Call::Property {
                //         callee: Box::new((span.clone(), lhs)),
                //         prop: Box::new(prop),
                //     },
                // ));
            }
            Some((_, Token::Equal)) => {
                tokens.next();
                let rhs = parse_expr(tokens)?;
                let lhs_span = span.clone();
                span.end = rhs.0.end;

                let name = match &lhs {
                    Expr::Primary((_, Primary::Identifier(name))) => *name,
                    // Expr::Call((_, Call::Property { prop, .. })) => prop.1,
                    Expr::Call(_) => todo!("set property"),
                    _ => unreachable!(),
                };

                lhs = Expr::Assign((
                    lhs_span.clone(),
                    Assign {
                        lhs: Box::new((lhs_span, name)),
                        rhs: Box::new(rhs),
                    },
                ));
            }
            _ => break,
        }
    }

    while let Some(op) = binary_op(tokens.peek()) {
        let (l_bp, r_bp) = binding_power(op.1.clone());

        if l_bp < min_bp {
            break;
        }

        tokens.next();
        let rhs = parse_expr_bp(tokens, r_bp)?;
        let lhs_span = span.clone();
        span.end = rhs.0.end;
        lhs = Expr::Binary((
            lhs_span.clone(),
            Binary {
                op,
                lhs: Box::new((lhs_span, lhs)),
                rhs: Box::new(rhs),
            },
        ));
    }

    Ok((span, lhs))
}

fn unary_op(token: &Token) -> Option<UnaryOp> {
    match token {
        Token::Bang => Some(UnaryOp::Not),
        Token::Minus => Some(UnaryOp::Neg),
        _ => None,
    }
}

fn binary_op(token: Option<&&Spanned<Token>>) -> Option<Spanned<BinaryOp>> {
    match token {
        Some((span, Token::Plus)) => Some((span.clone(), BinaryOp::Add)),
        Some((span, Token::Minus)) => Some((span.clone(), BinaryOp::Sub)),
        Some((span, Token::Star)) => Some((span.clone(), BinaryOp::Mul)),
        Some((span, Token::Slash)) => Some((span.clone(), BinaryOp::Div)),
        Some((span, Token::And)) => Some((span.clone(), BinaryOp::And)),
        Some((span, Token::Or)) => Some((span.clone(), BinaryOp::Or)),
        Some((span, Token::EqualEqual)) => Some((span.clone(), BinaryOp::Eq)),
        Some((span, Token::BangEqual)) => Some((span.clone(), BinaryOp::Ne)),
        Some((span, Token::Less)) => Some((span.clone(), BinaryOp::Lt)),
        Some((span, Token::Greater)) => Some((span.clone(), BinaryOp::Gt)),
        Some((span, Token::LessEqual)) => Some((span.clone(), BinaryOp::Le)),
        Some((span, Token::GreaterEqual)) => Some((span.clone(), BinaryOp::Ge)),
        _ => None,
    }
}

fn binding_power(op: BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::Or => (1, 2),
        BinaryOp::And => (3, 4),
        BinaryOp::Eq | BinaryOp::Ne => (5, 6),
        BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => (7, 8),
        BinaryOp::Add | BinaryOp::Sub => (9, 10),
        BinaryOp::Mul | BinaryOp::Div => (11, 12),
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer;

    use super::*;

    macro_rules! assert_parse {
        ($source:expr, $expected:expr) => {
            let tokens = lexer::lex($source).unwrap();
            let mut tokens = multipeek::multipeek(&tokens);
            let prog = parse(&mut tokens);
            assert_eq!(prog.map(|p| p.to_sexpr()), Ok($expected.to_string()));
        };
    }

    #[test]
    fn test_parse_numbers() {
        assert_parse!("1;", "1");
        assert_parse!("1.0;", "1");
        assert_parse!("3.141592;", "3.141592");
        assert_parse!("0.0;", "0");
        assert_parse!("0.1;", "0.1");
    }

    #[test]
    fn test_parse_booleans() {
        assert_parse!("true;", "true");
        assert_parse!("false;", "false");
    }

    #[test]
    fn test_parse_strings() {
        assert_parse!("\"hello\";", "hello");
        assert_parse!("\"hello \\\"world\\\"\";", "hello \"world\"");
        assert_parse!("\"hello \tworld\";", "hello \tworld");
    }

    #[test]
    fn test_parse_nil() {
        assert_parse!("nil;", "nil");
    }

    #[test]
    fn test_parse_super() {
        assert_parse!("super.foo;", "(super foo)");
    }

    #[test]
    fn test_parse_grouping() {
        assert_parse!("(1);", "1");
        assert_parse!("(1 + 2) * 3;", "(* (+ 1 2) 3)");
    }

    #[test]
    fn test_parse_unary_expr() {
        assert_parse!("!true;", "(! true)");
        assert_parse!("-1;", "(- 1)");
        assert_parse!("!nil;", "(! nil)");
    }

    #[test]
    fn test_parse_binary_expr() {
        assert_parse!("1 + 2 * 3;", "(+ 1 (* 2 3))");
        assert_parse!("1 + 2 * 3.0;", "(+ 1 (* 2 3))");
        assert_parse!("1 + 2 * 3.0 + 4;", "(+ (+ 1 (* 2 3)) 4)");
        assert_parse!("1 + 2 * 3.0 + 4 * 5;", "(+ (+ 1 (* 2 3)) (* 4 5))");
        assert_parse!("1 < 2;", "(< 1 2)");
        assert_parse!("1 > 2;", "(> 1 2)");
        assert_parse!("1 <= 2;", "(<= 1 2)");
        assert_parse!("1 >= 2;", "(>= 1 2)");
        assert_parse!("1 == 2;", "(== 1 2)");
        assert_parse!("1 != 2;", "(!= 1 2)");
        assert_parse!("true and false;", "(and true false)");
        assert_parse!("true or false;", "(or true false)");
        assert_parse!("1 < 2 and 3 > 4;", "(and (< 1 2) (> 3 4))");
        assert_parse!("1 <= 2 or 3 >= 4;", "(or (<= 1 2) (>= 3 4))");
        assert_parse!(
            "1 == 2 and 3 != 4 or 5 < 6;",
            "(or (and (== 1 2) (!= 3 4)) (< 5 6))"
        );
    }

    #[test]
    fn test_parse_var_stmt() {
        assert_parse!("var x = 1;", "($var x 1)");
        assert_parse!("var x = 1 + 2;", "($var x (+ 1 2))");
        assert_parse!("var x = 1 + 2 * 3;", "($var x (+ 1 (* 2 3)))");
    }

    #[test]
    fn test_parse_empty_fun_stmt() {
        assert_parse!("fun foo() {}", "($fun foo () ())");
        assert_parse!("fun foo(a, b) {}", "($fun foo (a b) ())");
        assert_parse!(
            "fun foo(a, b, c, d, e, f, g) {}",
            "($fun foo (a b c d e f g) ())"
        );
    }

    #[test]
    fn test_parse_fun_stmt() {
        assert_parse!("fun say(msg) { 10; }", "($fun say (msg) (10))");
    }

    #[test]
    fn test_parse_calls() {
        assert_parse!("foo();", "($call foo ())");
        assert_parse!("foo(1, 2);", "($call foo (1 2))");
    }

    #[test]
    fn test_parse_if_stmt() {
        assert_parse!("if (1) { 2; }", "($if 1 ($block 2))");
        assert_parse!("if (1) 2;", "($if 1 2)");
        assert_parse!("if (1) { 2; } else { 3; }", "($if 1 ($block 2) ($block 3))");
        assert_parse!(
            "if (true) { if (false) { 1; } else { 2; } }",
            "($if true ($block ($if false ($block 1) ($block 2))))"
        );
        assert_parse!(
            "if (1 + 2 == 3) { print 4; } else { print 5; }",
            "($if (== (+ 1 2) 3) ($block ($print 4)) ($block ($print 5)))"
        );
    }

    #[test]
    fn test_parse_print_stmt() {
        assert_parse!("print 1;", "($print 1)");
        assert_parse!("print 1 + 2;", "($print (+ 1 2))");
        assert_parse!("print 1 + 2 * 3;", "($print (+ 1 (* 2 3)))");
    }

    #[test]
    fn test_parse_return_stmt() {
        assert_parse!("return 1;", "($return 1)");
        assert_parse!("return 1 + 2;", "($return (+ 1 2))");
        assert_parse!("return;", "$return");
        assert_parse!(
            "if (true) return; else print 1;",
            "($if true $return ($print 1))"
        );
        assert_parse!(
            "if (true) { return 1; } else { return 2; }",
            "($if true ($block ($return 1)) ($block ($return 2)))"
        );
        assert_parse!("fun foo() { return; }", "($fun foo () ($return))");
        assert_parse!("fun foo() { return 42; }", "($fun foo () (($return 42)))");
        assert_parse!("while (true) return;", "($while true $return)");
        assert_parse!("while (true) { return; }", "($while true ($block $return))");
    }

    #[test]
    fn test_parse_while_stmt() {
        assert_parse!(
            "while (true) { print 1; }",
            "($while true ($block ($print 1)))"
        );
    }

    #[test]
    fn test_parse_assignments() {
        assert_parse!("x = 1;", "($assign x 1)");
        assert_parse!("x = y = 2;", "($assign x ($assign y 2))");
    }
}
