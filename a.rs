use std::cmp::max;
use std::cmp::min;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::env;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::io;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Copy, PartialEq, Eq)]
struct TextPosition {
    line: usize,
    col: usize,
}

impl PartialOrd for TextPosition {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TextPosition {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.line < other.line {
            Ordering::Less
        } else if self.line > other.line {
            Ordering::Greater
        } else if self.col < other.col {
            Ordering::Less
        } else if self.col > other.col {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct TextSpan {
    start: TextPosition,
    end: TextPosition,
}

impl TextSpan {
    fn add(self, other: TextSpan) -> Self {
        let start = min(self.start, other.start);
        let end = max(self.end, other.end);

        Self {
            start: start,
            end: end,
        }
    }

    fn new(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Self {
            start: TextPosition {
                line: start_line,
                col: start_col,
            },
            end: TextPosition {
                line: end_line,
                col: end_col,
            },
        }
    }

    fn new0() -> Self {
        Self::new(0, 0, 0, 0)
    }
}

impl Display for TextSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(
            f,
            "[{},{}]-[{},{}]",
            self.start.line, self.start.col, self.end.line, self.end.col
        )
    }
}

impl Debug for TextSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(
            f,
            "[{},{}]-[{},{}]",
            self.start.line, self.start.col, self.end.line, self.end.col
        )
    }
}

#[derive(Debug, Clone)]
enum Token {
    INT(TextSpan, i64),
    ID(TextSpan, String),
    SPECIAL(TextSpan, String),
    ExpressionSeparator(TextSpan),
    INVALID(TextSpan, String),
    LPAREN(TextSpan),
    RPAREN(TextSpan),
}
impl Token {
    fn name(self: Token) -> String {
        match self {
            Token::INT(_, _) => String::from("INT"),
            Token::ID(_, _) => String::from("ID"),
            Token::SPECIAL(_, _) => String::from("SPECIAL"),
            Token::ExpressionSeparator(_) => String::from("EXPRESSION_SEPARATOR"),
            Token::INVALID(_, _) => String::from("INVALID"),
            Token::LPAREN(_) => String::from("LPAREN"),
            Token::RPAREN(_) => String::from("RPAREN"),
        }
    }
}

fn read_int(mut chars: CountingChars) -> (CountingChars, Token) {
    let start = TextPosition {
        line: chars.line,
        col: chars.col,
    };
    let mut n: i64 = 0;
    while let Some(c) = chars.peek() {
        if c.is_numeric() && c.to_owned() as i64 - 48 < 10 {
            let m = c.to_owned() as i64 - 48;
            n *= 10;
            n += m;
            chars.next();
        } else {
            break;
        }
    }
    let end = TextPosition {
        line: chars.line,
        col: chars.col,
    };
    (
        chars,
        Token::INT(
            TextSpan {
                start: start,
                end: end,
            },
            n,
        ),
    )
}

fn read_id(mut chars: CountingChars) -> (CountingChars, Token) {
    let start = TextPosition {
        line: chars.line,
        col: chars.col,
    };
    let mut s: String = String::new();
    while let Some(c) = chars.peek() {
        if c.is_alphabetic() || c.is_numeric() && c.to_owned() as u32 - 48 >= 10 {
            s.push(c.clone());
            chars.next();
        } else {
            break;
        }
    }
    let end = TextPosition {
        line: chars.line,
        col: chars.col,
    };
    (
        chars,
        Token::ID(
            TextSpan {
                start: start,
                end: end,
            },
            s,
        ),
    )
}

fn read_special(mut chars: CountingChars) -> (CountingChars, Token) {
    let start = TextPosition {
        line: chars.line,
        col: chars.col,
    };
    if let Some(c) = chars.next() {
        let end = TextPosition {
            line: chars.line,
            col: chars.col,
        };
        if c == ';' {
            (
                chars,
                Token::ExpressionSeparator(TextSpan {
                    start: start,
                    end: end,
                }),
            )
        } else {
            (
                chars,
                Token::SPECIAL(
                    TextSpan {
                        start: start,
                        end: end,
                    },
                    c.to_string(),
                ),
            )
        }
    } else {
        let end = TextPosition {
            line: chars.line,
            col: chars.col,
        };
        (
            chars,
            Token::INVALID(
                TextSpan {
                    start: start,
                    end: end,
                },
                String::new(),
            ),
        )
    }
}

struct CountingChars<'lexing> {
    chars: Peekable<Chars<'lexing>>,
    line: usize,
    col: usize,
}

impl<'lexing> CountingChars<'lexing> {
    fn next(&mut self) -> Option<char> {
        if let Some('\n') = self.chars.peek() {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        self.chars.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek().clone()
    }
}

fn lex_string(s: &str, line_offset: usize) -> Vec<Token> {
    let mut v: Vec<Token> = Vec::new();
    let mut it = CountingChars {
        chars: s.chars().peekable(),
        line: line_offset,
        col: 0,
    };
    while let Some(c) = it.peek() {
        let token: Token;
        if c.is_whitespace() {
            it.next();
        } else if *c == '(' {
            let start = TextPosition {
                line: it.line,
                col: it.col,
            };
            it.next();
            let end = TextPosition {
                line: it.line,
                col: it.col,
            };
            v.push(Token::LPAREN(TextSpan {
                start: start,
                end: end,
            }));
        } else if *c == ')' {
            let start = TextPosition {
                line: it.line,
                col: it.col,
            };
            it.next();
            let end = TextPosition {
                line: it.line,
                col: it.col,
            };
            v.push(Token::RPAREN(TextSpan {
                start: start,
                end: end,
            }));
        } else if c.is_numeric() && c.to_owned() as u32 - 48 < 10 {
            (it, token) = read_int(it);
            v.push(token);
        } else if c.is_alphabetic() || c.is_numeric() && c.to_owned() as u32 - 48 >= 10 {
            (it, token) = read_id(it);
            v.push(token);
        } else {
            (it, token) = read_special(it);
            v.push(token);
        }
    }
    v
}

#[allow(dead_code)]
fn lex_interactive() -> io::Result<Vec<Token>> {
    let mut v: Vec<Token> = Vec::new();
    let mut buffer = String::new();
    let stdin = io::stdin();

    let mut bytes_read = 1;
    let mut line_offset = 0;
    while bytes_read > 0 {
        bytes_read = stdin.read_line(&mut buffer)?;
        v.append(&mut lex_string(&buffer, line_offset));
        buffer = String::from("");
        line_offset += 1;
    }
    Ok(v)
}

trait Printable {
    fn print(&self);
}

impl Printable for Vec<Token> {
    fn print(&self) -> () {
        for t in self {
            println!("{:?}", t);
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
enum AST {
    // TODO rename to expression, make one enum for every semantic unit
    Err(TextSpan, String),
    Val(TextSpan, i64),
    Sym(TextSpan, String),
    // TODO Abstraction
    App(TextSpan, Box<AST>, Box<AST>),
    // TODO move this
    Expressions(TextSpan, Vec<AST>),
}

impl AST {
    fn has_errors(&self) -> bool {
        match self {
            AST::Err(_, _) => true,
            AST::App(_, e1, e2) => e1.has_errors() || e2.has_errors(),
            AST::Expressions(_, exprs) => {
                for e in exprs.iter() {
                    if e.has_errors() {
                        return true;
                    }
                }
                false
            }
            _ => false,
        }
    }

    fn report_errors(&self) -> () {
        match self {
            AST::Err(_, _) => println!("{}", self.pretty_string()),
            AST::App(_, e1, e2) => {
                e1.report_errors();
                e2.report_errors();
            }
            AST::Expressions(_, exprs) => {
                for e in exprs.iter() {
                    e.report_errors();
                }
            }
            _ => (),
        }
    }
}

impl TextSpan {
    fn from_token(token: Token) -> Self {
        match token {
            Token::INT(span, _) => span,
            Token::ID(span, _) => span,
            Token::SPECIAL(span, _) => span,
            Token::ExpressionSeparator(span) => span,
            Token::INVALID(span, _) => span,
            Token::LPAREN(span) => span,
            Token::RPAREN(span) => span,
        }
    }

    fn from_ast(ast: &AST) -> Self {
        match ast {
            AST::Err(span, _) => span.clone(),
            AST::Val(span, _) => span.clone(),
            AST::Sym(span, _) => span.clone(),
            AST::App(span, _, _) => span.clone(),
            AST::Expressions(span, _) => span.clone(),
        }
    }
}

impl Printable for AST {
    fn print(&self) -> () {
        println!("{:?}", self);
    }
}

trait PrettyPrint {
    fn pretty_print(&self);
    fn pretty_string(&self) -> String;
}

impl PrettyPrint for AST {
    fn pretty_print(&self) -> () {
        println!("{}", self.pretty_string());
    }

    fn pretty_string(&self) -> String {
        match self {
            AST::Err(span, reason) => format!("{:?}", span) + "\t" + reason,
            AST::Val(_, i) => i.to_string(),
            AST::Sym(_, s) => String::from(s),
            AST::App(_, lhs, rhs) => [
                String::from("("),
                lhs.pretty_string(),
                String::from(" "),
                rhs.pretty_string(),
                String::from(")"),
            ]
            .join(""),
            AST::Expressions(_, exprs) => exprs
                .iter()
                .map(|e| e.pretty_string())
                .collect::<Vec<_>>()
                .join(";\n"),
        }
    }
}

// Val  :=  <INT>
// LHS  :=  <String> | <Special>
// E    :=  Val | (LHS Val)
// Stms :=  E (';' E)*

fn parse_expression(mut tokens: MyIt) -> (MyIt, AST) {
    let result = match tokens.next() {
        None => AST::Err(
            TextSpan::new0(),
            String::from("Expeted expression but reached end of input!"),
        ),
        Some(token) => match token {
            Token::INT(span, n) => AST::Val(*span, n.clone()),
            Token::ID(span, s) => AST::Sym(*span, s.clone()),
            Token::SPECIAL(span, s) => AST::Sym(*span, s.clone()),
            Token::LPAREN(span) => {
                let start_span = span.clone();
                let ast1;
                (tokens, ast1) = parse_expression(tokens);
                let ast2;
                (tokens, ast2) = parse_expression(tokens);
                match tokens.next() {
                    None => AST::Err(
                        TextSpan::from_ast(&ast2),
                        String::from("Expected right parens but reached end of input!"),
                    ),
                    Some(closing) => match closing {
                        Token::RPAREN(end_span) => {
                            AST::App(start_span.add(*end_span), Box::new(ast1), Box::new(ast2))
                        }
                        t => AST::Err(
                            TextSpan::from_token(t.clone()),
                            format!(
                                "Expected RPAREN, found {} at {}",
                                t.clone().name(),
                                TextSpan::from_token(t.clone())
                            ),
                        ),
                    },
                }
            }
            t => AST::Err(
                TextSpan::from_token(t.clone()),
                format!("Expected expression but found {}", t.clone().name()),
            ),
        },
    };
    (tokens, result)
}

struct MyIt<'parsing> {
    tokens: Peekable<std::slice::Iter<'parsing, Token>>,
}

impl<'parsing> MyIt<'parsing> {
    fn next(&mut self) -> Option<&Token> {
        self.tokens.next().clone()
    }

    fn peek(&mut self) -> Option<&&Token> {
        self.tokens.peek().clone()
    }
}

#[allow(unused)]
fn parse_program(tokens: &Vec<Token>) -> AST {
    let mut it = MyIt {
        tokens: tokens.iter().peekable(),
    };
    let mut expressions: Vec<AST> = Vec::new();

    let expr;
    (it, expr) = parse_expression(it);
    expressions.push(expr);

    while let Some(token) = it.peek().clone() {
        match token {
            Token::ExpressionSeparator(span) => {
                it.next();
                ()
            }
            _ => {
                let expr = AST::Err(
                    TextSpan::from_token((**token).clone()),
                    format!(
                        "Expected statement separator, found {}",
                        (**token).clone().name()
                    ),
                );
                expressions.push(expr);
            }
        }
        let expr;
        (it, expr) = parse_expression(it);
        expressions.push(expr);
    }
    let span_start = if let Some(e) = expressions.first() {
        TextSpan::from_ast(e)
    } else {
        TextSpan::new0()
    };
    let span_end = if let Some(e) = expressions.last() {
        TextSpan::from_ast(e)
    } else {
        TextSpan::new0()
    };
    AST::Expressions(span_start.add(span_end), expressions)
}

fn main() -> io::Result<()> {
    let args = env::args().collect::<HashSet<_>>();
    let source = String::from("0; ((+ 111) 222); 333; 444; (+ 555) ");
    let tokens = lex_string(&source, 0);
    if args.contains("--lex") {
        tokens.print();
        return Ok(());
    }
    let program = parse_program(&tokens);
    if program.has_errors() {
        println!("{}", source);
        println!("########################");
        program.print();
        println!("########################");
        program.pretty_print();
        println!("########################");
        program.report_errors();
        return Ok(());
    }
    if args.contains("--parse") || args.contains("--format") {
        program.pretty_print();
        return Ok(());
    }

    Ok(())
}
