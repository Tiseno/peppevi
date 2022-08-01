use std::io;
use std::str::Chars;
use std::iter::Peekable;
use std::fmt::Formatter;
use std::fmt::Error;
use std::fmt::Debug;
use std::cmp::Ordering;
use std::cmp::min;
use std::cmp::max;

#[derive(Clone)]
#[derive(Copy)]
#[derive(PartialEq)]
#[derive(Eq)]
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

#[derive(Clone)]
#[derive(Copy)]
#[derive(PartialEq)]
#[derive(Eq)]
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
            start: TextPosition{ line: start_line, col: start_col },
            end: TextPosition{ line: end_line, col: end_col },
        }
    }

    fn new0() -> Self {
        Self::new(0,0,0,0)
    }
}

impl Debug for TextSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        // return write!(f, "");
        if self.start.line == self.end.line {
            write!(f, "{}#{}-{}", self.start.line, self.start.col, self.end.col)
        } else {
            write!(f, "{}-{}#{}-{}", self.start.line, self.end.line, self.start.col, self.end.col)
        }
    }
}

#[derive(Debug)]
#[derive(Clone)]
enum Token {
    INT(TextSpan, i64),
    ID(TextSpan, String),
    SPECIAL(TextSpan, String),
    INVALID(TextSpan, String),
    LPAREN(TextSpan),
    RPAREN(TextSpan),
}

fn read_int(mut chars: CountingChars) -> (CountingChars, Token) {
    let start = TextPosition{line: chars.line, col: chars.col};
    let mut n: i64 = 0;
    while let Some(c) = chars.peek() {
        if c.is_numeric() && c.to_owned() as i64 - 48 < 10{
            let m = c.to_owned() as i64 - 48;
            n *= 10;
            n += m;
            chars.next();
        } else {
            break;
        }
    }
    let end = TextPosition{line: chars.line, col: chars.col};
    (chars, Token::INT(TextSpan{ start: start, end: end }, n))
}

fn read_id(mut chars: CountingChars) -> (CountingChars, Token) {
    let start = TextPosition{line: chars.line, col: chars.col};
    let mut s: String = String::new();
    while let Some(c) = chars.peek() {
        if c.is_alphabetic() || c.is_numeric() && c.to_owned() as u32 - 48 >= 10 {
            s.push(c.clone());
            chars.next();
        } else {
            break;
        }
    }
    let end = TextPosition{line: chars.line, col: chars.col};
    (chars, Token::ID(TextSpan{ start: start, end: end }, s))
}

fn read_special(mut chars: CountingChars) -> (CountingChars, Token) {
    let start = TextPosition{line: chars.line, col: chars.col};
    if let Some(c) = chars.next() {
        let end = TextPosition{line: chars.line, col: chars.col};
        (chars, Token::SPECIAL(TextSpan{ start: start, end: end }, c.to_string()))
    } else {
        let end = TextPosition{line: chars.line, col: chars.col};
        (chars, Token::INVALID(TextSpan{ start: start, end: end }, String::new()))
    }
}

struct CountingChars<'lexing> {
    chars: Peekable<Chars<'lexing>>,
    line: usize,
    col: usize,
}

impl<'lexing> CountingChars<'lexing> {
    fn next(&mut self) -> Option<char> {
        if let Some('\n') = self.chars.peek(){
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
    let mut it = CountingChars{ chars: s.chars().peekable(), line: line_offset, col: 0 };
    while let Some(c) = it.peek() {
        let token: Token;
        if c.is_whitespace() {
            it.next();
        } else if *c == '(' {
            let start = TextPosition{line: it.line, col: it.col};
            it.next();
            let end = TextPosition{line: it.line, col: it.col};
            v.push(Token::LPAREN(TextSpan{ start: start, end: end }));
        } else if *c == ')' {
            let start = TextPosition{line: it.line, col: it.col};
            it.next();
            let end = TextPosition{line: it.line, col: it.col};
            v.push(Token::RPAREN(TextSpan{ start: start, end: end }));
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
    Err(TextSpan, String),
    Val(TextSpan, i64),
    Sym(TextSpan, String),
    App(TextSpan, Box<AST>, Box<AST>),
}

impl AST {
    fn name(token: Token) -> String {
        match token {
            Token::INT(_, _) => String::from("INT"),
            Token::ID(_, _) => String::from("ID"),
            Token::SPECIAL(_, _) => String::from("SPECIAL"),
            Token::INVALID(_, _) => String::from("INVALID"),
            Token::LPAREN(_) => String::from("LPAREN"),
            Token::RPAREN(_) => String::from("RPAREN"),
        }
    }
}

impl TextSpan {
    fn from(token: Token) -> Self {
        match token {
            Token::INT(span, _) => span,
            Token::ID(span, _) => span,
            Token::SPECIAL(span, _) => span,
            Token::INVALID(span, _) => span,
            Token::LPAREN(span) => span,
            Token::RPAREN(span) => span,
        }
    }
}

impl Printable for AST {
    fn print(&self) -> () {
        println!("{:?}", self);
    }
}

// Val :=   <INT>
// LHS :=   <ID> | <Sp>
// E   :=     Val | (LHS Val)

fn parse_e(mut tokens: std::slice::Iter<Token>) -> (std::slice::Iter<Token>, AST) {
    let result = match tokens.next() {
        None => AST::Err(TextSpan::new0(), String::from("End of input!")),
        Some(token) =>
            match token {
                Token::INT(span, n) => AST::Val(*span, n.clone()),
                Token::ID(span, s) => AST::Sym(*span, s.clone()),
                Token::SPECIAL(span, s) => AST::Sym(*span, s.clone()),
                Token::LPAREN(span) => {
                    let ast1;
                    (tokens, ast1) = parse_e(tokens);
                    let ast2;
                    (tokens, ast2) = parse_e(tokens);
                    match tokens.next() {
                        None => AST::Err(TextSpan::new0(), String::from("End of input when expected right parens!")),
                        Some(closing) => match closing {
                            Token::RPAREN(span2) => AST::App(span.add(*span2), Box::new(ast1), Box::new(ast2)),
                            t => AST::Err(TextSpan::new0(), format!("Expected RPAREN, found {:?}", t)),
                        },
                    }
                }
                t => AST::Err(TextSpan::from(t.clone()), format!("Expected INT, ID, or LPAREN, found {}", AST::name(t.clone()))),
            }
    };
    (tokens, result)
}

#[allow(unused)]
fn parse_tokens(tokens: &Vec<Token>) -> AST {
    let (tokens, ast) = parse_e(tokens.iter());
    ast
}

fn main() -> io::Result<()> {
    let program = String::from("((\n+ 1) \n1)");//  hello \n(5+4) s; 4\n hello \n");
    let tokens = lex_string(&program, 0);
    let ast = parse_tokens(&tokens);
    ast.print();


    // lex_interactive()?.print();
    Ok(())
}
