use std::str::Chars;
use std::iter::Peekable;

#[derive(Debug)]
pub enum SExpr {
    Atom(SAtom),
    List(Vec<SExpr>),
    Quote(Box<SExpr>),
}

#[derive(Debug)]
pub enum SAtom {
    Symbol(String),
    Integer(i64),
    Bool(bool),
    Char(char),
}

pub fn parse(src: &str) -> Result<Vec<SExpr>, String> {
    let mut result = vec![];
    let mut parser = Parser::new(src.chars());
    while !parser.eof() {
        result.push(parser.parse_sexpr()?);
    }
    Ok(result)
}

struct Parser<'a> {
    iter: Peekable<Chars<'a>>,
}

impl<'a> Parser<'a> {
    fn new(iter: Chars<'a>) -> Parser {
        Parser {iter: iter.peekable()}
    }

    fn parse_sexpr(&mut self) -> Result<SExpr, String> {
        self.skip_spaces();
        if let Some(ch) = self.peek() {
            if ch == '\'' {
                self.iter.next();
                Ok(SExpr::Quote(Box::new(self.parse_sexpr()?)))
            } else if Parser::is_opening(ch) {
                Ok(SExpr::List(self.parse_list()?))
            } else {
                Ok(SExpr::Atom(self.parse_atom()?))
            }
        } else {
            Err("excepting sexpr, found eof".to_string())
        }
    }

    fn parse_list(&mut self) -> Result<Vec<SExpr>, String> {
        assert!(Parser::is_opening(self.iter.next().unwrap()));
        self.skip_spaces();
        let mut result = vec![];
        while let Some(ch) = self.peek() {
            if Parser::is_closing(ch) {
                break;
            }
            result.push(self.parse_sexpr()?);
        }
        if self.eof() {
            return Err("unclosed list".to_string());
        }
        assert!(Parser::is_closing(self.iter.next().unwrap()));
        Ok(result)
    }

    fn parse_atom(&mut self) -> Result<SAtom, String> {
        self.skip_spaces();
        if self.eof() {
            Err("excepting atom, found eof".to_string())
        } else if let Some(result) = self.try_parse_integer() {
            Ok(SAtom::Integer(result?))
        } else if let Some(result) = self.try_parse_bool_or_char() {
            Ok(result?)
        } else {
            Ok(SAtom::Symbol(self.parse_symbol()))
        }
    }

    fn try_parse_integer(&mut self) -> Option<Result<i64, String>> {
        let ch = self.peek().unwrap();
        if !(ch == '-' || ch == '+' || (ch >= '0' && ch <= '9')) {
            None
        } else {
            let save = self.iter.clone();
            let sign = if ch == '-' { -1 } else { 1 };
            if ch == '-' || ch == '+' {
                // it might be a symbol, e.g. (- x y) instead of -1
                self.iter.next();
                if let Some(ch2) = self.peek() {
                    if !(ch2 >= '0' && ch2 <= '9') {
                        // is a symbol
                        self.iter = save;
                        return None;
                    }
                }
            }
            
            if self.eof() {
                return Some(Err("excepting a digit, found eof".to_string()));
            }

            let mut result = 0i64;
            while let Some(ch) = self.peek() {
                if let Some(d) = ch.to_digit(10) {
                    result = result * 10 + d as i64;
                } else if Parser::is_closing(ch) || Parser::is_opening(ch) || Parser::is_space(ch) {
                    break;
                } else {
                    // not a integer
                    //self.iter = save;
                    //return None;
                    return Some(Err(format!("excepting a digit, found {}", ch)));
                }
                self.iter.next();
            }

            result *= sign;
            Some(Ok(result))
        }
    }

    fn try_parse_bool_or_char(&mut self) -> Option<Result<SAtom, String>> {
        if let Some(ch) = self.peek() {
            if ch != '#' {
                None
            } else {
                self.iter.next();
                let ch = self.peek();
                if let None = ch {
                    return Some(Err("excepting #t, #f or char, found eof".to_string()));
                }
                let ch = ch.unwrap();
                self.iter.next();
                match ch {
                    't' => Some(Ok(SAtom::Bool(true))),
                    'f' => Some(Ok(SAtom::Bool(false))),
                    '\\' => {
                        if let Some(ch) = self.iter.next() {
                            Some(Ok(SAtom::Char(ch)))
                        } else {
                            Some(Err("excepting char, found eof".to_string()))
                        }
                    },
                    _ => Some(Err(format!("excepting #t, #f or char, found {}", ch))),
                }
            }
        } else {None}
    }

    fn parse_symbol(&mut self) -> String {
        let mut result = String::new();
        while let Some(ch) = self.peek() {
            if !Parser::is_space(ch) && !Parser::is_opening(ch) && !Parser::is_closing(ch) {
                result.push(ch);
                self.iter.next();
            } else {
                break;
            }
        }
        result
    }

    fn eof(&mut self) -> bool {
        if let None = self.peek() {
            true
        } else {
            false
        }
    }

    fn skip_spaces(&mut self) {
        while let Some(ch) = self.peek() {
            if Parser::is_space(ch) {
                self.iter.next();
            } else {
                break;
            }
        }
    }

    fn is_space(ch: char) -> bool {
        match ch {
            ' ' | '\t' | '\r' | '\n' => true,
            _ => false
        }
    }

    fn is_opening(ch: char) -> bool {
        match ch {
            '(' | '[' => true,
            _ => false,
        }
    }
    
    fn is_closing(ch: char) -> bool {
        match ch {
            ')' | ']' => true,
            _ => false,
        }
    }

    fn peek(&mut self) -> Option<char> {
        // self.iter.peek returns Option<&char>
        if let Some(ch) = self.iter.peek() {
            Some(*ch)
        } else {
            None
        }
    }
}