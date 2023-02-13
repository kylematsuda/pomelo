//! Oppen-style pretty printer.
//!
//! See crate-level docs for some helpful resources.
use std::borrow::Cow;
use std::collections::VecDeque;

use crate::buffer::Buffer;

pub const LINE_WIDTH: usize = 80;

const INFINITY: isize = 0xFFFF;
const NEWLINE: &str = "\n";

const MAX_BLANKS: usize = 0xFFFF;
const LINEBREAK: Token = Token::Break {
    blank_spaces: MAX_BLANKS,
    overflow_indent: 0,
};

/// Determines formatting for a group.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Breaks {
    Consistent,
    Inconsistent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Text(Cow<'static, str>),
    Break {
        /// Spaces to insert if not a linebreak
        blank_spaces: usize,
        /// Indent if we overflow onto a new line
        overflow_indent: usize,
    },
    Begin {
        /// Indent for this group
        indent: usize,
        /// Consistent or inconsistent breaking for this group
        breaks: Breaks,
    },
    End,
}

impl Token {
    fn len(&self) -> Option<usize> {
        match self {
            Self::Text(s) => Some(s.len()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BufElt {
    token: Token,
    size: isize,
}

impl From<(Token, isize)> for BufElt {
    fn from(value: (Token, isize)) -> Self {
        Self {
            token: value.0,
            size: value.1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PrintStackEntry {
    offset: usize,
    // `None` means it fits
    breaks: Option<Breaks>,
}

impl From<(usize, Option<Breaks>)> for PrintStackEntry {
    fn from(value: (usize, Option<Breaks>)) -> Self {
        Self {
            offset: value.0,
            breaks: value.1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Printer {
    margin: isize,
    space: isize,
    left_total: isize,
    right_total: isize,
    buffer: Buffer<BufElt>,
    /// Stack of pointers to the start of the current group in the buffer
    scan_stack: VecDeque<usize>,
    print_stack: Vec<PrintStackEntry>,
    output: String,
}

impl Printer {
    pub fn new(line_width: usize) -> Self {
        Self {
            margin: line_width as isize,
            space: line_width as isize,
            left_total: 1,
            right_total: 1,
            buffer: Buffer::new(),
            scan_stack: VecDeque::new(),
            print_stack: Vec::new(),
            output: String::new(),
        }
    }

    pub fn output(mut self) -> String {
        if !self.scan_stack.is_empty() {
            self.check_stack(0);
            self.advance_left();
        }
        self.output
    }

    pub fn text(&mut self, s: impl Into<Cow<'static, str>>) {
        self.scan(Token::Text(s.into()))
    }

    pub fn cgroup(&mut self, indent: usize) {
        self.scan(Token::Begin { indent, breaks: Breaks::Consistent })
    }

    pub fn igroup(&mut self, indent: usize) {
        self.scan(Token::Begin { indent, breaks: Breaks::Inconsistent })
    }

    pub fn endgroup(&mut self) {
        self.scan(Token::End)
    }

    pub fn linebreak(&mut self) {
        self.scan(LINEBREAK)
    }

    pub fn zerobreak(&mut self) {
        self.scan(Token::Break { blank_spaces: 0, overflow_indent: 0 })
    }

    pub fn space(&mut self) {
        self.scan(Token::Break { blank_spaces: 1, overflow_indent: 0 })
    }
}

impl Printer {
    fn scan(&mut self, t: Token) {
        match t {
            Token::Begin { .. } => {
                if self.scan_stack.is_empty() {
                    self.left_total = 1;
                    self.right_total = 1;
                    self.buffer.clear();
                }
                let right = self.buffer.push_right((t, -self.right_total).into());
                self.scan_stack.push_back(right);
            }
            Token::End => {
                if self.scan_stack.is_empty() {
                    self.print(t, 0);
                } else {
                    let right = self.buffer.push_right((t, -1).into());
                    self.scan_stack.push_back(right);
                }
            }
            Token::Break { blank_spaces, .. } => {
                if self.scan_stack.is_empty() {
                    self.left_total = 1;
                    self.right_total = 1;
                    self.buffer.clear();
                }
                self.check_stack(0);
                let right = self.buffer.push_right((t, -self.right_total).into());
                self.scan_stack.push_back(right);
                self.right_total += blank_spaces as isize;
            }
            Token::Text(_) => {
                let l = t.len().unwrap() as isize;
                if self.scan_stack.is_empty() {
                    self.print(t, l);
                } else {
                    self.buffer.push_right((t, l).into());
                    self.right_total += l;
                    self.check_stream();
                }
            }
        }
    }

    fn print(&mut self, token: Token, size: isize) {
        match token {
            Token::Begin { indent, breaks } => {
                if size > self.space {
                    self.print_stack
                        .push((self.space as usize - indent, Some(breaks)).into());
                } else {
                    self.print_stack.push((0, None).into());
                }
            }
            Token::End => {
                self.print_stack.pop();
            }
            Token::Break {
                blank_spaces,
                overflow_indent,
            } => {
                let stack_top = self.print_stack.last().expect("print_stack is nonempty");
                match stack_top.breaks {
                    None => self.add_space_break(blank_spaces),
                    Some(Breaks::Consistent) => {
                        self.add_line_break(stack_top.offset, overflow_indent)
                    }
                    Some(Breaks::Inconsistent) => {
                        if size > self.space {
                            self.add_line_break(stack_top.offset, overflow_indent)
                        } else {
                            self.add_space_break(blank_spaces)
                        }
                    }
                }
            }
            Token::Text(s) => {
                // TODO: don't panic here!
                if size > self.space {
                    panic!("line too long")
                }
                self.space -= size;
                self.output.push_str(&s);
            }
        }
    }

    fn advance_left(&mut self) {
        if self.buffer.left_elem().unwrap().size < 0 {
            return;
        }

        let BufElt { token, size } = self.buffer.pop_left().unwrap();
        self.left_total += match &token {
            Token::Break { blank_spaces, .. } => *blank_spaces as isize,
            Token::Text(_) => size,
            _ => 0,
        };
        self.print(token, size);

        if !self.buffer.is_empty() {
            self.advance_left();
        }
    }

    fn check_stack(&mut self, mut k: isize) {
        while let Some(&x) = self.scan_stack.back() {
            let mut elt = &mut self.buffer[x];
            match elt.token {
                Token::Begin { .. } => {
                    if k <= 0 {
                        break;
                    }
                    self.scan_stack.pop_back().unwrap();
                    elt.size += self.right_total;
                    k -= 1;
                }
                Token::End => {
                    self.scan_stack.pop_back().unwrap();
                    elt.size = 1;
                    k += 1;
                }
                Token::Break { .. } => {
                    self.scan_stack.pop_back().unwrap();
                    elt.size += self.right_total;
                    if k <= 0 {
                        break;
                    }
                }
                Token::Text(_) => unreachable!(),
            }
        }
    }

    fn check_stream(&mut self) {
        while self.right_total - self.left_total > self.space {
            if let Some(left) = self.scan_stack.front() {
                if *left == self.buffer.left() {
                    let left = self.scan_stack.pop_front().unwrap();
                    self.buffer[left].size = INFINITY;
                }
            }
            self.advance_left();
            if self.buffer.is_empty() {
                break;
            }
        }
    }

    fn add_space_break(&mut self, blank_spaces: usize) {
        self.space -= blank_spaces as isize;
        self.insert_indent(blank_spaces);
    }

    fn add_line_break(&mut self, print_stack_offset: usize, overflow_indent: usize) {
        self.space = print_stack_offset as isize - overflow_indent as isize;
        self.insert_newline();
        self.insert_indent((self.margin - self.space) as usize);
    }

    fn insert_indent(&mut self, n: usize) {
        for _ in 0..n {
            self.output.push(' ');
        }
    }

    fn insert_newline(&mut self) {
        self.output.push_str(NEWLINE);
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn basic_test() {
        use super::*;
        use expect_test::expect;

        let src = [
            Token::Begin {
                indent: 0,
                breaks: Breaks::Consistent,
            },
            Token::Begin {
                indent: 0,
                breaks: Breaks::Consistent,
            },
            Token::Text("f(".into()),
            Token::Break {
                blank_spaces: 0,
                overflow_indent: 2,
            },
            Token::Text("a,".into()),
            Token::Break {
                blank_spaces: 1,
                overflow_indent: 2,
            },
            Token::Text("b,".into()),
            Token::Break {
                blank_spaces: 1,
                overflow_indent: 2,
            },
            Token::Text("c,".into()),
            Token::Break {
                blank_spaces: 1,
                overflow_indent: 2,
            },
            Token::Text("d".into()),
            Token::Break {
                blank_spaces: 0,
                overflow_indent: 0,
            },
            Token::Text(")".into()),
            Token::End,
            Token::Break {
                blank_spaces: 1,
                overflow_indent: 2,
            },
            Token::Begin {
                indent: 2,
                breaks: Breaks::Inconsistent,
            },
            Token::Text("+".into()),
            Token::Break {
                blank_spaces: 1,
                overflow_indent: 2,
            },
            Token::Begin {
                indent: 0,
                breaks: Breaks::Consistent,
            },
            Token::Text("g(".into()),
            Token::Break {
                blank_spaces: 0,
                overflow_indent: 2,
            },
            Token::Text("a,".into()),
            Token::Break {
                blank_spaces: 1,
                overflow_indent: 2,
            },
            Token::Text("b,".into()),
            Token::Break {
                blank_spaces: 1,
                overflow_indent: 2,
            },
            Token::Text("c,".into()),
            Token::Break {
                blank_spaces: 1,
                overflow_indent: 2,
            },
            Token::Text("d".into()),
            Token::Break {
                blank_spaces: 0,
                overflow_indent: 0,
            },
            Token::Text(")".into()),
            Token::End,
            Token::End,
            Token::End,
        ];

        let mut printer = Printer::new(20);
        let expected = expect![[r#"
            f(a, b, c, d)
              + g(a, b, c, d)"#]];

        for tok in src.clone() {
            printer.scan(tok);
        }
        let result = printer.output();
        expected.assert_eq(&result);

        let mut printer = Printer::new(40);
        let expected = expect![[r#"
            f(a, b, c, d) + g(a, b, c, d)"#]];

        for tok in src {
            printer.scan(tok);
        }
        let result = printer.output();
        expected.assert_eq(&result);
    }
}
