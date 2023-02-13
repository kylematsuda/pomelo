//! Oppen-style pretty printer.
//!
//! See crate-level docs for some helpful resources.
use std::borrow::Cow;
use std::collections::VecDeque;
use std::ops::{Index, IndexMut};

const LINE_WIDTH: usize = 80;
const NEWLINE: &str = "\n";

/// Determines formatting for a group.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Breaks {
    Consistent,
    Inconsistent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
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
    pub fn len(&self) -> Option<usize> {
        match self {
            Self::Text(s) => Some(s.len()),
            _ => None,
        }
    }
}

const MAX_BLANKS: usize = 0xFFFF;
const LINEBREAK: Token = Token::Break {
    blank_spaces: MAX_BLANKS,
    overflow_indent: 0,
};

#[derive(Debug, Clone)]
pub struct Buffer<T> {
    offset: usize,
    storage: VecDeque<T>,
}

impl<T> Buffer<T> {
    pub fn new() -> Self {
        Self {
            offset: 0,
            storage: VecDeque::new(),
        }
    }

    pub fn push_right(&mut self, elem: T) {
        self.storage.push_back(elem)
    }

    pub fn right_elem(&self) -> Option<&T> {
        self.storage.back()
    }

    pub fn right(&self) -> usize {
        self.storage.len() - 1
    }

    pub fn left(&self) -> usize {
        self.offset
    }

    pub fn left_elem(&self) -> Option<&T> {
        self.storage.front()
    }

    pub fn pop_left(&mut self) -> Option<T> {
        self.offset += 1;
        self.storage.pop_front()
    }

    pub fn clear(&mut self) {
        self.offset = 0;
        self.storage.clear();
    }

    pub fn len(&self) -> usize {
        self.storage.len()
    }
}

impl<T> Index<usize> for Buffer<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.storage[index.checked_sub(self.offset).unwrap()]
    }
}

impl<T> IndexMut<usize> for Buffer<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.storage[index.checked_sub(self.offset).unwrap()]
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
    const INFINITY: isize = 0xFFFF;

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

    pub fn advance_left(&mut self) {
        if self.buffer.left_elem().unwrap().size < 0 {
            return;
        }

        let BufElt { token, size } = self.buffer.pop_left().unwrap();
        self.left_total += match &token {
            Token::Break { blank_spaces, .. } => *blank_spaces as isize,
            Token::Text(s) => s.len() as isize,
            // TODO: figure out what to do here?
            // probably just a size of 0?
            _ => unreachable!(),
        };
        self.print(token, size);

        if self.buffer.len() != 0 {
            self.advance_left();
        }
    }

    pub fn indent(&mut self, n: usize) {
        for _ in 0..n {
            self.output.push(' ');
        }
    }

    pub fn newline(&mut self) {
        self.output.push_str(NEWLINE);
    }

    pub fn add_space_break(&mut self, blank_spaces: usize) {
        self.space -= blank_spaces as isize;
        self.indent(blank_spaces);
    }

    pub fn add_line_break(&mut self, print_stack_offset: usize, overflow_indent: usize) {
        self.space = print_stack_offset as isize - overflow_indent as isize;
        self.newline();
        self.indent((self.margin - self.space) as usize);
    }

    pub fn print(&mut self, token: Token, size: isize) {
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
            },
            Token::Text(s) => {
                // TODO: don't panic here!
                if size > self.space {
                    panic!("line to long")
                }
                self.space -= size;
                self.output.push_str(&s);
            }
        }
    }

    pub fn check_stack(&mut self, k: isize) {
        if !self.scan_stack.is_empty() {
            return;
        }

        // Try to understand why this is top() and not pop() in the Oppen paper..
        let x = self.scan_stack.pop_back().unwrap();
        match self.buffer[x].token {
            Token::Begin { .. } => {
                if k > 0 {
                    self.buffer[x].size += self.right_total;
                    self.check_stack(k - 1);
                }
            }
            Token::End => {
                self.buffer[x].size = 1;
                self.check_stack(k + 1);
            }
            _ => {
                self.buffer[x].size += self.right_total;
                if k > 0 {
                    self.check_stack(k);
                }
            }
        }
    }

    pub fn check_stream(&mut self) {
        if self.right_total - self.left_total > self.space {
            if let Some(left) = self.scan_stack.front() {
                if *left == self.buffer.left() {
                    let left = self.scan_stack.pop_front().unwrap();
                    self.buffer[left].size = Self::INFINITY;
                }
            }
            self.advance_left();
            if self.buffer.len() != 0 {
                self.check_stream();
            }
        }
    }

    pub fn print_token(&mut self, token: Option<Token>) {
        if token.is_none() {
            todo!()
        }

        let t = token.unwrap();
        match t {
            Token::Begin { .. } => {
                if self.scan_stack.is_empty() {
                    self.left_total = 1;
                    self.right_total = 1;
                    self.buffer.clear();
                }
                self.buffer.push_right((t, -self.right_total).into());
                self.scan_stack.push_back(self.buffer.right());
            }
            Token::End => {
                if self.scan_stack.is_empty() {
                    self.print(t, 0);
                } else {
                    self.buffer.push_right((t, -1).into());
                    self.scan_stack.push_back(self.buffer.right());
                }
            }
            Token::Break { blank_spaces, .. } => {
                if self.scan_stack.is_empty() {
                    self.left_total = 1;
                    self.right_total = 1;
                    self.buffer.clear();
                }
                self.check_stack(0);
                self.scan_stack.push_back(self.buffer.right());
                self.buffer.push_right((t, -self.right_total).into());
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
}

#[cfg(test)]
mod tests {

}
