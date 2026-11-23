// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ops::Range;

pub const SMALL_PIECE: usize = 256;

pub trait Node<'a>: Copy {
    fn bytes(self) -> Option<&'a [u8]>;
    fn byte(self) -> Option<u8>;
    fn is_nil(self) -> bool;
    fn cell(self) -> Option<(Self, Self)>;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Limit {
    pub bytes: usize,
    pub cells: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Refusal {
    Invalid,
    OverLimit,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum Piece<'a> {
    Borrowed(&'a [u8]),
    Copied(Range<usize>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Gathered<'a> {
    pieces: Vec<Piece<'a>>,
    copied: Vec<u8>,
    total: usize,
    cells: usize,
}

impl<'a> Gathered<'a> {
    pub fn total(&self) -> usize {
        self.total
    }

    pub fn cells(&self) -> usize {
        self.cells
    }

    pub fn chunks(&self) -> Vec<&[u8]> {
        self.pieces
            .iter()
            .filter_map(|piece| match piece {
                Piece::Borrowed(bytes) => Some(*bytes),
                Piece::Copied(range) => self.copied.get(range.clone()),
            })
            .collect()
    }

    fn push(&mut self, bytes: &'a [u8]) {
        if bytes.is_empty() {
            return;
        }
        self.total = self.total.saturating_add(bytes.len());
        if bytes.len() >= SMALL_PIECE {
            self.pieces.push(Piece::Borrowed(bytes));
            return;
        }
        let start = self.copied.len();
        self.copied.extend_from_slice(bytes);
        let end = self.copied.len();
        match self.pieces.last_mut() {
            Some(Piece::Copied(range)) if range.end == start => range.end = end,
            _ => self.pieces.push(Piece::Copied(start..end)),
        }
    }

    fn push_byte(&mut self, byte: u8) {
        let start = self.copied.len();
        self.copied.push(byte);
        self.total = self.total.saturating_add(1);
        match self.pieces.last_mut() {
            Some(Piece::Copied(range)) if range.end == start => range.end = start.saturating_add(1),
            _ => self
                .pieces
                .push(Piece::Copied(start..start.saturating_add(1))),
        }
    }

    fn over(&self, limit: Option<Limit>) -> bool {
        limit.is_some_and(|limit| self.total > limit.bytes || self.cells > limit.cells)
    }
}

pub fn gather<'a, N: Node<'a>>(data: N, limit: Option<Limit>) -> Result<Gathered<'a>, Refusal> {
    let mut out = Gathered::default();
    let mut pending = vec![(data, false)];
    while let Some((term, head)) = pending.pop() {
        if let Some(bytes) = term.bytes() {
            out.push(bytes);
        } else if let Some(byte) = term.byte().filter(|_| head) {
            out.push_byte(byte);
        } else if let Some((first, rest)) = term.cell() {
            out.cells = out.cells.saturating_add(1);
            pending.push((rest, false));
            pending.push((first, true));
        } else if !term.is_nil() {
            return Err(Refusal::Invalid);
        }
        if out.over(limit) {
            return Err(Refusal::OverLimit);
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::{Gathered, Limit, Node, Refusal, SMALL_PIECE, gather};

    #[derive(Clone, Debug)]
    enum Tree {
        Bytes(Vec<u8>),
        Int(i64),
        Nil,
        Cons(Box<Tree>, Box<Tree>),
        Atom,
    }

    impl<'a> Node<'a> for &'a Tree {
        fn bytes(self) -> Option<&'a [u8]> {
            match self {
                Tree::Bytes(bytes) => Some(bytes),
                _ => None,
            }
        }

        fn byte(self) -> Option<u8> {
            match self {
                Tree::Int(value) => u8::try_from(*value).ok(),
                _ => None,
            }
        }

        fn is_nil(self) -> bool {
            matches!(self, Tree::Nil)
        }

        fn cell(self) -> Option<(Self, Self)> {
            match self {
                Tree::Cons(head, tail) => Some((head, tail)),
                _ => None,
            }
        }
    }

    fn list(items: Vec<Tree>, tail: Tree) -> Tree {
        items.into_iter().rev().fold(tail, |rest, item| {
            Tree::Cons(Box::new(item), Box::new(rest))
        })
    }

    fn bin(text: &str) -> Tree {
        Tree::Bytes(text.as_bytes().to_vec())
    }

    fn flat(found: &Gathered<'_>) -> Vec<u8> {
        found.chunks().concat()
    }

    fn json_array(count: usize) -> Tree {
        let mut items = vec![Tree::Int(91)];
        for i in 0..count {
            if i > 0 {
                items.push(Tree::Int(44));
            }
            items.push(bin(&i.to_string()));
        }
        items.push(Tree::Int(93));
        list(items, Tree::Nil)
    }

    #[test]
    fn iodata_with_byte_integers_is_gathered_in_order() {
        let tree = list(
            vec![
                list(vec![bin("\""), bin("a")], bin("\\\"")),
                bin("b"),
                Tree::Int(34),
            ],
            Tree::Nil,
        );
        let found = gather(&tree, None).unwrap();
        assert_eq!(flat(&found), b"\"a\\\"b\"");
        assert_eq!(found.total(), 6);
        assert_eq!(found.chunks().len(), 1);
        let array = json_array(3);
        assert_eq!(flat(&gather(&array, None).unwrap()), b"[0,1,2]");
    }

    #[test]
    fn large_binaries_are_borrowed_and_small_runs_are_copied_once() {
        let big = Tree::Bytes(vec![b'x'; SMALL_PIECE * 4]);
        let held = big.bytes().map(<[u8]>::as_ptr);
        let tree = list(
            vec![Tree::Int(123), bin("\"k\":"), big, Tree::Int(125)],
            Tree::Nil,
        );
        let found = gather(&tree, None).unwrap();
        let chunks = found.chunks();
        assert_eq!(chunks.len(), 3);
        assert_eq!(chunks.first().copied(), Some(&b"{\"k\":"[..]));
        assert_eq!(chunks.get(1).map(|c| c.as_ptr()), held);
        assert_eq!(chunks.get(2).copied(), Some(&b"}"[..]));
    }

    #[test]
    fn improper_tails_and_bad_terms() {
        let tail_binary = list(vec![bin("a")], bin("b"));
        assert_eq!(flat(&gather(&tail_binary, None).unwrap()), b"ab");
        let tail_integer = list(vec![bin("a")], Tree::Int(98));
        assert_eq!(gather(&tail_integer, None), Err(Refusal::Invalid));
        let top_integer = Tree::Int(97);
        assert_eq!(gather(&top_integer, None), Err(Refusal::Invalid));
        let wide = list(vec![Tree::Int(256)], Tree::Nil);
        assert_eq!(gather(&wide, None), Err(Refusal::Invalid));
        let negative = list(vec![Tree::Int(-1)], Tree::Nil);
        assert_eq!(gather(&negative, None), Err(Refusal::Invalid));
        let atom = list(vec![Tree::Atom], Tree::Nil);
        assert_eq!(gather(&atom, None), Err(Refusal::Invalid));
        assert_eq!(flat(&gather(&Tree::Nil, None).unwrap()), b"");
    }

    #[test]
    fn a_limit_stops_the_walk_before_a_bad_tail() {
        let tree = list(vec![json_array(2_000), Tree::Atom], Tree::Nil);
        assert_eq!(gather(&tree, None), Err(Refusal::Invalid));
        let limit = Limit {
            bytes: 1_024,
            cells: 64 * 1024,
        };
        assert_eq!(gather(&tree, Some(limit)), Err(Refusal::OverLimit));
        let small = json_array(10);
        let found = gather(&small, Some(limit)).unwrap();
        assert_eq!(found.cells(), 21);
    }

    #[test]
    fn many_empty_cells_hit_the_cell_limit() {
        let tree = list((0..4_000).map(|_| Tree::Nil).collect(), Tree::Nil);
        let limit = Limit {
            bytes: 64 * 1024,
            cells: 1_000,
        };
        assert_eq!(gather(&tree, Some(limit)), Err(Refusal::OverLimit));
        assert_eq!(gather(&tree, None).unwrap().total(), 0);
    }
}
