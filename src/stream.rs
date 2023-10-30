use std::collections::VecDeque;
use std::iter::{Fuse, FusedIterator};

pub struct Stream<I> where I: Iterator {
    iter: Fuse<I>,
    peeked: VecDeque<I::Item>
}

impl<I> Stream<I> where I: Iterator {
    pub fn new(it: I) -> Self { Self { iter: it.fuse(), peeked: VecDeque::new() }}

    pub fn check_done(&mut self) -> bool {
        if self.peeked.is_empty() {
            self.peek().is_none()
        } else {
            false
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        self.peek_index(0)
    }

    pub fn peek_index(&mut self, n: usize) -> Option<&I::Item> {
        while n >= self.peeked.len() {
            // dbg!(&self.peeked.len());
            if let Some(item) = self.iter.next() {
                self.peeked.push_back(item);
            } else {
                return None;
            }
        }
        Some(&self.peeked[n])
    }

    pub fn peek_slice(&mut self, n: usize) -> &[I::Item] {
        if n == 0 {
            return &[];
        }
        self.peek_index(n - 1);
        if n < self.peeked.len() {
            &self.peeked.make_contiguous()[..n]
        } else {
            self.peeked.make_contiguous()
        }
    }

    pub fn advance_stream(&mut self, n: usize) -> usize {
        for i in 0..n {
            if self.next().is_none() {
                return i;
            }
        }
        return n;
    }

    pub fn next_vec(&mut self, n: usize) -> Vec<I::Item> {
        if n == 0 {
            return vec![];
        }
        self.peek_index(n - 1);
        if n < self.peeked.len() {
            self.peeked.drain(..n).collect()
        } else {
            self.peeked.drain(..).collect()
        }
    }
}

impl<I> Stream<I> where I: Iterator<Item=char> {
    pub fn peek_string(&mut self, len: usize) -> String {
        String::from_iter(self.peek_slice(len))
    }

    pub fn next_string(&mut self, len: usize) -> String {
        String::from_iter(self.next_vec(len))
    }

    pub fn startswith(&mut self, chars: impl AsRef<str>) -> bool {
        let chars = chars.as_ref();
        for (i, chr) in chars.char_indices() {
            if !self.peek_index(i).is_some_and(|c| c == &chr) {
                return false;
            }
        }
        return true;
    }
}

impl<I> Iterator for Stream<I> where I: Iterator {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.peeked.is_empty() {
            self.iter.next()
        } else {
            self.peeked.pop_front()
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<I> FusedIterator for Stream<I> where I: Iterator { }
impl<I> ExactSizeIterator for Stream<I> where I: ExactSizeIterator { }

impl<I> Clone for Stream<I> where I: Iterator + Clone, I::Item: Clone {
    fn clone(&self) -> Self {
        Stream { iter: self.iter.clone(), peeked: self.peeked.clone() }
    }
}