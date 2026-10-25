// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::text::advance_one;

#[derive(Clone, Copy, Debug)]
pub(crate) enum Token {
    Close,
    Fail,
    Open(usize),
    Skip(usize),
    Step(usize),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct Closer {
    pub(crate) at: usize,
    last_checked: Option<usize>,
}

impl Closer {
    fn or_checked(self, landing: Option<usize>) -> Self {
        Self {
            at: self.at,
            last_checked: self.last_checked.or(landing),
        }
    }

    pub(crate) fn within(self, opener: usize, limit: usize) -> Option<usize> {
        self.last_checked
            .is_none_or(|landing| landing - opener <= limit)
            .then_some(self.at)
    }
}

#[derive(Clone, Copy, Debug)]
enum Frame {
    Then { at: usize, landing: Option<usize> },
    Inner { at: usize, inner: usize },
    Outer { at: usize, last_checked: usize },
}

#[derive(Clone, Copy, Debug, Default)]
struct Slot {
    state: u32,
    last_checked: u32,
}

#[derive(Clone, Debug, Default)]
struct ClosingMemo {
    slots: Vec<Slot>,
    frames: Vec<Frame>,
}

impl ClosingMemo {
    fn get(&self, position: usize) -> Option<Option<Closer>> {
        let slot = self.slots[position];
        match slot.state {
            0 => None,
            1 => Some(None),
            state => Some(Some(Closer {
                at: state as usize - 2,
                last_checked: slot.last_checked.checked_sub(1).map(|value| value as usize),
            })),
        }
    }

    fn set(&mut self, position: usize, value: Option<Closer>) {
        let slot = match value {
            None => Slot {
                state: 1,
                last_checked: 0,
            },
            Some(closer) => Slot {
                state: closer.at as u32 + 2,
                last_checked: closer.last_checked.map_or(0, |value| value as u32 + 1),
            },
        };
        self.slots[position] = slot;
    }

    fn resolve(
        &mut self,
        len: usize,
        start: usize,
        close_len: usize,
        mut classify: impl FnMut(usize) -> Token,
    ) -> Option<Closer> {
        if self.slots.is_empty() {
            self.slots = vec![Slot::default(); len + 1];
        }
        let mut frames = std::mem::take(&mut self.frames);
        let mut position = start;
        let mut result;
        'descend: loop {
            result = loop {
                if position >= len {
                    break None;
                }
                if let Some(known) = self.get(position) {
                    break known;
                }
                match classify(position) {
                    Token::Close => {
                        let closer = Some(Closer {
                            at: position,
                            last_checked: None,
                        });
                        self.set(position, closer);
                        break closer;
                    }
                    Token::Fail => {
                        self.set(position, None);
                        break None;
                    }
                    Token::Skip(advance) => {
                        frames.push(Frame::Then {
                            at: position,
                            landing: None,
                        });
                        position += advance;
                    }
                    Token::Step(advance) => {
                        frames.push(Frame::Then {
                            at: position,
                            landing: Some(position + advance),
                        });
                        position += advance;
                    }
                    Token::Open(advance) => {
                        frames.push(Frame::Inner {
                            at: position,
                            inner: position + advance,
                        });
                        position += advance;
                    }
                }
            };
            while let Some(frame) = frames.pop() {
                match frame {
                    Frame::Then { at, landing } => {
                        result = result.map(|closer| closer.or_checked(landing));
                        self.set(at, result);
                    }
                    Frame::Inner { at, inner } => match result {
                        None => self.set(at, None),
                        Some(closer) => {
                            frames.push(Frame::Outer {
                                at,
                                last_checked: closer.last_checked.unwrap_or(inner),
                            });
                            position = closer.at + close_len;
                            continue 'descend;
                        }
                    },
                    Frame::Outer { at, last_checked } => {
                        result = result.map(|closer| closer.or_checked(Some(last_checked)));
                        self.set(at, result);
                    }
                }
            }
            break;
        }
        self.frames = frames;
        result
    }
}

const MIN_UNINDEXED_STEPS: usize = 256;

fn scan_briefly(
    len: usize,
    start: usize,
    close_len: usize,
    steps: &mut usize,
    classify: &mut impl FnMut(usize) -> Token,
) -> Option<Option<Closer>> {
    let mut position = start;
    let mut depth = 0usize;
    let mut last_checked = None;
    while *steps > 0 {
        *steps -= 1;
        if position >= len {
            return Some(None);
        }
        match classify(position) {
            Token::Close if depth == 0 => {
                return Some(Some(Closer {
                    at: position,
                    last_checked,
                }));
            }
            Token::Close => {
                depth -= 1;
                position += close_len;
            }
            Token::Fail => return Some(None),
            Token::Skip(advance) => position += advance,
            Token::Open(advance) => {
                depth += 1;
                position += advance;
                last_checked = Some(position);
            }
            Token::Step(advance) => {
                position += advance;
                last_checked = Some(position);
            }
        }
    }
    None
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Pattern {
    AngleOpen,
    AngleClose,
    At,
    OpenBracket,
    CloseBracket,
    OpenParen,
    Paren,
    UrlTerminator,
    PathDelimiter,
    AppProtocolInvalid,
    HttpScheme,
    NestedHttpUrl,
    AngleSyntax,
    DoubledMarker,
}

const PATTERNS: usize = 14;

pub(crate) const ANGLE_SYNTAX_PREFIXES: [&str; 9] =
    ["<:", "<a:", "<id:", "<@", "<#", "</", "<t:", "<+", "<sms:"];

impl Pattern {
    fn find(self, bytes: &[u8], from: usize, end: usize) -> Option<usize> {
        let mut window = bytes[from..end].iter();
        let offset = match self {
            Self::AngleOpen => window.position(|byte| *byte == b'<'),
            Self::AngleClose => window.position(|byte| *byte == b'>'),
            Self::At => window.position(|byte| *byte == b'@'),
            Self::OpenBracket => window.position(|byte| *byte == b'['),
            Self::CloseBracket => window.position(|byte| *byte == b']'),
            Self::OpenParen => window.position(|byte| *byte == b'('),
            Self::Paren => window.position(|byte| matches!(byte, b'(' | b')')),
            Self::UrlTerminator => window.position(|byte| is_url_terminator(*byte)),
            Self::PathDelimiter => window.position(|byte| matches!(byte, b'/' | b'?' | b'#')),
            _ => (from..end).position(|position| self.matches(bytes, position)),
        };
        offset.map(|offset| from + offset)
    }

    fn matches(self, bytes: &[u8], position: usize) -> bool {
        let byte = bytes[position];
        let rest = &bytes[position..];
        match self {
            Self::AngleOpen => byte == b'<',
            Self::AngleClose => byte == b'>',
            Self::At => byte == b'@',
            Self::OpenBracket => byte == b'[',
            Self::CloseBracket => byte == b']',
            Self::OpenParen => byte == b'(',
            Self::Paren => matches!(byte, b'(' | b')'),
            Self::UrlTerminator => is_url_terminator(byte),
            Self::PathDelimiter => matches!(byte, b'/' | b'?' | b'#'),
            Self::AppProtocolInvalid => matches!(
                byte,
                b'"' | b'\'' | b'<' | b'>' | b'\\' | b'|' | b' ' | b'\t' | b'\r' | b'\n'
            ),
            Self::HttpScheme => byte == b'h' && http_scheme_len(rest).is_some(),
            Self::NestedHttpUrl => byte == b'<' && http_scheme_len(&rest[1..]).is_some(),
            Self::AngleSyntax => {
                byte == b'<'
                    && ANGLE_SYNTAX_PREFIXES
                        .iter()
                        .any(|prefix| rest.starts_with(prefix.as_bytes()))
            }
            Self::DoubledMarker => {
                matches!(byte, b'_' | b'*' | b'~' | b'|') && rest.get(1) == Some(&byte)
            }
        }
    }
}

pub(crate) fn http_scheme_len(bytes: &[u8]) -> Option<usize> {
    if bytes.starts_with(b"http://") {
        Some(7)
    } else if bytes.starts_with(b"https://") {
        Some(8)
    } else {
        None
    }
}

#[derive(Clone, Debug, Default)]
struct NextMatch {
    next: Vec<u32>,
}

impl NextMatch {
    fn find(&mut self, bytes: &[u8], from: usize, limit: usize, pattern: Pattern) -> Option<usize> {
        let len = bytes.len();
        if self.next.is_empty() {
            self.next = vec![0; len];
        }
        let mut position = from;
        let found = loop {
            if position == len {
                break len;
            }
            let known = self.next[position];
            if known != 0 {
                break known as usize - 1;
            }
            if pattern.matches(bytes, position) {
                break position;
            }
            position += 1;
        };
        let stored = found as u32 + 1;
        self.next[from..position].fill(stored);
        if found == position && found < len {
            self.next[found] = stored;
        }
        (found < limit).then_some(found)
    }
}

enum ChainLink {
    Hit(usize),
    Next(usize),
    End,
}

#[derive(Clone, Debug, Default)]
struct ChainMemo {
    hits: Vec<u32>,
    chain: Vec<usize>,
}

impl ChainMemo {
    fn resolve(
        &mut self,
        len: usize,
        first: usize,
        mut link: impl FnMut(usize) -> ChainLink,
    ) -> Option<usize> {
        if self.hits.is_empty() {
            self.hits = vec![0; len];
        }
        self.chain.clear();
        let mut node = first;
        let found = loop {
            match self.hits[node] {
                0 => {}
                1 => break None,
                known => break Some(known as usize - 2),
            }
            self.chain.push(node);
            match link(node) {
                ChainLink::Hit(at) => break Some(at),
                ChainLink::Next(next) => node = next,
                ChainLink::End => break None,
            }
        };
        let stored = found.map_or(1, |at| at as u32 + 2);
        for &node in &self.chain {
            self.hits[node] = stored;
        }
        found
    }
}

fn is_url_terminator(byte: u8) -> bool {
    matches!(byte, b' ' | b'\t' | b'\n' | b'\r' | b'"')
}

pub(crate) fn step(text: &str, position: usize) -> Token {
    Token::Step(advance_one(text, position))
}

pub(crate) const FORMATTING_SCANS: usize = 9;

#[derive(Clone, Copy, Debug)]
pub(crate) enum Closing {
    Formatting(usize),
    Bracket,
    Paren,
    UrlParen,
}

impl Closing {
    fn index(self) -> usize {
        match self {
            Self::Formatting(index) => index,
            Self::Bracket => FORMATTING_SCANS,
            Self::Paren => FORMATTING_SCANS + 1,
            Self::UrlParen => FORMATTING_SCANS + 2,
        }
    }
}

const CLOSINGS: usize = FORMATTING_SCANS + 3;

#[derive(Clone, Debug, Default)]
struct Memos {
    formatting: [ClosingMemo; FORMATTING_SCANS],
    brackets: ClosingMemo,
    parens: ClosingMemo,
    url_parens: ClosingMemo,
    next: [NextMatch; PATTERNS],
    link_syntax: ChainMemo,
    angle_email: ChainMemo,
}

impl Memos {
    fn closing(&mut self, closing: Closing) -> &mut ClosingMemo {
        match closing {
            Closing::Formatting(index) => &mut self.formatting[index],
            Closing::Bracket => &mut self.brackets,
            Closing::Paren => &mut self.parens,
            Closing::UrlParen => &mut self.url_parens,
        }
    }
}

#[derive(Clone, Debug, Default)]
struct CodeSpanScan {
    start: usize,
    frontier: usize,
    ended: bool,
    longest_runs: Vec<(usize, usize)>,
}

#[derive(Clone, Debug)]
pub(crate) struct Lookahead<'a> {
    pub(crate) text: &'a str,
    backtick_run: (usize, usize),
    asterisk_run: (usize, usize),
    code_span: CodeSpanScan,
    closing_steps: [usize; CLOSINGS],
    pattern_steps: [usize; PATTERNS],
    memos: Option<Box<Memos>>,
}

impl<'a> Lookahead<'a> {
    pub(crate) fn new(text: &'a str) -> Self {
        Self {
            text,
            backtick_run: (0, 0),
            asterisk_run: (0, 0),
            code_span: CodeSpanScan::default(),
            closing_steps: [text.len() + MIN_UNINDEXED_STEPS; CLOSINGS],
            pattern_steps: [text.len() + MIN_UNINDEXED_STEPS; PATTERNS],
            memos: None,
        }
    }

    fn memos(&mut self) -> &mut Memos {
        self.memos.get_or_insert_with(Box::default)
    }

    pub(crate) fn next(&mut self, pattern: Pattern, from: usize, limit: usize) -> Option<usize> {
        if from >= limit {
            return None;
        }
        let bytes = self.text.as_bytes();
        let steps = &mut self.pattern_steps[pattern as usize];
        if *steps > 0 {
            let brief_end = limit.min(from + *steps);
            let found = pattern.find(bytes, from, brief_end);
            *steps -= found.map_or(brief_end, |found| found + 1) - from;
            if found.is_some() || brief_end == limit {
                return found;
            }
        }
        self.memos().next[pattern as usize].find(bytes, from, limit, pattern)
    }

    pub(crate) fn closer(
        &mut self,
        closing: Closing,
        start: usize,
        close_len: usize,
        mut classify: impl FnMut(&mut Self, usize) -> Token,
    ) -> Option<Closer> {
        let len = self.text.len();
        let mut steps = self.closing_steps[closing.index()];
        if steps > 0 {
            let result = scan_briefly(len, start, close_len, &mut steps, &mut |position| {
                classify(self, position)
            });
            self.closing_steps[closing.index()] = steps;
            if let Some(result) = result {
                return result;
            }
        }
        let mut memo = std::mem::take(self.memos().closing(closing));
        let result = memo.resolve(len, start, close_len, |position| classify(self, position));
        *self.memos().closing(closing) = memo;
        result
    }

    pub(crate) fn run_end(&mut self, position: usize, byte: u8) -> usize {
        let cache = if byte == b'`' {
            &mut self.backtick_run
        } else {
            &mut self.asterisk_run
        };
        let (start, end) = *cache;
        if start <= position && position < end {
            return end;
        }
        let bytes = self.text.as_bytes();
        let mut end = position;
        while end < bytes.len() && bytes[end] == byte {
            end += 1;
        }
        *cache = (position, end);
        end
    }

    pub(crate) fn code_span_end(
        &mut self,
        opener: usize,
        marker_length: usize,
        limit: usize,
    ) -> Option<usize> {
        let start = opener + marker_length;
        if self.code_span.start != start || self.code_span.frontier < start {
            self.code_span = CodeSpanScan {
                start,
                frontier: start,
                ended: false,
                longest_runs: Vec::new(),
            };
        }
        let bytes = self.text.as_bytes();
        loop {
            let scan = &mut self.code_span;
            let index = scan
                .longest_runs
                .partition_point(|(count, _)| *count < marker_length);
            if let Some(&(count, run_start)) = scan.longest_runs.get(index) {
                return (run_start - opener <= limit)
                    .then(|| run_start + count - marker_length - opener);
            }
            if scan.ended || scan.frontier - opener > limit {
                return None;
            }
            let Some(offset) = bytes[scan.frontier..]
                .iter()
                .position(|byte| matches!(byte, b'`' | b'\n' | b'\r'))
            else {
                scan.ended = true;
                continue;
            };
            let run_start = scan.frontier + offset;
            if bytes[run_start] != b'`' {
                scan.ended = true;
                continue;
            }
            let mut run_end = run_start;
            while run_end < bytes.len() && bytes[run_end] == b'`' {
                run_end += 1;
            }
            let count = run_end - run_start;
            if scan
                .longest_runs
                .last()
                .is_none_or(|(longest, _)| count > *longest)
            {
                scan.longest_runs.push((count, run_start));
            }
            scan.frontier = run_end;
        }
    }

    pub(crate) fn closing_bracket(&mut self, opener: usize) -> Option<usize> {
        let text = self.text;
        let bytes = text.as_bytes();
        self.closer(Closing::Bracket, opener + 1, 1, |_, position| {
            match bytes[position] {
                b'[' => Token::Open(1),
                b']' => Token::Close,
                b'\\' if position + 1 < bytes.len() => Token::Skip(2),
                _ => step(text, position),
            }
        })
        .map(|closer| closer.at)
    }

    pub(crate) fn closing_paren(&mut self, start: usize) -> Option<usize> {
        let bytes = self.text.as_bytes();
        self.closer(Closing::Paren, start, 1, |_, position| {
            match bytes[position] {
                b'(' => Token::Open(1),
                b')' => Token::Close,
                _ => Token::Step(1),
            }
        })
        .map(|closer| closer.at)
    }

    pub(crate) fn url_stop(&mut self, start: usize) -> usize {
        let bytes = self.text.as_bytes();
        let terminator = self
            .next(Pattern::UrlTerminator, start, bytes.len())
            .unwrap_or(bytes.len());
        if self.next(Pattern::Paren, start, terminator).is_none() {
            return terminator;
        }
        self.closer(Closing::UrlParen, start, 1, |_, position| {
            match bytes[position] {
                b'(' => Token::Open(1),
                b')' => Token::Close,
                byte if is_url_terminator(byte) => Token::Fail,
                _ => Token::Step(1),
            }
        })
        .map_or(terminator, |closer| closer.at)
    }

    pub(crate) fn has_link_syntax(&mut self, start: usize, end: usize) -> bool {
        let Some(first) = self.next(Pattern::OpenBracket, start, end) else {
            return false;
        };
        let bytes = self.text.as_bytes();
        let len = bytes.len();
        let mut link_syntax = std::mem::take(&mut self.memos().link_syntax);
        let found = link_syntax.resolve(len, first, |open| {
            let Some(close) = self.next(Pattern::CloseBracket, open + 1, len) else {
                return ChainLink::End;
            };
            if bytes.get(close + 1) == Some(&b'(') {
                return ChainLink::Hit(close);
            }
            self.next(Pattern::OpenBracket, close + 1, len)
                .map_or(ChainLink::End, ChainLink::Next)
        });
        self.memos().link_syntax = link_syntax;
        found.is_some_and(|close| close + 1 < end)
    }

    pub(crate) fn has_angle_email(&mut self, start: usize, end: usize) -> bool {
        let Some(first) = self.next(Pattern::AngleOpen, start, end) else {
            return false;
        };
        let bytes = self.text.as_bytes();
        let len = bytes.len();
        let mut angle_email = std::mem::take(&mut self.memos().angle_email);
        let found = angle_email.resolve(len, first, |open| {
            let Some(close) = self.next(Pattern::AngleClose, open + 1, len) else {
                return ChainLink::End;
            };
            if close > open + 1
                && bytes[open + 1] != b'@'
                && self.next(Pattern::At, open + 1, close).is_some()
            {
                return ChainLink::Hit(close);
            }
            self.next(Pattern::AngleOpen, close + 1, len)
                .map_or(ChainLink::End, ChainLink::Next)
        });
        self.memos().angle_email = angle_email;
        found.is_some_and(|close| close < end)
    }
}
