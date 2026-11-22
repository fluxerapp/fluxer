// SPDX-License-Identifier: AGPL-3.0-or-later

#![forbid(unsafe_code)]
#![deny(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects
)]

use std::ops::Range;

#[cfg(feature = "nif")]
pub mod nifs;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Malformed;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Key {
    Plain(Range<usize>),
    Escaped(Range<usize>),
}

impl Key {
    pub fn range(&self) -> Range<usize> {
        match self {
            Key::Plain(range) | Key::Escaped(range) => range.clone(),
        }
    }

    pub fn matches(&self, bytes: &[u8], wanted: &[u8]) -> bool {
        match self {
            Key::Plain(range) => bytes.get(range.clone()) == Some(wanted),
            Key::Escaped(range) => bytes
                .get(range.clone())
                .and_then(|raw| unescape(raw).ok())
                .is_some_and(|text| text.as_bytes() == wanted),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Member {
    pub key: Key,
    pub value: Range<usize>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Location {
    Found {
        value: Range<usize>,
        cut: Range<usize>,
    },
    Absent {
        close: usize,
        empty: bool,
    },
}

pub const MAX_DEPTH: usize = 256;

#[derive(Default)]
struct Stack {
    words: [u64; MAX_DEPTH / 64],
    depth: usize,
}

impl Stack {
    fn push(&mut self, object: bool) -> Result<(), Malformed> {
        let word = self.words.get_mut(self.depth / 64).ok_or(Malformed)?;
        let bit = 1u64 << (self.depth % 64);
        *word = if object { *word | bit } else { *word & !bit };
        self.depth = self.depth.saturating_add(1);
        Ok(())
    }

    fn pop(&mut self) -> Result<bool, Malformed> {
        self.depth = self.depth.checked_sub(1).ok_or(Malformed)?;
        let word = self.words.get(self.depth / 64).ok_or(Malformed)?;
        Ok(word & (1u64 << (self.depth % 64)) != 0)
    }
}

struct Cursor<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Cursor<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Cursor { bytes, pos: 0 }
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn bump(&mut self) {
        self.pos = self.pos.saturating_add(1);
    }

    fn advance(&mut self, by: usize) {
        self.pos = self.pos.saturating_add(by);
    }

    fn rest(&self) -> &'a [u8] {
        self.bytes.get(self.pos..).unwrap_or_default()
    }

    fn skip_ws(&mut self) {
        while let Some(b' ' | b'\t' | b'\n' | b'\r') = self.peek() {
            self.bump();
        }
    }

    fn expect(&mut self, byte: u8) -> Result<(), Malformed> {
        if self.peek() == Some(byte) {
            self.bump();
            Ok(())
        } else {
            Err(Malformed)
        }
    }

    fn finish(&mut self) -> Result<(), Malformed> {
        self.skip_ws();
        if self.pos == self.bytes.len() {
            Ok(())
        } else {
            Err(Malformed)
        }
    }

    fn string(&mut self) -> Result<bool, Malformed> {
        self.expect(b'"')?;
        let mut escaped = false;
        loop {
            let found = memchr::memchr2(b'"', b'\\', self.rest()).ok_or(Malformed)?;
            self.advance(found);
            match self.peek() {
                Some(b'"') => {
                    self.bump();
                    return Ok(escaped);
                }
                Some(_) => {
                    escaped = true;
                    self.advance(2);
                }
                None => return Err(Malformed),
            }
        }
    }

    fn literal(&mut self, word: &[u8]) -> Result<(), Malformed> {
        if self.rest().starts_with(word) {
            self.advance(word.len());
            Ok(())
        } else {
            Err(Malformed)
        }
    }

    fn number(&mut self) -> Result<(), Malformed> {
        let start = self.pos;
        while let Some(b'0'..=b'9' | b'-' | b'+' | b'.' | b'e' | b'E') = self.peek() {
            self.bump();
        }
        if self.pos == start {
            Err(Malformed)
        } else {
            Ok(())
        }
    }

    fn nested(&mut self) -> Result<(), Malformed> {
        let mut stack = Stack::default();
        loop {
            let found = self
                .rest()
                .iter()
                .position(|byte| matches!(byte, b'"' | b'{' | b'}' | b'[' | b']'))
                .ok_or(Malformed)?;
            self.advance(found);
            match self.peek() {
                Some(b'"') => {
                    self.string()?;
                }
                Some(b'{') => {
                    stack.push(true)?;
                    self.bump();
                }
                Some(b'[') => {
                    stack.push(false)?;
                    self.bump();
                }
                Some(closer) => {
                    if stack.pop()? != (closer == b'}') {
                        return Err(Malformed);
                    }
                    self.bump();
                    if stack.depth == 0 {
                        return Ok(());
                    }
                }
                None => return Err(Malformed),
            }
        }
    }

    fn value(&mut self) -> Result<Range<usize>, Malformed> {
        let start = self.pos;
        match self.peek() {
            Some(b'"') => self.string().map(|_| ()),
            Some(b'{' | b'[') => self.nested(),
            Some(b't') => self.literal(b"true"),
            Some(b'f') => self.literal(b"false"),
            Some(b'n') => self.literal(b"null"),
            Some(b'-' | b'0'..=b'9') => self.number(),
            _ => Err(Malformed),
        }?;
        Ok(start..self.pos)
    }

    fn key(&mut self) -> Result<Key, Malformed> {
        let start = self.pos.saturating_add(1);
        let escaped = self.string()?;
        let range = start..self.pos.saturating_sub(1);
        self.skip_ws();
        self.expect(b':')?;
        self.skip_ws();
        Ok(if escaped {
            Key::Escaped(range)
        } else {
            Key::Plain(range)
        })
    }

    fn open(&mut self, opener: u8, closer: u8) -> Result<bool, Malformed> {
        self.skip_ws();
        self.expect(opener)?;
        self.skip_ws();
        if self.peek() == Some(closer) {
            self.bump();
            self.finish()?;
            Ok(false)
        } else {
            Ok(true)
        }
    }

    fn separator(&mut self, closer: u8) -> Result<bool, Malformed> {
        self.skip_ws();
        match self.peek() {
            Some(b',') => {
                self.bump();
                self.skip_ws();
                Ok(true)
            }
            Some(byte) if byte == closer => Ok(false),
            _ => Err(Malformed),
        }
    }
}

pub fn object(bytes: &[u8]) -> Result<Vec<Member>, Malformed> {
    let mut cursor = Cursor::new(bytes);
    let mut members = Vec::with_capacity(16);
    if !cursor.open(b'{', b'}')? {
        return Ok(members);
    }
    loop {
        let key = cursor.key()?;
        let value = cursor.value()?;
        members.push(Member { key, value });
        if !cursor.separator(b'}')? {
            cursor.bump();
            cursor.finish()?;
            return Ok(members);
        }
    }
}

pub fn array(bytes: &[u8]) -> Result<Vec<Range<usize>>, Malformed> {
    let mut cursor = Cursor::new(bytes);
    let mut items = Vec::with_capacity(16);
    if !cursor.open(b'[', b']')? {
        return Ok(items);
    }
    loop {
        items.push(cursor.value()?);
        if !cursor.separator(b']')? {
            cursor.bump();
            cursor.finish()?;
            return Ok(items);
        }
    }
}

pub fn locate(bytes: &[u8], wanted: &[u8]) -> Result<Location, Malformed> {
    let mut cursor = Cursor::new(bytes);
    if !cursor.open(b'{', b'}')? {
        let close = bytes
            .iter()
            .rposition(|byte| *byte == b'}')
            .ok_or(Malformed)?;
        return Ok(Location::Absent { close, empty: true });
    }
    let mut previous_end: Option<usize> = None;
    loop {
        let key_start = cursor.pos;
        let key = cursor.key()?;
        let value = cursor.value()?;
        let more = cursor.separator(b'}')?;
        if key.matches(bytes, wanted) {
            let cut = match (more, previous_end) {
                (true, _) => key_start..cursor.pos,
                (false, Some(end)) => end..value.end,
                (false, None) => key_start..value.end,
            };
            return Ok(Location::Found { value, cut });
        }
        if !more {
            let close = cursor.pos;
            cursor.bump();
            cursor.finish()?;
            return Ok(Location::Absent {
                close,
                empty: false,
            });
        }
        previous_end = Some(value.end);
    }
}

pub fn set_field<'a>(
    bytes: &'a [u8],
    key: &'a [u8],
    value: &'a [u8],
) -> Result<Vec<&'a [u8]>, Malformed> {
    let slice = |range: Range<usize>| bytes.get(range).ok_or(Malformed);
    match locate(bytes, key)? {
        Location::Found { value: old, .. } => Ok(vec![
            slice(0..old.start)?,
            value,
            slice(old.end..bytes.len())?,
        ]),
        Location::Absent { close, empty } => Ok(vec![
            slice(0..close)?,
            if empty { b"\"" } else { b",\"" },
            key,
            b"\":",
            value,
            slice(close..bytes.len())?,
        ]),
    }
}

fn hex(digits: &[u8]) -> Result<u32, Malformed> {
    let text = std::str::from_utf8(digits).map_err(|_| Malformed)?;
    if digits.len() == 4 {
        u32::from_str_radix(text, 16).map_err(|_| Malformed)
    } else {
        Err(Malformed)
    }
}

pub fn unescape(raw: &[u8]) -> Result<String, Malformed> {
    let mut out: Vec<u8> = Vec::with_capacity(raw.len());
    let mut rest = raw;
    while let Some(found) = memchr::memchr(b'\\', rest) {
        let (plain, tail) = rest.split_at(found);
        out.extend_from_slice(plain);
        let (escape, after) = match tail.get(1) {
            Some(b'u') => {
                let first = hex(tail.get(2..6).ok_or(Malformed)?)?;
                if (0xD800..0xDC00).contains(&first) {
                    if tail.get(6..8) != Some(&b"\\u"[..]) {
                        return Err(Malformed);
                    }
                    let second = hex(tail.get(8..12).ok_or(Malformed)?)?;
                    if !(0xDC00..0xE000).contains(&second) {
                        return Err(Malformed);
                    }
                    let high = first.saturating_sub(0xD800) << 10;
                    let low = second.saturating_sub(0xDC00);
                    let code = 0x10000_u32.saturating_add(high).saturating_add(low);
                    (char::from_u32(code).ok_or(Malformed)?, 12)
                } else {
                    (char::from_u32(first).ok_or(Malformed)?, 6)
                }
            }
            Some(b'"') => ('"', 2),
            Some(b'\\') => ('\\', 2),
            Some(b'/') => ('/', 2),
            Some(b'b') => ('\u{8}', 2),
            Some(b'f') => ('\u{c}', 2),
            Some(b'n') => ('\n', 2),
            Some(b'r') => ('\r', 2),
            Some(b't') => ('\t', 2),
            _ => return Err(Malformed),
        };
        let mut buffer = [0u8; 4];
        out.extend_from_slice(escape.encode_utf8(&mut buffer).as_bytes());
        rest = tail.get(after..).ok_or(Malformed)?;
    }
    out.extend_from_slice(rest);
    String::from_utf8(out).map_err(|_| Malformed)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::indexing_slicing)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use serde_json::Value;

    fn text(bytes: &[u8], range: Range<usize>) -> &str {
        std::str::from_utf8(&bytes[range]).unwrap()
    }

    fn keys(bytes: &[u8]) -> Vec<String> {
        object(bytes)
            .unwrap()
            .iter()
            .map(|member| match &member.key {
                Key::Plain(range) => text(bytes, range.clone()).to_string(),
                Key::Escaped(range) => unescape(&bytes[range.clone()]).unwrap(),
            })
            .collect()
    }

    #[test]
    fn object_slices_top_level_members_in_order() {
        let bytes = br#" { "b" : 1 , "a":{"x":[1,{"y":"}"}]}, "c":"q\"uote", "d":null } "#;
        let members = object(bytes).unwrap();
        assert_eq!(keys(bytes), ["b", "a", "c", "d"]);
        let values: Vec<&str> = members
            .iter()
            .map(|member| text(bytes, member.value.clone()))
            .collect();
        assert_eq!(
            values,
            ["1", r#"{"x":[1,{"y":"}"}]}"#, r#""q\"uote""#, "null"]
        );
    }

    #[test]
    fn empty_containers() {
        assert_eq!(object(b"{}").unwrap(), vec![]);
        assert_eq!(object(b" { } ").unwrap(), vec![]);
        assert_eq!(array(b"[]").unwrap(), vec![]);
        assert_eq!(array(b"[ ]").unwrap(), vec![]);
    }

    #[test]
    fn array_slices_elements() {
        let bytes = br#"[1, "two", {"three":[3]}, [4], true, false, null, -5.5e3]"#;
        let items: Vec<&str> = array(bytes)
            .unwrap()
            .into_iter()
            .map(|range| text(bytes, range))
            .collect();
        assert_eq!(
            items,
            [
                "1",
                r#""two""#,
                r#"{"three":[3]}"#,
                "[4]",
                "true",
                "false",
                "null",
                "-5.5e3"
            ]
        );
    }

    #[test]
    fn escaped_keys_are_marked_and_decoded() {
        let bytes = br#"{"a\"b":1,"\u00e9\ud83d\ude00":2,"plain":3}"#;
        assert_eq!(keys(bytes), ["a\"b", "\u{e9}\u{1F600}", "plain"]);
        let members = object(bytes).unwrap();
        assert!(matches!(members[0].key, Key::Escaped(_)));
        assert!(matches!(members[2].key, Key::Plain(_)));
    }

    #[test]
    fn malformed_inputs_are_rejected() {
        for bad in [
            &b""[..],
            b"{",
            b"[",
            b"{\"a\"}",
            b"{\"a\":}",
            b"{\"a\":1,}",
            b"{\"a\":1 \"b\":2}",
            b"{\"a\":[1}",
            b"{\"a\":{]}",
            b"{\"a\":tru}",
            b"{\"a\":\"open}",
            b"{} x",
            b"[1,]",
            b"1",
        ] {
            assert_eq!(object(bad).err(), Some(Malformed), "{bad:?}");
        }
        assert_eq!(array(b"{}").err(), Some(Malformed));
        assert_eq!(unescape(br"\q").err(), Some(Malformed));
        let deep = format!(
            "{{\"a\":{}{}}}",
            "[".repeat(MAX_DEPTH),
            "]".repeat(MAX_DEPTH)
        );
        assert_eq!(object(deep.as_bytes()).unwrap().len(), 1);
        let deeper = format!(
            "{{\"a\":{}{}}}",
            "[".repeat(MAX_DEPTH + 1),
            "]".repeat(MAX_DEPTH + 1)
        );
        assert_eq!(object(deeper.as_bytes()).err(), Some(Malformed));
        assert_eq!(unescape(br"\ud800").err(), Some(Malformed));
    }

    #[test]
    fn locate_finds_value_and_cut() {
        let bytes = br#"{"a":1, "b":[2], "c":3}"#;
        let Location::Found { value, cut } = locate(bytes, b"b").unwrap() else {
            unreachable!()
        };
        assert_eq!(text(bytes, value), "[2]");
        let mut without = bytes[..cut.start].to_vec();
        without.extend_from_slice(&bytes[cut.end..]);
        assert_eq!(without, br#"{"a":1, "c":3}"#);

        let Location::Found { cut, .. } = locate(bytes, b"c").unwrap() else {
            unreachable!()
        };
        let mut without = bytes[..cut.start].to_vec();
        without.extend_from_slice(&bytes[cut.end..]);
        assert_eq!(without, br#"{"a":1, "b":[2]}"#);

        let single = br#"{ "only":true }"#;
        let Location::Found { cut, .. } = locate(single, b"only").unwrap() else {
            unreachable!()
        };
        let mut without = single[..cut.start].to_vec();
        without.extend_from_slice(&single[cut.end..]);
        assert_eq!(without, b"{  }");
    }

    #[test]
    fn locate_reports_the_closing_brace() {
        assert_eq!(
            locate(br#"{"a":1} "#, b"z").unwrap(),
            Location::Absent {
                close: 6,
                empty: false
            }
        );
        assert_eq!(
            locate(b" { } ", b"z").unwrap(),
            Location::Absent {
                close: 3,
                empty: true
            }
        );
    }

    #[test]
    fn set_field_replaces_or_appends() {
        let join = |pieces: Vec<&[u8]>| pieces.concat();
        assert_eq!(
            join(set_field(br#"{"a":1,"b":2}"#, b"a", b"9").unwrap()),
            br#"{"a":9,"b":2}"#
        );
        assert_eq!(
            join(set_field(br#"{"a":1}"#, b"z", b"[]").unwrap()),
            br#"{"a":1,"z":[]}"#
        );
        assert_eq!(join(set_field(b"{}", b"z", b"0").unwrap()), br#"{"z":0}"#);
    }

    fn arb_json() -> impl Strategy<Value = Value> {
        let leaf = prop_oneof![
            Just(Value::Null),
            any::<bool>().prop_map(Value::Bool),
            any::<i64>().prop_map(Value::from),
            ".*".prop_map(Value::String),
        ];
        leaf.prop_recursive(4, 64, 8, |inner| {
            prop_oneof![
                prop::collection::vec(inner.clone(), 0..8).prop_map(Value::Array),
                prop::collection::btree_map(".*", inner, 0..8)
                    .prop_map(|map| Value::Object(map.into_iter().collect())),
            ]
        })
    }

    proptest! {
        #[test]
        fn object_matches_serde(map in prop::collection::btree_map(".*", arb_json(), 0..12), pretty in any::<bool>()) {
            let value = Value::Object(map.clone().into_iter().collect());
            let bytes = if pretty {
                serde_json::to_vec_pretty(&value).unwrap()
            } else {
                serde_json::to_vec(&value).unwrap()
            };
            let members = object(&bytes).unwrap();
            prop_assert_eq!(members.len(), map.len());
            for (member, (key, expected)) in members.iter().zip(map.iter()) {
                let decoded = match &member.key {
                    Key::Plain(range) => text(&bytes, range.clone()).to_string(),
                    Key::Escaped(range) => unescape(&bytes[range.clone()]).unwrap(),
                };
                prop_assert_eq!(&decoded, key);
                let slice: Value = serde_json::from_slice(&bytes[member.value.clone()]).unwrap();
                prop_assert_eq!(&slice, expected);
                prop_assert!(member.key.matches(&bytes, key.as_bytes()));
            }
        }

        #[test]
        fn array_matches_serde(items in prop::collection::vec(arb_json(), 0..12)) {
            let bytes = serde_json::to_vec(&Value::Array(items.clone())).unwrap();
            let ranges = array(&bytes).unwrap();
            prop_assert_eq!(ranges.len(), items.len());
            for (range, expected) in ranges.into_iter().zip(items.iter()) {
                let slice: Value = serde_json::from_slice(&bytes[range]).unwrap();
                prop_assert_eq!(&slice, expected);
            }
        }

        #[test]
        fn removing_the_cut_keeps_valid_json(map in prop::collection::btree_map("[a-z]{1,4}", arb_json(), 1..8), pick in any::<prop::sample::Index>()) {
            let value = Value::Object(map.clone().into_iter().collect());
            let bytes = serde_json::to_vec(&value).unwrap();
            let key = pick.get(&map.keys().collect::<Vec<_>>()).to_string();
            let Location::Found { cut, .. } = locate(&bytes, key.as_bytes()).unwrap() else {
                return Err(TestCaseError::fail("missing key"));
            };
            let mut without = bytes[..cut.start].to_vec();
            without.extend_from_slice(&bytes[cut.end..]);
            let mut expected = map.clone();
            expected.remove(&key);
            let parsed: Value = serde_json::from_slice(&without).unwrap();
            prop_assert_eq!(parsed, Value::Object(expected.into_iter().collect()));
        }

        #[test]
        fn never_panics_on_noise(bytes in prop::collection::vec(any::<u8>(), 0..256)) {
            let _ = object(&bytes);
            let _ = array(&bytes);
            let _ = locate(&bytes, b"a");
            let _ = unescape(&bytes);
        }
    }
}
