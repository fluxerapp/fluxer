// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::external_path;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Target<'a> {
    pub path: &'a str,
    pub query: &'a str,
}

pub fn split_target(target: &str) -> Target<'_> {
    match target.find('?') {
        Some(q) => Target {
            path: &target[..q],
            query: &target[q + 1..],
        },
        None => Target {
            path: target,
            query: "",
        },
    }
}

#[derive(Clone, Debug, Default)]
pub struct Query {
    pairs: Vec<(String, String)>,
}

impl Query {
    pub fn parse(raw: &str) -> Self {
        let mut pairs = Vec::new();
        for field in raw.split('&') {
            if field.is_empty() {
                continue;
            }
            let eq = field.find('=').unwrap_or(field.len());
            let key = external_path::percent_decode_string(&field[..eq], true);
            let value = if eq < field.len() {
                external_path::percent_decode_string(&field[eq + 1..], true)
            } else {
                String::new()
            };
            pairs.push((key, value));
        }
        Self { pairs }
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.pairs
            .iter()
            .find_map(|(k, v)| (k == key).then_some(v.as_str()))
    }

    pub fn bool_value(&self, key: &str, default_value: bool) -> bool {
        self.get(key)
            .map(|raw| raw.eq_ignore_ascii_case("true") || raw == "1")
            .unwrap_or(default_value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::ADVERSARIAL_TEXT_INPUTS;

    fn target<'a>(path: &'a str, query: &'a str) -> Target<'a> {
        Target { path, query }
    }

    #[test]
    fn split_target_cuts_at_the_first_question_mark() {
        assert_eq!(
            target("/avatars/1/a.png", "size=64"),
            split_target("/avatars/1/a.png?size=64")
        );
        assert_eq!(
            target("/avatars/1/a.png", ""),
            split_target("/avatars/1/a.png")
        );
        assert_eq!(target("", ""), split_target(""));
        assert_eq!(target("", "size=64"), split_target("?size=64"));
        assert_eq!(target("/a", ""), split_target("/a?"));
        assert_eq!(target("/a", "b?c=1"), split_target("/a?b?c=1"));
    }

    #[test]
    fn adversarial_targets_split_without_losing_or_duplicating_bytes() {
        for text in ADVERSARIAL_TEXT_INPUTS {
            for raw in [
                (*text).to_owned(),
                format!("/{text}"),
                format!("/media/{text}?v={text}"),
                format!("{text}?{text}"),
            ] {
                let split = split_target(&raw);
                assert!(!split.path.contains('?'), "{raw}");
                assert!(raw.starts_with(split.path), "{raw}");
                assert!(raw.ends_with(split.query), "{raw}");
                assert_eq!(
                    raw.len(),
                    split.path.len() + split.query.len() + usize::from(raw.contains('?')),
                    "{raw}"
                );
            }
        }
    }

    #[test]
    fn query_parse_decodes_pairs_and_keeps_the_first_value_for_a_repeated_key() {
        let query = Query::parse("size=64&size=128&download&flag=&a+b=c+d&%2F=%2F");
        assert_eq!(Some("64"), query.get("size"));
        assert_eq!(Some(""), query.get("download"));
        assert_eq!(Some(""), query.get("flag"));
        assert_eq!(Some("c d"), query.get("a b"));
        assert_eq!(Some("/"), query.get("/"));
        assert_eq!(None, query.get("missing"));
        assert_eq!(6, query.pairs.len());
    }

    #[test]
    fn query_parse_skips_empty_fields_and_tolerates_broken_escapes() {
        let query = Query::parse("&&a=1&&&b=%zz&c=%&d=%FF&e=%C3%A9");
        assert_eq!(Some("1"), query.get("a"));
        assert_eq!(Some("%zz"), query.get("b"));
        assert_eq!(Some("%"), query.get("c"));
        assert_eq!(Some("\u{fffd}"), query.get("d"));
        assert_eq!(Some("\u{e9}"), query.get("e"));
        assert_eq!(None, query.get(""));
        assert_eq!(5, query.pairs.len());
        assert_eq!(0, Query::parse("&&&").pairs.len());
        assert_eq!(0, Query::parse("").pairs.len());
    }

    #[test]
    fn bool_value_accepts_only_true_and_one() {
        let query = Query::parse("a=true&b=TRUE&c=1&d=yes&e=&f=0&g");
        assert!(query.bool_value("a", false));
        assert!(query.bool_value("b", false));
        assert!(query.bool_value("c", false));
        assert!(!query.bool_value("d", true));
        assert!(!query.bool_value("e", true));
        assert!(!query.bool_value("f", true));
        assert!(!query.bool_value("g", true));
        assert!(query.bool_value("missing", true));
        assert!(!query.bool_value("missing", false));
    }

    #[test]
    fn adversarial_query_text_parses_into_one_pair_per_non_empty_field() {
        for text in ADVERSARIAL_TEXT_INPUTS {
            for raw in [
                (*text).to_owned(),
                format!("size={text}"),
                format!("{text}={text}"),
                format!("{text}&{text}"),
                format!("&{text}&&size=64&"),
            ] {
                let query = Query::parse(&raw);
                assert_eq!(
                    raw.split('&').filter(|field| !field.is_empty()).count(),
                    query.pairs.len(),
                    "{raw}"
                );
                for (key, _) in &query.pairs {
                    let first = query
                        .pairs
                        .iter()
                        .find_map(|(k, v)| (k == key).then_some(v.as_str()));
                    assert_eq!(first, query.get(key), "{raw}");
                }
                assert_eq!(None, query.get("definitely-absent"), "{raw}");
            }
        }
    }
}
