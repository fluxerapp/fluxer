// SPDX-License-Identifier: AGPL-3.0-or-later

pub fn count_noun(count: u64, singular: &str, plural: &str) -> String {
    if count == 1 {
        format!("{count} {singular}")
    } else {
        format!("{count} {plural}")
    }
}

pub fn noun_for(count: u64, singular: &'static str, plural: &'static str) -> &'static str {
    if count == 1 { singular } else { plural }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn count_noun_picks_the_singular_only_for_one() {
        assert_eq!(count_noun(0, "result", "results"), "0 results");
        assert_eq!(count_noun(1, "result", "results"), "1 result");
        assert_eq!(count_noun(2, "entry", "entries"), "2 entries");
        assert_eq!(noun_for(1, "item", "items"), "item");
        assert_eq!(noun_for(3, "item", "items"), "items");
    }
}
