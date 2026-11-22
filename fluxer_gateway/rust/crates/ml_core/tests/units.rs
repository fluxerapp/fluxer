// SPDX-License-Identifier: AGPL-3.0-or-later

use ml_core::casefold;
use ml_core::casefold_table::{FOLD_BYTES, FOLD_KEYS, FOLD_SPANS};
use ml_core::{ListKey, Range, Ranges};
use perm_core::ChannelId;

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }
}

#[test]
fn ascii_word_fold_equals_byte_lowercase() {
    let mut rng = Rng(0x1234_5678_9abc_def1);
    for _ in 0..200_000 {
        let len = (rng.next() % 40) as usize;
        let bytes: Vec<u8> = (0..len).map(|_| (rng.next() % 128) as u8).collect();
        assert_eq!(
            casefold::fold(&bytes),
            bytes.to_ascii_lowercase(),
            "{bytes:?}"
        );
    }
}

#[test]
fn unicode_fold_is_the_concatenation_of_scalar_folds() {
    let mut rng = Rng(0xdead_beef_cafe_f00d);
    let interesting: Vec<char> = FOLD_KEYS
        .iter()
        .filter_map(|c| char::from_u32(*c))
        .chain("aZß\u{301}İıΣσς€😀ﬃ".chars())
        .collect();
    for _ in 0..50_000 {
        let len = 1 + (rng.next() % 12) as usize;
        let text: String = (0..len)
            .map(|_| match rng.next() % 3 {
                0 => interesting[(rng.next() % interesting.len() as u64) as usize],
                1 => char::from_u32((rng.next() % 0x11_0000) as u32).unwrap_or('x'),
                _ => (b'A' + (rng.next() % 26) as u8) as char,
            })
            .collect();
        let whole = casefold::fold(text.as_bytes());
        let parts: Vec<u8> = text
            .chars()
            .flat_map(|c| casefold::fold(c.to_string().as_bytes()))
            .collect();
        assert_eq!(whole, parts, "{text:?}");
        assert!(std::str::from_utf8(&whole).is_ok());
        assert_eq!(
            casefold::fold(&whole),
            casefold::fold(&casefold::fold(&whole))
        );
    }
}

#[test]
fn fold_table_is_sorted_and_in_bounds() {
    assert!(FOLD_KEYS.windows(2).all(|w| w[0] < w[1]));
    for (off, len) in FOLD_SPANS.iter() {
        assert!(usize::from(*off) + usize::from(*len) <= FOLD_BYTES.len());
    }
    assert_eq!(casefold::fold("ẞ".as_bytes()), b"ss".to_vec());
    assert_eq!(
        casefold::fold("Ärla".as_bytes()),
        "ärla".as_bytes().to_vec()
    );
}

#[test]
fn invalid_utf8_keeps_its_bytes() {
    assert_eq!(casefold::fold(&[0xff, 0x41]), vec![0xff, 0x41]);
    assert_eq!(casefold::fold(&[b'A', 0xc3]), vec![b'A', 0xc3]);
}

#[test]
fn list_keys_order_like_decimal_binaries() {
    let mut rng = Rng(77);
    for _ in 0..100_000 {
        let a = rng.next() >> (rng.next() % 64);
        let b = rng.next() >> (rng.next() % 64);
        let ka = ListKey::new(ChannelId::new(a));
        let kb = ListKey::new(ChannelId::new(b));
        assert_eq!(ka.cmp(&kb), a.to_string().cmp(&b.to_string()), "{a} {b}");
        assert_eq!(ka.decimal(), a.to_string().into_bytes());
    }
}

#[test]
fn ranges_accept_only_normalized_input() {
    assert!(Ranges::normalized(&[(0, 99), (100, 199)]).is_ok());
    assert!(Ranges::normalized(&[(0, 100)]).is_err());
    assert!(Ranges::normalized(&[(5, 4)]).is_err());
    assert!(Ranges::normalized(&[(100, 199), (0, 99)]).is_err());
    assert!(Ranges::normalized(&[(0, 99), (99, 120)]).is_err());
    assert!(Ranges::normalized(&[(100_000, 100_000)]).is_ok());
    assert!(Ranges::normalized(&[(100_001, 100_001)]).is_err());
    let eleven: Vec<(u32, u32)> = (0..11).map(|i| (i * 100, i * 100 + 99)).collect();
    assert!(Ranges::normalized(&eleven).is_err());
    assert_eq!(Ranges::sync(&eleven).map(|r| r.len()), Ok(11));
    let twenty: Vec<(u32, u32)> = (0..20).map(|i| (i * 2, i * 2)).collect();
    assert_eq!(Ranges::sync(&twenty).map(|r| r.len()), Ok(20));
    let more: Vec<(u32, u32)> = (0..21).map(|i| (i * 2, i * 2)).collect();
    assert!(Ranges::sync(&more).is_err());
    assert_eq!(Range::new(3, 2), None);
}

#[test]
fn fold_is_idempotent_on_every_scalar() {
    for code in 0..=0x10_ffffu32 {
        let Some(c) = char::from_u32(code) else {
            continue;
        };
        let once = casefold::fold(c.to_string().as_bytes());
        assert_eq!(casefold::fold(&once), once, "U+{code:04X}");
    }
}

const HOIST_GUILD: u64 = 100;

fn hoisted_role(id: u64, position: i64, hoist_position: Option<i64>) -> perm_core::RoleSpec {
    perm_core::RoleSpec {
        id: perm_core::RoleId::new(id),
        permissions: perm_core::Permissions::empty(),
        position: perm_core::RankPos::Int(position),
        hoist: perm_core::Hoist::Hoisted(hoist_position),
    }
}

fn hoisted_ids(roles: Vec<perm_core::RoleSpec>) -> Vec<u64> {
    let guild = perm_core::GuildId::new(HOIST_GUILD);
    let shared = perm_core::PermsShared::new(guild, perm_core::Config::default());
    let mut snapshot = perm_core::Snapshot::new(guild, None);
    snapshot.roles = roles;
    shared.load(&snapshot).expect("snapshot loads");
    let model = shared.model().expect("model is loaded");
    ml_core::sections::hoist_order(&model)
        .into_iter()
        .filter_map(|idx| model.roles().get(idx).map(|role| role.id().get()))
        .collect()
}

#[test]
fn tied_hoist_positions_sort_by_lower_id_first() {
    let roles = vec![
        hoisted_role(201, 1, None),
        hoisted_role(202, 1, None),
        hoisted_role(203, 1, None),
    ];
    let mut reversed = roles.clone();
    reversed.reverse();
    assert_eq!(hoisted_ids(roles), vec![201, 202, 203]);
    assert_eq!(hoisted_ids(reversed), vec![201, 202, 203]);
}

#[test]
fn higher_hoist_position_sorts_first() {
    let roles = vec![
        hoisted_role(201, 1, None),
        hoisted_role(202, 3, None),
        hoisted_role(203, 2, None),
    ];
    assert_eq!(hoisted_ids(roles), vec![202, 203, 201]);
}

#[test]
fn hoist_position_overrides_position() {
    let roles = vec![hoisted_role(201, 5, Some(1)), hoisted_role(202, 1, Some(2))];
    assert_eq!(hoisted_ids(roles), vec![202, 201]);
}
