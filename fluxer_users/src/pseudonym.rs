// SPDX-License-Identifier: AGPL-3.0-or-later

use hmac::{KeyInit, Mac};
use std::sync::{LazyLock, OnceLock};

type HmacSha256 = hmac::Hmac<sha2::Sha256>;

const SECRET_ENV: &str = "FLUXER_PROFILE_PSEUDONYM_SECRET";
const DEVELOPMENT_SECRET: &str = "fluxer-dev-profile-pseudonym-secret";
const SCALES_TEXT: &str = include_str!("../../fluxer_api/src/api/words/scales.txt");
const TAILS_TEXT: &str = include_str!("../../fluxer_api/src/api/words/tails.txt");
const MAX_USERNAME_LENGTH: usize = 32;
const MAX_ATTEMPTS: usize = 100;
const FALLBACK_USERNAME: &str = "BotUser";
const MAX_DISCRIMINATOR: u32 = 9999;
const RESERVED_DISCRIMINATORS: [u32; 55] = [
    1, 2, 3, 4, 5, 6, 7, 8, 9, 67, 69, 404, 420, 666, 911, 1000, 1111, 1234, 1337, 2000, 2025,
    2026, 2027, 2222, 2345, 3000, 3333, 3456, 4000, 4321, 4444, 4567, 5000, 5432, 5555, 5678, 6000,
    6543, 6666, 6789, 6969, 7000, 7654, 7777, 7890, 8000, 8008, 8055, 8080, 8765, 8888, 9000, 9001,
    9876, 9999,
];

static SECRET: OnceLock<Vec<u8>> = OnceLock::new();
static SCALES: LazyLock<Vec<&'static str>> = LazyLock::new(|| word_list(SCALES_TEXT));
static TAILS: LazyLock<Vec<&'static str>> = LazyLock::new(|| word_list(TAILS_TEXT));

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pseudonym {
    pub username: String,
    pub discriminator: i32,
}

pub fn configure(secret: Option<String>, environment: Option<&str>) -> anyhow::Result<()> {
    let secret = match secret.filter(|value| !value.trim().is_empty()) {
        Some(value) => value,
        None if matches!(environment, Some("development" | "test")) => {
            tracing::warn!("{SECRET_ENV} is not set, using the development secret");
            DEVELOPMENT_SECRET.to_owned()
        }
        None => anyhow::bail!("{SECRET_ENV} is required"),
    };
    SECRET
        .set(secret.into_bytes())
        .map_err(|_| anyhow::anyhow!("{SECRET_ENV} was already configured"))
}

pub fn configure_from_env() -> anyhow::Result<()> {
    configure(
        std::env::var(SECRET_ENV).ok(),
        std::env::var("FLUXER_ENV").ok().as_deref(),
    )
}

pub fn pseudonym(user_id: i64) -> Pseudonym {
    let secret = SECRET
        .get()
        .map_or(DEVELOPMENT_SECRET.as_bytes(), Vec::as_slice);
    pseudonym_with_secret(secret, user_id)
}

pub fn pseudonym_with_secret(secret: &[u8], user_id: i64) -> Pseudonym {
    let mut mac = HmacSha256::new_from_slice(secret).expect("hmac accepts any key length");
    mac.update(user_id.to_string().as_bytes());
    let seed: [u8; 32] = mac.finalize().into_bytes().into();
    Pseudonym {
        username: seeded_username(&seed),
        discriminator: seeded_discriminator(&seed),
    }
}

fn word_list(text: &'static str) -> Vec<&'static str> {
    text.trim()
        .split('\n')
        .filter(|word| !word.is_empty())
        .collect()
}

fn capitalize(word: &str) -> String {
    let mut chars = word.chars();
    chars.next().map_or_else(String::new, |first| {
        first.to_uppercase().chain(chars).collect()
    })
}

fn is_valid_username(value: &str) -> bool {
    let trimmed = value.trim();
    let lower = trimmed.to_lowercase();
    !trimmed.is_empty()
        && trimmed.encode_utf16().count() <= MAX_USERNAME_LENGTH
        && trimmed
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
        && lower != "everyone"
        && lower != "here"
        && !lower.contains("fluxer")
        && !lower.contains("system message")
}

fn seed_word(seed: &[u8; 32], index: usize) -> u32 {
    let offset = (index % (seed.len() / 4)) * 4;
    u32::from_be_bytes([
        seed[offset],
        seed[offset + 1],
        seed[offset + 2],
        seed[offset + 3],
    ])
}

fn seeded_username(seed: &[u8; 32]) -> String {
    let scales = &*SCALES;
    let tails = &*TAILS;
    let mut picks = 0;
    let mut pick = |size: usize| {
        let value = seed_word(seed, picks) as usize % size;
        picks += 1;
        value
    };
    for _ in 0..MAX_ATTEMPTS {
        let scale = scales[pick(scales.len())];
        let tail = tails[pick(tails.len())];
        let candidate = capitalize(scale) + &capitalize(tail);
        if is_valid_username(&candidate) {
            return candidate;
        }
    }
    tails
        .iter()
        .map(|tail| capitalize(tail))
        .find(|candidate| is_valid_username(candidate))
        .unwrap_or_else(|| FALLBACK_USERNAME.to_owned())
}

fn seeded_discriminator(seed: &[u8; 32]) -> i32 {
    let mut value = seed_word(seed, seed.len() / 4 - 1) % MAX_DISCRIMINATOR + 1;
    while RESERVED_DISCRIMINATORS.contains(&value) {
        value = value % MAX_DISCRIMINATOR + 1;
    }
    value as i32
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    const VECTORS: &str =
        include_str!("../../fluxer_common/src/testdata/profile_pseudonym_vectors.json");

    fn fixture() -> Value {
        serde_json::from_str(VECTORS).expect("the pseudonym vectors parse as json")
    }

    #[test]
    fn matches_the_shared_vectors() {
        let fixture = fixture();
        let vectors = fixture["vectors"].as_array().expect("a vector list");
        assert!(!vectors.is_empty());
        for vector in vectors {
            let secret = vector["secret"].as_str().expect("a secret");
            let user_id = vector["user_id"]
                .as_str()
                .expect("a user id")
                .parse::<i64>()
                .expect("a numeric user id");
            let expected = Pseudonym {
                username: vector["username"].as_str().expect("a username").to_owned(),
                discriminator: vector["discriminator"]
                    .as_str()
                    .expect("a discriminator")
                    .parse()
                    .expect("a numeric discriminator"),
            };
            assert_eq!(
                pseudonym_with_secret(secret.as_bytes(), user_id),
                expected,
                "{user_id}"
            );
            assert_eq!(
                format!("{:04}", expected.discriminator),
                vector["discriminator"].as_str().unwrap()
            );
        }
    }

    #[test]
    fn shares_the_reserved_list_and_development_secret() {
        let fixture = fixture();
        let reserved = fixture["reserved_discriminators"]
            .as_array()
            .expect("a reserved list")
            .iter()
            .map(|value| value.as_u64().expect("a number") as u32)
            .collect::<Vec<_>>();
        assert_eq!(reserved, RESERVED_DISCRIMINATORS.to_vec());
        assert_eq!(fixture["development_secret"], DEVELOPMENT_SECRET);
    }

    #[test]
    fn is_stable_per_account_and_differs_across_accounts() {
        let secret = b"stable-secret";
        let first = pseudonym_with_secret(secret, 1_174_109_840_998_400_001);
        assert_eq!(
            first,
            pseudonym_with_secret(secret, 1_174_109_840_998_400_001)
        );
        let names = (0..200)
            .map(|offset| pseudonym_with_secret(secret, 1_174_109_840_998_400_001 + offset))
            .collect::<std::collections::HashSet<_>>();
        assert_eq!(names.len(), 200);
        assert_ne!(
            first,
            pseudonym_with_secret(b"other-secret", 1_174_109_840_998_400_001)
        );
    }

    #[test]
    fn looks_like_a_generated_account() {
        for user_id in 0..500 {
            let generated = pseudonym_with_secret(b"format-secret", user_id);
            assert!(
                is_valid_username(&generated.username),
                "{}",
                generated.username
            );
            assert!(
                SCALES.iter().any(|scale| {
                    generated
                        .username
                        .strip_prefix(&capitalize(scale))
                        .is_some_and(|rest| TAILS.iter().any(|tail| rest == capitalize(tail)))
                }),
                "{}",
                generated.username
            );
            let discriminator = generated.discriminator as u32;
            assert!((1..=MAX_DISCRIMINATOR).contains(&discriminator));
            assert!(!RESERVED_DISCRIMINATORS.contains(&discriminator));
        }
    }

    #[test]
    fn configuration_requires_a_secret_outside_development() {
        assert!(configure(None, None).is_err());
        assert!(configure(Some("  ".to_owned()), Some("production")).is_err());
    }
}
