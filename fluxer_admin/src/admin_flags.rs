// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types::EnforcementKindType;

pub struct U64Flag {
    pub name: &'static str,
    pub value: u64,
}

pub struct I32Flag {
    pub name: &'static str,
    pub value: i32,
}

pub mod user_flag_bits {
    pub const STAFF: u64 = 1 << 0;
    pub const PARTNER: u64 = 1 << 2;
    pub const BUG_HUNTER: u64 = 1 << 3;
    pub const FRIENDLY_BOT: u64 = 1 << 4;
    pub const FRIENDLY_BOT_MANUAL_APPROVAL: u64 = 1 << 5;
    pub const SPAMMER: u64 = 1 << 6;
    pub const PROFILE_HIDDEN: u64 = 1 << 7;
    pub const HIGH_GLOBAL_RATE_LIMIT: u64 = 1 << 33;
    pub const DELETED: u64 = 1 << 34;
    pub const SELF_DELETED: u64 = 1 << 36;
    pub const DISABLED: u64 = 1 << 38;
    pub const HAS_SESSION_STARTED: u64 = 1 << 39;
    pub const RATE_LIMIT_BYPASS: u64 = 1 << 47;
    pub const REPORT_BANNED: u64 = 1 << 48;
    pub const VERIFIED_NOT_UNDERAGE: u64 = 1 << 49;
    pub const ACCOUNT_LIMITED: u64 = 1 << 50;
    pub const HAS_DISMISSED_PREMIUM_ONBOARDING: u64 = 1 << 51;
    pub const APP_STORE_REVIEWER: u64 = 1 << 53;
    pub const STAFF_HIDDEN: u64 = 1 << 57;
    pub const AGE_VERIFIED_ADULT: u64 = 1 << 60;
    pub const LIMIT_EXEMPT: u64 = 1 << 62;
}

pub const USER_FLAGS: &[U64Flag] = &[
    U64Flag {
        name: "STAFF",
        value: user_flag_bits::STAFF,
    },
    U64Flag {
        name: "PARTNER",
        value: user_flag_bits::PARTNER,
    },
    U64Flag {
        name: "BUG_HUNTER",
        value: user_flag_bits::BUG_HUNTER,
    },
    U64Flag {
        name: "FRIENDLY_BOT",
        value: user_flag_bits::FRIENDLY_BOT,
    },
    U64Flag {
        name: "FRIENDLY_BOT_MANUAL_APPROVAL",
        value: user_flag_bits::FRIENDLY_BOT_MANUAL_APPROVAL,
    },
    U64Flag {
        name: "SPAMMER",
        value: user_flag_bits::SPAMMER,
    },
    U64Flag {
        name: "PROFILE_HIDDEN",
        value: user_flag_bits::PROFILE_HIDDEN,
    },
    U64Flag {
        name: "HIGH_GLOBAL_RATE_LIMIT",
        value: user_flag_bits::HIGH_GLOBAL_RATE_LIMIT,
    },
    U64Flag {
        name: "DELETED",
        value: user_flag_bits::DELETED,
    },
    U64Flag {
        name: "SELF_DELETED",
        value: user_flag_bits::SELF_DELETED,
    },
    U64Flag {
        name: "DISABLED",
        value: user_flag_bits::DISABLED,
    },
    U64Flag {
        name: "HAS_SESSION_STARTED",
        value: user_flag_bits::HAS_SESSION_STARTED,
    },
    U64Flag {
        name: "RATE_LIMIT_BYPASS",
        value: user_flag_bits::RATE_LIMIT_BYPASS,
    },
    U64Flag {
        name: "REPORT_BANNED",
        value: user_flag_bits::REPORT_BANNED,
    },
    U64Flag {
        name: "VERIFIED_NOT_UNDERAGE",
        value: user_flag_bits::VERIFIED_NOT_UNDERAGE,
    },
    U64Flag {
        name: "ACCOUNT_LIMITED",
        value: user_flag_bits::ACCOUNT_LIMITED,
    },
    U64Flag {
        name: "HAS_DISMISSED_PREMIUM_ONBOARDING",
        value: user_flag_bits::HAS_DISMISSED_PREMIUM_ONBOARDING,
    },
    U64Flag {
        name: "APP_STORE_REVIEWER",
        value: user_flag_bits::APP_STORE_REVIEWER,
    },
    U64Flag {
        name: "STAFF_HIDDEN",
        value: user_flag_bits::STAFF_HIDDEN,
    },
    U64Flag {
        name: "AGE_VERIFIED_ADULT",
        value: user_flag_bits::AGE_VERIFIED_ADULT,
    },
    U64Flag {
        name: "LIMIT_EXEMPT",
        value: user_flag_bits::LIMIT_EXEMPT,
    },
];

const RESTRICTION_USER_FLAGS: u64 = user_flag_bits::DISABLED
    | user_flag_bits::DELETED
    | user_flag_bits::SPAMMER
    | user_flag_bits::REPORT_BANNED;
const PURCHASE_DISABLED: i32 = 1 << 6;

pub fn user_flag_is_editable(flag: u64, current_flags: u64, suspended: bool) -> bool {
    let self_disabled = flag == user_flag_bits::DISABLED && current_flags & flag != 0 && !suspended;
    flag & RESTRICTION_USER_FLAGS == 0 || self_disabled
}

pub fn premium_flag_is_editable(flag: i32) -> bool {
    flag & PURCHASE_DISABLED == 0
}

pub const PREMIUM_FLAGS: &[I32Flag] = &[
    I32Flag {
        name: "DISCRIMINATOR",
        value: 1 << 0,
    },
    I32Flag {
        name: "BADGE_HIDDEN",
        value: 1 << 1,
    },
    I32Flag {
        name: "BADGE_MASKED",
        value: 1 << 2,
    },
    I32Flag {
        name: "BADGE_TIMESTAMP_HIDDEN",
        value: 1 << 3,
    },
    I32Flag {
        name: "BADGE_SEQUENCE_HIDDEN",
        value: 1 << 4,
    },
    I32Flag {
        name: "PERKS_SANITIZED",
        value: 1 << 5,
    },
    I32Flag {
        name: "PURCHASE_DISABLED",
        value: PURCHASE_DISABLED,
    },
    I32Flag {
        name: "ENABLED_OVERRIDE",
        value: 1 << 7,
    },
    I32Flag {
        name: "PERKS_DISABLED",
        value: 1 << 8,
    },
];

pub const DELETION_REASONS: &[(&str, &str)] = &[
    ("1", "User requested"),
    ("2", "Other"),
    ("3", "Spam"),
    ("4", "Cheating or exploitation"),
    ("5", "Coordinated raiding or manipulation"),
    ("6", "Disruptive automation"),
    ("7", "Nonconsensual sexual content"),
    ("8", "Scam or social engineering"),
    ("9", "Child sexual content"),
    ("10", "Privacy violation or doxxing"),
    ("11", "Harassment or bullying"),
    ("12", "Payment fraud"),
    ("13", "Child safety violation"),
    ("14", "Billing dispute or abuse"),
    ("15", "Unsolicited explicit content"),
    ("16", "Graphic violence"),
    ("17", "Ban evasion"),
    ("18", "Token or credential scam"),
    ("19", "Inactivity"),
    ("20", "Hate speech or extremist content"),
    ("21", "Malicious links or malware distribution"),
    ("22", "Impersonation or fake identity"),
    ("23", "Threats or violent speech"),
    ("24", "Violent extremism"),
    ("25", "Self-harm promotion"),
    ("26", "Sexual content outside 18+ spaces"),
    ("27", "Illegal goods or activity"),
    ("28", "Harmful deception"),
    ("29", "Reporting abuse"),
    ("30", "Under the minimum age"),
    ("31", "Copyright"),
];

pub fn deletion_reason_label(code: i32) -> String {
    let value = code.to_string();
    DELETION_REASONS
        .iter()
        .find(|(candidate, _)| *candidate == value)
        .map_or_else(
            || format!("Reason {code}"),
            |(_, label)| (*label).to_owned(),
        )
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EnforcementHandling {
    Standard,
    Protected,
    Restricted,
}

pub struct EnforcementReason {
    pub code: i32,
    pub handling: EnforcementHandling,
    pub kinds: Option<&'static [EnforcementKindType]>,
}

impl EnforcementReason {
    pub fn allows(&self, kind: EnforcementKindType) -> bool {
        self.kinds.is_none_or(|kinds| kinds.contains(&kind))
            && (kind != EnforcementKindType::FeatureLimit
                || ENFORCEMENT_LIMITS
                    .iter()
                    .any(|limit| limit.codes.contains(&self.code)))
    }
}

pub struct EnforcementLimitOption {
    pub value: &'static str,
    pub label: &'static str,
    pub codes: &'static [i32],
}

pub const ENFORCEMENT_LIMITS: &[EnforcementLimitOption] = &[
    EnforcementLimitOption {
        value: "direct_messages",
        label: "Direct messages",
        codes: &[3, 6, 8, 18, 21],
    },
    EnforcementLimitOption {
        value: "reports",
        label: "Reports",
        codes: &[29],
    },
    EnforcementLimitOption {
        value: "purchases",
        label: "Purchases",
        codes: &[12, 14],
    },
];

pub const COPYRIGHT_REASON_CODE: i32 = 31;

const RESTRICTED_KINDS: &[EnforcementKindType] = &[
    EnforcementKindType::Suspension,
    EnforcementKindType::ContentRemoval,
];
const PAYMENT_KINDS: &[EnforcementKindType] = &[
    EnforcementKindType::Warning,
    EnforcementKindType::Suspension,
    EnforcementKindType::Closure,
    EnforcementKindType::FeatureLimit,
];

const fn reason(
    code: i32,
    handling: EnforcementHandling,
    kinds: Option<&'static [EnforcementKindType]>,
) -> EnforcementReason {
    EnforcementReason {
        code,
        handling,
        kinds,
    }
}

pub const ENFORCEMENT_REASONS: &[EnforcementReason] = {
    use EnforcementHandling::{Protected, Restricted, Standard};
    use EnforcementKindType::{
        Closure, CommunityRemoval, ContentRemoval, FeatureLimit, ProfileReset, Suspension, Warning,
    };
    &[
        reason(3, Standard, None),
        reason(4, Standard, None),
        reason(5, Standard, None),
        reason(6, Standard, None),
        reason(7, Protected, None),
        reason(8, Standard, None),
        reason(9, Restricted, Some(RESTRICTED_KINDS)),
        reason(10, Protected, None),
        reason(11, Protected, None),
        reason(12, Standard, Some(PAYMENT_KINDS)),
        reason(13, Restricted, Some(RESTRICTED_KINDS)),
        reason(14, Standard, Some(PAYMENT_KINDS)),
        reason(15, Protected, None),
        reason(16, Standard, None),
        reason(17, Standard, Some(&[Warning, Suspension, Closure])),
        reason(18, Standard, None),
        reason(20, Standard, None),
        reason(21, Standard, None),
        reason(22, Standard, None),
        reason(23, Protected, None),
        reason(24, Restricted, Some(RESTRICTED_KINDS)),
        reason(
            25,
            Standard,
            Some(&[
                Warning,
                Suspension,
                ContentRemoval,
                ProfileReset,
                CommunityRemoval,
            ]),
        ),
        reason(26, Standard, None),
        reason(27, Standard, None),
        reason(28, Standard, None),
        reason(29, Standard, Some(&[Warning, FeatureLimit, Suspension])),
        reason(30, Standard, Some(&[Closure])),
        reason(
            31,
            Standard,
            Some(&[Warning, ContentRemoval, ProfileReset, CommunityRemoval]),
        ),
    ]
};

pub fn enforcement_reason(code: i32) -> Option<&'static EnforcementReason> {
    ENFORCEMENT_REASONS
        .iter()
        .find(|reason| reason.code == code)
}

pub const GUILD_FEATURES: &[&str] = &[
    "ANIMATED_ICON",
    "ANIMATED_BANNER",
    "AUDIO_BITRATE_128_KBPS",
    "AUDIO_BITRATE_256_KBPS",
    "AUDIO_BITRATE_384_KBPS",
    "BANNER",
    "CLONE_EMOJI_DISABLED",
    "CLONE_EMOJI_ENABLED",
    "CLONE_STICKER_DISABLED",
    "CLONE_STICKER_ENABLED",
    "DETACHED_BANNER",
    "INVITE_SPLASH",
    "INVITES_DISABLED",
    "RAID_DETECTED",
    "TEXT_CHANNEL_FLEXIBLE_NAMES",
    "HIDE_OWNER_CROWN",
    "MORE_EMOJI",
    "MORE_STICKERS",
    "UNLIMITED_EMOJI",
    "UNLIMITED_STICKERS",
    "EXPRESSION_PURGE_ALLOWED",
    "VANITY_URL",
    "DISCOVERABLE",
    "PARTNERED",
    "VERIFIED",
    "VIP_VOICE",
    "VOICE_E2EE",
    "UNAVAILABLE_FOR_EVERYONE",
    "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF",
    "UNAVAILABLE_HIDDEN",
    "VISIONARY",
    "LARGE_GUILD_OVERRIDE",
    "VERY_LARGE_GUILD",
    "ANNOUNCEMENT_CHANNELS_DISABLED",
];

pub const DEPRECATED_GUILD_FEATURES: &[&str] = &["CLONE_EMOJI_DISABLED", "CLONE_STICKER_DISABLED"];

#[cfg(test)]
mod tests {
    use super::{
        DELETION_REASONS, ENFORCEMENT_LIMITS, ENFORCEMENT_REASONS, EnforcementHandling, USER_FLAGS,
    };
    use crate::api::generated::types::DeletionReasonCode;
    use std::collections::HashMap;

    const CORE_TS: &str = include_str!("../../packages/constants/src/Core.ts");
    const ENFORCEMENT_REASONS_TS: &str =
        include_str!("../../packages/constants/src/EnforcementReasons.ts");

    fn quoted_values(list: &str) -> Vec<String> {
        list.split('\'')
            .skip(1)
            .step_by(2)
            .map(ToOwned::to_owned)
            .collect()
    }

    fn deletion_reason_codes() -> HashMap<String, i32> {
        let block = CORE_TS
            .split("export const DeletionReasons = {")
            .nth(1)
            .and_then(|rest| rest.split('}').next())
            .expect("DeletionReasons block");
        block
            .lines()
            .filter_map(|line| {
                let (name, code) = line.trim().trim_end_matches(',').split_once(": ")?;
                Some((name.to_owned(), code.parse().ok()?))
            })
            .collect()
    }

    fn kind_lists() -> HashMap<String, Vec<String>> {
        ENFORCEMENT_REASONS_TS
            .lines()
            .filter_map(|line| {
                let rest = line.strip_prefix("const ")?;
                let (name, value) = rest.split_once(": ReadonlyArray<EnforcementKind> = ")?;
                Some((name.to_owned(), quoted_values(value)))
            })
            .collect()
    }

    fn typescript_reasons() -> Vec<(i32, String, Option<Vec<String>>)> {
        let codes = deletion_reason_codes();
        let lists = kind_lists();
        let block = ENFORCEMENT_REASONS_TS
            .split("export const ENFORCEMENT_REASONS")
            .nth(1)
            .and_then(|rest| rest.split("\n};").next())
            .expect("ENFORCEMENT_REASONS block");
        block
            .split("[DeletionReasons.")
            .skip(1)
            .map(|entry| {
                let name = entry.split(']').next().expect("reason name");
                let handling = entry
                    .split("handling: '")
                    .nth(1)
                    .and_then(|rest| rest.split('\'').next())
                    .expect("handling");
                let kinds = entry.split("kinds: ").nth(1).expect("kinds").trim_start();
                let kinds = if kinds.starts_with("'any'") {
                    None
                } else if kinds.starts_with('[') {
                    Some(quoted_values(kinds.split(']').next().expect("kind list")))
                } else {
                    let list = kinds
                        .split(|c: char| !c.is_ascii_alphanumeric() && c != '_')
                        .next()
                        .expect("kind list name");
                    Some(lists.get(list).expect("named kind list").clone())
                };
                (
                    *codes.get(name).expect("reason code"),
                    handling.to_owned(),
                    kinds,
                )
            })
            .collect()
    }

    fn typescript_reason_rules() -> HashMap<String, Vec<i32>> {
        let codes = deletion_reason_codes();
        let block = ENFORCEMENT_REASONS_TS
            .split("export const ENFORCEMENT_REASONS")
            .nth(1)
            .and_then(|rest| rest.split("\n};").next())
            .expect("ENFORCEMENT_REASONS block");
        let mut rules: HashMap<String, Vec<i32>> = HashMap::new();
        for entry in block.split("[DeletionReasons.").skip(1) {
            let name = entry.split(']').next().expect("reason name");
            let rule = entry
                .split("rule: '")
                .nth(1)
                .and_then(|rest| rest.split('\'').next())
                .expect("rule");
            rules
                .entry(rule.to_owned())
                .or_default()
                .push(*codes.get(name).expect("reason code"));
        }
        rules
    }

    fn typescript_limit_rules(limit: &str) -> Vec<String> {
        let block = ENFORCEMENT_REASONS_TS
            .split("const ENFORCEMENT_LIMIT_RULES")
            .nth(1)
            .and_then(|rest| rest.split("\n};").next())
            .expect("ENFORCEMENT_LIMIT_RULES block");
        let value = block
            .split(&format!("{limit}: "))
            .nth(1)
            .and_then(|rest| rest.split(",\n").next())
            .expect("limit rules");
        if value.starts_with("ENFORCEMENT_SPAMMER_LIMIT_RULES") {
            let spammer = ENFORCEMENT_REASONS_TS
                .split("const ENFORCEMENT_SPAMMER_LIMIT_RULES: ")
                .nth(1)
                .and_then(|rest| rest.split("]);").next())
                .expect("spammer rules");
            return quoted_values(spammer);
        }
        quoted_values(value)
    }

    #[test]
    fn enforcement_limits_match_the_shared_constants() {
        let rules = typescript_reason_rules();
        assert_eq!(ENFORCEMENT_LIMITS.len(), 3);
        for limit in ENFORCEMENT_LIMITS {
            let mut expected: Vec<i32> = typescript_limit_rules(limit.value)
                .iter()
                .flat_map(|rule| rules.get(rule).cloned().unwrap_or_default())
                .collect();
            expected.sort_unstable();
            let mut actual = limit.codes.to_vec();
            actual.sort_unstable();
            assert_eq!(actual, expected, "codes of the {} limit", limit.value);
        }
    }

    #[test]
    fn feature_limits_are_offered_only_for_reasons_with_a_limit() {
        use crate::api::generated::types::EnforcementKindType::FeatureLimit;
        let offered: Vec<i32> = ENFORCEMENT_REASONS
            .iter()
            .filter(|reason| reason.allows(FeatureLimit))
            .map(|reason| reason.code)
            .collect();
        assert_eq!(offered, vec![3, 6, 8, 12, 14, 18, 21, 29]);
    }

    #[test]
    fn restriction_flags_are_read_only_except_a_self_disabled_account() {
        use super::{premium_flag_is_editable, user_flag_bits as bits, user_flag_is_editable};
        for flag in [bits::DELETED, bits::SPAMMER, bits::REPORT_BANNED] {
            assert!(!user_flag_is_editable(flag, 0, false));
            assert!(!user_flag_is_editable(flag, flag, false));
        }
        assert!(!user_flag_is_editable(bits::DISABLED, 0, false));
        assert!(!user_flag_is_editable(bits::DISABLED, bits::DISABLED, true));
        assert!(user_flag_is_editable(bits::DISABLED, bits::DISABLED, false));
        assert!(user_flag_is_editable(bits::ACCOUNT_LIMITED, 0, true));
        assert!(user_flag_is_editable(bits::STAFF, bits::STAFF, false));
        assert!(!premium_flag_is_editable(1 << 6));
        assert!(premium_flag_is_editable(1 << 0));
    }

    #[test]
    fn enforcement_reasons_match_the_shared_constants() {
        let expected = typescript_reasons();
        assert_eq!(expected.len(), ENFORCEMENT_REASONS.len());
        for (code, handling, kinds) in expected {
            let reason = super::enforcement_reason(code)
                .unwrap_or_else(|| panic!("{code} is missing from ENFORCEMENT_REASONS"));
            let rust_handling = match reason.handling {
                EnforcementHandling::Standard => "standard",
                EnforcementHandling::Protected => "protected",
                EnforcementHandling::Restricted => "restricted",
            };
            assert_eq!(rust_handling, handling, "handling of {code}");
            let rust_kinds = reason
                .kinds
                .map(|kinds| kinds.iter().map(ToString::to_string).collect::<Vec<_>>());
            assert_eq!(rust_kinds, kinds, "kinds of {code}");
        }
    }

    #[test]
    fn user_flags_cover_every_flag_in_the_admin_spec() {
        let spec: serde_json::Value =
            serde_json::from_str(include_str!("../openapi-admin.json")).expect("admin spec");
        let values = spec["components"]["schemas"]["UserFlags"]["x-bitflagValues"]
            .as_array()
            .expect("UserFlags bitflag values");
        assert!(!values.is_empty());
        for entry in values {
            let name = entry["name"].as_str().expect("flag name");
            let value: u64 = entry["value"]
                .as_str()
                .expect("flag value")
                .parse()
                .expect("numeric flag value");
            assert!(
                USER_FLAGS
                    .iter()
                    .any(|flag| flag.name == name && flag.value == value),
                "{name} ({value}) is missing from USER_FLAGS"
            );
        }
    }

    #[test]
    fn deletion_reasons_match_the_generated_reason_codes() {
        let spec: serde_json::Value =
            serde_json::from_str(include_str!("../openapi-admin.json")).expect("admin spec");
        let mut spec_codes: Vec<i32> = spec["components"]["schemas"]["DeletionReasonCode"]["enum"]
            .as_array()
            .expect("DeletionReasonCode values")
            .iter()
            .map(|value| {
                i32::try_from(value.as_i64().expect("numeric reason code")).expect("i32 code")
            })
            .collect();
        spec_codes.sort_unstable();
        let mut label_codes: Vec<i32> = DELETION_REASONS
            .iter()
            .map(|(code, _)| code.parse().expect("numeric reason code"))
            .collect();
        label_codes.sort_unstable();
        assert_eq!(label_codes, spec_codes);
        for code in label_codes {
            assert!(
                DeletionReasonCode::try_from(code).is_ok(),
                "{code} is not a generated DeletionReasonCode"
            );
        }
    }
}
