// SPDX-License-Identifier: AGPL-3.0-or-later

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
        value: 1 << 6,
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

#[cfg(test)]
mod tests {
    use super::USER_FLAGS;

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
}
