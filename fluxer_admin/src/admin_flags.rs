// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::generated::types::EnforcementKindType;

pub struct U64Flag {
    pub name: &'static str,
    pub value: u64,
    pub help: &'static str,
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
    pub const AGE_VERIFIED_ADULT: u64 = 1 << 60;
    pub const LIMIT_EXEMPT: u64 = 1 << 62;
}

pub const USER_FLAGS: &[U64Flag] = &[
    U64Flag {
        name: "STAFF",
        value: user_flag_bits::STAFF,
        help: "Shows the staff badge and lifts some limits. It does not grant admin panel access, which comes from ACLs.",
    },
    U64Flag {
        name: "PARTNER",
        value: user_flag_bits::PARTNER,
        help: "Shows the partner badge on the profile.",
    },
    U64Flag {
        name: "BUG_HUNTER",
        value: user_flag_bits::BUG_HUNTER,
        help: "Shows the bug hunter badge on the profile.",
    },
    U64Flag {
        name: "FRIENDLY_BOT",
        value: user_flag_bits::FRIENDLY_BOT,
        help: "On a bot account, accepts friend requests automatically.",
    },
    U64Flag {
        name: "FRIENDLY_BOT_MANUAL_APPROVAL",
        value: user_flag_bits::FRIENDLY_BOT_MANUAL_APPROVAL,
        help: "On a bot account with friendly bot set, leaves friend requests for the bot to accept.",
    },
    U64Flag {
        name: "SPAMMER",
        value: user_flag_bits::SPAMMER,
        help: "Marks the account as a spammer. Set by enforcement actions.",
    },
    U64Flag {
        name: "PROFILE_HIDDEN",
        value: user_flag_bits::PROFILE_HIDDEN,
        help: "Hides the profile details from other people.",
    },
    U64Flag {
        name: "HIGH_GLOBAL_RATE_LIMIT",
        value: user_flag_bits::HIGH_GLOBAL_RATE_LIMIT,
        help: "Raises the global request limit from 50 to 1200 per window.",
    },
    U64Flag {
        name: "DELETED",
        value: user_flag_bits::DELETED,
        help: "The account was deleted by an admin action. Set by the deletion flow.",
    },
    U64Flag {
        name: "SELF_DELETED",
        value: user_flag_bits::SELF_DELETED,
        help: "The account holder asked for deletion. Set by the deletion flow.",
    },
    U64Flag {
        name: "DISABLED",
        value: user_flag_bits::DISABLED,
        help: "The account cannot sign in. Suspensions set it.",
    },
    U64Flag {
        name: "HAS_SESSION_STARTED",
        value: user_flag_bits::HAS_SESSION_STARTED,
        help: "Set the first time the account connects. Nothing to change by hand.",
    },
    U64Flag {
        name: "RATE_LIMIT_BYPASS",
        value: user_flag_bits::RATE_LIMIT_BYPASS,
        help: "Skips request rate limits entirely. Use only for trusted automation.",
    },
    U64Flag {
        name: "REPORT_BANNED",
        value: user_flag_bits::REPORT_BANNED,
        help: "The account can no longer file reports.",
    },
    U64Flag {
        name: "VERIFIED_NOT_UNDERAGE",
        value: user_flag_bits::VERIFIED_NOT_UNDERAGE,
        help: "Set by the age check of the official instance. Self-hosted instances do not run that check.",
    },
    U64Flag {
        name: "ACCOUNT_LIMITED",
        value: user_flag_bits::ACCOUNT_LIMITED,
        help: "Limits what a new or suspicious account can do until it is reviewed.",
    },
    U64Flag {
        name: "HAS_DISMISSED_PREMIUM_ONBOARDING",
        value: user_flag_bits::HAS_DISMISSED_PREMIUM_ONBOARDING,
        help: "Set by the app after the person closed the premium introduction.",
    },
    U64Flag {
        name: "APP_STORE_REVIEWER",
        value: user_flag_bits::APP_STORE_REVIEWER,
        help: "For accounts used by app store reviewers. Skips new IP checks and blocks direct messages from strangers.",
    },
    U64Flag {
        name: "AGE_VERIFIED_ADULT",
        value: user_flag_bits::AGE_VERIFIED_ADULT,
        help: "Set by the age verification of the official instance. Self-hosted instances do not run it.",
    },
    U64Flag {
        name: "LIMIT_EXEMPT",
        value: user_flag_bits::LIMIT_EXEMPT,
        help: "Exempts the account from account limits and the new conversation limit.",
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
        name: "BADGE_MASKED",
        value: 1 << 2,
    },
    I32Flag {
        name: "BADGE_TIMESTAMP_HIDDEN",
        value: 1 << 3,
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
    I32Flag {
        name: "OPERATOR_PASS_BADGE_TIMESTAMP_HIDDEN",
        value: 1 << 11,
    },
    I32Flag {
        name: "OPERATOR_PASS_BADGE_COUNT_HIDDEN",
        value: 1 << 12,
    },
];

const DELETION_REASONS: &[(i32, &str)] = &[
    (1, "User requested"),
    (2, "Other"),
    (3, "Spam"),
    (4, "Cheating or exploitation"),
    (5, "Coordinated raiding or manipulation"),
    (6, "Disruptive automation"),
    (7, "Nonconsensual sexual content"),
    (8, "Scam or social engineering"),
    (9, "Child sexual content"),
    (10, "Privacy violation or doxxing"),
    (11, "Harassment or bullying"),
    (12, "Payment fraud"),
    (13, "Child safety violation"),
    (14, "Billing dispute or abuse"),
    (15, "Unsolicited explicit content"),
    (16, "Graphic violence"),
    (17, "Ban evasion"),
    (18, "Token or credential scam"),
    (19, "Inactivity"),
    (20, "Hate speech or extremist content"),
    (21, "Malicious links or malware distribution"),
    (22, "Impersonation or fake identity"),
    (23, "Threats or violent speech"),
    (24, "Violent extremism"),
    (25, "Self-harm promotion"),
    (26, "Sexual content outside 18+ spaces"),
    (27, "Illegal goods or activity"),
    (28, "Harmful deception"),
    (29, "Reporting abuse"),
    (30, "Under the minimum age"),
    (31, "Copyright"),
    (32, "Below minimum age"),
    (33, "Unclaimed account abandoned"),
    (34, "Operator Pass payment dispute"),
];

pub fn deletion_reason_label(code: i32) -> String {
    DELETION_REASONS
        .iter()
        .find(|(candidate, _)| *candidate == code)
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
        codes: &[12, 14, 34],
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
        reason(34, Standard, Some(PAYMENT_KINDS)),
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
    "VISITS_ENABLED",
];

pub const DEPRECATED_GUILD_FEATURES: &[&str] = &["CLONE_EMOJI_DISABLED", "CLONE_STICKER_DISABLED"];
