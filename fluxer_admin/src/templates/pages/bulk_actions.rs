// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            form::{
                checkbox, csrf_input, danger_button, form_actions, form_field_group, select_input,
                submit_button, text_input, textarea_input,
            },
            page_container::page_header,
            section_card::section_card_simple,
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

const DELETION_REASONS: &[(&str, &str)] = &[
    ("1", "User requested"),
    ("2", "Other"),
    ("3", "Spam"),
    ("4", "Cheating or exploitation"),
    ("5", "Coordinated raiding or manipulation"),
    ("6", "Automation or self-bot usage"),
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
];
struct UserFlag {
    name: &'static str,
    value: u64,
}
const PATCHABLE_USER_FLAGS: &[UserFlag] = &[
    UserFlag {
        name: "STAFF",
        value: 1 << 0,
    },
    UserFlag {
        name: "PARTNER",
        value: 1 << 2,
    },
    UserFlag {
        name: "BUG_HUNTER",
        value: 1 << 3,
    },
    UserFlag {
        name: "FRIENDLY_BOT",
        value: 1 << 4,
    },
    UserFlag {
        name: "FRIENDLY_BOT_MANUAL_APPROVAL",
        value: 1 << 5,
    },
    UserFlag {
        name: "SPAMMER",
        value: 1 << 6,
    },
    UserFlag {
        name: "HIGH_GLOBAL_RATE_LIMIT",
        value: 1 << 33,
    },
    UserFlag {
        name: "DELETED",
        value: 1 << 34,
    },
    UserFlag {
        name: "DISABLED_SUSPICIOUS_ACTIVITY",
        value: 1 << 35,
    },
    UserFlag {
        name: "SELF_DELETED",
        value: 1 << 36,
    },
    UserFlag {
        name: "DISABLED",
        value: 1 << 38,
    },
    UserFlag {
        name: "HAS_SESSION_STARTED",
        value: 1 << 39,
    },
    UserFlag {
        name: "RATE_LIMIT_BYPASS",
        value: 1 << 47,
    },
    UserFlag {
        name: "REPORT_BANNED",
        value: 1 << 48,
    },
    UserFlag {
        name: "VERIFIED_NOT_UNDERAGE",
        value: 1 << 49,
    },
    UserFlag {
        name: "HAS_DISMISSED_PREMIUM_ONBOARDING",
        value: 1 << 51,
    },
    UserFlag {
        name: "APP_STORE_REVIEWER",
        value: 1 << 53,
    },
    UserFlag {
        name: "STAFF_HIDDEN",
        value: 1 << 57,
    },
    UserFlag {
        name: "AGE_VERIFIED_ADULT",
        value: 1 << 60,
    },
    UserFlag {
        name: "FORCE_INBOUND_PHONE_VERIFICATION",
        value: 1 << 61,
    },
    UserFlag {
        name: "NOT_SUSPICIOUS",
        value: 1 << 62,
    },
];

const SUSPICIOUS_ACTIVITY_FLAGS: &[&str] = &[
    "REQUIRE_VERIFIED_EMAIL",
    "REQUIRE_REVERIFIED_EMAIL",
    "REQUIRE_VERIFIED_PHONE",
    "REQUIRE_REVERIFIED_PHONE",
    "REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE",
    "REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE",
    "REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE",
    "REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE",
    "REQUIRE_INBOUND_PHONE_VERIFICATION",
];
const GUILD_FEATURES: &[&str] = &[
    "ANIMATED_ICON",
    "ANIMATED_BANNER",
    "BANNER",
    "CLONE_EMOJI_DISABLED",
    "CLONE_STICKER_DISABLED",
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
];

pub fn bulk_actions_page(config: &AdminConfig, auth: &AuthContext, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    let admin_acls = auth
        .admin_user
        .as_ref()
        .map(|u| u.acls.as_slice())
        .unwrap_or(&[]);

    let content = html! {
        (page_header("Bulk Actions", Some("Perform bulk operations on users and guilds")))
        div class="space-y-6" {
            @if acl::has_permission(admin_acls, acl::BULK_UPDATE_USER_FLAGS) {
                (bulk_update_user_flags_section(base, csrf_token))
            }
            @if acl::has_permission(admin_acls, acl::BULK_UPDATE_SUSPICIOUS_ACTIVITY) {
                (bulk_update_suspicious_activity_section(base, csrf_token))
            }
            @if acl::has_permission(admin_acls, acl::BULK_UPDATE_GUILD_FEATURES) {
                (bulk_update_guild_features_section(base, csrf_token))
            }
            @if acl::has_permission(admin_acls, acl::BULK_ADD_GUILD_MEMBERS) {
                (bulk_add_guild_members_section(base, csrf_token))
            }
            @if acl::has_permission(admin_acls, acl::BULK_DELETE_USERS) {
                (bulk_schedule_deletion_section(base, csrf_token))
            }
        }
    };
    admin_layout(config, auth, "Bulk Actions", "bulk-actions", None, content)
}

fn flag_checkbox_grid(prefix: &str, flags: &[&str]) -> Markup {
    html! {
        div class="grid grid-cols-1 gap-3 sm:grid-cols-2" {
            @for flag in flags {
                (checkbox(prefix, flag, flag, false, true))
            }
        }
    }
}

fn user_flag_checkbox_grid(prefix: &str) -> Markup {
    html! {
        div class="grid grid-cols-1 gap-3 sm:grid-cols-2" {
            @for flag in PATCHABLE_USER_FLAGS {
                (checkbox(prefix, &flag.value.to_string(), flag.name, false, true))
            }
        }
    }
}

fn bulk_update_user_flags_section(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Bulk Update User Flags",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-update-user-flags"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (textarea_input("user_ids", "User IDs (one per line)", "123456789\n987654321", "", 5, true))
                    div {
                        p class="font-semibold text-neutral-500 text-xs uppercase tracking-wide mb-2" {
                            "Flags to Add"
                        }
                        (user_flag_checkbox_grid("add_flags[]"))
                    }
                    div {
                        p class="font-semibold text-neutral-500 text-xs uppercase tracking-wide mb-2" {
                            "Flags to Remove"
                        }
                        (user_flag_checkbox_grid("remove_flags[]"))
                    }
                    (text_input("audit_log_reason", "Audit Log Reason (optional)", "", "Reason for this bulk operation"))
                    (form_actions(html! {
                        (submit_button("Update User Flags"))
                    }))
                }
            }
        },
    )
}

fn bulk_update_suspicious_activity_section(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Bulk Update Suspicious Activity Flags",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-update-suspicious-activity-flags"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (textarea_input("user_ids", "User IDs (one per line)", "123456789\n987654321", "", 5, true))
                    div {
                        p class="font-semibold text-neutral-500 text-xs uppercase tracking-wide mb-2" {
                            "Flags to Add"
                        }
                        (flag_checkbox_grid("add_flags[]", SUSPICIOUS_ACTIVITY_FLAGS))
                    }
                    div {
                        p class="font-semibold text-neutral-500 text-xs uppercase tracking-wide mb-2" {
                            "Flags to Remove"
                        }
                        (flag_checkbox_grid("remove_flags[]", SUSPICIOUS_ACTIVITY_FLAGS))
                    }
                    (text_input("audit_log_reason", "Audit Log Reason (optional)", "", "Reason for this bulk operation"))
                    (form_actions(html! {
                        (submit_button("Update Suspicious Activity Flags"))
                    }))
                }
            }
        },
    )
}

fn bulk_update_guild_features_section(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Bulk Update Guild Features",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-update-guild-features"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (textarea_input("guild_ids", "Guild IDs (one per line)", "123456789\n987654321", "", 5, true))
                    div {
                        p class="font-semibold text-neutral-500 text-xs uppercase tracking-wide mb-2" {
                            "Features to Add"
                        }
                        (flag_checkbox_grid("add_features[]", GUILD_FEATURES))
                    }
                    div {
                        p class="font-semibold text-neutral-500 text-xs uppercase tracking-wide mb-2" {
                            "Features to Remove"
                        }
                        (flag_checkbox_grid("remove_features[]", GUILD_FEATURES))
                    }
                    (form_field_group("Custom features to add", "custom_add_features", false, None,
                        Some("Comma-separated list of custom features not in the standard set."),
                        html! {
                            input type="text" id="custom_add_features" name="custom_add_features"
                                placeholder="CUSTOM_FEATURE_1, CUSTOM_FEATURE_2"
                                class="w-full rounded-lg border border-neutral-300 bg-white \
                                       text-neutral-900 text-sm h-8 px-3 py-1.5 \
                                       focus:border-brand-primary focus:outline-none \
                                       focus:ring-2 focus:ring-brand-primary/20";
                        },
                    ))
                    (form_field_group("Custom features to remove", "custom_remove_features", false, None,
                        Some("Comma-separated list of custom features to remove."),
                        html! {
                            input type="text" id="custom_remove_features" name="custom_remove_features"
                                placeholder="CUSTOM_FEATURE_1, CUSTOM_FEATURE_2"
                                class="w-full rounded-lg border border-neutral-300 bg-white \
                                       text-neutral-900 text-sm h-8 px-3 py-1.5 \
                                       focus:border-brand-primary focus:outline-none \
                                       focus:ring-2 focus:ring-brand-primary/20";
                        },
                    ))
                    (text_input("audit_log_reason", "Audit Log Reason (optional)", "", "Reason for this bulk operation"))
                    (form_actions(html! {
                        (submit_button("Update Guild Features"))
                    }))
                }
            }
        },
    )
}

fn bulk_add_guild_members_section(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Bulk Add Guild Members",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-add-guild-members"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (text_input("guild_id", "Guild ID", "", "123456789"))
                    (textarea_input("user_ids", "User IDs (one per line)", "123456789\n987654321", "", 5, true))
                    (text_input("audit_log_reason", "Audit Log Reason (optional)", "", "Reason for this bulk operation"))
                    (form_actions(html! {
                        (submit_button("Add Members"))
                    }))
                }
            }
        },
    )
}

fn bulk_schedule_deletion_section(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Bulk Schedule User Deletion",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-schedule-user-deletion"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (textarea_input("user_ids", "User IDs (one per line)", "123456789\n987654321", "", 5, true))
                    (select_input("reason_code", "Deletion Reason", DELETION_REASONS, "1"))
                    (text_input("public_reason", "Public Reason (optional)", "", "Terms of service violation"))
                    (form_field_group("Days Until Deletion", "days_until_deletion", true, None, None, html! {
                        input type="number" id="days_until_deletion" name="days_until_deletion"
                            value="14" min="14" required
                            class="w-full rounded-lg border border-neutral-300 bg-white \
                                   text-neutral-900 text-sm h-8 px-3 py-1.5 \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-2 focus:ring-brand-primary/20";
                    }))
                    (text_input("audit_log_reason", "Audit Log Reason (optional)", "", "Reason for this bulk operation"))
                    (form_actions(html! {
                        (danger_button("Schedule Deletion"))
                    }))
                }
            }
        },
    )
}
