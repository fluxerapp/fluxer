// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::GuildDetailInfo,
    config::AdminConfig,
    templates::components::{
        form::{checkbox, csrf_input, form_actions, submit_button},
        page_container::card_with_header,
    },
};
use maud::{Markup, html};

const INPUT_CLS: &str = "block w-full rounded-md border border-neutral-300 px-3 py-2 \
                          text-sm shadow-sm focus:border-brand-primary focus:outline-none \
                          focus:ring-1 focus:ring-brand-primary";
const SELECT_CLS: &str = "block w-full rounded-md border border-neutral-300 px-3 py-2 \
                           text-sm shadow-sm focus:border-brand-primary focus:outline-none \
                           focus:ring-1 focus:ring-brand-primary";
const CONTENT_WARNING_TEXT_MAX_LENGTH: usize = 200;
const DISABLED_OPERATIONS: &[(&str, i32)] = &[
    ("PUSH_NOTIFICATIONS", 1 << 0),
    ("EVERYONE_MENTIONS", 1 << 1),
    ("TYPING_EVENTS", 1 << 2),
    ("INSTANT_INVITES", 1 << 3),
    ("SEND_MESSAGE", 1 << 4),
    ("REACTIONS", 1 << 5),
    ("MEMBER_LIST_UPDATES", 1 << 6),
];

pub fn settings_tab(
    config: &AdminConfig,
    guild: &GuildDetailInfo,
    csrf_token: &str,
    admin_acls: &[String],
) -> Markup {
    let can_edit = acl::has_permission(admin_acls, acl::GUILD_UPDATE_SETTINGS);

    if !can_edit {
        return settings_tab_readonly(guild);
    }

    let base = &config.base_path;
    html! {
        div class="space-y-6" {
            (card_with_header("Guild Settings", html! {
                form method="post"
                    action={(base) "/guilds/" (guild.id) "?action=update_settings&tab=settings"} {
                    (csrf_input(csrf_token))
                    div class="grid gap-4 md:grid-cols-2" {
                        (select_field(
                            "guild-verification-level",
                            "verification_level",
                            "Verification Level",
                            guild.verification_level.unwrap_or(0).min(3),
                            &[
                                (0, "None"),
                                (1, "Low (verified email)"),
                                (2, "Medium (5+ minutes)"),
                                (3, "High (10+ minutes)"),
                            ],
                        ))
                        (select_field(
                            "guild-mfa-level",
                            "mfa_level",
                            "MFA Level",
                            guild.mfa_level.unwrap_or(0),
                            &[(0, "None"), (1, "Elevated")],
                        ))
                        (select_field(
                            "guild-explicit-content-filter",
                            "explicit_content_filter",
                            "Explicit Content Filter",
                            guild.explicit_content_filter.unwrap_or(0),
                            &[
                                (0, "Disabled"),
                                (1, "Members without roles"),
                                (2, "All members"),
                            ],
                        ))
                        (select_field(
                            "guild-default-notifications",
                            "default_message_notifications",
                            "Default Notifications",
                            guild.default_message_notifications.unwrap_or(0),
                            &[(0, "All messages"), (1, "Only mentions")],
                        ))
                    }
                    div class="mt-6 space-y-4 border-t border-neutral-200 pt-6" {
                        h3 class="text-sm font-semibold text-neutral-900" {
                            "Content Rating"
                        }
                        p class="text-sm text-neutral-500" {
                            "Adult content drives the age-verification gate. Content warnings are an advisory shown before entering, with optional custom copy."
                        }
                        input type="hidden" name="nsfw_submitted" value="1";
                        input type="hidden" name="content_warning_submitted" value="1";
                        div class="flex flex-wrap gap-6" {
                            (checkbox(
                                "nsfw",
                                "on",
                                "Adult content (18+)",
                                guild.nsfw.unwrap_or(false),
                                true,
                            ))
                            (checkbox(
                                "content_warning_level",
                                "on",
                                "Show a content warning",
                                guild.content_warning_level == Some(1),
                                true,
                            ))
                        }
                        div class="space-y-2" {
                            label for="guild-content-warning-text"
                                class="block text-sm font-medium text-neutral-700" {
                                "Custom warning text"
                            }
                            textarea id="guild-content-warning-text"
                                name="content_warning_text"
                                maxlength=(CONTENT_WARNING_TEXT_MAX_LENGTH)
                                rows="3"
                                placeholder="This contains sensitive content."
                                class=(INPUT_CLS) {
                                (guild.content_warning_text.as_deref().unwrap_or(""))
                            }
                            p class="text-xs text-neutral-500" {
                                "Up to " (CONTENT_WARNING_TEXT_MAX_LENGTH) " characters. Shown verbatim in the consent dialog when the warning is enabled. Leave empty to use the localized default."
                            }
                        }
                    }
                    div class="mt-6 border-t border-neutral-200 pt-6" {
                        (form_actions(html! {
                        (submit_button("Save Settings"))
                        }))
                    }
                }
            }))

            (card_with_header("Disabled Operations", html! {
                form method="post" id="disabled-ops-form"
                    action={(base) "/guilds/" (guild.id) "?action=update_disabled_operations&tab=settings"} {
                    (csrf_input(csrf_token))
                    div class="space-y-3" {
                        @for &(name, value) in DISABLED_OPERATIONS {
                            (checkbox(
                                "disabled_operations[]",
                                &value.to_string(),
                                name,
                                guild.disabled_operations.unwrap_or(0) & value == value,
                                true,
                            ))
                        }
                    }
                    div class="mt-6 border-t border-neutral-200 pt-6" {
                        (form_actions(html! {
                        (submit_button("Save Changes"))
                        }))
                    }
                }
            }))

            (card_with_header("Clear Guild Fields", html! {
                form method="post"
                    action={(base) "/guilds/" (guild.id) "?action=clear_fields&tab=settings"} {
                    (csrf_input(csrf_token))
                    div class="space-y-4" {
                        div class="space-y-2" {
                            (checkbox("fields[]", "icon", "Icon", false, true))
                            (checkbox("fields[]", "banner", "Banner", false, true))
                            (checkbox("fields[]", "splash", "Splash", false, true))
                            (checkbox("fields[]", "embed_splash", "Embed Splash", false, true))
                        }
                        (form_actions(html! {
                            (submit_button("Clear Selected Fields"))
                        }))
                    }
                }
            }))

            (card_with_header("Update Name", html! {
                form method="post"
                    action={(base) "/guilds/" (guild.id) "?action=update_name"} {
                    (csrf_input(csrf_token))
                    div class="space-y-3" {
                        input type="text" name="name" value=(guild.name)
                            class=(INPUT_CLS);
                        (form_actions(html! {
                            button type="submit"
                                class="inline-flex items-center rounded-md bg-brand-primary px-4 \
                                       py-2 text-sm font-medium text-white shadow-sm \
                                       hover:bg-brand-primary-dark" {
                                "Update Name"
                            }
                        }))
                    }
                }
            }))
        }
    }
}

fn select_field(
    id: &str,
    name: &str,
    label: &str,
    current: i32,
    options: &[(i32, &str)],
) -> Markup {
    html! {
        div class="space-y-2" {
            label for=(id) class="block text-sm font-medium text-neutral-700" {
                (label)
            }
            select id=(id) name=(name) class=(SELECT_CLS) {
                @for &(value, option_label) in options {
                    option value=(value) selected[value == current] {
                        (option_label)
                    }
                }
            }
        }
    }
}

fn settings_tab_readonly(guild: &GuildDetailInfo) -> Markup {
    let verification_label = match guild.verification_level.unwrap_or(0).min(3) {
        0 => "None",
        1 => "Low (verified email)",
        2 => "Medium (5+ minutes)",
        3 => "High (10+ minutes)",
        _ => "Unknown",
    };
    let mfa_label = match guild.mfa_level.unwrap_or(0) {
        0 => "None",
        1 => "Elevated",
        _ => "Unknown",
    };
    let content_filter_label = match guild.explicit_content_filter.unwrap_or(0) {
        0 => "Disabled",
        1 => "Members without roles",
        2 => "All members",
        _ => "Unknown",
    };
    let notifications_label = match guild.default_message_notifications.unwrap_or(0) {
        0 => "All messages",
        1 => "Only mentions",
        _ => "Unknown",
    };
    let nsfw_status = if guild.nsfw.unwrap_or(false) {
        "Yes"
    } else {
        "No"
    };
    let content_warning_status = if guild.content_warning_level == Some(1) {
        "Yes"
    } else {
        "No"
    };

    html! {
        div class="space-y-6" {
            (card_with_header("Guild Settings", html! {
                p class="mb-4 text-sm text-neutral-500" {
                    "You do not have permission to edit guild settings."
                }
                div class="grid gap-4 md:grid-cols-2" {
                    (readonly_field("Verification Level", verification_label))
                    (readonly_field("MFA Level", mfa_label))
                    (readonly_field("Explicit Content Filter", content_filter_label))
                    (readonly_field("Default Notifications", notifications_label))
                    (readonly_field("Adult Content (18+)", nsfw_status))
                    (readonly_field("Content Warning", content_warning_status))
                }
            }))
        }
    }
}

fn readonly_field(label: &str, value: &str) -> Markup {
    html! {
        div class="space-y-1" {
            dt class="text-sm font-medium text-neutral-700" { (label) }
            dd class="text-sm text-neutral-900" { (value) }
        }
    }
}
