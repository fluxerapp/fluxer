// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{GuildAuditLogEntry, GuildAuditLogUser, GuildInfo},
    config::AdminConfig,
    templates::components::{page_container::card_with_header, table::data_table},
    utils::{
        bigint::format_discriminator, timestamps::snowflake_creation_date, user_tag::user_tag,
    },
};
use maud::{Markup, html};

const PAGE_SIZE: usize = 50;

pub fn audit_log_tab(
    config: &AdminConfig,
    guild: &GuildInfo,
    entries: &[GuildAuditLogEntry],
    users: &[GuildAuditLogUser],
) -> Markup {
    let base = &config.base_path;
    let oldest_log_id = if entries.len() == PAGE_SIZE {
        entries.last().map(|e| e.id.as_str())
    } else {
        None
    };

    html! {
        div class="space-y-4" {
            div class="flex items-center justify-between" {
                h3 class="text-base font-medium text-neutral-900" {
                    "Guild Audit Log"
                }
                p class="text-sm text-neutral-500" {
                    @if entries.len() == PAGE_SIZE {
                        "Showing latest " (PAGE_SIZE)
                    } @else {
                        "Showing " (entries.len())
                    }
                }
            }

            @if entries.is_empty() {
                (card_with_header("Audit Log", html! {
                    p class="text-sm text-neutral-500" {
                        "No audit log entries for this guild."
                    }
                }))
            } @else {
                (data_table(
                    &["Timestamp", "Action", "Actor", "Target", "Details"],
                    html! {
                        @for entry in entries {
                            (audit_log_row(base, entry, users))
                        }
                    },
                ))
            }

            @if let Some(before_id) = oldest_log_id {
                div class="flex justify-center gap-2" {
                    a href={(base) "/guilds/" (guild.id) "?tab=audit_log&before=" (before_id)}
                        class="inline-flex items-center rounded-md border border-neutral-300 \
                               bg-white px-3 py-2 text-sm font-medium text-neutral-700 \
                               hover:bg-neutral-50" {
                        "Older entries \u{2192}"
                    }
                }
            }
        }
    }
}

fn action_label(action_type: i32) -> &'static str {
    match action_type {
        1 => "Guild Update",
        10 => "Channel Create",
        11 => "Channel Update",
        12 => "Channel Delete",
        13 => "Overwrite Create",
        14 => "Overwrite Update",
        15 => "Overwrite Delete",
        20 => "Member Kick",
        21 => "Member Prune",
        22 => "Member Ban",
        23 => "Member Unban",
        24 => "Member Update",
        25 => "Member Role Update",
        26 => "Member Move",
        27 => "Member Disconnect",
        28 => "Bot Add",
        30 => "Role Create",
        31 => "Role Update",
        32 => "Role Delete",
        40 => "Invite Create",
        41 => "Invite Update",
        42 => "Invite Delete",
        50 => "Webhook Create",
        51 => "Webhook Update",
        52 => "Webhook Delete",
        60 => "Emoji Create",
        61 => "Emoji Update",
        62 => "Emoji Delete",
        72 => "Message Delete",
        73 => "Message Bulk Delete",
        74 => "Message Pin",
        75 => "Message Unpin",
        90 => "Sticker Create",
        91 => "Sticker Update",
        92 => "Sticker Delete",
        _ => "Unknown",
    }
}

fn find_user<'a>(users: &'a [GuildAuditLogUser], id: &str) -> Option<&'a GuildAuditLogUser> {
    users.iter().find(|u| u.id == id)
}

fn format_user(user: Option<&GuildAuditLogUser>) -> String {
    let Some(u) = user else {
        return "Unknown".to_string();
    };
    let disc = u.discriminator.as_deref().unwrap_or("0000");
    let disc = format_discriminator(disc);
    let tag = user_tag(&u.username, &disc, false);
    match &u.global_name {
        Some(gn) if !gn.trim().is_empty() => format!("{} ({})", gn, tag),
        _ => tag,
    }
}

fn audit_log_row(base: &str, entry: &GuildAuditLogEntry, users: &[GuildAuditLogUser]) -> Markup {
    html! {
        tr class="hover:bg-neutral-50 transition-colors" {
            td class="px-4 py-3 text-sm" {
                span class="text-neutral-900" { (snowflake_creation_date(&entry.id)) }
                br;
                span class="text-xs text-neutral-500" { (entry.id) }
            }
            td class="whitespace-nowrap px-4 py-3 text-sm text-neutral-900" {
                (action_label(entry.action_type))
            }
            td class="whitespace-nowrap px-4 py-3 text-sm" {
                @if let Some(ref uid) = entry.user_id {
                    a href={(base) "/users/" (uid)} class="hover:underline" {
                        (format_user(find_user(users, uid)))
                    }
                } @else {
                    span class="text-neutral-500" { "System" }
                }
            }
            td class="whitespace-nowrap px-4 py-3 text-sm" {
                @if let Some(ref tid) = entry.target_id {
                    span class="text-sm" { (tid) }
                } @else {
                    span class="text-neutral-500" { "\u{2014}" }
                }
            }
            td class="px-4 py-3 text-sm" {
                (details_cell(entry))
            }
        }
    }
}

fn details_cell(entry: &GuildAuditLogEntry) -> Markup {
    let has_content = entry.reason.is_some()
        || entry.options.is_some()
        || entry.changes.as_ref().is_some_and(|c| !c.is_empty());

    if !has_content {
        return html! { span class="text-neutral-500" { "\u{2014}" } };
    }

    html! {
        div class="max-w-md space-y-1" {
            @if let Some(ref reason) = entry.reason {
                p class="text-sm" {
                    strong { "Reason: " }
                    (reason)
                }
            }
            @if let Some(ref opts) = entry.options {
                details {
                    summary class="cursor-pointer text-sm text-neutral-600" {
                        "Options"
                    }
                    pre class="mt-1 max-h-48 overflow-auto rounded bg-neutral-100 p-2 text-xs" {
                        (serde_json::to_string_pretty(opts).unwrap_or_default())
                    }
                }
            }
            @if let Some(ref changes) = entry.changes {
                @if !changes.is_empty() {
                    details {
                        summary class="cursor-pointer text-sm text-neutral-600" {
                            "Changes (" (changes.len()) ")"
                        }
                        pre class="mt-1 max-h-64 overflow-auto rounded bg-neutral-100 p-2 text-xs" {
                            (serde_json::to_string_pretty(changes).unwrap_or_default())
                        }
                    }
                }
            }
        }
    }
}
