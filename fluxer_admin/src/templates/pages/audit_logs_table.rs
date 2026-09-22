// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{
        AuditLogChannelSummary, AuditLogEntry, AuditLogGuildSummary, AuditLogUserSummary,
    },
    templates::components::{
        badge::{BadgeVariant, badge},
        resource_link::{ResourceType, resource_link},
        table::{table_body, table_cell, table_head, table_header_cell, table_row},
    },
    utils::bigint::format_discriminator,
};
use maud::{Markup, html};

pub fn format_action(action: &str) -> String {
    capitalise(&action.replace('_', " "))
}

pub fn action_badge_variant(action: &str) -> BadgeVariant {
    match action {
        "temp_ban"
        | "disable_suspicious_activity"
        | "schedule_deletion"
        | "ban_ip"
        | "ban_email" => BadgeVariant::Danger,
        "unban" | "cancel_deletion" | "unban_ip" | "unban_email" => BadgeVariant::Success,
        "update_flags" | "update_features" | "set_acls" | "update_settings" => BadgeVariant::Info,
        "delete_message" => BadgeVariant::Warning,
        _ => BadgeVariant::Default,
    }
}

fn capitalise(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().to_string() + chars.as_str(),
    }
}

fn type_label(target_type: &str) -> String {
    capitalise(&target_type.replace('_', " "))
}

fn user_label(user: &AuditLogUserSummary) -> String {
    let tag = format!(
        "{}#{}",
        user.username,
        format_discriminator(&user.discriminator)
    );
    match user
        .global_name
        .as_ref()
        .filter(|value| !value.trim().is_empty())
    {
        Some(global_name) => format!("{global_name} ({tag})"),
        None => tag,
    }
}

fn channel_label(channel: &AuditLogChannelSummary) -> String {
    channel
        .name
        .as_ref()
        .filter(|value| !value.trim().is_empty())
        .map(|name| format!("#{name}"))
        .unwrap_or_else(|| "Channel".to_owned())
}

pub fn admin_user_cell(base: &str, entry: &AuditLogEntry) -> Markup {
    if entry.admin_user_id.is_empty() {
        return html! { span class="text-sm text-neutral-500 italic" { "-" } };
    }
    let label = entry
        .admin_user
        .as_ref()
        .map(user_label)
        .unwrap_or_else(|| entry.admin_user_id.clone());
    html! {
        (resource_link(base, ResourceType::User, &entry.admin_user_id, html! {
            div class="flex flex-col" {
                span class="text-sm font-medium" { (label) }
                span class="text-xs text-neutral-500" { "ID: " (&entry.admin_user_id) }
            }
        }))
    }
}

fn target_guild_label(guild: Option<&AuditLogGuildSummary>) -> &str {
    guild.map(|guild| guild.name.as_str()).unwrap_or("Guild")
}

pub fn target_cell(base: &str, entry: &AuditLogEntry) -> Markup {
    if let Some(user) = entry.target_user.as_ref() {
        let label = user_label(user);
        return html! {
            (resource_link(base, ResourceType::User, &entry.target_id, html! {
                div class="flex flex-col" {
                    span class="text-sm font-medium" { (label) }
                    span class="text-xs text-neutral-500" { (type_label(&entry.target_type)) ": " (&entry.target_id) }
                }
            }))
        };
    }
    match entry.target_type.as_str() {
        "guild" => html! {
            (resource_link(base, ResourceType::Guild, &entry.target_id, html! {
                div class="flex flex-col" {
                    span class="text-sm font-medium" { (target_guild_label(entry.target_guild.as_ref())) }
                    span class="text-xs text-neutral-500" { "ID: " (&entry.target_id) }
                }
            }))
        },
        "channel" => html! {
            (channel_link(base, &entry.target_id, entry.target_channel.as_ref(), html! {
                div class="flex flex-col" {
                    span class="text-sm font-medium" {
                        @if let Some(channel) = entry.target_channel.as_ref() {
                            (channel_label(channel))
                        } @else {
                            "Channel"
                        }
                    }
                    span class="text-xs text-neutral-500" { "ID: " (&entry.target_id) }
                }
            }))
        },
        "bulk_job" => html! {
            (job_link(base, &entry.target_id, html! {
                div class="flex flex-col" {
                    span class="text-sm font-medium" { "Bulk job" }
                    span class="text-xs text-neutral-500 break-all" { "ID: " (&entry.target_id) }
                }
            }))
        },
        "message" => html! {
            div class="flex flex-col" {
                span class="text-sm font-medium" { "Message" }
                span class="text-xs text-neutral-500 break-all" { "ID: " (&entry.target_id) }
                @if let Some(channel) = entry.target_channel.as_ref() {
                    span class="text-xs text-neutral-500" {
                        "Channel: "
                        (channel_link(base, &channel.id, Some(channel), html! { (channel_label(channel)) }))
                    }
                }
            }
        },
        _ => html! {
            div class="flex flex-col" {
                span class="text-sm font-medium" { (type_label(&entry.target_type)) }
                span class="text-xs text-neutral-500 break-all" { (&entry.target_id) }
            }
        },
    }
}

fn job_link(base: &str, job_id: &str, display: Markup) -> Markup {
    html! {
        a href={(base) "/jobs/" (job_id)} title={"Job " (job_id)}
            class="text-neutral-900 underline decoration-neutral-300 hover:text-neutral-600 hover:decoration-neutral-500 text-sm" {
            (display)
        }
    }
}

fn channel_href(base: &str, channel_id: &str) -> String {
    format!(
        "{base}/messages?channel_id={}&context_limit=50",
        urlencoding::encode(channel_id)
    )
}

fn channel_link(
    base: &str,
    channel_id: &str,
    channel: Option<&AuditLogChannelSummary>,
    display: Markup,
) -> Markup {
    let href = channel_href(base, channel_id);
    let title = channel
        .map(channel_label)
        .unwrap_or_else(|| format!("Channel {channel_id}"));
    html! {
        a href=(href) title=(title)
            class="text-neutral-900 underline decoration-neutral-300 hover:text-neutral-600 hover:decoration-neutral-500 text-sm" {
            (display)
        }
    }
}

fn is_user_id_key(key: &str) -> bool {
    key == "user_id"
        || key == "target_user_id"
        || key == "admin_user_id"
        || key.ends_with("_user_id")
}

fn is_guild_id_key(key: &str) -> bool {
    key == "guild_id" || key.ends_with("_guild_id")
}

fn is_channel_id_key(key: &str) -> bool {
    key == "channel_id" || key.ends_with("_channel_id")
}

fn metadata_value(base: &str, entry: &AuditLogEntry, key: &str, value: &str) -> Markup {
    if is_user_id_key(key)
        && let Some(user) = entry.related_users.get(value)
    {
        let label = user_label(user);
        return html! {
            (resource_link(base, ResourceType::User, value, html! {
                span class="text-sm text-neutral-700" { (label) }
            }))
            span class="ml-2 text-xs text-neutral-500" { (value) }
        };
    }
    if is_guild_id_key(key)
        && let Some(guild) = entry.related_guilds.get(value)
    {
        return html! {
            (resource_link(base, ResourceType::Guild, value, html! {
                span class="text-sm text-neutral-700" { (&guild.name) }
            }))
            span class="ml-2 text-xs text-neutral-500" { (value) }
        };
    }
    if is_channel_id_key(key)
        && let Some(channel) = entry.related_channels.get(value)
    {
        return html! {
            (channel_link(base, value, Some(channel), html! {
                span class="text-sm text-neutral-700" { (channel_label(channel)) }
            }))
            span class="ml-2 text-xs text-neutral-500" { (value) }
        };
    }
    html! { span class="text-sm text-neutral-500 break-all" { (value) } }
}

pub fn details_row(base: &str, entry: &AuditLogEntry) -> Markup {
    let has_reason = entry
        .audit_log_reason
        .as_deref()
        .is_some_and(|r| !r.is_empty());
    if !has_reason && entry.metadata.is_empty() {
        return html! { p class="text-sm text-neutral-500 italic" { "No additional details" } };
    }
    html! {
        div class="flex flex-col gap-3" {
            @if let Some(r) = entry.audit_log_reason.as_deref() {
                @if !r.is_empty() {
                    div class="flex gap-2 items-start" {
                        span class="text-sm font-semibold min-w-[120px]" { "Reason" }
                        span class="text-sm text-neutral-500" { (r) }
                    }
                }
            }
            @for (key, value) in &entry.metadata {
                div class="flex gap-2 items-start" {
                    span class="text-sm font-semibold min-w-[120px]" { (key) }
                    span class="min-w-0" { (metadata_value(base, entry, key, value)) }
                }
            }
        }
    }
}

pub fn log_row(base: &str, entry: &AuditLogEntry, index: usize) -> Markup {
    let expanded_id = format!("expanded-{index}");
    let has_details = entry.audit_log_reason.is_some() || !entry.metadata.is_empty();

    html! {
        (table_row(html! {
            (table_cell(true, html! {
                span class="text-sm whitespace-nowrap" {
                    (entry.created_at)
                }
            }))
            (table_cell(false, badge(&format_action(&entry.action), action_badge_variant(&entry.action))))
            (table_cell(false, admin_user_cell(base, entry)))
            (table_cell(false, target_cell(base, entry)))
            (table_cell(true, html! {
                @if has_details {
                    button type="button"
                        class="text-sm text-neutral-600 hover:text-neutral-900 underline"
                        onclick={
                            "document.getElementById('" (expanded_id)
                            "').classList.toggle('hidden')"
                        } {
                        "Show details"
                    }
                } @else {
                    span class="text-sm text-neutral-500 italic" { "-" }
                }
            }))
        }))
        @if has_details {
            tr id=(expanded_id) class="hidden bg-neutral-50" {
                td colspan="5" class="px-6 py-4" {
                    (details_row(base, entry))
                }
            }
        }
    }
}

pub fn audit_log_table_headers() -> Markup {
    html! {
        (table_head(html! {
            tr {
                (table_header_cell("Timestamp"))
                (table_header_cell("Action"))
                (table_header_cell("Admin"))
                (table_header_cell("Target"))
                (table_header_cell("Details"))
            }
        }))
    }
}

pub fn audit_log_table_body(base: &str, entries: &[AuditLogEntry]) -> Markup {
    table_body(html! {
        @for (i, entry) in entries.iter().enumerate() {
            (log_row(base, entry, i))
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(target_type: &str, target_id: &str) -> AuditLogEntry {
        serde_json::from_value(serde_json::json!({
            "log_id": "1900000000000000001",
            "admin_user_id": "1500000000000000001",
            "action": "bulk_schedule_deletion",
            "target_id": target_id,
            "target_type": target_type,
            "audit_log_reason": "Raid cleanup",
            "created_at": "2026-09-14T12:00:00.000Z"
        }))
        .expect("audit log fixture must deserialize")
    }

    #[test]
    fn bulk_job_targets_link_to_the_job_detail_page() {
        let markup = target_cell("/admin", &entry("bulk_job", "1900000000000000002")).into_string();
        assert!(markup.contains(r#"href="/admin/jobs/1900000000000000002""#));
        assert!(markup.contains("Bulk job"));
        assert!(!markup.contains("/admin/users/"));
    }

    #[test]
    fn unknown_target_types_stay_unlinked() {
        let markup = target_cell("/admin", &entry("email_domain", "spam.example")).into_string();
        assert!(!markup.contains("<a "));
        assert!(markup.contains("Email domain"));
    }
}
