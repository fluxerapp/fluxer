// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    activity::{normalize_activity_timestamp, resolve_activity_image_url},
    admin_flags,
    api::types::{AdminUser, AdminUserActivity, LimitConfigResponse, ListUserChangeLogResponse},
    config::AdminConfig,
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{checkbox, csrf_input, form_actions, submit_button},
        page_container::{card_with_header, detail_row},
    },
    utils::{bigint::format_discriminator, timestamps::snowflake_creation_date},
};
use maud::{Markup, html};
use std::time::{SystemTime, UNIX_EPOCH};

struct ActivityDisplayLines {
    listening_source: Option<String>,
    primary: String,
    secondary: Option<String>,
}

fn activity_type_label(activity_type: i32) -> &'static str {
    match activity_type {
        0 => "Playing",
        1 => "Streaming",
        2 => "Listening",
        3 => "Watching",
        4 => "Custom",
        5 => "Competing",
        _ => "Unknown",
    }
}

fn activity_header(activity: &AdminUserActivity, listening_source: Option<&str>) -> String {
    match activity.activity_type {
        2 => listening_source
            .map(|value| format!("Listening to {value}"))
            .unwrap_or_else(|| "Listening to Music".to_owned()),
        3 => "Watching".to_owned(),
        5 => "Competing in".to_owned(),
        _ => "Playing".to_owned(),
    }
}

fn normalize_text(value: Option<&str>) -> Option<String> {
    let value = value?.trim();
    if value.is_empty() {
        None
    } else {
        Some(value.to_owned())
    }
}

fn equals_ignore_case(a: &str, b: &str) -> bool {
    a.eq_ignore_ascii_case(b)
}

fn dedupe_line(value: Option<String>, others: &[Option<String>]) -> Option<String> {
    let value = value?;
    for other in others.iter().flatten() {
        if equals_ignore_case(&value, other) {
            return None;
        }
    }
    Some(value)
}

fn split_artist_title(details: &str) -> Option<(String, String)> {
    let (artist, title) = details.split_once(" - ")?;
    let artist = artist.trim();
    let title = title.trim();
    if artist.is_empty() || title.is_empty() {
        None
    } else {
        Some((artist.to_owned(), title.to_owned()))
    }
}

fn resolve_listening_source(
    name: Option<&str>,
    title: &str,
    artist: Option<&str>,
) -> Option<String> {
    let name = name?;
    if equals_ignore_case(name, title) {
        return None;
    }
    if artist.is_some_and(|value| equals_ignore_case(name, value)) {
        return None;
    }
    if artist.is_some_and(|value| equals_ignore_case(&format!("{value} - {name}"), title)) {
        return None;
    }
    if title.ends_with(&format!(" - {name}")) {
        return None;
    }
    Some(name.to_owned())
}

fn format_activity_lines(activity: &AdminUserActivity) -> ActivityDisplayLines {
    let name = normalize_text(Some(&activity.name));
    let details = normalize_text(activity.details.as_deref());
    let state = normalize_text(activity.state.as_deref());

    if activity.activity_type == 2 {
        if let Some(details_value) = &details {
            if let Some(state_value) = &state {
                let artist_prefix = format!("{state_value} - ");
                if details_value.starts_with(&artist_prefix) {
                    let title = details_value[artist_prefix.len()..].trim().to_owned();
                    return ActivityDisplayLines {
                        listening_source: resolve_listening_source(
                            name.as_deref(),
                            &title,
                            Some(state_value.as_str()),
                        ),
                        primary: title,
                        secondary: Some(state_value.clone()),
                    };
                }
                if let Some((artist, title)) = split_artist_title(details_value) {
                    if equals_ignore_case(&artist, state_value) {
                        return ActivityDisplayLines {
                            listening_source: resolve_listening_source(
                                name.as_deref(),
                                &title,
                                Some(state_value.as_str()),
                            ),
                            primary: title,
                            secondary: Some(state_value.clone()),
                        };
                    }
                    if equals_ignore_case(&title, state_value) {
                        return ActivityDisplayLines {
                            listening_source: resolve_listening_source(
                                name.as_deref(),
                                &title,
                                Some(artist.as_str()),
                            ),
                            primary: title,
                            secondary: Some(artist),
                        };
                    }
                }
                return ActivityDisplayLines {
                    listening_source: resolve_listening_source(
                        name.as_deref(),
                        details_value,
                        Some(state_value.as_str()),
                    ),
                    primary: details_value.clone(),
                    secondary: Some(state_value.clone()),
                };
            }
            if let Some((artist, title)) = split_artist_title(details_value) {
                return ActivityDisplayLines {
                    listening_source: resolve_listening_source(
                        name.as_deref(),
                        &title,
                        Some(artist.as_str()),
                    ),
                    primary: title,
                    secondary: Some(artist),
                };
            }
            return ActivityDisplayLines {
                listening_source: resolve_listening_source(name.as_deref(), details_value, None),
                primary: details_value.clone(),
                secondary: None,
            };
        }
        let primary = name.unwrap_or_else(|| "Unknown".to_owned());
        return ActivityDisplayLines {
            listening_source: None,
            primary,
            secondary: state,
        };
    }

    let primary = match activity.status_display_type {
        Some(2) => state.clone().or(details.clone()).or(name.clone()),
        _ => details.clone().or(name.clone()).or(state.clone()),
    }
    .unwrap_or_else(|| "Unknown".to_owned());
    let secondary = match activity.status_display_type {
        Some(2) => dedupe_line(
            details,
            &[Some(primary.clone()), state.clone(), name.clone()],
        )
        .or_else(|| dedupe_line(name, &[Some(primary.clone()), state.clone()])),
        _ => dedupe_line(
            state,
            &[Some(primary.clone()), details.clone(), name.clone()],
        )
        .or_else(|| dedupe_line(name, &[Some(primary.clone()), details.clone()])),
    };
    ActivityDisplayLines {
        listening_source: None,
        primary,
        secondary,
    }
}

fn resolve_activity_image(value: Option<&str>, application_id: Option<&str>) -> Option<String> {
    resolve_activity_image_url(value, application_id)
}

fn proxied_activity_image_url(config: &AdminConfig, url: &str) -> String {
    format!(
        "{}/activity-image?url={}",
        config.base_path,
        urlencoding::encode(url)
    )
}

fn format_duration_label(total_seconds: i64) -> String {
    let hours = total_seconds / 3600;
    let minutes = (total_seconds % 3600) / 60;
    let seconds = total_seconds % 60;
    if hours > 0 {
        format!("{hours}:{minutes:02}:{seconds:02}")
    } else {
        format!("{minutes}:{seconds:02}")
    }
}

fn render_activity_timer(activity: &AdminUserActivity) -> Option<Markup> {
    let timestamps = activity.timestamps.as_ref()?;
    let now = SystemTime::now().duration_since(UNIX_EPOCH).ok()?.as_secs() as i64;
    let start = timestamps.start.map(normalize_activity_timestamp);
    let end = timestamps.end.map(normalize_activity_timestamp);
    let text = match (start, end) {
        (Some(start), Some(end)) if end > start => {
            let elapsed = format_duration_label((now - start).max(0));
            let remaining = format_duration_label((end - now).max(0));
            format!("{elapsed} elapsed • {remaining} left")
        }
        (Some(start), _) => format!("{} elapsed", format_duration_label((now - start).max(0))),
        (_, Some(end)) if end > now => {
            format!("{} left", format_duration_label((end - now).max(0)))
        }
        _ => return None,
    };
    Some(html! {
        div
            class="text-[0.75rem] leading-5 text-neutral-500"
            data-activity-timer=""
            data-start=(start.map(|value| value.to_string()).unwrap_or_default())
            data-end=(end.map(|value| value.to_string()).unwrap_or_default()) {
            (text)
        }
    })
}

fn render_activity_progress(activity: &AdminUserActivity) -> Option<Markup> {
    let timestamps = activity.timestamps.as_ref()?;
    let (Some(start), Some(end)) = (
        timestamps.start.map(normalize_activity_timestamp),
        timestamps.end.map(normalize_activity_timestamp),
    ) else {
        return None;
    };
    if end <= start {
        return None;
    }
    let now = SystemTime::now().duration_since(UNIX_EPOCH).ok()?.as_secs() as i64;
    let duration = (end - start) as f64;
    let elapsed = (now - start).clamp(0, end - start) as f64;
    let width = ((elapsed / duration) * 100.0).clamp(0.0, 100.0);
    Some(html! {
        div class="mt-1 h-1 w-full overflow-hidden rounded-full bg-neutral-200" aria-hidden="true" {
            div
                class="h-full rounded-full bg-sky-500 transition-[width] duration-1000 linear"
                data-activity-progress=""
                data-start=(start)
                data-end=(end)
                style={ "width: " (format!("{width:.2}%")) ";" } {}
        }
    })
}

fn render_activity_line(class_name: &str, text: &str, href: Option<&str>) -> Markup {
    if let Some(href) = href.filter(|value| !value.trim().is_empty()) {
        html! {
            a href=(href)
                target="_blank"
                rel="noreferrer noopener"
                class=(class_name) {
                (text)
            }
        }
    } else {
        html! {
            div class=(class_name) { (text) }
        }
    }
}

fn render_activity(config: &AdminConfig, activity: &AdminUserActivity) -> Markup {
    let display = format_activity_lines(activity);
    let header = activity_header(activity, display.listening_source.as_deref());
    let large_image = resolve_activity_image(
        activity
            .assets
            .as_ref()
            .and_then(|assets| assets.large_image.as_deref()),
        activity.application_id.as_deref(),
    );
    let small_image = resolve_activity_image(
        activity
            .assets
            .as_ref()
            .and_then(|assets| assets.small_image.as_deref()),
        activity.application_id.as_deref(),
    );
    let large_text = activity
        .assets
        .as_ref()
        .and_then(|assets| assets.large_text.as_deref());
    let raw_json = serde_json::to_string_pretty(activity).unwrap_or_else(|_| "{}".to_owned());
    let raw_key = format!(
        "{}:{}:{}",
        activity.activity_type,
        activity.application_id.as_deref().unwrap_or(""),
        activity.name
    );
    html! {
        div class="w-full min-w-0 max-w-full space-y-3" {
            div class="overflow-hidden rounded-lg border border-neutral-200 bg-white p-4" {
                div class="mb-3 flex items-center justify-between gap-3" {
                    div class="text-[0.72rem] font-semibold uppercase tracking-[0.04em] text-neutral-800" {
                        (header)
                    }
                    span class="rounded-full bg-white/85 px-2 py-1 text-[0.68rem] text-neutral-600 ring-1 ring-neutral-200" {
                        @if let Some(application_id) = &activity.application_id {
                            "app " (application_id)
                        } @else {
                            (activity_type_label(activity.activity_type))
                        }
                    }
                }
                div class="flex items-start gap-3" {
                    div class="relative h-16 w-16 shrink-0 overflow-hidden rounded-md border border-neutral-200 bg-neutral-50" {
                        @if let Some(large_image) = large_image.as_deref() {
                            img
                                src=(proxied_activity_image_url(config, large_image))
                                alt=""
                                title=(large_text.unwrap_or(""))
                                referrerpolicy="no-referrer"
                                class="h-full w-full object-cover";
                        } @else {
                            div class="flex h-full w-full items-center justify-center bg-neutral-100 px-2 text-[0.62rem] font-semibold uppercase tracking-[0.08em] text-neutral-700" {
                                (activity_type_label(activity.activity_type))
                            }
                        }
                        @if let Some(small_image) = small_image.as_deref() {
                            img
                                src=(proxied_activity_image_url(config, small_image))
                                alt=""
                                title=(
                                    activity
                                        .assets
                                        .as_ref()
                                        .and_then(|assets| assets.small_text.as_deref())
                                        .unwrap_or("")
                                )
                                referrerpolicy="no-referrer"
                                class="absolute -bottom-0.5 -right-0.5 h-6 w-6 rounded-full border-2 border-white object-cover";
                        }
                    }
                    div class="min-w-0 flex-1 space-y-1" {
                        (render_activity_line(
                            "block truncate text-[0.92rem] font-semibold leading-5 text-neutral-900 no-underline",
                            &display.primary,
                            activity.details_url.as_deref(),
                        ))
                        @if let Some(secondary) = &display.secondary {
                            (render_activity_line(
                                "block truncate text-[0.78rem] leading-5 text-neutral-500 no-underline",
                                secondary,
                                activity.state_url.as_deref(),
                            ))
                        }
                        @if let Some(progress) = render_activity_progress(activity) {
                            (progress)
                        }
                        @if let Some(timer) = render_activity_timer(activity) {
                            (timer)
                        }
                    }
                }
                @if !activity.buttons.is_empty() {
                    div class="mt-3 flex flex-wrap gap-2" {
                        @for button in &activity.buttons {
                            a href=(&button.url)
                                target="_blank"
                                rel="noreferrer noopener"
                                class="inline-flex max-w-full items-center rounded-full bg-white px-3 py-1.5 text-[0.75rem] font-medium text-neutral-700 ring-1 ring-neutral-200 transition hover:bg-sky-50 hover:text-sky-800" {
                                (&button.label)
                            }
                        }
                    }
                }
            }
            details data-activity-raw=(raw_key) class="max-w-full overflow-hidden rounded-lg border border-slate-200 bg-white" {
                summary class="cursor-pointer list-none px-4 py-3 text-sm font-medium text-slate-700" {
                    "Raw Activity Payload"
                }
                pre class="max-w-full overflow-x-auto whitespace-pre-wrap break-all border-t border-neutral-200 bg-neutral-50 p-4 text-xs text-neutral-700" {
                    (raw_json)
                }
            }
        }
    }
}

fn render_activities(config: &AdminConfig, activities: &[AdminUserActivity]) -> Markup {
    if activities.is_empty() {
        return html! {
            span class="text-neutral-400" { "None" }
        };
    }
    html! {
        div class="space-y-3" {
            @for activity in activities {
                (render_activity(config, activity))
            }
        }
    }
}

pub fn activity_detail_row(config: &AdminConfig, user: &AdminUser) -> Markup {
    html! {
        div
            id="user-activity-row"
            hx-get={(config.base_path) "/users/" (user.id) "/activity-fragment"}
            hx-trigger="every 5s"
            hx-swap="outerHTML" {
            (detail_row("Activity", render_activities(config, &user.activities)))
        }
    }
}

pub fn overview_tab(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
    change_log: Option<&ListUserChangeLogResponse>,
) -> Markup {
    render_overview_tab(
        config, user, admin_acls, csrf_token, change_log, None, false,
    )
}

pub fn overview_tab_with_limit_config(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
    change_log: Option<&ListUserChangeLogResponse>,
    limit_config: Option<&LimitConfigResponse>,
) -> Markup {
    render_overview_tab(
        config,
        user,
        admin_acls,
        csrf_token,
        change_log,
        limit_config,
        true,
    )
}

fn render_overview_tab(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
    change_log: Option<&ListUserChangeLogResponse>,
    limit_config: Option<&LimitConfigResponse>,
    show_traits: bool,
) -> Markup {
    html! {
        div class="space-y-6" {
            @if let Some(until) = &user.temp_banned_until {
                div class="rounded-lg border border-red-200 bg-red-50 p-4 text-sm font-medium text-red-900" {
                    "Temporarily banned until " (until)
                }
            }
            @if user.temp_banned_until.is_none() {
                @if let Some(pending) = &user.pending_deletion_at {
                    div class="rounded-lg border border-orange-200 bg-orange-50 p-4 text-sm text-orange-900" {
                        div class="font-medium" { "Scheduled for deletion: " (pending) }
                        @if let Some(code) = user.deletion_reason_code {
                            div class="mt-1" { "Reason code: " (code) }
                        }
                        @if let Some(reason) = &user.deletion_public_reason {
                            div class="mt-1" { "Public reason: " (reason) }
                        }
                    }
                }
            }
            @if let Some(pending) = &user.pending_bulk_message_deletion_at {
                div class="rounded-lg border border-neutral-200 bg-neutral-50 p-4" {
                    div class="font-medium text-neutral-700 text-sm" {
                        "Bulk message deletion scheduled for " (pending)
                    }
                    @if acl::has_permission(admin_acls, acl::USER_CANCEL_BULK_MESSAGE_DELETION) {
                        form method="post"
                            action={(config.base_path) "/users/" (user.id) "?action=cancel_bulk_message_deletion&tab=overview"} {
                            (csrf_input(csrf_token))
                            button type="submit"
                                class="mt-3 rounded bg-neutral-900 px-4 py-2 font-medium text-sm text-white transition-colors hover:bg-neutral-800" {
                                "Cancel Bulk Message Deletion"
                            }
                        }
                    }
                }
            }
            (card_with_header("Account Information", html! {
                dl class="divide-y divide-neutral-100" {
                    (detail_row("User ID", html! {
                        span class="font-mono text-xs" { (user.id) }
                    }))
                    (detail_row("Created", html! {
                        (snowflake_creation_date(&user.id))
                    }))
                    (detail_row("Username", html! {
                        (user.username) "#" (format_discriminator(&user.discriminator))
                    }))
                    (detail_row("Display Name", html! {
                        @if let Some(ref name) = user.global_name {
                            (name)
                        } @else {
                            span class="text-neutral-400" { "Not set" }
                        }
                    }))
                    @if acl::has_permission(admin_acls, acl::USER_VIEW_EMAIL) {
                        (detail_row("Email", html! {
                            @if let Some(ref email) = user.email {
                                (email)
                                @if user.email_verified { span class="ml-1 text-green-600" { "verified" } }
                                @else { span class="ml-1 text-red-600" { "unverified" } }
                                @if user.email_bounced { span class="ml-1 text-orange-600" { "(bounced)" } }
                            } @else {
                                span class="text-neutral-400" { "Not set" }
                            }
                        }))
                    }
                    (detail_row("Phone", html! {
                        @if user.has_verified_phone {
                            span class="text-green-700" { "Verified" }
                        } @else {
                            span class="text-neutral-400" { "Not verified" }
                        }
                    }))
                    @if acl::has_permission(admin_acls, acl::USER_VIEW_DOB) {
                        (detail_row("Date of Birth", html! {
                            (user.date_of_birth.as_deref().unwrap_or("Not set"))
                        }))
                    }
                    (detail_row("Locale", html! {
                        (user.locale.as_deref().unwrap_or("Not set"))
                    }))
                    @if let Some(ref bio) = user.bio {
                        (detail_row("Bio", html! { (bio) }))
                    }
                    @if let Some(ref pronouns) = user.pronouns {
                        (detail_row("Pronouns", html! { (pronouns) }))
                    }
                    @if user.bot {
                        (detail_row("Type", html! { (badge("Bot", BadgeVariant::Info)) }))
                    }
                    @if user.system {
                        (detail_row("Type", html! { (badge("System", BadgeVariant::Warning)) }))
                    }
                    (detail_row("Authenticators", html! {
                        @if user.authenticator_types.is_empty() {
                            span class="text-neutral-400" { "None" }
                        } @else {
                            (user.authenticator_types.iter().map(|t| match t {
                                0 => "TOTP",
                                2 => "WebAuthn",
                                _ => "Unknown",
                            }).collect::<Vec<_>>().join(", "))
                        }
                    }))
                    (detail_row("Premium", html! {
                        @match user.premium_type {
                            Some(1) => {
                                "Subscription"
                                @if let Some(ref value) = user.premium_since {
                                    span class="ml-1 text-neutral-500" { " since " (value) }
                                }
                                @if let Some(ref value) = user.premium_until {
                                    span class="ml-1 text-neutral-500" { " until " (value) }
                                }
                            }
                            Some(2) => {
                                "Lifetime"
                                @if let Some(value) = user.premium_lifetime_sequence {
                                    span class="ml-1 text-neutral-500" { " #" (value) }
                                }
                            }
                            _ => { span class="text-neutral-400" { "None" } }
                        }
                    }))
                    @if let Some(ref value) = user.premium_grace_ends_at {
                        (detail_row("Grace Period Ends", html! { (value) }))
                    }
                    (detail_row("Last Active", html! {
                        (user.last_active_at.as_deref().unwrap_or("Never"))
                    }))
                    @if acl::has_permission(admin_acls, acl::USER_VIEW_IP) {
                        (detail_row("Last IP", html! {
                            @if let Some(ip) = &user.last_active_ip {
                                span class="font-mono text-xs" { (ip) }
                                a href={(config.base_path) "/users?ip=" (urlencoding::encode(ip))}
                                    class="ml-2 text-xs text-blue-600 hover:underline" {
                                    "Find related"
                                }
                                @if let Some(reverse) = &user.last_active_ip_reverse {
                                    span class="ml-2 text-neutral-500" { "(" (reverse) ")" }
                                }
                            } @else {
                                span class="text-neutral-400" { "Not recorded" }
                            }
                        }))
                        (detail_row("Location", html! {
                            (user.last_active_location.as_deref().unwrap_or("Unknown"))
                        }))
                    }
                    (activity_detail_row(config, user))
                }
            }))
            (flags_card(config, user, admin_acls, csrf_token))
            (acls_card(config, user, admin_acls, csrf_token))
            @if show_traits {
                (traits_card(config, user, admin_acls, csrf_token, limit_config))
            }
            @if acl::has_permission(admin_acls, acl::USER_VIEW_CONTACT_LOG) {
                (card_with_header("Contact Change Log", html! {
                    @if let Some(log) = change_log {
                        @if log.entries.is_empty() {
                            p class="text-sm text-neutral-500" { "No contact changes recorded." }
                        } @else {
                            dl class="divide-y divide-neutral-100" {
                                @for entry in &log.entries {
                                    div class="py-3 text-sm" {
                                        dt class="font-medium text-neutral-900" {
                                            (entry.field)
                                            span class="ml-2 font-normal text-xs text-neutral-500" { (entry.event_at) }
                                        }
                                        dd class="mt-1 font-mono text-xs text-neutral-700" {
                                            (entry.old_value.as_deref().unwrap_or("null"))
                                            " \u{2192} "
                                            (entry.new_value.as_deref().unwrap_or("null"))
                                        }
                                        @if let Some(ref reason) = entry.reason {
                                            dd class="text-xs text-neutral-500" { (reason) }
                                        }
                                    }
                                }
                            }
                        }
                    } @else {
                        p class="text-sm text-red-700" { "Failed to load change log." }
                    }
                }))
            }
        }
    }
}

fn flags_card(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
) -> Markup {
    let can_update_flags = acl::has_permission(admin_acls, acl::USER_UPDATE_FLAGS);
    let can_update_suspicious =
        acl::has_permission(admin_acls, acl::USER_UPDATE_SUSPICIOUS_ACTIVITY);
    html! {
        div class="space-y-6" {
            (u64_flag_form(
                config,
                &user.id,
                "User Flags",
                "update_flags",
                "flags[]",
                user.flags,
                admin_flags::USER_FLAGS,
                csrf_token,
                can_update_flags,
                Some(acl::USER_UPDATE_FLAGS),
            ))
            (i32_flag_form(
                config,
                &user.id,
                "Premium Flags",
                "update_premium_flags",
                "flags[]",
                user.premium_flags,
                admin_flags::PREMIUM_FLAGS,
                csrf_token,
                can_update_flags,
                Some(acl::USER_UPDATE_FLAGS),
            ))
            (i32_flag_form(
                config,
                &user.id,
                "Suspicious Activity Flags",
                "update_suspicious_flags",
                "suspicious_flags[]",
                user.suspicious_activity_flags,
                admin_flags::SUSPICIOUS_ACTIVITY_FLAGS,
                csrf_token,
                can_update_suspicious,
                Some(acl::USER_UPDATE_SUSPICIOUS_ACTIVITY),
            ))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn u64_flag_form(
    config: &AdminConfig,
    user_id: &str,
    title: &str,
    action: &str,
    input_name: &str,
    value: u64,
    flags: &[admin_flags::U64Flag],
    csrf_token: &str,
    can_edit: bool,
    required_acl: Option<&str>,
) -> Markup {
    flag_form_shell(
        config,
        user_id,
        title,
        action,
        value.to_string(),
        csrf_token,
        can_edit,
        required_acl,
        html! {
            @for flag in flags {
                (flag_checkbox(
                    input_name,
                    flag.value.to_string(),
                    flag.name,
                    value & flag.value != 0,
                    can_edit,
                ))
            }
        },
    )
}

#[allow(clippy::too_many_arguments)]
fn i32_flag_form(
    config: &AdminConfig,
    user_id: &str,
    title: &str,
    action: &str,
    input_name: &str,
    value: i32,
    flags: &[admin_flags::I32Flag],
    csrf_token: &str,
    can_edit: bool,
    required_acl: Option<&str>,
) -> Markup {
    flag_form_shell(
        config,
        user_id,
        title,
        action,
        value.to_string(),
        csrf_token,
        can_edit,
        required_acl,
        html! {
            @for flag in flags {
                (flag_checkbox(
                    input_name,
                    flag.value.to_string(),
                    flag.name,
                    value & flag.value != 0,
                    can_edit,
                ))
            }
        },
    )
}

#[allow(clippy::too_many_arguments)]
fn flag_form_shell(
    config: &AdminConfig,
    user_id: &str,
    title: &str,
    action: &str,
    _raw_value: String,
    csrf_token: &str,
    can_edit: bool,
    required_acl: Option<&str>,
    checkboxes: Markup,
) -> Markup {
    card_with_header(
        title,
        html! {
            form method="post" action={(config.base_path) "/users/" (user_id) "?action=" (action) "&tab=overview"} class="space-y-4" {
                (csrf_input(csrf_token))
                div class="grid grid-cols-1 gap-2 sm:grid-cols-2" {
                    (checkboxes)
                }
                @if can_edit {
                    (form_actions(html! {
                        (submit_button("Save"))
                    }))
                } @else {
                    @if let Some(required) = required_acl {
                        p class="text-xs text-neutral-500" {
                            "Read-only \u{2014} " (required) " permission required."
                        }
                    }
                }
            }
        },
    )
}

fn flag_checkbox(
    input_name: &str,
    value: String,
    label: &str,
    checked: bool,
    can_edit: bool,
) -> Markup {
    checkbox(input_name, &value, label, checked, can_edit)
}

fn acls_card(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
) -> Markup {
    let can_edit = acl::has_permission(admin_acls, acl::ACL_SET_USER);
    card_with_header(
        "ACLs",
        html! {
            @if can_edit {
                form method="post" action={(config.base_path) "/users/" (user.id) "?action=update_acls&tab=overview"} {
                    (csrf_input(csrf_token))
                    div class="space-y-4" {
                        div class="grid grid-cols-1 gap-2 sm:grid-cols-2" {
                            @for item in acl::ALL_ACLS {
                                @let checked = user.acls.iter().any(|value| value == item);
                                (flag_checkbox("acls[]", item.to_string(), item, checked, true))
                            }
                        }
                        @for item in &user.acls {
                            @if !acl::ALL_ACLS.iter().any(|known| known == &item.as_str()) {
                                input type="hidden" name="acls[]" value=(item);
                            }
                        }
                        (form_actions(html! {
                            (submit_button("Save ACLs"))
                        }))
                    }
                }
            } @else {
                @if user.acls.is_empty() {
                    p class="text-sm text-neutral-500" { "No ACLs assigned." }
                } @else {
                    div class="flex flex-wrap gap-2" {
                        @for item in &user.acls {
                            (badge(item, BadgeVariant::Default))
                        }
                    }
                }
                p class="mt-4 text-xs text-neutral-500" {
                    "Read-only \u{2014} acl:set:user permission required."
                }
            }
        },
    )
}

fn traits_card(
    config: &AdminConfig,
    user: &AdminUser,
    admin_acls: &[String],
    csrf_token: &str,
    limit_config: Option<&LimitConfigResponse>,
) -> Markup {
    let trait_definitions = parse_trait_definitions(limit_config);
    let custom_traits = custom_traits(user, &trait_definitions);
    let can_edit = acl::has_permission(admin_acls, acl::USER_UPDATE_TRAITS);
    card_with_header(
        "Traits",
        html! {
            div class="space-y-4" {
                @if limit_config.is_some() {
                    @if can_edit {
                        (traits_form(config, user, csrf_token, &trait_definitions, &custom_traits))
                    } @else {
                        (assigned_traits(&user.traits))
                        p class="mt-2 text-xs text-neutral-500" { "Read-only \u{2014} trait update permission required." }
                    }
                } @else {
                    (assigned_traits(&user.traits))
                    p class="text-sm text-red-700" { "Failed to load limit configuration." }
                }
                @if trait_definitions.is_empty() {
                    p class="text-neutral-500 text-xs" {
                        "No trait definitions declared. "
                        a href={(config.base_path) "/instance-config"} class="text-blue-600 underline" {
                            "Open Instance Configuration"
                        }
                    }
                }
            }
        },
    )
}

fn assigned_traits(traits: &[String]) -> Markup {
    html! {
        @if traits.is_empty() {
            p class="text-neutral-500 text-sm" { "No traits assigned." }
        } @else {
            div class="flex flex-wrap gap-2" {
                @for trait_name in traits {
                    (badge(trait_name, BadgeVariant::Default))
                }
            }
        }
    }
}

fn traits_form(
    config: &AdminConfig,
    user: &AdminUser,
    csrf_token: &str,
    trait_definitions: &[&str],
    custom_traits: &[&str],
) -> Markup {
    html! {
        form method="post" action={(config.base_path) "/users/" (user.id) "?action=update_traits&tab=overview"} {
            (csrf_input(csrf_token))
            div class="space-y-4" {
                @if !trait_definitions.is_empty() {
                    div class="grid grid-cols-1 gap-2 sm:grid-cols-2" {
                        @for definition in trait_definitions {
                            (flag_checkbox(
                                "traits[]",
                                definition.to_string(),
                                definition,
                                user.traits.iter().any(|t| t == definition),
                                true,
                            ))
                        }
                    }
                }
                div class="flex flex-col gap-1" {
                    label for="custom-traits" class="text-neutral-500 text-xs" {
                        "Custom traits (comma or newline separated)"
                    }
                    textarea id="custom-traits" name="traits" rows="2"
                        placeholder="beta-tester, experimental"
                        class="w-full rounded-lg border border-neutral-300 bg-white px-3 py-1.5 text-sm text-neutral-900 placeholder:text-neutral-400 focus:border-brand-primary focus:outline-none focus:ring-2 focus:ring-brand-primary/20" {
                        (custom_traits.join("\n"))
                    }
                }
                (form_actions(html! {
                    (submit_button("Save Traits"))
                }))
            }
        }
    }
}

fn parse_trait_definitions(limit_config: Option<&LimitConfigResponse>) -> Vec<&str> {
    limit_config
        .map(|response| {
            response
                .limit_config
                .trait_definitions
                .iter()
                .map(|value| value.trim())
                .filter(|value| !value.is_empty())
                .collect()
        })
        .unwrap_or_default()
}

fn custom_traits<'a>(user: &'a AdminUser, trait_definitions: &[&str]) -> Vec<&'a str> {
    user.traits
        .iter()
        .map(String::as_str)
        .filter(|trait_name| !trait_definitions.contains(trait_name))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{activity_header, format_activity_lines};
    use crate::api::types::AdminUserActivity;

    #[test]
    fn listening_header_uses_source_name() {
        let activity = AdminUserActivity {
            name: "Spotify".to_owned(),
            activity_type: 2,
            status_display_type: None,
            application_id: None,
            details: Some("Artist - Song".to_owned()),
            details_url: None,
            state: Some("Artist".to_owned()),
            state_url: None,
            timestamps: None,
            assets: None,
            buttons: Vec::new(),
            party: None,
            metadata: None,
        };
        let display = format_activity_lines(&activity);
        assert_eq!(display.primary, "Song");
        assert_eq!(display.secondary.as_deref(), Some("Artist"));
        assert_eq!(
            activity_header(&activity, display.listening_source.as_deref()),
            "Listening to Spotify"
        );
    }
}
