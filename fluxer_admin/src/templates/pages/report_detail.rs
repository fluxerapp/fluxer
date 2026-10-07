// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::{
        AdminUser, GuildDetailInfo, ReportEntry, ReportFlowAnswerStepEntry, ReportFlowAnswersEntry,
        ReportProfileSnapshot, ReportProfileSnapshotAsset,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            data_field::{
                data_field, data_field_link_mono, data_field_mono, data_field_muted,
                data_field_text,
            },
            form::{FORM_CONTROL_CLASS, FORM_LABEL_CLASS, csrf_input, opt_out_checkbox},
            media::{guild_icon_url, initials, user_avatar_url},
            message_data::ordered_messages,
            message_list::{message_deletion_script, message_list},
            nsfw_indicators::{
                adult_content_badge, channel_nsfw_state_badge, content_warning_badge,
            },
            page_container::page_header_with_back,
            report_category::report_category,
            report_webhook::{
                ReportedWebhook, reported_webhook, webhook_creator_badges, webhook_creator_link,
                webhook_identity,
            },
            resource_link::{ResourceType, resource_link},
            section_card::section_card,
        },
        layout::admin_layout,
    },
    utils::{timestamps::format_admin_timestamp, user_tag::user_tag},
};
use maud::{Markup, html};

#[derive(Default)]
pub struct LiveProfile {
    pub user: Option<AdminUser>,
    pub guild: Option<GuildDetailInfo>,
}

fn status_badge(status: i32) -> Markup {
    let (label, variant) = match status {
        0 => ("Pending", BadgeVariant::Warning),
        1 => ("Resolved", BadgeVariant::Success),
        _ => ("Unknown", BadgeVariant::Default),
    };
    html! {
        span class="inline-flex w-fit self-start" {
            (badge(label, variant))
        }
    }
}

fn report_type_label(report_type: i32) -> &'static str {
    match report_type {
        0 => "Message",
        1 => "User",
        2 => "Guild",
        _ => "Unknown",
    }
}

fn reporter_label(report: &ReportEntry) -> String {
    if let Some(tag) = &report.reporter_tag {
        return tag.to_owned();
    }
    if let Some(username) = &report.reporter_username {
        let discriminator = report.reporter_discriminator.as_deref().unwrap_or("0000");
        return user_tag(username, discriminator, false);
    }
    if let Some(email) = &report.reporter_email {
        return email.to_owned();
    }
    "Anonymous".to_owned()
}

fn reported_user_label(report: &ReportEntry) -> String {
    if let Some(tag) = &report.reported_user_tag {
        return tag.to_owned();
    }
    if let Some(username) = &report.reported_user_username {
        let discriminator = report
            .reported_user_discriminator
            .as_deref()
            .unwrap_or("0000");
        return user_tag(username, discriminator, false);
    }
    format!(
        "User {}",
        report.reported_user_id.as_deref().unwrap_or("unknown")
    )
}

fn has_acl(auth: &AuthContext, permission: &str) -> bool {
    auth.admin_user
        .as_ref()
        .is_some_and(|admin| acl::has_permission(&admin.acls, permission))
}

fn basic_info_section(config: &AdminConfig, auth: &AuthContext, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    let show_pii = has_acl(auth, acl::REPORT_VIEW_REPORTER_PII);
    html! {
        (section_card(Some("Basic Information"), None, None, html! {
            div class="grid grid-cols-1 sm:grid-cols-2 gap-4" {
                (data_field_mono("Report ID", &report.report_id))
                (data_field_text("Reported At", &format_admin_timestamp(&report.reported_at)))
                (data_field_text("Type", report_type_label(report.report_type)))
                (category_field(report))
                (reason_field(report))
                @if let Some(ref reporter_id) = report.reporter_id {
                    (data_field_link_mono("Reporter", &format!("{base}/users/{reporter_id}"), &reporter_label(report)))
                } @else {
                    (data_field_text("Reporter", &reporter_label(report)))
                }
                @if show_pii {
                    @if let Some(ref value) = report.reporter_email {
                        (data_field_text("Reporter Email", value))
                    }
                    @if let Some(ref value) = report.reporter_full_legal_name {
                        (data_field_text("Full Legal Name", value))
                    }
                    @if let Some(ref value) = report.reporter_country_of_residence {
                        (data_field_text("Country of Residence", value))
                    }
                }
                (data_field("Status", status_badge(report.status)))
            }
        }))
    }
}

fn category_field(report: &ReportEntry) -> Markup {
    match report
        .category
        .as_deref()
        .filter(|category| !category.is_empty())
    {
        Some(category) => data_field(
            "Category",
            html! {
                p class="text-sm text-gray-900 break-words" { (report_category(category)) }
            },
        ),
        None => data_field_text("Category", ""),
    }
}

fn reason_field(report: &ReportEntry) -> Markup {
    let Some(ref reason) = report.reason else {
        return html! {};
    };
    html! {
        (data_field("Reason", html! {
            div class="flex flex-wrap items-center gap-2" data-report-reason=(reason) {
                span class="text-sm text-gray-900 break-words" {
                    (report.reason_label.as_deref().unwrap_or(reason))
                }
                span class="font-mono text-neutral-500 text-xs break-all" { (reason) }
                @if report.reason_highest_priority == Some(true) {
                    (badge("Highest priority", BadgeVariant::Danger))
                }
            }
        }))
    }
}

fn answer_step_line(step: &ReportFlowAnswerStepEntry) -> String {
    let answers = step
        .option_label
        .as_deref()
        .or(step.option_id.as_deref())
        .into_iter()
        .chain(step.items.iter().map(|item| item.label.as_str()))
        .collect::<Vec<_>>();
    if answers.is_empty() {
        step.screen_title.clone()
    } else {
        format!("{}: {}", step.screen_title, answers.join(", "))
    }
}

fn surface_label(surface: &str) -> &str {
    match surface {
        "in_app" => "In app",
        "dsa" => "DSA form",
        other => other,
    }
}

fn report_answers_section(report: &ReportEntry) -> Markup {
    let Some(ref flow) = report.flow else {
        return html! {};
    };
    html! {
        (section_card(Some("Report Answers"), None, None, html! {
            div class="space-y-4" data-report-answers="" {
                (answer_steps(flow))
                div class="space-y-1 text-neutral-500 text-sm" {
                    @if let Some(ref locale) = flow.locale {
                        p { "Shown to the reporter in " (locale) }
                    }
                    p { "Form revision " span class="font-mono" { (flow.revision_hash) } }
                    p { "Surface: " (surface_label(&flow.surface)) }
                    @if report.reporter_good_faith_confirmed == Some(true) {
                        p { "Good-faith statement: Confirmed" }
                    }
                }
            }
        }))
    }
}

fn answer_steps(flow: &ReportFlowAnswersEntry) -> Markup {
    html! {
        ol class="space-y-2" {
            @for (index, step) in flow.steps.iter().enumerate() {
                li class="flex gap-3 text-sm text-neutral-900" data-report-answer-step=(step.screen_id) {
                    span class="w-6 flex-shrink-0 text-right font-mono text-neutral-500" {
                        (index + 1) "."
                    }
                    span class="min-w-0 break-words" { (answer_step_line(step)) }
                }
            }
        }
    }
}

fn message_lookup_href(base: &str, channel_id: &str, message_id: Option<&str>) -> String {
    let mut href = format!(
        "{base}/messages?channel_id={}&context_limit=50",
        urlencoding::encode(channel_id)
    );
    if let Some(message_id) = message_id.filter(|id| !id.is_empty()) {
        href.push_str("&message_id=");
        href.push_str(&urlencoding::encode(message_id));
    }
    href
}

fn snapshot_note() -> Markup {
    html! {
        span class="text-neutral-500 text-xs italic" { "(at time of report)" }
    }
}

fn guild_content_badges(report: &ReportEntry) -> Markup {
    let fallback = report
        .reported_guild_nsfw_level
        .filter(|_| report.reported_guild_nsfw.is_none())
        .map(|level| level == 3);
    let adult = report.reported_guild_nsfw.or(fallback).unwrap_or(false);
    let show_cw = report.reported_guild_content_warning_level == Some(1);
    if !adult && !show_cw {
        return html! {};
    }
    html! {
        span class="mt-1 inline-flex flex-wrap items-center gap-2" {
            (adult_content_badge(adult, None))
            (content_warning_badge(
                report.reported_guild_content_warning_level,
                report.reported_guild_content_warning_text.as_deref(),
                false,
            ))
            (snapshot_note())
        }
    }
}

fn channel_content_badges(report: &ReportEntry) -> Markup {
    let is_nsfw = report
        .reported_channel_effective_nsfw
        .or(report.reported_channel_nsfw)
        .unwrap_or(false);
    let warning_level = report
        .reported_channel_effective_content_warning_level
        .or(report.reported_channel_content_warning_level);
    let warning_text = report
        .reported_channel_effective_content_warning_text
        .as_deref()
        .or(report.reported_channel_content_warning_text.as_deref());
    if !is_nsfw && warning_level != Some(1) {
        return html! {};
    }
    html! {
        span class="mt-1 inline-flex flex-wrap items-center gap-2" {
            (channel_nsfw_state_badge(
                is_nsfw,
                report.reported_channel_nsfw_override,
                None,
                report.reported_guild_nsfw,
                warning_level,
                warning_text,
                true,
            ))
            (snapshot_note())
        }
    }
}

fn reported_entity_section(config: &AdminConfig, report: &ReportEntry) -> Markup {
    html! {
        (section_card(Some("Reported Entity"), None, None, html! {
            div class="grid grid-cols-1 gap-4" {
                @match report.report_type {
                    0 => {
                        @if let Some(webhook) = reported_webhook(config, report) {
                            (reported_webhook_fields(config, report, &webhook))
                        } @else {
                            (reported_user_field(config, "User", report))
                        }
                        (reported_message_fields(config, report))
                        (reported_guild_field(config, report))
                    }
                    1 => {
                        (reported_user_field(config, "User", report))
                        (reported_guild_field(config, report))
                    }
                    2 => {
                        (reported_guild_field(config, report))
                    }
                    _ => {
                        (data_field_text("Entity", "Unknown"))
                    }
                }
                @if let Some(ref invite) = report.reported_guild_invite_code {
                    (data_field_text("Guild Invite Code", invite))
                }
            }
        }))
    }
}

fn reported_user_field(config: &AdminConfig, label: &str, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        @if let Some(ref id) = report.reported_user_id {
            (data_field(label, html! {
                span class="inline-flex flex-wrap items-center gap-2" {
                    a href={(base) "/users/" (id)} class="inline-flex items-center gap-2 text-neutral-900 underline decoration-neutral-300 hover:text-neutral-600 hover:decoration-neutral-500" {
                        img
                            src=(user_avatar_url(config, id, report.reported_user_avatar_hash.as_deref(), 80, true))
                            alt=(format!("{}'s avatar", reported_user_label(report)))
                            class="h-8 w-8 rounded-full object-cover";
                        span { (reported_user_label(report)) }
                    }
                    @if report.reported_user_bot == Some(true) {
                        span class="inline-flex" data-report-user-bot=(id) {
                            (badge("Bot", BadgeVariant::Info))
                        }
                    }
                }
            }))
        } @else {
            (data_field_text(label, &reported_user_label(report)))
        }
    }
}

fn reported_webhook_fields(
    config: &AdminConfig,
    report: &ReportEntry,
    webhook: &ReportedWebhook<'_>,
) -> Markup {
    let base = &config.base_path;
    let moved_channel = webhook
        .channel_id
        .filter(|id| Some(*id) != report.reported_channel_id.as_deref());
    let moved_guild = webhook
        .guild_id
        .filter(|id| Some(*id) != report.reported_guild_id.as_deref());
    html! {
        (data_field("Webhook", webhook_identity(webhook)))
        (data_field_link_mono("Webhook ID", &webhook.reports_href, webhook.id))
        @if let Some(configured_name) = webhook.configured_name {
            (data_field_text("Configured Name", configured_name))
        }
        @if let Some(ref avatar_url) = webhook.configured_avatar_url {
            (data_field("Configured Avatar", html! {
                img src=(avatar_url) alt="Configured webhook avatar" data-report-webhook-configured-avatar
                    class="h-8 w-8 rounded-full object-cover";
            }))
        }
        @if let Some(ref kind) = webhook.kind {
            (data_field_text("Webhook Type", kind))
        }
        @if let Some(ref created_at) = webhook.created_at {
            (data_field_text("Webhook Created", created_at))
        }
        @if let Some(channel_id) = moved_channel {
            (data_field_link_mono("Webhook Channel ID", &message_lookup_href(base, channel_id, None), channel_id))
        }
        @if let Some(guild_id) = moved_guild {
            (data_field_link_mono("Webhook Guild ID", &format!("{base}/guilds/{guild_id}"), guild_id))
        }
        @if let Some(ref application) = webhook.application {
            (data_field_link_mono("Application ID", &application.href, application.id))
        }
        @if webhook.record_deleted {
            (data_field_muted("Webhook Record", "Deleted before the report"))
        }
        @if let Some(ref creator) = webhook.creator {
            (data_field("Webhook Creator", html! {
                span class="inline-flex flex-wrap items-center gap-2" {
                    (webhook_creator_link(creator))
                    (webhook_creator_badges(creator))
                }
            }))
        }
    }
}

fn reported_message_fields(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        @if let Some(ref message_id) = report.reported_message_id {
            @if let Some(ref channel_id) = report.reported_channel_id {
                (data_field_link_mono("Message ID", &message_lookup_href(base, channel_id, Some(message_id)), message_id))
            } @else {
                (data_field_mono("Message ID", message_id))
            }
        }
        @if let Some(ref channel_id) = report.reported_channel_id {
            (data_field_link_mono("Channel ID", &message_lookup_href(base, channel_id, report.reported_message_id.as_deref()), channel_id))
        }
        @if report.reported_channel_name.is_some()
            || report.reported_channel_nsfw == Some(true)
            || report.reported_channel_effective_nsfw == Some(true)
            || report.reported_channel_content_warning_level == Some(1)
            || report.reported_channel_effective_content_warning_level == Some(1) {
            div class="space-y-1" {
                @if let Some(ref name) = report.reported_channel_name {
                    (data_field_text("Channel Name", name))
                }
                (channel_content_badges(report))
            }
        }
    }
}

fn reported_guild_field(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        @if report.reported_guild_id.is_some()
            || report.reported_guild_name.is_some()
            || report.reported_guild_nsfw == Some(true)
            || report.reported_guild_content_warning_level == Some(1) {
            div class="space-y-1" {
                @if let Some(ref guild_id) = report.reported_guild_id {
                    div class="flex min-w-0 items-center justify-between gap-4 py-2 text-sm" {
                        dt class="font-medium text-neutral-500" { "Guild" }
                        dd class="min-w-0 text-right text-neutral-900" {
                            a href={(base) "/guilds/" (guild_id)}
                                class="inline-flex min-w-0 items-center justify-end gap-2 text-blue-600 hover:underline" {
                                (reported_guild_icon(
                                    config,
                                    report,
                                    guild_id,
                                    report.reported_guild_name.as_deref().unwrap_or(guild_id),
                                ))
                                span class="truncate" {
                                    (report.reported_guild_name.as_deref().unwrap_or(guild_id))
                                }
                            }
                        }
                    }
                } @else if let Some(ref guild_name) = report.reported_guild_name {
                    (data_field_text("Guild", guild_name))
                }
                (guild_content_badges(report))
            }
        }
    }
}

fn reported_guild_icon(
    config: &AdminConfig,
    report: &ReportEntry,
    guild_id: &str,
    label: &str,
) -> Markup {
    match guild_icon_url(
        config,
        guild_id,
        report.reported_guild_icon_hash.as_deref(),
        80,
        true,
    ) {
        Some(url) => html! {
            img src=(url) alt="" class="h-8 w-8 flex-shrink-0 rounded-full object-cover";
        },
        None => html! {
            span class="flex h-8 w-8 flex-shrink-0 items-center justify-center rounded-full bg-neutral-200 text-neutral-600 text-xs" {
                (initials(label))
            }
        },
    }
}

fn additional_info_section(report: &ReportEntry) -> Markup {
    html! {
        @if let Some(ref info) = report.additional_info {
            (section_card(Some("Additional Information"), None, None, html! {
                p class="whitespace-pre-wrap break-words text-neutral-700" { (info) }
            }))
        }
    }
}

fn status_card(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        (section_card(Some("Status"), None, None, html! {
            div class="mb-4 flex justify-start" {
                (status_badge(report.status))
            }
            @if let Some(ref resolved_at) = report.resolved_at {
                p class="text-sm text-neutral-500" {
                    span class="font-medium" { "Resolved at: " }
                    (format_admin_timestamp(resolved_at))
                }
            }
            @if let Some(ref resolved_by) = report.resolved_by_admin_id {
                p class="text-sm text-neutral-500" {
                    span class="font-medium" { "Resolved by: " }
                    (resource_link(base, ResourceType::User, resolved_by, html! {
                        (resolved_by)
                    }))
                }
            }
            @if let Some(ref comment) = report.public_comment {
                div class="mt-3 border-neutral-200 border-t pt-3" {
                    p class="mb-1 font-medium text-neutral-700 text-sm" { "Public comment:" }
                    p class="whitespace-pre-wrap break-words text-neutral-600 text-sm" {
                        (comment)
                    }
                }
            }
        }))
    }
}

fn actions_card(config: &AdminConfig, report: &ReportEntry, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    html! {
        (section_card(Some("Actions"), None, None, html! {
            div class="flex flex-col gap-3" {
                @if report.status == 0 {
                    (resolve_report_form(base, &report.report_id, csrf_token))
                }
                @if report.report_type == 0 || report.report_type == 1 {
                    @if let Some(ref reported_id) = report.reported_user_id {
                        (nav_link(&format!("{base}/users/{reported_id}"), "View Reported User"))
                    }
                }
                @if report.report_type == 2 {
                    @if let Some(ref guild_id) = report.reported_guild_id {
                        (nav_link(&format!("{base}/guilds/{guild_id}"), "View Reported Guild"))
                    }
                }
                @if let Some(ref reporter_id) = report.reporter_id {
                    (nav_link(&format!("{base}/users/{reporter_id}"), "View Reporter"))
                }
                @if report.report_type == 1 {
                    @if let Some(ref channel_id) = report.mutual_dm_channel_id {
                        (nav_link(&message_lookup_href(base, channel_id, None), "View Mutual DM Channel"))
                    }
                }
            }
        }))
    }
}

fn resolve_report_form(base: &str, report_id: &str, csrf_token: &str) -> Markup {
    html! {
        form method="post" action={(base) "/reports/" (report_id) "/resolve"} class="flex flex-col gap-3"
            data-admin-refresh-on-success="#report-detail" {
            (csrf_input(csrf_token))
            label for="resolution" class="block text-sm font-medium text-neutral-700" {
                "Public comment to the reporter (optional)"
            }
            textarea id="resolution" name="resolution" rows="3" maxlength="512"
                class="block w-full rounded-md border border-neutral-300 \
                       px-3 py-2 text-sm shadow-sm \
                       focus:border-brand-primary focus:outline-none \
                       focus:ring-1 focus:ring-brand-primary" {}
            (opt_out_checkbox("notify_reporter", "Include the public comment in the reporter notice"))
            p class="text-neutral-500 text-xs" data-resolve-notice-help="" {
                "The reporter is told the report was resolved either way, unless their account is gone or barred from reporting. \
                 When unticked, the comment is not sent or shown on the report. It is only kept in the audit log."
            }
            button type="submit"
                class="inline-flex w-full items-center justify-center gap-2 \
                       font-medium rounded-lg bg-neutral-900 text-white \
                       px-4 py-2 text-sm" {
                "Resolve Report"
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum SnapshotChange {
    Same,
    Changed,
    Unknown,
}

fn non_empty(value: Option<&str>) -> Option<&str> {
    value.filter(|value| !value.trim().is_empty())
}

fn compare_text(snapshot: Option<&str>, live: Option<Option<&str>>) -> SnapshotChange {
    match live {
        None => SnapshotChange::Unknown,
        Some(live) if non_empty(snapshot) == non_empty(live) => SnapshotChange::Same,
        Some(_) => SnapshotChange::Changed,
    }
}

fn compare_asset(
    snapshot: Option<&ReportProfileSnapshotAsset>,
    live: Option<Option<&str>>,
) -> SnapshotChange {
    compare_text(snapshot.map(|asset| asset.hash.as_str()), live)
}

fn padded_discriminator(value: Option<&str>) -> Option<String> {
    non_empty(value).map(|value| format!("{value:0>4}"))
}

fn snapshot_tag(
    username: Option<&str>,
    discriminator: Option<&str>,
    is_bot: bool,
) -> Option<String> {
    let username = non_empty(username)?;
    Some(match padded_discriminator(discriminator) {
        Some(discriminator) => user_tag(username, &discriminator, is_bot),
        None => username.to_owned(),
    })
}

fn snapshot_row(key: &str, label: &str, change: SnapshotChange, value: Markup) -> Markup {
    let changed = change == SnapshotChange::Changed;
    html! {
        div class="flex min-w-0 flex-col gap-1" data-snapshot-field=(key) data-snapshot-changed[changed] {
            p class="flex flex-wrap items-center gap-2 text-gray-500 text-xs" {
                (label)
                @if changed {
                    (badge("Changed since the report", BadgeVariant::Warning))
                }
            }
            (value)
        }
    }
}

fn snapshot_text(value: Option<&str>) -> Markup {
    match non_empty(value) {
        Some(value) => html! {
            p class="whitespace-pre-wrap break-words text-gray-900 text-sm" { (value) }
        },
        None => html! {
            p class="text-neutral-500 text-sm italic" { "Not set" }
        },
    }
}

fn snapshot_image(asset: Option<&ReportProfileSnapshotAsset>, alt: &str, wide: bool) -> Markup {
    let Some(asset) = asset else {
        return snapshot_text(None);
    };
    let image_class = if wide {
        "h-16 w-40 rounded-md border border-neutral-200 object-cover"
    } else {
        "h-16 w-16 rounded-full border border-neutral-200 object-cover"
    };
    html! {
        div class="flex flex-col gap-1" {
            @if let Some(url) = asset.url.as_deref() {
                a href=(url) target="_blank" rel="noopener noreferrer" class="w-fit" {
                    img src=(url) alt=(alt) class=(image_class) loading="lazy";
                }
            } @else {
                p class="text-amber-800 text-sm" data-snapshot-image-missing="" {
                    "Not preserved in the report"
                }
            }
            span class="break-all font-mono text-neutral-500 text-xs" { (asset.hash) }
        }
    }
}

fn snapshot_group(title: &str, note: Option<&str>, rows: Markup) -> Markup {
    html! {
        div class="space-y-3" {
            div class="space-y-1" {
                h3 class="font-semibold text-neutral-900 text-sm" { (title) }
                @if let Some(note) = note {
                    p class="text-neutral-500 text-xs" { (note) }
                }
            }
            div class="grid grid-cols-1 gap-4 sm:grid-cols-2" { (rows) }
        }
    }
}

fn profile_snapshot_section(
    config: &AdminConfig,
    report: &ReportEntry,
    live: &LiveProfile,
) -> Markup {
    let Some(snapshot) = report.reported_profile_snapshot.as_ref() else {
        return html! {};
    };
    if snapshot.user.is_none() && snapshot.member.is_none() && snapshot.guild.is_none() {
        return html! {};
    }
    let description = snapshot
        .captured_at
        .as_deref()
        .map(|captured| {
            format!(
                "Captured {}. Fields marked as changed differ from the live profile. Image links expire after 5 minutes. Reload the page for new ones.",
                format_admin_timestamp(captured)
            )
        })
        .unwrap_or_else(|| {
            "Fields marked as changed differ from the live profile. Image links expire after 5 minutes. Reload the page for new ones.".to_owned()
        });
    html! {
        (section_card(Some("At Report Time"), Some(&description), None, html! {
            div class="space-y-6" data-report-profile-snapshot="" {
                (snapshot_user_group(config, snapshot, live))
                (snapshot_member_group(snapshot))
                (snapshot_guild_group(config, snapshot, live))
            }
        }))
    }
}

fn snapshot_user_group(
    config: &AdminConfig,
    snapshot: &ReportProfileSnapshot,
    live: &LiveProfile,
) -> Markup {
    let Some(user) = snapshot.user.as_ref() else {
        return html! {};
    };
    let base = &config.base_path;
    let live_user = live.user.as_ref().filter(|live| live.id == user.id);
    let closed = non_empty(user.username.as_deref()).is_none();
    let note = if closed {
        Some("The account was closed when the report was filed, so only its ID was kept.")
    } else if live_user.is_none() {
        Some("The live account could not be loaded, so changes are not marked.")
    } else {
        None
    };
    let is_bot = live_user.is_some_and(|live| live.bot);
    let tag = snapshot_tag(
        user.username.as_deref(),
        user.discriminator.as_deref(),
        is_bot,
    );
    let tag_change = match live_user {
        None => SnapshotChange::Unknown,
        Some(live) => {
            let live_tag = snapshot_tag(Some(&live.username), Some(&live.discriminator), is_bot);
            if live_tag == tag {
                SnapshotChange::Same
            } else {
                SnapshotChange::Changed
            }
        }
    };
    let field = |value: fn(&AdminUser) -> Option<&str>| live_user.map(value);
    html! {
        (snapshot_group("Account", note, html! {
            (data_field_link_mono("Account ID", &format!("{base}/users/{}", user.id), &user.id))
            @if !closed {
                (snapshot_row("user.username", "Username", tag_change, snapshot_text(tag.as_deref())))
                (snapshot_row(
                    "user.global_name",
                    "Display Name",
                    compare_text(user.global_name.as_deref(), field(|live| live.global_name.as_deref())),
                    snapshot_text(user.global_name.as_deref()),
                ))
                (snapshot_row(
                    "user.pronouns",
                    "Pronouns",
                    compare_text(user.pronouns.as_deref(), field(|live| live.pronouns.as_deref())),
                    snapshot_text(user.pronouns.as_deref()),
                ))
                (snapshot_row(
                    "user.bio",
                    "Bio",
                    compare_text(user.bio.as_deref(), field(|live| live.bio.as_deref())),
                    snapshot_text(user.bio.as_deref()),
                ))
                (snapshot_row(
                    "user.avatar",
                    "Avatar",
                    compare_asset(user.avatar.as_ref(), field(|live| live.avatar.as_deref())),
                    snapshot_image(user.avatar.as_ref(), "Avatar at report time", false),
                ))
                (snapshot_row(
                    "user.banner",
                    "Banner",
                    compare_asset(user.banner.as_ref(), field(|live| live.banner.as_deref())),
                    snapshot_image(user.banner.as_ref(), "Banner at report time", true),
                ))
            }
        }))
    }
}

fn snapshot_member_group(snapshot: &ReportProfileSnapshot) -> Markup {
    let Some(member) = snapshot.member.as_ref() else {
        return html! {};
    };
    let unknown = SnapshotChange::Unknown;
    html! {
        (snapshot_group(
            "Community Profile",
            Some("The community profile is not compared with the live one."),
            html! {
                (data_field_mono("Community ID", &member.guild_id))
                (snapshot_row("member.nick", "Nickname", unknown, snapshot_text(member.nick.as_deref())))
                (snapshot_row("member.pronouns", "Pronouns", unknown, snapshot_text(member.pronouns.as_deref())))
                (snapshot_row("member.bio", "Bio", unknown, snapshot_text(member.bio.as_deref())))
                (snapshot_row(
                    "member.joined_at",
                    "Joined",
                    unknown,
                    snapshot_text(member.joined_at.as_deref().map(format_admin_timestamp).as_deref()),
                ))
                (snapshot_row(
                    "member.avatar",
                    "Avatar",
                    unknown,
                    snapshot_image(member.avatar.as_ref(), "Community avatar at report time", false),
                ))
                (snapshot_row(
                    "member.banner",
                    "Banner",
                    unknown,
                    snapshot_image(member.banner.as_ref(), "Community banner at report time", true),
                ))
            },
        ))
    }
}

fn snapshot_guild_group(
    config: &AdminConfig,
    snapshot: &ReportProfileSnapshot,
    live: &LiveProfile,
) -> Markup {
    let Some(guild) = snapshot.guild.as_ref() else {
        return html! {};
    };
    let base = &config.base_path;
    let live_guild = live.guild.as_ref().filter(|live| live.id == guild.id);
    let note = live_guild
        .is_none()
        .then_some("The live community could not be loaded, so changes are not marked.");
    let field = |value: fn(&GuildDetailInfo) -> Option<&str>| live_guild.map(value);
    html! {
        (snapshot_group("Community", note, html! {
            (data_field_link_mono("Community ID", &format!("{base}/guilds/{}", guild.id), &guild.id))
            (snapshot_row(
                "guild.name",
                "Name",
                compare_text(guild.name.as_deref(), field(|live| Some(live.name.as_str()))),
                snapshot_text(guild.name.as_deref()),
            ))
            (snapshot_row(
                "guild.vanity_url_code",
                "Vanity URL",
                compare_text(guild.vanity_url_code.as_deref(), field(|live| live.vanity_url_code.as_deref())),
                snapshot_text(guild.vanity_url_code.as_deref()),
            ))
            (snapshot_row(
                "guild.icon",
                "Icon",
                compare_asset(guild.icon.as_ref(), field(|live| live.icon.as_deref())),
                snapshot_image(guild.icon.as_ref(), "Icon at report time", false),
            ))
            (snapshot_row(
                "guild.banner",
                "Banner",
                compare_asset(guild.banner.as_ref(), field(|live| live.banner.as_deref())),
                snapshot_image(guild.banner.as_ref(), "Banner at report time", true),
            ))
            (snapshot_row(
                "guild.splash",
                "Invite Splash",
                compare_asset(guild.splash.as_ref(), field(|live| live.splash.as_deref())),
                snapshot_image(guild.splash.as_ref(), "Invite splash at report time", true),
            ))
        }))
    }
}

fn legal_hold_card(
    config: &AdminConfig,
    auth: &AuthContext,
    report: &ReportEntry,
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    let action = format!("{base}/reports/{}/legal-hold", report.report_id);
    let held = non_empty(report.legal_hold_until.as_deref());
    let ended = held.is_some_and(legal_hold_has_ended);
    let state = match (held, ended) {
        (None, _) => "none",
        (Some(_), true) => "ended",
        (Some(_), false) => "active",
    };
    let can_edit = has_acl(auth, acl::REPORT_RESOLVE);
    html! {
        (section_card(Some("Legal Hold"), None, None, html! {
            div class="flex flex-col gap-3" data-report-legal-hold=(state) {
                @if let Some(until) = held {
                    p class="text-neutral-700 text-sm" {
                        span class="font-medium" { @if ended { "Hold ended: " } @else { "Held until: " } }
                        span data-report-legal-hold-until=(until) { (format_admin_timestamp(until)) }
                    }
                    @if ended {
                        p class="text-neutral-500 text-sm" {
                            "The report is no longer held and is deleted 365 days after it was filed."
                        }
                    }
                    @if let Some(reason) = non_empty(report.legal_hold_reason.as_deref()) {
                        p class="whitespace-pre-wrap break-words text-neutral-700 text-sm" data-report-legal-hold-reason="" {
                            span class="font-medium" { "Reason: " }
                            (reason)
                        }
                    }
                } @else {
                    p class="text-neutral-500 text-sm" {
                        "No hold. The report and its stored evidence are deleted 365 days after it was filed."
                    }
                }
                @if can_edit {
                    form method="post" action=(action) class="flex flex-col gap-3" data-legal-hold-form="set"
                        data-admin-refresh-on-success="#report-detail" {
                        (csrf_input(csrf_token))
                        label for="legal_hold_until" class=(FORM_LABEL_CLASS) { "Hold until" }
                        input type="date" id="legal_hold_until" name="legal_hold_until" required
                            class={(FORM_CONTROL_CLASS) " h-8 px-3 py-1.5"};
                        p class="text-neutral-500 text-xs" { "The hold ends at the end of that day, UTC." }
                        label for="legal_hold_reason" class=(FORM_LABEL_CLASS) { "Hold reason" }
                        textarea id="legal_hold_reason" name="legal_hold_reason" rows="2" maxlength="512" required
                            class={(FORM_CONTROL_CLASS) " px-3 py-2"} {}
                        button type="submit"
                            class="inline-flex w-full items-center justify-center gap-2 \
                                   font-medium rounded-lg bg-neutral-900 text-white \
                                   px-4 py-2 text-sm" {
                            @if state == "active" { "Update Hold" } @else { "Place Hold" }
                        }
                    }
                    @if held.is_some() {
                        form method="post" action=(action) data-legal-hold-form="clear"
                            data-admin-refresh-on-success="#report-detail" {
                            (csrf_input(csrf_token))
                            input type="hidden" name="clear" value="1";
                            button type="submit"
                                class="inline-flex w-full items-center justify-center gap-2 \
                                       rounded-lg border border-neutral-300 bg-white \
                                       px-4 py-2 font-medium text-neutral-700 text-sm \
                                       hover:bg-neutral-50" {
                                "Clear Hold"
                            }
                        }
                    }
                }
            }
        }))
    }
}

fn legal_hold_has_ended(until: &str) -> bool {
    time::OffsetDateTime::parse(until, &time::format_description::well_known::Rfc3339)
        .is_ok_and(|until| until <= time::OffsetDateTime::now_utc())
}

fn nav_link(href: &str, label: &str) -> Markup {
    html! {
        a href=(href)
            class="label rounded-lg border border-neutral-300 bg-white px-3 py-2 \
                   text-neutral-700 text-center text-sm transition-colors \
                   hover:bg-neutral-50 block" {
            (label)
        }
    }
}

fn message_context_section(
    config: &AdminConfig,
    report: &ReportEntry,
    include_delete: bool,
    csrf_token: Option<&str>,
) -> Markup {
    let Some(values) = &report.message_context else {
        return html! {};
    };
    if report.report_type != 0 || values.is_empty() {
        return html! {};
    }
    let messages = ordered_messages(values);
    if messages.is_empty() {
        return html! {};
    }
    html! {
        div class="overflow-hidden rounded-lg border border-neutral-200 bg-white" {
            div class="px-4 pt-4 pb-2 sm:px-6 sm:pt-6" {
                h2 class="font-semibold text-base text-neutral-900" { "Message Context" }
            }
            div class="py-2" {
                (message_list(
                    config,
                    &config.base_path,
                    &messages,
                    include_delete,
                    report.reported_message_id.as_deref(),
                ))
            }
        }
        @if let Some(csrf_token) = csrf_token {
            (message_deletion_script(csrf_token))
        }
    }
}

pub fn report_detail_page(
    config: &AdminConfig,
    auth: &AuthContext,
    report: &ReportEntry,
    live: &LiveProfile,
    csrf_token: &str,
    is_htmx: bool,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        div id="report-detail" class="space-y-6" {
            (page_header_with_back(
                "Report Details",
                None,
                &format!("{base}/reports"),
                Some("Back to Reports"),
            ))
            div class="grid grid-cols-1 gap-6 lg:grid-cols-3" {
            div class="space-y-6 lg:col-span-2" {
                (basic_info_section(config, auth, report))
                (report_answers_section(report))
                (reported_entity_section(config, report))
                (profile_snapshot_section(config, report, live))
                (message_context_section(config, report, has_acl(auth, acl::MESSAGE_DELETE), Some(csrf_token)))
                (additional_info_section(report))
            }
            div class="space-y-6" {
                (status_card(config, report))
                (actions_card(config, report, csrf_token))
                (legal_hold_card(config, auth, report, csrf_token))
            }
        }
        }
    };
    if is_htmx {
        content
    } else {
        admin_layout(config, auth, "Report Details", "reports", None, content)
    }
}

pub fn report_detail_fragment(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        div data-report-fragment="" class="space-y-4" {
            (basic_info_section_fragment(config, report))
            (report_answers_section(report))
            (reported_entity_section(config, report))
            (message_context_section(config, report, false, None))
            (additional_info_section(report))
            a href={(base) "/reports/" (&report.report_id)}
                class="inline-flex min-h-[44px] w-full items-center justify-center \
                       rounded-lg bg-neutral-900 px-4 py-2 font-medium text-sm \
                       text-white transition-colors hover:bg-neutral-800" {
                "Open full report"
            }
        }
    }
}

fn basic_info_section_fragment(config: &AdminConfig, report: &ReportEntry) -> Markup {
    let base = &config.base_path;
    html! {
        (section_card(Some("Basic Information"), None, None, html! {
            div class="grid grid-cols-1 sm:grid-cols-2 gap-4" {
                (data_field_mono("Report ID", &report.report_id))
                (data_field_text("Reported At", &format_admin_timestamp(&report.reported_at)))
                (data_field_text("Type", report_type_label(report.report_type)))
                (category_field(report))
                (reason_field(report))
                @if let Some(ref reporter_id) = report.reporter_id {
                    (data_field_link_mono("Reporter", &format!("{base}/users/{reporter_id}"), &reporter_label(report)))
                } @else {
                    (data_field_text("Reporter", &reporter_label(report)))
                }
                (data_field("Status", status_badge(report.status)))
            }
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        api::types::ReportFlowAnswerItemEntry,
        config::{ProxyConfig, RuntimeEnv},
        session::Session,
        templates::components::report_webhook::webhook_type_label,
    };
    use serde_json::json;

    fn test_config() -> AdminConfig {
        AdminConfig {
            env: RuntimeEnv::Test,
            host: String::new(),
            port: 0,
            secret_key_base: "test-secret".to_owned(),
            base_path: "/admin".to_owned(),
            api_endpoint: String::new(),
            media_endpoint: "https://media.example.test".to_owned(),
            static_cdn_endpoint: String::new(),
            admin_endpoint: String::new(),
            web_app_endpoint: String::new(),
            oauth_client_id: String::new(),
            oauth_client_secret: String::new(),
            oauth_redirect_uri: String::new(),
            build_version: "test".to_owned(),
            self_hosted: false,
            proxy: ProxyConfig {
                trust_client_ip_header: false,
                client_ip_header_name: String::new(),
            },
        }
    }

    fn test_auth() -> AuthContext {
        AuthContext {
            session: Session {
                user_id: "1500000000000000000".to_owned(),
                access_token: "token".to_owned(),
                created_at: 0,
            },
            admin_user: None,
        }
    }

    fn step(
        screen_title: &str,
        option_id: Option<&str>,
        option_label: Option<&str>,
        items: &[&str],
    ) -> ReportFlowAnswerStepEntry {
        ReportFlowAnswerStepEntry {
            screen_id: "screen".to_owned(),
            screen_title: screen_title.to_owned(),
            option_id: option_id.map(str::to_owned),
            option_label: option_label.map(str::to_owned),
            items: items
                .iter()
                .map(|label| ReportFlowAnswerItemEntry {
                    id: label.to_lowercase(),
                    label: (*label).to_owned(),
                })
                .collect(),
        }
    }

    fn report(extra: serde_json::Value) -> ReportEntry {
        let mut value = json!({
            "report_id": "1800000000000000001",
            "reported_at": "2026-05-26T12:00:00.000Z",
            "status": 0,
            "report_type": 0,
            "category": "child_safety"
        });
        for (key, field) in extra.as_object().expect("object").clone() {
            value[key] = field;
        }
        serde_json::from_value(value).expect("valid report")
    }

    #[test]
    fn answer_step_lines_cover_choice_checklist_and_info_steps() {
        assert_eq!(
            answer_step_line(&step(
                "What is wrong with this message?",
                Some("sexual"),
                Some("Sexual content"),
                &[]
            )),
            "What is wrong with this message?: Sexual content"
        );
        assert_eq!(
            answer_step_line(&step(
                "Which parts of the profile?",
                None,
                None,
                &["Avatar", "Bio"]
            )),
            "Which parts of the profile?: Avatar, Bio"
        );
        assert_eq!(
            answer_step_line(&step("Before you report", None, None, &[])),
            "Before you report"
        );
        assert_eq!(
            answer_step_line(&step("abuse", Some("future_option"), None, &[])),
            "abuse: future_option"
        );
    }

    #[test]
    fn both_basic_info_sections_render_the_reason_row() {
        let config = test_config();
        let v2 = report(json!({
            "reason": "csam",
            "reason_label": "Child sexual abuse material",
            "reason_highest_priority": true
        }));
        for markup in [
            basic_info_section(&config, &test_auth(), &v2).into_string(),
            basic_info_section_fragment(&config, &v2).into_string(),
        ] {
            assert!(markup.contains(">Reason<"), "{markup}");
            assert!(markup.contains(r#"data-report-reason="csam""#), "{markup}");
            assert!(markup.contains("Child sexual abuse material"), "{markup}");
            assert!(markup.contains("Highest priority"), "{markup}");
        }
        let unknown = report(json!({
            "reason": "future_reason",
            "reason_label": "future_reason",
            "reason_highest_priority": null
        }));
        let markup = basic_info_section_fragment(&config, &unknown).into_string();
        assert!(markup.contains("future_reason"), "{markup}");
        assert!(!markup.contains("Highest priority"), "{markup}");
        let legacy = report(json!({}));
        for markup in [
            basic_info_section(&config, &test_auth(), &legacy).into_string(),
            basic_info_section_fragment(&config, &legacy).into_string(),
        ] {
            assert!(!markup.contains(">Reason<"), "{markup}");
        }
    }

    #[test]
    fn answers_section_lists_steps_and_flow_metadata() {
        let dsa = report(json!({
            "reason": "csam",
            "reason_label": "Child sexual abuse material",
            "reason_highest_priority": true,
            "reporter_good_faith_confirmed": true,
            "flow": {
                "revision_hash": "b7667e8b32c98c40",
                "surface": "dsa",
                "locale": "fr",
                "steps": [
                    {"screen_id": "root_message", "screen_title": "What is wrong with this message?", "option_id": "sexual", "option_label": "Sexual content", "items": []},
                    {"screen_id": "profile_intro", "screen_title": "Before you report", "option_id": null, "option_label": null, "items": []}
                ]
            }
        }));
        let markup = report_answers_section(&dsa).into_string();
        assert!(markup.contains("Report Answers"), "{markup}");
        assert!(
            markup.contains("What is wrong with this message?: Sexual content"),
            "{markup}"
        );
        assert!(
            markup.contains(r#"data-report-answer-step="profile_intro""#),
            "{markup}"
        );
        assert!(markup.contains("Shown to the reporter in fr"), "{markup}");
        assert!(markup.contains("b7667e8b32c98c40"), "{markup}");
        assert!(markup.contains("Surface: DSA form"), "{markup}");
        assert!(
            markup.contains("Good-faith statement: Confirmed"),
            "{markup}"
        );

        let in_app = report(json!({
            "flow": {"revision_hash": "16781a19a797f23f", "surface": "in_app", "locale": null, "steps": []}
        }));
        let markup = report_answers_section(&in_app).into_string();
        assert!(markup.contains("Surface: In app"), "{markup}");
        assert!(!markup.contains("Shown to the reporter in"), "{markup}");
        assert!(!markup.contains("Good-faith statement"), "{markup}");

        assert!(
            report_answers_section(&report(json!({})))
                .into_string()
                .is_empty()
        );
    }

    fn webhook_report() -> ReportEntry {
        report(json!({
            "category": "spam",
            "reported_user_id": null,
            "reported_webhook_id": "1700000000000000500",
            "reported_webhook_name": "Harbor Bulletin",
            "reported_webhook_avatar_hash": "abc123",
            "reported_message_id": "1700000000000000600",
            "reported_channel_id": "1700000000000000700",
            "reported_channel_name": "general",
            "reported_guild_id": "1700000000000000800",
            "reported_guild_name": "Harbor",
            "message_context": [
                {
                    "id": "1700000000000000600",
                    "channel_id": "1700000000000000700",
                    "content": "Free stuff here",
                    "timestamp": "2026-10-04T10:00:00.000Z",
                    "attachments": [],
                    "author_id": "1700000000000000500",
                    "author_username": "Harbor Bulletin",
                    "author_global_name": null,
                    "author_discriminator": "0000",
                    "author_avatar": "abc123",
                    "webhook_id": "1700000000000000500"
                },
                {
                    "id": "1700000000000000590",
                    "channel_id": "1700000000000000700",
                    "content": "hello",
                    "timestamp": "2026-10-04T09:59:00.000Z",
                    "attachments": [],
                    "author_id": "1700000000000000900",
                    "author_username": "member",
                    "author_global_name": null,
                    "author_discriminator": "0001",
                    "author_avatar": null,
                    "webhook_id": null
                }
            ]
        }))
    }

    #[test]
    fn reported_entity_names_the_webhook_and_never_links_it_as_a_user() {
        let config = test_config();
        let markup = reported_entity_section(&config, &webhook_report()).into_string();
        assert!(
            markup.contains(r#"data-report-webhook="1700000000000000500""#),
            "{markup}"
        );
        assert!(markup.contains("Harbor Bulletin"), "{markup}");
        assert!(markup.contains(">Webhook<"), "{markup}");
        assert!(markup.contains(">Webhook ID<"), "{markup}");
        assert!(
            markup.contains(r#"href="/admin/reports?reported_webhook_id=1700000000000000500""#),
            "{markup}"
        );
        assert!(
            markup.contains("https://media.example.test/avatars/1700000000000000500/abc123"),
            "{markup}"
        );
        assert!(markup.contains("1700000000000000700"), "{markup}");
        assert!(markup.contains("general"), "{markup}");
        assert!(
            markup.contains(r#"href="/admin/guilds/1700000000000000800""#),
            "{markup}"
        );
        assert!(!markup.contains("/admin/users/"), "{markup}");
        assert!(!markup.contains("User unknown"), "{markup}");

        let unnamed = report(json!({
            "reported_user_id": null,
            "reported_webhook_id": "1700000000000000500",
            "reported_webhook_name": null
        }));
        let markup = reported_entity_section(&config, &unnamed).into_string();
        assert!(markup.contains("Webhook 1700000000000000500"), "{markup}");

        let user = report(json!({
            "reported_user_id": "1700000000000000900",
            "reported_user_tag": "member#0001",
            "reported_webhook_id": null
        }));
        let markup = reported_entity_section(&config, &user).into_string();
        assert!(
            markup.contains(r#"href="/admin/users/1700000000000000900""#),
            "{markup}"
        );
        assert!(!markup.contains("data-report-webhook"), "{markup}");
    }

    #[test]
    fn reported_entity_links_the_webhook_creator() {
        let config = test_config();
        let live = report(json!({
            "reported_user_id": null,
            "reported_webhook_id": "1700000000000000500",
            "reported_webhook_name": "Harbor Bulletin",
            "reported_webhook_default_name": "Harbor Hook",
            "reported_webhook_creator_id": "1700000000000000950",
            "reported_webhook_creator_tag": "keeper#0042",
            "reported_webhook_creator_username": "keeper",
            "reported_webhook_creator_global_name": "Harbor Keeper",
            "reported_webhook_creator_discriminator": "0042",
            "reported_webhook_creator_avatar_hash": "def456"
        }));
        let markup = reported_entity_section(&config, &live).into_string();
        assert!(markup.contains(">Webhook Creator<"), "{markup}");
        assert!(
            markup.contains(r#"data-report-webhook-creator="1700000000000000950""#),
            "{markup}"
        );
        assert!(
            markup.contains(r#"href="/admin/users/1700000000000000950""#),
            "{markup}"
        );
        assert!(markup.contains("Harbor Keeper (keeper#0042)"), "{markup}");
        assert!(
            markup.contains("https://media.example.test/avatars/1700000000000000950/def456"),
            "{markup}"
        );
        assert!(markup.contains(">Configured Name<"), "{markup}");
        assert!(markup.contains("Harbor Hook"), "{markup}");
        assert!(
            !markup.contains("/admin/users/1700000000000000500"),
            "{markup}"
        );
        let actions = actions_card(&config, &live, "csrf").into_string();
        assert!(!actions.contains("View Reported User"), "{actions}");

        let deleted = report(json!({
            "reported_user_id": null,
            "reported_webhook_id": "1700000000000000500",
            "reported_webhook_name": "Harbor Hook",
            "reported_webhook_default_name": "Harbor Hook",
            "reported_webhook_creator_id": "1700000000000000950",
            "reported_webhook_creator_tag": null,
            "reported_webhook_creator_username": null,
            "reported_webhook_creator_global_name": null,
            "reported_webhook_creator_discriminator": null,
            "reported_webhook_creator_avatar_hash": null
        }));
        let markup = reported_entity_section(&config, &deleted).into_string();
        assert!(
            markup.contains(r#"href="/admin/users/1700000000000000950""#),
            "{markup}"
        );
        assert!(markup.contains("User 1700000000000000950"), "{markup}");
        assert!(!markup.contains(">Configured Name<"), "{markup}");
        assert!(
            markup.contains("data-report-webhook-creator-deleted"),
            "{markup}"
        );
        assert!(markup.contains(">Account Deleted<"), "{markup}");
        assert!(
            !markup.contains("data-report-webhook-creator-bot"),
            "{markup}"
        );

        let live_markup = reported_entity_section(&config, &live).into_string();
        assert!(
            !live_markup.contains("data-report-webhook-creator-deleted"),
            "{live_markup}"
        );

        let markup = reported_entity_section(&config, &webhook_report()).into_string();
        assert!(!markup.contains(">Webhook Creator<"), "{markup}");
    }

    fn webhook_context_report(extra: serde_json::Value) -> ReportEntry {
        let mut value = serde_json::to_value(webhook_report()).expect("serialize report");
        for (key, field) in extra.as_object().expect("object").clone() {
            value[key] = field;
        }
        serde_json::from_value(value).expect("valid report")
    }

    #[test]
    fn reported_entity_shows_the_webhook_context() {
        let config = test_config();
        let in_place = webhook_context_report(json!({
            "reported_webhook_default_name": "Harbor Bulletin",
            "reported_webhook_default_avatar_hash": "abc123",
            "reported_webhook_type": 1,
            "reported_webhook_channel_id": "1700000000000000700",
            "reported_webhook_guild_id": "1700000000000000800",
            "reported_webhook_created_at": "2026-10-04T09:05:00.000Z",
            "reported_webhook_creator_id": "1700000000000000950",
            "reported_webhook_creator_username": "keeper",
            "reported_webhook_creator_discriminator": "0042"
        }));
        let markup = reported_entity_section(&config, &in_place).into_string();
        assert!(markup.contains(">Webhook Type<"), "{markup}");
        assert!(markup.contains(">Incoming<"), "{markup}");
        assert!(markup.contains(">Webhook Created<"), "{markup}");
        assert!(markup.contains("Oct 4, 2026, 9:05 AM UTC"), "{markup}");
        assert!(!markup.contains(">Configured Name<"), "{markup}");
        assert!(!markup.contains(">Configured Avatar<"), "{markup}");
        assert!(!markup.contains(">Webhook Channel ID<"), "{markup}");
        assert!(!markup.contains(">Webhook Guild ID<"), "{markup}");
        assert!(!markup.contains(">Application ID<"), "{markup}");
        assert!(!markup.contains(">Webhook Record<"), "{markup}");
        assert!(
            !markup.contains("data-report-webhook-creator-bot"),
            "{markup}"
        );

        let moved_bot = webhook_context_report(json!({
            "reported_webhook_name": "Relay Override",
            "reported_webhook_default_name": "Relay",
            "reported_webhook_default_avatar_hash": "fed321",
            "reported_webhook_type": 2,
            "reported_webhook_channel_id": "1700000000000000701",
            "reported_webhook_guild_id": "1700000000000000801",
            "reported_webhook_application_id": "1700000000000000960",
            "reported_webhook_created_at": "2026-10-04T09:05:00.000Z",
            "reported_webhook_creator_id": "1700000000000000960",
            "reported_webhook_creator_tag": "relaybot#0000",
            "reported_webhook_creator_username": "relaybot",
            "reported_webhook_creator_discriminator": "0000"
        }));
        let markup = reported_entity_section(&config, &moved_bot).into_string();
        assert!(markup.contains(">Channel Follower<"), "{markup}");
        assert!(markup.contains(">Configured Avatar<"), "{markup}");
        assert!(
            markup.contains("https://media.example.test/avatars/1700000000000000500/fed321"),
            "{markup}"
        );
        assert!(markup.contains(">Webhook Channel ID<"), "{markup}");
        assert!(
            markup.contains(
                r#"href="/admin/messages?channel_id=1700000000000000701&amp;context_limit=50""#
            ),
            "{markup}"
        );
        assert!(markup.contains(">Webhook Guild ID<"), "{markup}");
        assert!(
            markup.contains(r#"href="/admin/guilds/1700000000000000801""#),
            "{markup}"
        );
        assert!(markup.contains(">Application ID<"), "{markup}");
        assert!(
            markup.contains(r#"href="/admin/applications/1700000000000000960""#),
            "{markup}"
        );
        assert!(
            markup.contains("data-report-webhook-creator-bot"),
            "{markup}"
        );
        assert!(
            !markup.contains("data-report-webhook-creator-deleted"),
            "{markup}"
        );

        let deleted_webhook = webhook_context_report(json!({
            "reported_webhook_created_at": "2026-10-04T09:05:00.000Z"
        }));
        let markup = reported_entity_section(&config, &deleted_webhook).into_string();
        assert!(markup.contains(">Webhook Record<"), "{markup}");
        assert!(markup.contains("Deleted before the report"), "{markup}");
        assert!(markup.contains(">Webhook Created<"), "{markup}");
        assert!(!markup.contains(">Webhook Type<"), "{markup}");
        assert!(!markup.contains(">Configured Avatar<"), "{markup}");
        assert!(!markup.contains(">Webhook Creator<"), "{markup}");

        let legacy = reported_entity_section(&config, &webhook_report()).into_string();
        assert!(!legacy.contains(">Webhook Record<"), "{legacy}");
        assert!(!legacy.contains(">Webhook Created<"), "{legacy}");
    }

    #[test]
    fn unknown_webhook_types_show_their_number() {
        assert_eq!(webhook_type_label(1), "Incoming");
        assert_eq!(webhook_type_label(2), "Channel Follower");
        assert_eq!(webhook_type_label(7), "Type 7");
    }

    #[test]
    fn webhook_actions_offer_no_reported_user_link() {
        let markup = actions_card(&test_config(), &webhook_report(), "csrf").into_string();
        assert!(!markup.contains("View Reported User"), "{markup}");
        assert!(
            !markup.contains("/admin/users/1700000000000000500"),
            "{markup}"
        );
    }

    #[test]
    fn message_context_shows_webhook_authors_without_user_links() {
        let config = test_config();
        let markup = message_context_section(&config, &webhook_report(), false, None).into_string();
        assert!(
            markup.contains(r#"data-message-webhook="1700000000000000500""#),
            "{markup}"
        );
        assert!(markup.contains(">Harbor Bulletin<"), "{markup}");
        assert!(!markup.contains("@Harbor Bulletin"), "{markup}");
        assert!(
            !markup.contains("/admin/users/1700000000000000500"),
            "{markup}"
        );
        assert!(
            markup.contains(r#"href="/admin/users/1700000000000000900""#),
            "{markup}"
        );
    }

    fn auth_with_acls(acls: &[&str]) -> AuthContext {
        let mut auth = test_auth();
        auth.admin_user = Some(
            serde_json::from_value(json!({
                "id": "1500000000000000000",
                "username": "admin",
                "discriminator": "0001",
                "acls": acls
            }))
            .expect("valid admin user"),
        );
        auth
    }

    fn image_report(category: &str) -> ReportEntry {
        report(json!({
            "report_id": "1800000000000000009",
            "category": category,
            "reported_message_id": "1700000000000000600",
            "reported_channel_id": "1700000000000000700",
            "message_context": [{
                "id": "1700000000000000600",
                "channel_id": "1700000000000000700",
                "content": "",
                "timestamp": "2026-10-04T10:00:00.000Z",
                "author_id": "1700000000000000900",
                "author_username": "member",
                "author_global_name": null,
                "author_discriminator": "0001",
                "author_avatar": null,
                "attachments": [{
                    "id": "1700000000000000610",
                    "url": "https://media.example.test/attachments/image.png",
                    "filename": "image.png",
                    "content_type": "image/png",
                    "size": 4096
                }]
            }]
        }))
    }

    const DELETE_BUTTON: &str = r#"class="delete-message-btn"#;

    fn render_page(auth: &AuthContext, report: &ReportEntry) -> String {
        report_detail_page(
            &test_config(),
            auth,
            report,
            &LiveProfile::default(),
            "csrf",
            true,
        )
        .into_string()
    }

    #[test]
    fn message_delete_button_follows_the_acl() {
        let auth = auth_with_acls(&[acl::REPORT_VIEW, acl::MESSAGE_DELETE]);
        let markup = render_page(&auth, &image_report("child_safety"));
        assert!(markup.contains("image.png"), "{markup}");
        assert!(markup.contains(DELETE_BUTTON), "{markup}");

        let markup = render_page(&auth_with_acls(&[acl::WILDCARD]), &image_report("spam"));
        assert!(markup.contains(DELETE_BUTTON), "{markup}");

        let markup = render_page(&auth_with_acls(&[acl::REPORT_VIEW]), &image_report("spam"));
        assert!(markup.contains("image.png"), "{markup}");
        assert!(!markup.contains(DELETE_BUTTON), "{markup}");

        let markup = render_page(&test_auth(), &image_report("child_safety"));
        assert!(!markup.contains(DELETE_BUTTON), "{markup}");
    }

    #[test]
    fn peek_fragment_offers_no_message_actions() {
        let markup =
            report_detail_fragment(&test_config(), &image_report("child_safety")).into_string();
        assert!(markup.contains("image.png"), "{markup}");
        assert!(!markup.contains(DELETE_BUTTON), "{markup}");
    }

    #[test]
    fn category_rows_show_the_label_and_keep_the_key() {
        let config = test_config();
        let doxxing = report(json!({"category": "doxxing"}));
        for markup in [
            basic_info_section(&config, &test_auth(), &doxxing).into_string(),
            basic_info_section_fragment(&config, &doxxing).into_string(),
        ] {
            assert!(markup.contains(">Category<"), "{markup}");
            assert!(
                markup.contains(r#"data-report-category="doxxing""#),
                "{markup}"
            );
            assert!(
                markup.contains(">Sharing personal information<"),
                "{markup}"
            );
        }
        let unknown = report(json!({"category": "future_value"}));
        let markup = basic_info_section_fragment(&config, &unknown).into_string();
        assert!(
            markup.contains(r#"data-report-category="future_value""#),
            "{markup}"
        );
        assert!(markup.contains(">future_value<"), "{markup}");
        let missing = report(json!({"category": null}));
        let markup = basic_info_section_fragment(&config, &missing).into_string();
        assert!(markup.contains(">Category<"), "{markup}");
        assert!(!markup.contains("data-report-category"), "{markup}");
    }

    #[test]
    fn resolve_form_shares_the_comment_by_default_and_says_the_reporter_is_always_told() {
        let markup = resolve_report_form("/admin", "1500000000000000001", "csrf").into_string();
        assert!(markup.contains(r#"action="/admin/reports/1500000000000000001/resolve""#));
        assert!(markup.contains(r##"data-admin-refresh-on-success="#report-detail""##));
        assert!(markup.contains(r#"name="resolution""#));
        assert!(markup.contains(r#"maxlength="512""#));
        assert!(markup.contains(r#"name="notify_reporter" value="true" checked"#));
        assert!(markup.contains(r#"name="notify_reporter_present" value="1""#));
        assert!(
            markup.contains(">Include the public comment in the reporter notice<"),
            "{markup}"
        );
        assert!(
            markup.contains("The reporter is told the report was resolved either way"),
            "{markup}"
        );
        assert!(
            markup.contains("When unticked, the comment is not sent or shown on the report. It is only kept in the audit log."),
            "{markup}"
        );
        assert!(!markup.contains("Notify the reporter"), "{markup}");
    }

    #[test]
    fn reported_user_shows_a_bot_badge_only_for_bot_accounts() {
        let config = test_config();
        let bot = report(json!({
            "report_type": 1,
            "reported_user_id": "1700000000000000900",
            "reported_user_tag": "relaybot#0000",
            "reported_user_bot": true
        }));
        let markup = reported_entity_section(&config, &bot).into_string();
        assert!(
            markup.contains(r#"data-report-user-bot="1700000000000000900""#),
            "{markup}"
        );
        assert!(markup.contains(">Bot<"), "{markup}");
        for flag in [json!(false), json!(null)] {
            let human = report(json!({
                "report_type": 1,
                "reported_user_id": "1700000000000000900",
                "reported_user_tag": "member#0001",
                "reported_user_bot": flag
            }));
            let markup = reported_entity_section(&config, &human).into_string();
            assert!(!markup.contains("data-report-user-bot"), "{markup}");
        }
        let old = report(json!({"report_type": 1, "reported_user_id": "1700000000000000900"}));
        let markup = reported_entity_section(&config, &old).into_string();
        assert!(!markup.contains("data-report-user-bot"), "{markup}");
    }

    fn context_report() -> ReportEntry {
        report(json!({
            "category": "spam",
            "reported_message_id": "1700000000000000600",
            "reported_channel_id": "1700000000000000700",
            "message_context": [
                {
                    "id": "1700000000000000600",
                    "channel_id": "1700000000000000700",
                    "content": "",
                    "timestamp": "2026-10-04T10:00:00.000Z",
                    "attachments": [],
                    "missing_attachments": [{
                        "id": "1700000000000000610",
                        "filename": "harbor-notes.pdf",
                        "nsfw": null,
                        "content_type": "application/pdf",
                        "width": null,
                        "height": null,
                        "size": 2048
                    }],
                    "author_id": "1700000000000000900",
                    "author_username": "relaybot",
                    "author_global_name": null,
                    "author_discriminator": "0000",
                    "author_avatar": null,
                    "webhook_id": null,
                    "author_bot": true
                },
                {
                    "id": "1700000000000000590",
                    "channel_id": "1700000000000000700",
                    "content": "hello",
                    "timestamp": "2026-10-04T09:59:00.000Z",
                    "attachments": [],
                    "missing_attachments": [],
                    "author_id": "1700000000000000901",
                    "author_username": "member",
                    "author_global_name": null,
                    "author_discriminator": "0001",
                    "author_avatar": null,
                    "webhook_id": null,
                    "author_bot": false
                },
                {
                    "id": "1700000000000000580",
                    "channel_id": "1700000000000000700",
                    "content": "Free stuff",
                    "timestamp": "2026-10-04T09:58:00.000Z",
                    "attachments": [],
                    "author_id": "1700000000000000500",
                    "author_username": "Harbor Bulletin",
                    "author_global_name": null,
                    "author_discriminator": "0000",
                    "author_avatar": null,
                    "webhook_id": "1700000000000000500",
                    "author_bot": null
                }
            ]
        }))
    }

    #[test]
    fn message_context_marks_bot_authors_only() {
        let markup =
            message_context_section(&test_config(), &context_report(), false, None).into_string();
        assert!(
            markup.contains(r#"data-message-author-bot="1700000000000000900""#),
            "{markup}"
        );
        assert_eq!(
            markup.matches("data-message-author-bot").count(),
            1,
            "{markup}"
        );
        assert!(
            markup.contains(r#"data-message-webhook="1700000000000000500""#),
            "{markup}"
        );
    }

    #[test]
    fn message_context_formats_timestamps_and_ids_every_grouped_row() {
        let message = |id: &str, content: &str, timestamp: &str| {
            json!({
                "id": id,
                "channel_id": "1700000000000000700",
                "content": content,
                "timestamp": timestamp,
                "attachments": [],
                "author_id": "1700000000000000900",
                "author_username": "relaybot",
                "author_global_name": null,
                "author_discriminator": "0000",
                "author_avatar": null,
                "webhook_id": null,
                "author_bot": true
            })
        };
        let grouped = report(json!({
            "category": "spam",
            "reported_message_id": "1700000000000000600",
            "reported_channel_id": "1700000000000000700",
            "message_context": [
                message("1700000000000000600", "Reminder", "2026-10-05T16:46:12.378Z"),
                message("1700000000000000590", "Joined", "2026-10-05T16:45:00.000Z")
            ]
        }));
        let markup = message_context_section(&test_config(), &grouped, false, None).into_string();
        assert!(markup.contains("Oct 5, 2026, 4:45 PM UTC"), "{markup}");
        assert!(!markup.contains("2026-10-05T16:4"), "{markup}");
        assert!(
            markup.contains(r#"<span class="">1700000000000000590</span>"#),
            "{markup}"
        );
        let reported = markup
            .split(r#"data-message-id="1700000000000000600""#)
            .nth(1)
            .expect("reported row");
        assert!(
            reported.contains(r#"<span class="">1700000000000000600</span>"#),
            "{markup}"
        );
    }

    #[test]
    fn message_context_names_attachments_that_were_not_preserved() {
        let markup =
            message_context_section(&test_config(), &context_report(), false, None).into_string();
        assert!(
            markup.contains(r#"data-missing-attachment="1700000000000000610""#),
            "{markup}"
        );
        assert!(
            markup.contains("harbor-notes.pdf was not preserved in the report snapshot"),
            "{markup}"
        );
        assert_eq!(
            markup.matches("data-missing-attachment").count(),
            1,
            "{markup}"
        );
        let fragment = report_detail_fragment(&test_config(), &context_report()).into_string();
        assert!(
            fragment.contains("harbor-notes.pdf was not preserved in the report snapshot"),
            "{fragment}"
        );
        let markup = message_context_section(&test_config(), &image_report("spam"), false, None)
            .into_string();
        assert!(!markup.contains("data-missing-attachment"), "{markup}");
        assert!(!markup.contains("data-message-author-bot"), "{markup}");
    }

    fn snapshot_report() -> ReportEntry {
        report(json!({
            "report_type": 1,
            "category": "harassment",
            "reported_user_id": "1700000000000000900",
            "reported_guild_id": "1700000000000000800",
            "reported_profile_snapshot": {
                "captured_at": "2026-10-04T10:00:00.000Z",
                "user": {
                    "id": "1700000000000000900",
                    "username": "target",
                    "discriminator": "42",
                    "global_name": "Jordan Target",
                    "bio": "Original bio",
                    "pronouns": "they/them",
                    "avatar": {"hash": "avatar1", "url": "https://reports.example.test/avatar1?sig=abc"},
                    "banner": {"hash": "banner1", "url": null}
                },
                "member": {
                    "guild_id": "1700000000000000800",
                    "nick": "Harbor Nick",
                    "bio": null,
                    "pronouns": null,
                    "joined_at": "2026-10-01T08:00:00.000Z",
                    "avatar": null,
                    "banner": null
                },
                "guild": null
            }
        }))
    }

    fn live_user(extra: serde_json::Value) -> AdminUser {
        let mut value = json!({
            "id": "1700000000000000900",
            "username": "target",
            "discriminator": 42,
            "global_name": "Jordan Target",
            "bio": "Original bio",
            "pronouns": "they/them",
            "avatar": "avatar1",
            "banner": "banner1",
            "email": null,
            "accent_color": null,
            "date_of_birth": null,
            "locale": null,
            "premium_type": null,
            "premium_since": null,
            "premium_until": null,
            "premium_grace_ends_at": null,
            "premium_lifetime_sequence": null,
            "temp_banned_until": null,
            "pending_deletion_at": null,
            "pending_bulk_message_deletion_at": null,
            "deletion_reason_code": null,
            "deletion_public_reason": null,
            "last_active_at": null,
            "last_active_ip": null,
            "last_active_ip_reverse": null,
            "last_active_location": null
        });
        for (key, field) in extra.as_object().expect("object").clone() {
            value[key] = field;
        }
        serde_json::from_value(value).expect("valid admin user")
    }

    fn changed_fields(markup: &str) -> Vec<String> {
        markup
            .split("data-snapshot-field=\"")
            .skip(1)
            .filter_map(|rest| {
                let key_end = rest.find('"')?;
                let tag_end = rest.find('>')?;
                rest[..tag_end]
                    .contains("data-snapshot-changed")
                    .then(|| rest[..key_end].to_owned())
            })
            .collect()
    }

    #[test]
    fn snapshot_card_marks_profile_fields_that_changed_since_the_report() {
        let config = test_config();
        let live = LiveProfile {
            user: Some(live_user(json!({"bio": "New bio", "avatar": "avatar2"}))),
            guild: None,
        };
        let markup = profile_snapshot_section(&config, &snapshot_report(), &live).into_string();
        assert!(markup.contains(">At Report Time<"), "{markup}");
        assert!(markup.contains("data-report-profile-snapshot"), "{markup}");
        assert!(
            markup.contains("Captured Oct 4, 2026, 10:00 AM UTC"),
            "{markup}"
        );
        assert!(markup.contains("target#0042"), "{markup}");
        assert!(markup.contains("Original bio"), "{markup}");
        assert!(markup.contains("Jordan Target"), "{markup}");
        assert!(
            markup.contains(r#"src="https://reports.example.test/avatar1?sig=abc""#),
            "{markup}"
        );
        assert!(markup.contains("Not preserved in the report"), "{markup}");
        assert_eq!(
            changed_fields(&markup),
            ["user.bio", "user.avatar"],
            "{markup}"
        );
        assert!(markup.contains("Changed since the report"), "{markup}");
        assert!(markup.contains(">Community Profile<"), "{markup}");
        assert!(markup.contains("Harbor Nick"), "{markup}");
        assert!(
            markup.contains("The community profile is not compared with the live one."),
            "{markup}"
        );
        assert!(!markup.contains("could not be loaded"), "{markup}");
        assert!(
            markup.contains(r#"href="/admin/users/1700000000000000900""#),
            "{markup}"
        );

        let renamed = LiveProfile {
            user: Some(live_user(
                json!({"username": "renamed", "discriminator": 7, "banner": null}),
            )),
            guild: None,
        };
        let markup = profile_snapshot_section(&config, &snapshot_report(), &renamed).into_string();
        assert_eq!(
            changed_fields(&markup),
            ["user.username", "user.banner"],
            "{markup}"
        );

        let unchanged = LiveProfile {
            user: Some(live_user(json!({}))),
            guild: None,
        };
        let markup =
            profile_snapshot_section(&config, &snapshot_report(), &unchanged).into_string();
        assert!(changed_fields(&markup).is_empty(), "{markup}");
    }

    #[test]
    fn snapshot_card_marks_nothing_without_the_live_profile() {
        let config = test_config();
        let markup = profile_snapshot_section(&config, &snapshot_report(), &LiveProfile::default())
            .into_string();
        assert!(markup.contains("Original bio"), "{markup}");
        assert!(changed_fields(&markup).is_empty(), "{markup}");
        assert!(
            markup.contains("The live account could not be loaded, so changes are not marked."),
            "{markup}"
        );
        let other = LiveProfile {
            user: Some(live_user(
                json!({"id": "1700000000000000999", "bio": "Other"}),
            )),
            guild: None,
        };
        let markup = profile_snapshot_section(&config, &snapshot_report(), &other).into_string();
        assert!(changed_fields(&markup).is_empty(), "{markup}");
    }

    #[test]
    fn snapshot_card_shows_a_closed_account_by_id_only() {
        let closed = report(json!({
            "report_type": 1,
            "reported_user_id": "1700000000000000900",
            "reported_profile_snapshot": {
                "captured_at": "2026-10-04T10:00:00.000Z",
                "user": {
                    "id": "1700000000000000900",
                    "username": null,
                    "discriminator": null,
                    "global_name": null,
                    "bio": null,
                    "pronouns": null,
                    "avatar": null,
                    "banner": null
                },
                "member": null,
                "guild": null
            }
        }));
        let markup = profile_snapshot_section(&test_config(), &closed, &LiveProfile::default())
            .into_string();
        assert!(
            markup.contains("The account was closed when the report was filed"),
            "{markup}"
        );
        assert!(markup.contains("1700000000000000900"), "{markup}");
        assert!(!markup.contains("data-snapshot-field"), "{markup}");
    }

    fn live_guild(extra: serde_json::Value) -> GuildDetailInfo {
        let mut value = json!({
            "id": "1700000000000000800",
            "owner_id": "1700000000000000901",
            "name": "Harbor",
            "vanity_url_code": null,
            "icon": "icon1",
            "banner": null,
            "splash": null
        });
        for (key, field) in extra.as_object().expect("object").clone() {
            value[key] = field;
        }
        serde_json::from_value(value).expect("valid guild")
    }

    #[test]
    fn snapshot_card_marks_community_changes() {
        let config = test_config();
        let guild_report = report(json!({
            "report_type": 2,
            "category": "other",
            "reported_guild_id": "1700000000000000800",
            "reported_profile_snapshot": {
                "captured_at": "2026-10-04T10:00:00.000Z",
                "user": null,
                "member": null,
                "guild": {
                    "id": "1700000000000000800",
                    "name": "Harbor",
                    "vanity_url_code": null,
                    "icon": {"hash": "icon1", "url": "https://reports.example.test/icon1"},
                    "banner": null,
                    "splash": null
                }
            }
        }));
        let live = LiveProfile {
            user: None,
            guild: Some(live_guild(
                json!({"name": "Harbor Renamed", "icon": "icon2"}),
            )),
        };
        let markup = profile_snapshot_section(&config, &guild_report, &live).into_string();
        assert!(markup.contains(">Community<"), "{markup}");
        assert_eq!(
            changed_fields(&markup),
            ["guild.name", "guild.icon"],
            "{markup}"
        );
        assert!(!markup.contains(">Account<"), "{markup}");
        assert!(
            markup.contains(r#"href="/admin/guilds/1700000000000000800""#),
            "{markup}"
        );
        let same = LiveProfile {
            user: None,
            guild: Some(live_guild(json!({}))),
        };
        let markup = profile_snapshot_section(&config, &guild_report, &same).into_string();
        assert!(changed_fields(&markup).is_empty(), "{markup}");
        let markup =
            profile_snapshot_section(&config, &guild_report, &LiveProfile::default()).into_string();
        assert!(
            markup.contains("The live community could not be loaded"),
            "{markup}"
        );
    }

    #[test]
    fn reports_without_a_snapshot_have_no_card() {
        let config = test_config();
        for old in [
            report(json!({})),
            report(json!({"reported_profile_snapshot": null})),
            report(
                json!({"reported_profile_snapshot": {"captured_at": "2026-10-04T10:00:00.000Z", "user": null, "member": null, "guild": null}}),
            ),
        ] {
            let markup =
                profile_snapshot_section(&config, &old, &LiveProfile::default()).into_string();
            assert!(markup.is_empty(), "{markup}");
        }
        let page = render_page(&test_auth(), &snapshot_report());
        assert!(page.contains("At Report Time"), "{page}");
        let fragment = report_detail_fragment(&config, &snapshot_report()).into_string();
        assert!(!fragment.contains("At Report Time"), "{fragment}");
    }

    #[test]
    fn legal_hold_card_shows_the_hold_and_offers_the_form_with_report_resolve() {
        let config = test_config();
        let resolver = auth_with_acls(&[acl::REPORT_VIEW, acl::REPORT_RESOLVE]);
        let viewer = auth_with_acls(&[acl::REPORT_VIEW]);

        let open = report(json!({}));
        let markup = legal_hold_card(&config, &resolver, &open, "csrf").into_string();
        assert!(markup.contains(">Legal Hold<"), "{markup}");
        assert!(
            markup.contains(r#"data-report-legal-hold="none""#),
            "{markup}"
        );
        assert!(
            markup.contains("deleted 365 days after it was filed"),
            "{markup}"
        );
        assert!(
            markup.contains(r#"action="/admin/reports/1800000000000000001/legal-hold""#),
            "{markup}"
        );
        assert!(
            markup.contains(r#"type="date" id="legal_hold_until" name="legal_hold_until""#),
            "{markup}"
        );
        assert!(markup.contains(r#"name="legal_hold_reason""#), "{markup}");
        assert!(markup.contains(r#"maxlength="512""#), "{markup}");
        assert!(markup.contains(">Place Hold<"), "{markup}");
        assert!(!markup.contains("Clear Hold"), "{markup}");

        let held = report(json!({
            "legal_hold_until": "2099-01-31T23:59:59.999Z",
            "legal_hold_reason": "Court order 42"
        }));
        let markup = legal_hold_card(&config, &resolver, &held, "csrf").into_string();
        assert!(
            markup.contains(r#"data-report-legal-hold="active""#),
            "{markup}"
        );
        assert!(markup.contains("Held until: "), "{markup}");
        assert!(markup.contains("Jan 31, 2099, 11:59 PM UTC"), "{markup}");
        assert!(markup.contains("Court order 42"), "{markup}");
        assert!(markup.contains(">Update Hold<"), "{markup}");
        assert!(markup.contains(">Clear Hold<"), "{markup}");
        assert!(markup.contains(r#"name="clear" value="1""#), "{markup}");
        assert_eq!(
            markup
                .matches(r##"data-admin-refresh-on-success="#report-detail""##)
                .count(),
            2,
            "{markup}"
        );

        let markup = legal_hold_card(&config, &viewer, &held, "csrf").into_string();
        assert!(markup.contains("Court order 42"), "{markup}");
        assert!(!markup.contains("<form"), "{markup}");

        let page = render_page(&resolver, &held);
        assert!(
            page.contains(r#"data-report-legal-hold="active""#),
            "{page}"
        );
        assert!(page.contains(r#"id="report-detail""#), "{page}");
        let resolved = report(json!({"status": 1}));
        let page = render_page(&resolver, &resolved);
        assert!(page.contains(">Place Hold<"), "{page}");

        let lapsed = report(json!({
            "legal_hold_until": "2026-01-31T23:59:59.999Z",
            "legal_hold_reason": "Court order 41"
        }));
        let markup = legal_hold_card(&config, &resolver, &lapsed, "csrf").into_string();
        assert!(
            markup.contains(r#"data-report-legal-hold="ended""#),
            "{markup}"
        );
        assert!(markup.contains("Hold ended: "), "{markup}");
        assert!(!markup.contains("Held until"), "{markup}");
        assert!(markup.contains("Jan 31, 2026, 11:59 PM UTC"), "{markup}");
        assert!(markup.contains("is no longer held"), "{markup}");
        assert!(markup.contains(">Place Hold<"), "{markup}");
        assert!(!markup.contains(">Update Hold<"), "{markup}");
        assert!(markup.contains(">Clear Hold<"), "{markup}");
    }
}
