// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::{
        generated::types::{
            AdminOperatorPassOfferResponse, AdminOperatorPassReleaseFileResponse,
            AdminOperatorPassReleasePreviewResponse, AdminOperatorPassRemovalEndReasonSchema,
            AdminOperatorPassRemovalHistoryEntryResponse, AdminOperatorPassRemovalResponse,
            AdminOperatorPassRemovalStateSchema, AdminOperatorPassRowResponse,
            AdminOperatorPassStoredReleaseResponse, AdminOperatorPassUnitEventResponse,
            AdminOperatorPassUnitResponse, AdminOperatorPassUserResponse,
            AdminOperatorPassWallAsOfResponse, AdminOperatorPassWallEntryResponse,
            AdminOperatorPassWallStateResponse, OperatorPassPurchaseResponse,
            OperatorPassPurchaseStatusSchema, OperatorPassWallDeclineReasonSchema,
            OperatorPassWallDisplayStateSchema, OperatorPassWallReviewStateSchema,
        },
        operator_pass::{WallTimeline, WallTimelineEvent},
        types::FlashMessage,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            empty_state::empty_state_full,
            error_display::error_alert,
            form::{
                FORM_INPUT_CLASS, FORM_SELECT_CLASS, FORM_TEXTAREA_CLASS, csrf_input,
                danger_button, secondary_button, select_chevron, submit_button,
            },
            page_container::{card_with_header, detail_row, page_header},
            resource_link::{ResourceType, resource_link},
            table::{
                data_table, table, table_body, table_cell, table_container, table_head, table_row,
            },
        },
        layout::admin_layout,
        pages::{
            operator_pass_visionary::visionary_activation_card, payment_disputes::format_amount,
        },
    },
    utils::timestamps::{format_admin_date, format_admin_timestamp},
};
use base64::Engine;
use chrono::{DateTime, Utc};
use maud::{Markup, html};
use std::collections::BTreeSet;

fn wall_table(headers: &[&str], rows: Markup) -> Markup {
    table_container(table(html! {
        (table_head(html! {
            tr {
                @for header in headers {
                    th class="whitespace-nowrap px-3 py-3 text-left font-medium text-neutral-600 text-sm sm:px-6" { (header) }
                }
            }
        }))
        (table_body(rows))
    }))
}

pub const WALL_PATH: &str = "/operator-pass-wall";
pub const RELEASES_PATH: &str = "/operator-pass-releases";

pub fn format_time(at: Option<&DateTime<Utc>>) -> String {
    at.and_then(|at| serde_json::to_value(at).ok())
        .and_then(|value| value.as_str().map(format_admin_timestamp))
        .unwrap_or_else(|| "Not set".to_owned())
}

fn format_date(at: &DateTime<Utc>) -> Markup {
    let iso = serde_json::to_value(at)
        .ok()
        .and_then(|value| value.as_str().map(str::to_owned))
        .unwrap_or_default();
    html! {
        time datetime=(iso) title=(format_admin_timestamp(&iso)) { (format_admin_date(&iso)) }
    }
}

fn format_optional_date(at: Option<&DateTime<Utc>>, empty: &str) -> Markup {
    match at {
        Some(at) => format_date(at),
        None => html! { (empty) },
    }
}

fn event_label(kind: &str) -> &str {
    match kind {
        "delivered" => "Pass delivered",
        "listed" => "Listing went on the wall",
        "changed" => "Name changed",
        "opted_out" => "Opted out",
        "taken_down" => "Pulled from the wall",
        "shown_again" => "Put back on the wall",
        "approved" => "Staff approved the name and avatar",
        "declined" => "Staff declined, statement of reasons sent",
        "fallback" => "Wall fell back to Operator",
        "display_adjusted" => "Staff adjusted the display, no statement",
        "legal_hold_set" => "Legal hold placed",
        "legal_hold_cleared" => "Legal hold released",
        "release_published" => "Published in a release list",
        "erased" => "Erased from a release list",
        "count_changed" => "Number of passes changed",
        other => other,
    }
}

impl OperatorPassWallDeclineReasonSchema {
    fn label(self) -> &'static str {
        match self {
            Self::Guidelines => "Breaks the community guidelines",
            Self::Impersonation => "Pretends to be someone else",
            Self::Rights => "Uses a name or image without the rights to it",
            Self::AgeSuitability => "Not suitable for younger users",
        }
    }
}

fn operators_file_data_url(file: &str) -> String {
    format!(
        "data:text/plain;charset=utf-8;base64,{}",
        base64::engine::general_purpose::STANDARD.encode(file.as_bytes())
    )
}

fn text_field(name: &str, label: &str, max_length: u16) -> Markup {
    html! {
        label class="flex flex-col gap-1" {
            span class=(LABEL_CLASS) { (label) }
            input type="text" name=(name) required maxlength=(max_length) class=(FORM_INPUT_CLASS);
        }
    }
}

pub const TOOLS_PATH: &str = "/operator-pass-wall/tools";

pub const BUSINESS_NAME_MAX_CHARS: usize = 32;

pub const BUSINESS_LOGO_MAX_BYTES: usize = 2 * 1024 * 1024;

pub const BUSINESS_CHECKS: [(&str, &str); 3] = [
    (
        "check_email",
        "The request came from the email address of this account",
    ),
    (
        "check_name",
        "The company name matches the invoice or a trading name the business uses",
    ),
    (
        "check_rules",
        "The business is in no refused category, and the logo is legible at 48 pixels with no URL, QR code, slogan, price or photo of a person",
    ),
];

const REFRESH_ID: &str = "operator-pass-wall";

const REFRESH_TARGET: &str = "#operator-pass-wall";

const LABEL_CLASS: &str = "font-semibold text-neutral-600 text-sm";

const REVIEW_TARGET_HOURS: i64 = 72;

const TIER_SCALES: [f64; 4] = [1.0, 1.25, 1.5, 2.0];

const BASE_AVATAR_PX: f64 = 48.0;

fn proxied_avatar(base: &str, api_route: Option<&str>) -> Option<String> {
    let route = api_route?;
    let rest = route.strip_prefix("/admin/users/")?;
    let (user_id, tail) = rest.split_once('/')?;
    let sha = tail.strip_prefix("operator-pass/wall/avatars/")?;
    let valid_user = !user_id.is_empty() && user_id.bytes().all(|byte| byte.is_ascii_digit());
    let valid_sha = sha.len() == 64 && sha.bytes().all(|byte| byte.is_ascii_hexdigit());
    (valid_user && valid_sha).then(|| format!("{base}{WALL_PATH}/users/{user_id}/avatars/{sha}"))
}

fn review_state_label(
    entry: &AdminOperatorPassWallEntryResponse,
    display: Option<OperatorPassWallDisplayStateSchema>,
) -> String {
    if entry.wall_opt_out || entry.listed_at.is_none() {
        return "Not listed".to_owned();
    }
    let shows = match display {
        Some(OperatorPassWallDisplayStateSchema::Hidden) => "Hidden from the wall for now",
        Some(OperatorPassWallDisplayStateSchema::Removed) => "Off the wall under section 8",
        Some(OperatorPassWallDisplayStateSchema::Fallback) => {
            "The wall shows Operator with the default avatar"
        }
        Some(OperatorPassWallDisplayStateSchema::Live) | None => {
            "The wall shows the approved version"
        }
    };
    let review = match entry.review_state {
        Some(OperatorPassWallReviewStateSchema::Pending) => "a change waits for review",
        Some(OperatorPassWallReviewStateSchema::Declined) => "the latest change was declined",
        Some(OperatorPassWallReviewStateSchema::Approved) => "approved",
        None => "nothing submitted yet",
    };
    format!("{shows}, {review}")
}

fn waited_label(entry: &AdminOperatorPassWallEntryResponse, now: DateTime<Utc>) -> String {
    let Some(since) = entry.pending_since else {
        return String::new();
    };
    let hours = (now - since).num_hours().max(0);
    if hours >= REVIEW_TARGET_HOURS {
        format!("Waiting {hours} hours, past the {REVIEW_TARGET_HOURS} hour target")
    } else {
        format!(
            "Waiting {hours} hours, {} left of the {REVIEW_TARGET_HOURS} hour target",
            REVIEW_TARGET_HOURS - hours
        )
    }
}

fn avatar_img(src: Option<&str>, size_class: &str) -> Markup {
    html! {
        @match src {
            Some(src) => { img src=(src) alt="" class={"shrink-0 rounded-full object-cover " (size_class)} loading="lazy"; }
            None => { span class={"inline-block shrink-0 rounded-full bg-neutral-300 " (size_class)} {} }
        }
    }
}

fn diff_cell(base: &str, entry: &AdminOperatorPassWallEntryResponse) -> Markup {
    let live_avatar = proxied_avatar(base, entry.live_avatar_private_url.as_deref())
        .or_else(|| entry.shown_avatar_url.clone());
    let pending_avatar = proxied_avatar(base, entry.pending_avatar_url.as_deref());
    html! {
        div class="grid min-w-44 gap-3" {
            div class="flex min-w-0 flex-col gap-1" {
                span class={(LABEL_CLASS) " whitespace-nowrap"} { "On the wall now" }
                div class="flex min-w-0 items-center gap-2" {
                    (avatar_img(live_avatar.as_deref(), "h-12 w-12"))
                    span class="min-w-0 [overflow-wrap:anywhere]" { (entry.shown_name.as_deref().unwrap_or("Not shown")) }
                }
            }
            div class="flex min-w-0 flex-col gap-1" {
                span class={(LABEL_CLASS) " whitespace-nowrap"} { "Waiting for review" }
                div class="flex min-w-0 items-center gap-2" {
                    (avatar_img(pending_avatar.as_deref(), "h-12 w-12"))
                    span class="min-w-0 font-semibold [overflow-wrap:anywhere]" { (entry.pending_name.as_deref().unwrap_or("No name")) }
                }
            }
        }
    }
}

fn reason_select() -> Markup {
    html! {
        label class="flex flex-col gap-1" {
            span class=(LABEL_CLASS) { "Reason" }
            div class="relative" {
                select name="reason" required class=(FORM_SELECT_CLASS) {
                    @for reason in [
                        OperatorPassWallDeclineReasonSchema::Guidelines,
                        OperatorPassWallDeclineReasonSchema::Impersonation,
                        OperatorPassWallDeclineReasonSchema::Rights,
                        OperatorPassWallDeclineReasonSchema::AgeSuitability,
                    ] {
                        option value=(reason) { (reason.label()) }
                    }
                }
                (select_chevron())
            }
        }
    }
}

fn facts_field(required: bool) -> Markup {
    html! {
        label class="flex flex-col gap-1" {
            span class=(LABEL_CLASS) { "Facts (sent to them)" }
            @if required {
                textarea name="note" required maxlength="300" rows="3" class=(FORM_TEXTAREA_CLASS) {}
            } @else {
                textarea name="note" maxlength="300" rows="3" class=(FORM_TEXTAREA_CLASS) {}
            }
        }
    }
}

fn collapsed_form(label: &str, body: Markup) -> Markup {
    html! {
        details class="rounded-lg border border-red-200" {
            summary class="cursor-pointer rounded-lg px-3 py-2 font-medium text-red-700 text-sm" { (label) }
            div class="border-red-200 border-t p-3" { (body) }
        }
    }
}

#[derive(Clone, Copy)]
struct PostFormOptions<'a> {
    csrf_token: &'a str,
    back: &'a str,
    class: Option<&'a str>,
    onsubmit: Option<&'a str>,
}

fn post_form(action: &str, options: PostFormOptions<'_>, body: Markup) -> Markup {
    html! {
        form method="post" data-admin-refresh-on-success=(REFRESH_TARGET) action=(action)
            class=[options.class] onsubmit=[options.onsubmit] {
            (csrf_input(options.csrf_token))
            input type="hidden" name="back" value=(options.back);
            (body)
        }
    }
}

fn review_actions(
    base: &str,
    csrf_token: &str,
    entry: &AdminOperatorPassWallEntryResponse,
    back: &str,
) -> Markup {
    let Some(revision_id) = entry.pending_revision_id.as_deref() else {
        return html! {};
    };
    let action = |name: &str| format!("{base}{WALL_PATH}/users/{}/{name}", *entry.user_id);
    let form_options = PostFormOptions {
        csrf_token,
        back,
        class: None,
        onsubmit: None,
    };
    html! {
        div class="flex min-w-80 flex-col gap-3" {
            (post_form(&action("approve"), form_options, html! {
                input type="hidden" name="revision_id" value=(revision_id);
                (submit_button("Approve"))
            }))
            (collapsed_form("Decline", post_form(&action("decline"), PostFormOptions {
                class: Some("flex flex-col gap-2"),
                onsubmit: Some("return confirm('Decline and email them a statement of reasons?')"),
                ..form_options
            }, html! {
                input type="hidden" name="revision_id" value=(revision_id);
                p class="text-sm text-neutral-600" { "Sends a statement of reasons." }
                (reason_select())
                (facts_field(true))
                (danger_button("Decline and send the statement"))
            })))
        }
    }
}

fn user_cell(base: &str, entry: &AdminOperatorPassWallEntryResponse) -> Markup {
    let user_id: &str = &entry.user_id;
    html! {
        div class="flex min-w-36 items-center gap-2" {
            @if let Some(avatar) = &entry.avatar_url {
                img src=(avatar) alt="" class="h-8 w-8 rounded-full" loading="lazy";
            }
            (resource_link(base, ResourceType::User, user_id, html! {
                span class="[overflow-wrap:anywhere]" { (entry.username.as_deref().unwrap_or(user_id)) }
            }))
        }
    }
}

fn queue_table(
    base: &str,
    csrf_token: &str,
    entries: &[AdminOperatorPassWallEntryResponse],
    can_moderate: bool,
    now: DateTime<Utc>,
) -> Markup {
    if entries.is_empty() {
        return empty_state_full(
            None,
            "Nothing to review.",
            Some(
                "New names and avatars wait here until someone approves or declines them. The wall keeps the approved version or the fallback until then.",
            ),
        );
    }
    let headers: &[&str] = if can_moderate {
        &["Select", "Account", "Change", "Waiting", "Actions"]
    } else {
        &["Account", "Change", "Waiting"]
    };
    let table = wall_table(
        headers,
        html! {
            @for entry in entries {
                (table_row(html! {
                    @if can_moderate {
                        (table_cell(false, html! {
                            @if let Some(revision_id) = entry.pending_revision_id.as_deref() {
                                input type="checkbox" name="selected" form="wall-bulk"
                                    value={(&*entry.user_id) ":" (revision_id)}
                                    aria-label={"Select the change of " (entry.username.as_deref().unwrap_or(&entry.user_id))};
                            }
                        }))
                    }
                    (table_cell(false, html! {
                        (user_cell(base, entry))
                        a href={(base) (WALL_PATH) "/users/" (&*entry.user_id)} class="text-sm underline" { "Timeline" }
                    }))
                    (table_cell(false, diff_cell(base, entry)))
                    (table_cell(true, html! {
                        span class="block min-w-28" { (waited_label(entry, now)) }
                        @if let Some(source) = entry.pending_source.as_deref() {
                            span class="mt-1 block text-xs" { "From " (source) }
                        }
                    }))
                    @if can_moderate {
                        (table_cell(false, review_actions(base, csrf_token, entry, "queue")))
                    }
                }))
            }
        },
    );
    html! {
        (table)
        @if can_moderate {
            form id="wall-bulk" method="post" data-admin-refresh-on-success=(REFRESH_TARGET) action={(base) (WALL_PATH) "/bulk"}
                class="mt-4 flex flex-col gap-2 rounded-lg border border-neutral-200 p-3"
                onsubmit="return confirm('Apply this to every selected change?')" {
                (csrf_input(csrf_token))
                h3 class="font-semibold text-sm" { "Selected changes" }
                div class="flex flex-wrap gap-2" {
                    button type="submit" name="action" value="approve" class="rounded-lg bg-neutral-900 px-3 py-2 text-sm text-white" { "Approve selected" }
                }
                (collapsed_form("Decline selected", html! {
                    div class="flex flex-col gap-2" {
                        p class="text-sm text-neutral-600" { "Declining needs a reason and the facts, and emails each person a statement of reasons." }
                        (reason_select())
                        (facts_field(false))
                        div {
                            button type="submit" name="action" value="decline" class="rounded-lg bg-red-600 px-3 py-2 text-sm text-white" { "Decline selected" }
                        }
                    }
                }))
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn operator_pass_wall_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    flash: Option<&FlashMessage>,
    entries: Option<&[AdminOperatorPassWallEntryResponse]>,
    can_moderate: bool,
    now: DateTime<Utc>,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header("Operators' Wall", Some("Review within 72h.")))
        div class="flex flex-col gap-6" {
            p class="flex flex-wrap gap-4 text-sm" {
                a href={(base) (TOOLS_PATH)} class="underline" { "Wall tools: publishing, freeze and time travel" }
            }
            (card_with_header("Look up an account", html! {
                form method="get" action={(base) (WALL_PATH) "/users"} class="flex flex-wrap items-end gap-2" {
                    label class="flex flex-col gap-1" {
                        span class=(LABEL_CLASS) { "User ID" }
                        input type="text" name="user_id" required inputmode="numeric" class=(FORM_INPUT_CLASS);
                    }
                    (secondary_button("Open"))
                }
            }))
            @match entries {
                Some(entries) => {
                    (card_with_header("Changes to review", queue_table(base, csrf_token, entries, can_moderate, now)))
                }
                None => {
                    (error_alert("Could not load the review queue. The API request failed or this account lacks the user:lookup ACL."))
                }
            }
        }
    };
    admin_layout(
        config,
        auth,
        "Operators' Wall",
        "operator-pass-wall",
        flash,
        html! { div id=(REFRESH_ID) { (content) } },
    )
}

fn decision_label(kind: &str) -> &str {
    match kind {
        "approve" => "Approved",
        "decline" => "Declined",
        "revert" => "Put back an earlier version",
        "force_fallback" => "Forced the fallback",
        "hide" => "Hidden",
        "unhide" => "Put back on the wall",
        other => other,
    }
}

fn revision_status_label(status: &str) -> &str {
    match status {
        "pending" => "Waiting for review",
        "approved" => "Approved",
        "declined" => "Declined",
        "superseded" => "Replaced by a newer change",
        "reverted" => "Pulled",
        other => other,
    }
}

fn timeline_section(
    base: &str,
    csrf_token: &str,
    user_id: &str,
    timeline: &WallTimeline,
    can_moderate: bool,
) -> Markup {
    let action = |name: &str| format!("{base}{WALL_PATH}/users/{user_id}/{name}");
    let live_id = timeline
        .entry
        .as_ref()
        .and_then(|entry| entry.live_revision_id.as_ref().map(|id| id.to_string()));
    html! {
        div class="flex flex-col gap-6" {
            (card_with_header("Revisions", html! {
                @if timeline.revisions.is_empty() {
                    p class="text-sm text-neutral-600" { "No name or avatar was ever submitted." }
                } @else {
                    (wall_table(&["Submitted", "Avatar", "Name", "Source", "Status", "Actions"], html! {
                        @for revision in &timeline.revisions {
                            (table_row(html! {
                                (table_cell(true, format_date(&revision.created_at)))
                                (table_cell(false, avatar_img(proxied_avatar(base, revision.avatar_url.as_deref()).as_deref(), "h-10 w-10")))
                                (table_cell(false, html! {
                                    @if revision.name.is_empty() { span class="text-neutral-500" { "Deleted" } } @else { (revision.name) }
                                }))
                                (table_cell(true, html! { (revision.source) @if revision.business { ", business" } }))
                                (table_cell(true, html! {
                                    (revision_status_label(&revision.status))
                                    @if live_id.as_deref() == Some(&*revision.id) { ", on the wall now" }
                                }))
                                (table_cell(false, html! {
                                    @if can_moderate && !revision.business && live_id.as_deref() != Some(&*revision.id)
                                        && (revision.status == "approved" || revision.status == "reverted") {
                                        (post_form(&action("revert"), PostFormOptions {
                                            csrf_token,
                                            back: "user",
                                            class: None,
                                            onsubmit: Some("return confirm('Put this revision back on the wall?')"),
                                        }, html! {
                                            input type="hidden" name="revision_id" value=(&*revision.id);
                                            (secondary_button("Put back"))
                                        }))
                                    }
                                }))
                            }))
                        }
                    }))
                }
            }))
            (card_with_header("Decisions", html! {
                @if timeline.decisions.is_empty() {
                    p class="text-sm text-neutral-600" { "No decisions yet." }
                } @else {
                    (wall_table(&["When", "Decision", "Reason", "Facts", "Statement", "By"], html! {
                        @for decision in &timeline.decisions {
                            (table_row(html! {
                                (table_cell(true, format_date(&decision.decided_at)))
                                (table_cell(false, html! { (decision_label(&decision.kind)) }))
                                (table_cell(true, html! {
                                    @if let Some(reason) = &decision.reason { (reason.label()) }
                                }))
                                (table_cell(false, html! { (decision.note.as_deref().unwrap_or("")) }))
                                (table_cell(true, html! { (decision.statement_id.as_deref().unwrap_or("")) }))
                                (table_cell(true, html! {
                                    @match &decision.decided_by {
                                        Some(by) => { (resource_link(base, ResourceType::User, by, html! { (&**by) })) }
                                        None => { "System" }
                                    }
                                }))
                            }))
                        }
                    }))
                }
            }))
            (card_with_header("Wall history", timeline_history(&timeline.events)))
        }
    }
}

fn timeline_history(events: &[WallTimelineEvent]) -> Markup {
    if events.is_empty() {
        return empty_state_full(None, "No history yet.", None);
    }
    wall_table(
        &["When", "What", "Name", "Details", "By"],
        html! {
            @for event in events {
                (table_row(html! {
                    (table_cell(true, format_date(&event.occurred_at)))
                    (table_cell(false, html! { (event_label(&event.kind)) }))
                    (table_cell(false, html! { (event.name.as_deref().unwrap_or("")) }))
                    (table_cell(true, html! {
                        @if let Some(reason) = &event.reason { div { "Reason: " (reason) } }
                        @if let Some(cause) = &event.cause { div { "Cause: " (cause) } }
                        @if let Some(purchase) = &event.purchase_id { div { "Purchase: " (purchase) } }
                        @if let Some(year) = event.release_year { div { "List: " (year) } }
                        @if let Some(tag) = &event.release_tag { div { "Tag: " (tag) } }
                        @if let Some(count) = event.pass_count { div { "Passes: " (count) } }
                    }))
                    (table_cell(true, html! { (event.actor_id.as_deref().unwrap_or("System")) }))
                }))
            }
        },
    )
}

fn can_force_fallback(entry: &AdminOperatorPassWallEntryResponse) -> bool {
    if entry.hidden {
        return false;
    }
    entry.display_state != Some(OperatorPassWallDisplayStateSchema::Fallback)
}

fn listing_actions(
    base: &str,
    csrf_token: &str,
    entry: &AdminOperatorPassWallEntryResponse,
) -> Markup {
    let action = |name: &str| format!("{base}{WALL_PATH}/users/{}/{name}", *entry.user_id);
    let form_options = PostFormOptions {
        csrf_token,
        back: "user",
        class: None,
        onsubmit: None,
    };
    html! {
        div class="flex flex-col gap-4" {
            (review_actions(base, csrf_token, entry, "user"))
            @if can_force_fallback(entry) {
                (collapsed_form("Force the fallback", post_form(&action("force-fallback"), PostFormOptions {
                    class: Some("flex flex-col gap-2"),
                    onsubmit: Some("return confirm('Take the approved name and avatar off the wall and email a statement of reasons?')"),
                    ..form_options
                }, html! {
                    p class="text-sm text-neutral-600" { "Sends a statement of reasons. The listing keeps its size, place and badge, and shows Operator with the default avatar." }
                    (reason_select())
                    (facts_field(true))
                        (danger_button("Force the fallback"))
                })))
            }
            @if entry.hidden {
                (post_form(&action("show"), form_options, secondary_button("Put back on the wall")))
            } @else {
                (collapsed_form("Hide from the wall", post_form(&action("hide"), PostFormOptions {
                    class: Some("flex flex-col gap-2"),
                    onsubmit: Some("return confirm('Hide this listing from the wall while you look into it and email a statement of reasons?')"),
                    ..form_options
                }, html! {
                    p class="text-sm text-neutral-600" { "Hides the listing under section 3.1 of the Operator Pass terms while a report or possible breach is looked into, and sends a statement of reasons." }
                    (reason_select())
                    (facts_field(true))
                    (danger_button("Hide from the wall"))
                })))
            }
            (post_form(&action(if entry.legal_hold { "legal-release" } else { "legal-hold" }), form_options, html! {
                p class="mb-2 text-sm text-neutral-600" {
                    @if entry.legal_hold {
                        "The name and avatar copies are kept past the retention periods for an open appeal or a legal hold."
                    } @else {
                        "Copies follow the retention periods. Declined ones go after a year, all of them 60 days after leaving the wall."
                    }
                }
                (secondary_button(if entry.legal_hold { "Release the legal hold" } else { "Place a legal hold" }))
            }))
        }
    }
}

impl AdminOperatorPassRemovalStateSchema {
    fn label(self) -> &'static str {
        match self {
            Self::Notice => "Notice sent, 7 days to reply",
            Self::Removed => "Bonus removed",
        }
    }
}

fn removal_fields() -> Markup {
    html! {
        label class="flex flex-col gap-1" {
            span class=(LABEL_CLASS) { "Rule broken" }
            input type="text" name="rule" required maxlength="500" class=(FORM_INPUT_CLASS);
        }
        label class="flex flex-col gap-1" {
            span class=(LABEL_CLASS) { "Facts (sent to them)" }
            textarea name="facts" required maxlength="4000" rows="3" class=(FORM_TEXTAREA_CLASS) {}
        }
    }
}

impl AdminOperatorPassRemovalEndReasonSchema {
    fn label(self) -> &'static str {
        match self {
            Self::Expired => "ran out",
            Self::Restored => "restored by staff",
            Self::Replaced => "replaced by a later removal",
        }
    }
}

fn removal_history_table(history: &[AdminOperatorPassRemovalHistoryEntryResponse]) -> Markup {
    html! {
        div class="mt-4" {
            p class="mb-2 font-semibold text-sm" { "Every removal, oldest first" }
            @if history.is_empty() {
                p class="text-sm text-neutral-600" { "No removals so far." }
            } @else {
                (data_table(&["Removed", "Length", "Ended", "Rule", "Serious", "Notice sent"], html! {
                    @for entry in history {
                        (table_row(html! {
                            (table_cell(true, format_date(&entry.removed_at)))
                            (table_cell(false, html! {
                                @match &entry.removed_until {
                                    Some(until) => { "Limited, until " (format_date(until)) }
                                    None => { "Permanent" }
                                }
                            }))
                            (table_cell(false, html! {
                                @match &entry.ended_at {
                                    Some(ended) => {
                                        (format_date(ended))
                                        @if let Some(reason) = &entry.end_reason { ", " (reason.label()) }
                                    }
                                    None => { "Still in place" }
                                }
                            }))
                            (table_cell(false, html! { (entry.rule) }))
                            (table_cell(false, html! { (if entry.serious { "Yes" } else { "No" }) }))
                            (table_cell(true, format_optional_date(entry.notice_sent_at.as_ref(), "Not sent")))
                        }))
                    }
                }))
            }
        }
    }
}

fn removal_section(
    base: &str,
    csrf_token: &str,
    user_id: &str,
    removal: Option<&AdminOperatorPassRemovalResponse>,
    history: &[AdminOperatorPassRemovalHistoryEntryResponse],
    can_moderate: bool,
) -> Markup {
    let action = |name: &str| format!("{base}{WALL_PATH}/users/{user_id}/{name}");
    let removed = removal
        .is_some_and(|removal| removal.state == AdminOperatorPassRemovalStateSchema::Removed);
    let form_options = PostFormOptions {
        csrf_token,
        back: "user",
        class: None,
        onsubmit: None,
    };
    card_with_header(
        "Removal under section 8",
        html! {
            @match removal {
                Some(removal) => {
                    (detail_row("State", html! { (removal.state.label()) }))
                    (detail_row("Rule", html! { (removal.rule) }))
                    (detail_row("Facts (sent to them)", html! { (removal.facts) }))
                    @if removal.notice_sent_at.is_some() {
                        (detail_row("Notice sent", format_optional_date(removal.notice_sent_at.as_ref(), "Not sent")))
                    }
                    @if removal.removed_at.is_some() {
                        (detail_row("Removed", format_optional_date(removal.removed_at.as_ref(), "Not removed")))
                        (detail_row("Ends", html! {
                            @match &removal.removed_until {
                                Some(until) => { (format_date(until)) }
                                None => { "Permanent" }
                            }
                        }))
                    }
                }
                None => { p class="text-sm text-neutral-600" { "No notice or removal on this account." } }
            }
            (removal_history_table(history))
            @if can_moderate {
                div class="mt-4 flex flex-col gap-4" {
                    @if removal.is_some() {
                        (post_form(&action("restore"), PostFormOptions {
                            onsubmit: Some("return confirm('End the notice or removal and restore the bonus with the original badge date?')"),
                            ..form_options
                        }, secondary_button("Restore the bonus")))
                    }
                    @if !removed {
                        (collapsed_form("Remove the bonus", post_form(&action("remove"), PostFormOptions {
                            class: Some("flex flex-col gap-2"),
                            onsubmit: Some("return confirm('Remove the bonus and email a statement of reasons?')"),
                            ..form_options
                        }, html! {
                            p class="text-sm text-neutral-600" {
                                "Sends a statement of reasons. Use this for a breach under section 8 of the Operator Pass terms. It takes the listing off the wall and the badge off the profile. Leave the days empty for a permanent removal, which needs a serious breach or a notice sent at least 7 days earlier."
                            }
                            (removal_fields())
                            label class="flex flex-col gap-1" {
                                span class=(LABEL_CLASS) { "Days, empty for permanent" }
                                input type="number" name="duration_days" min="1" max="3650" class=(FORM_INPUT_CLASS);
                            }
                            label class="flex items-center gap-2 text-sm" {
                                input type="checkbox" name="serious" value="yes";
                                "Serious breach, act without the 7 day notice"
                            }
                            (danger_button("Remove the bonus"))
                        })))
                    }
                    @if removal.is_none() {
                        (collapsed_form("Send the 7 day notice", post_form(&action("removal-notice"), PostFormOptions {
                            class: Some("flex flex-col gap-2"),
                            onsubmit: Some("return confirm('Email them the 7 day notice before a permanent removal?')"),
                            ..form_options
                        }, html! {
                            p class="text-sm text-neutral-600" { "Comes before a permanent removal, unless the breach is serious." }
                            (removal_fields())
                            (secondary_button("Send the notice"))
                        })))
                    }
                }
            }
        },
    )
}

fn unit_event_label(kind: &str) -> &str {
    match kind {
        "purchased" => "Purchased",
        "country_rule" => "Country withdrawal rule applied",
        "delivered" => "Delivered",
        "offer_created" => "Offer made",
        "offer_accepted" => "Offer accepted",
        "offer_declined" => "Offer declined",
        "offer_withdrawn" => "Offer taken back",
        "offer_cancelled" => "Offer canceled",
        "transferred" => "Passed on",
        "paused" => "Paused",
        "resumed" => "Resumed",
        "withdrawn" => "Withdrawn",
        "revoked" => "Revoked",
        other => other,
    }
}

fn unit_event_details(base: &str, event: &AdminOperatorPassUnitEventResponse) -> Markup {
    html! {
        @if let Some(payment) = &event.payment_id { div { "Payment: " (payment) } }
        @if let Some(purchase) = &event.purchase_id { div { "Purchase: " (purchase) } }
        @if let Some(sender) = &event.sender_id {
            div { "From: " (resource_link(base, ResourceType::User, sender, html! { (&**sender) })) }
        }
        @if let Some(recipient) = &event.recipient_id {
            div { "To: " (resource_link(base, ResourceType::User, recipient, html! { (&**recipient) })) }
        }
        @if let Some(offer) = &event.offer_id { div { "Offer: " (offer) } }
        @if let Some(country) = &event.country {
            div {
                "Country: " (country)
                @if let Some(days) = event.days { ", " (days) " days" }
                @if let Some(version) = &event.rule_version { ", rule " (version) }
            }
        }
        @if let Some(reason) = &event.reason { div { "Reason: " (reason) } }
    }
}

fn withdrawal_rule(unit: &AdminOperatorPassUnitResponse) -> String {
    match (&unit.withdrawal_country, unit.withdrawal_days) {
        (Some(country), Some(days)) => format!("{country}, {days} days"),
        (Some(country), None) => country.clone(),
        _ => "None".to_owned(),
    }
}

fn unit_status_label(status: &str) -> &str {
    match status {
        "held" => "On the account",
        "withdrawn" => "Withdrawn and refunded",
        "revoked" => "Ended",
        other => other,
    }
}

fn unit_source_label(source: &str) -> &str {
    match source {
        "purchase" => "Bought",
        "transfer" => "Received",
        "admin" => "Added by staff",
        other => other,
    }
}

fn offer_status_label(status: &str) -> &str {
    match status {
        "pending" => "Waiting for an answer",
        "accepted" => "Accepted",
        "completed" => "Moved",
        "declined" => "Declined",
        "withdrawn" => "Taken back",
        "cancelled" => "Canceled",
        other => other,
    }
}

fn units_table(base: &str, units: &[AdminOperatorPassUnitResponse]) -> Markup {
    if units.is_empty() {
        return html! { p class="text-sm text-neutral-600" { "This account has no passes and paid for none." } };
    }
    html! {
        div class="flex flex-col gap-4" {
            @for unit in units {
                div class="rounded-lg border border-neutral-200 p-3" {
                    (detail_row("Pass", html! { (unit.id) }))
                    (detail_row("Source", html! { (unit_source_label(&unit.source)) }))
                    (detail_row("Status", html! { (unit_status_label(&unit.status)) }))
                    (detail_row("Owner", resource_link(base, ResourceType::User, &unit.holder_id, html! { (&*unit.holder_id) })))
                    @if let Some(buyer) = &unit.buyer_id {
                        (detail_row("Buyer", resource_link(base, ResourceType::User, buyer, html! { (&**buyer) })))
                    }
                    @if let Some(purchase) = &unit.purchase_id {
                        (detail_row("Purchase", html! { (purchase) }))
                    }
                    (detail_row("Delivered", format_date(&unit.delivered_at)))
                    (detail_row("First delivered", format_date(&unit.first_delivered_at)))
                    (detail_row("Can be passed on", html! { (if unit.transferable { "Yes" } else { "No" }) }))
                    (detail_row("Withdrawal rule", html! { (withdrawal_rule(unit)) }))
                    @if unit.paused_at.is_some() {
                        (detail_row("Paused", format_optional_date(unit.paused_at.as_ref(), "")))
                    }
                    @if unit.ended_at.is_some() {
                        (detail_row("Ended", html! {
                            (format_optional_date(unit.ended_at.as_ref(), ""))
                            @if let Some(reason) = &unit.end_reason { ", " (reason) }
                        }))
                    }
                    details class="mt-2 text-sm" {
                        summary class="cursor-pointer font-semibold" { "History (" (unit.events.len()) ")" }
                        (data_table(&["When", "Event", "Details"], html! {
                            @for event in &unit.events {
                                (table_row(html! {
                                    (table_cell(true, format_date(&event.occurred_at)))
                                    (table_cell(false, html! { (unit_event_label(&event.kind)) }))
                                    (table_cell(false, unit_event_details(base, event)))
                                }))
                            }
                        }))
                    }
                }
            }
        }
    }
}

pub const WITHDRAWAL_CHANNELS: &[(&str, &str)] = &[
    ("email", "An email to support"),
    ("support", "Another support channel"),
    ("form", "The withdrawal form in section 7.2"),
    ("other", "Any other clear statement"),
];

pub const REISSUE_REASONS: &[(&str, &str)] = &[
    ("name_change", "The holder changed their name (section 3)"),
    ("correction", "A mistake on our side (section 6)"),
    (
        "objection",
        "The person named on the page objected (privacy section 5), erase earlier copies",
    ),
];

impl OperatorPassPurchaseStatusSchema {
    pub fn label(self) -> &'static str {
        match self {
            Self::Pending => "Waiting for payment",
            Self::Completed => "Paid",
            Self::Withdrawn => "Withdrawn",
            Self::Refunded => "Refunded",
            Self::Disputed => "Disputed",
            Self::DisputeLost => "Dispute lost",
            Self::DuplicateRefunded => "Refunded as a duplicate",
        }
    }
}

fn withdrawal_ends(purchase: &OperatorPassPurchaseResponse) -> Markup {
    match (&purchase.withdrawal_ends_on, &purchase.withdrawal_ends_at) {
        (Some(day), _) => html! { "End of " (day) " in America/Rio_Branco" },
        (None, Some(at)) => format_date(at),
        (None, None) => html! { "No right of withdrawal" },
    }
}

fn withdrawal_form(base: &str, csrf_token: &str, user_id: &str, purchase_id: &str) -> Markup {
    collapsed_form(
        "Record a withdrawal",
        html! {
            form method="post" data-admin-refresh-on-success=(REFRESH_TARGET) action={(base) (WALL_PATH) "/users/" (user_id) "/withdraw"}
                class="flex flex-col gap-2"
                onsubmit="return confirm('Record this withdrawal? Fluxer refunds the passes, ends them and emails the receipt.')" {
                (csrf_input(csrf_token))
                input type="hidden" name="back" value="user";
                input type="hidden" name="purchase_id" value=(purchase_id);
                p class="text-sm text-neutral-600" {
                    "Use this when the buyer withdrew under section 7.1 of the Operator Pass terms by email or in another clear way. Fluxer refunds the passes in full unless the payment was already refunded, ends them and emails the receipt to the account email."
                }
                (text_field("name", "Name of the buyer", 200))
                label class="flex flex-col gap-1" {
                    span class=(LABEL_CLASS) { "When Fluxer received it, in UTC" }
                    input type="datetime-local" name="received_at" required class=(FORM_INPUT_CLASS);
                }
                label class="flex flex-col gap-1" {
                    span class=(LABEL_CLASS) { "How they sent it" }
                    div class="relative" {
                        select name="channel" required class=(FORM_SELECT_CLASS) {
                            @for (channel, label) in WITHDRAWAL_CHANNELS {
                                option value=(channel) { (label) }
                            }
                        }
                        (select_chevron())
                    }
                }
                (danger_button("Record the withdrawal"))
            }
        },
    )
}

fn reissue_form(
    base: &str,
    csrf_token: &str,
    user_id: &str,
    purchase: &OperatorPassPurchaseResponse,
) -> Markup {
    collapsed_form(
        "Issue a new copy of a certificate page",
        html! {
            form method="post" data-admin-refresh-on-success=(REFRESH_TARGET) action={(base) (WALL_PATH) "/users/" (user_id) "/reissue"}
                class="flex flex-col gap-2"
                onsubmit="return confirm('Issue a new copy of this certificate page? The download in settings then serves the new copy.')" {
                (csrf_input(csrf_token))
                input type="hidden" name="back" value="user";
                input type="hidden" name="purchase_id" value=(purchase.id);
                p class="text-sm text-neutral-600" {
                    "The new copy keeps the certificate number, the payment date and the terms version. Earlier copies stay in the records, except when the person named on the page objected: then the name and the PDF file of every earlier copy are erased and only the number, the fingerprint and the issue time stay. Ask the buyer for the name to print instead. Checkout's name checks apply to the new name."
                }
                label class="flex flex-col gap-1" {
                    span class=(LABEL_CLASS) { "Page" }
                    div class="relative" {
                        select name="pass_index" required class=(FORM_SELECT_CLASS) {
                            @for index in 1..=purchase.quantity {
                                option value=(index) { "Page " (index) }
                            }
                        }
                        (select_chevron())
                    }
                }
                (text_field("holder_name", "Name to print", 48))
                label class="flex flex-col gap-1" {
                    span class=(LABEL_CLASS) { "Why" }
                    div class="relative" {
                        select name="reissue_reason" required class=(FORM_SELECT_CLASS) {
                            @for (reason, label) in REISSUE_REASONS {
                                option value=(reason) { (label) }
                            }
                        }
                        (select_chevron())
                    }
                }
                (danger_button("Issue the new copy"))
            }
        },
    )
}

fn purchases_section(
    base: &str,
    csrf_token: &str,
    user_id: &str,
    purchases: &[OperatorPassPurchaseResponse],
    can_moderate: bool,
) -> Markup {
    if purchases.is_empty() {
        return html! { p class="text-sm text-neutral-600" { "This account paid for no passes." } };
    }
    html! {
        div class="flex flex-col gap-4" {
            @for purchase in purchases {
                div class="rounded-lg border border-neutral-200 p-3" {
                    (detail_row("Purchase", html! { (purchase.id) }))
                    (detail_row("Status", html! { (purchase.status.label()) }))
                    (detail_row("Passes", html! { (purchase.quantity) }))
                    (detail_row("Amount", html! { (format_amount(purchase.amount_minor, &purchase.currency.to_string())) }))
                    (detail_row("Paid", format_optional_date(purchase.purchased_at.as_ref(), "Not yet")))
                    (detail_row("Delivered", format_optional_date(purchase.delivered_at.as_ref(), "Not yet")))
                    @if let Some(country) = &purchase.withdrawal_country {
                        (detail_row("Withdrawal rule", html! { (country) }))
                    }
                    (detail_row("Withdrawal period ends", withdrawal_ends(purchase)))
                    @if purchase.withdrawn_at.is_some() {
                        (detail_row("Withdrawn", format_optional_date(purchase.withdrawn_at.as_ref(), "")))
                    }
                    @if can_moderate && !matches!(
                        purchase.status,
                        OperatorPassPurchaseStatusSchema::Pending
                            | OperatorPassPurchaseStatusSchema::Withdrawn
                            | OperatorPassPurchaseStatusSchema::DuplicateRefunded
                    ) {
                        div class="mt-3" { (withdrawal_form(base, csrf_token, user_id, &purchase.id)) }
                    }
                    @if can_moderate && purchase.delivered_at.is_some() {
                        div class="mt-3" { (reissue_form(base, csrf_token, user_id, purchase)) }
                    }
                }
            }
        }
    }
}

fn offers_table(base: &str, offers: &[AdminOperatorPassOfferResponse]) -> Markup {
    if offers.is_empty() {
        return html! { p class="text-sm text-neutral-600" { "No offers made or received." } };
    }
    html! {
        (data_table(&["Pass", "Sender", "Recipient", "Status", "Created", "Accepted", "Arrives", "Ended", "Terms version"], html! {
            @for offer in offers {
                (table_row(html! {
                    (table_cell(false, html! { (offer.pass_id) }))
                    (table_cell(false, resource_link(base, ResourceType::User, &offer.sender_id, html! { (&*offer.sender_id) })))
                    (table_cell(false, resource_link(base, ResourceType::User, &offer.recipient_id, html! { (&*offer.recipient_id) })))
                    (table_cell(false, html! { (offer_status_label(&offer.status)) }))
                    (table_cell(true, format_date(&offer.created_at)))
                    (table_cell(true, format_optional_date(offer.accepted_at.as_ref(), "")))
                    (table_cell(true, format_optional_date(offer.arrives_at.as_ref(), "")))
                    (table_cell(true, format_optional_date(offer.ended_at.as_ref(), "")))
                    (table_cell(false, html! { (offer.terms_version.as_deref().unwrap_or("Not set")) }))
                }))
            }
        }))
    }
}

fn consent_rows(pass: &AdminOperatorPassRowResponse) -> Markup {
    html! {
        (detail_row("Source", html! { (unit_source_label(&pass.source)) }))
        (detail_row("Stored pass count", html! { (pass.pass_count) }))
        (detail_row("Granted", format_date(&pass.granted_at)))
        (detail_row("Paused", html! { (if pass.paused { "Yes" } else { "No" }) }))
        (detail_row("Terms version", html! { (pass.terms_version.as_deref().unwrap_or("Not set")) }))
        (detail_row("Delivery consent at", html! { (format_time(pass.waiver_consent_at.as_ref())) }))
        (detail_row("Consent text version", html! { (pass.waiver_consent_version.as_deref().unwrap_or("Not set")) }))
        (detail_row("Consent IP address", html! { (pass.waiver_consent_ip.as_deref().unwrap_or("Not set")) }))
        (detail_row("Consent user agent", html! { (pass.waiver_user_agent.as_deref().unwrap_or("Not set")) }))
        @if let Some(number) = &pass.invoice_number {
            (detail_row("Invoice number", html! { (number) }))
        }
    }
}

fn entry_rows(
    base: &str,
    entry: &AdminOperatorPassWallEntryResponse,
    removal: Option<AdminOperatorPassRemovalStateSchema>,
) -> Markup {
    let shown_avatar = proxied_avatar(base, entry.live_avatar_private_url.as_deref())
        .or_else(|| entry.shown_avatar_url.clone());
    let display = match removal {
        Some(AdminOperatorPassRemovalStateSchema::Removed) => {
            Some(OperatorPassWallDisplayStateSchema::Removed)
        }
        Some(AdminOperatorPassRemovalStateSchema::Notice) | None => entry.display_state,
    };
    let off_wall = match display {
        Some(OperatorPassWallDisplayStateSchema::Hidden) => Some("Nothing while it is hidden."),
        Some(OperatorPassWallDisplayStateSchema::Removed) => {
            Some("Nothing while the bonus is removed.")
        }
        Some(
            OperatorPassWallDisplayStateSchema::Live | OperatorPassWallDisplayStateSchema::Fallback,
        )
        | None => None,
    };
    html! {
        (detail_row("Wall shows", html! {
            @match off_wall {
                Some(text) => {
                    span class="text-neutral-600" {
                        (text)
                        @if let Some(name) = entry.shown_name.as_deref() { " Kept listing: " (name) }
                    }
                }
                None if entry.business => {
                    div class="flex items-center gap-2" {
                        @match shown_avatar.as_deref() {
                            Some(src) => { img src=(src) alt="" width="48" height="48" class="h-12 w-12 shrink-0 rounded-lg object-cover" loading="lazy"; }
                            None => { span class="inline-block h-12 w-12 shrink-0 rounded-lg bg-neutral-300" {} }
                        }
                        span { (entry.shown_name.as_deref().unwrap_or("Not shown")) }
                    }
                }
                None => {
                    div class="flex items-center gap-2" {
                        (avatar_img(shown_avatar.as_deref(), "h-10 w-10"))
                        span { (entry.shown_name.as_deref().unwrap_or("Not shown")) }
                    }
                }
            }
        }))
        (detail_row("State", html! { (review_state_label(entry, display)) }))
        (detail_row("Business listing", html! { (if entry.business { "Yes, in the Companies group" } else { "No" }) }))
        (detail_row("Latest submitted name", html! { (entry.wall_name.as_deref().unwrap_or("None")) }))
        @if let Some(pending) = &entry.pending_name {
            (detail_row("Waiting for review", html! {
                (pending) ", from " (entry.pending_source.as_deref().unwrap_or("unknown"))
                @if let Some(due) = entry.review_due_at.as_ref() { ", due " (format_date(due)) }
            }))
        }
        @if let Some(declined) = &entry.declined_name {
            (detail_row("Declined name", html! { (declined) }))
        }
        @if let Some(reason) = &entry.decline_reason {
            (detail_row("Decline reason", html! { (reason.label()) }))
        }
        @if let Some(note) = &entry.decline_note {
            (detail_row("Facts (sent to them)", html! { (note) }))
        }
        (detail_row("Follows profile changes", html! { (if entry.keep_as_is { "No, kept as it is" } else { "Yes" }) }))
        (detail_row("Chose the yearly list", html! { (if entry.release_list_opt_in { "Yes" } else { "No" }) }))
        (detail_row("Legal hold", html! { (if entry.legal_hold { "Yes" } else { "No" }) }))
        @if let Some(left) = entry.left_wall_at.as_ref() {
            (detail_row("Left the wall", format_date(left)))
        }
        (detail_row("Reviewed", format_optional_date(entry.reviewed_at.as_ref(), "Not yet")))
        @if let Some(listed) = entry.listed_at.as_ref() {
            (detail_row("Listed", format_date(listed)))
        }
        @if let Some(delivered) = entry.delivered_at.as_ref() {
            (detail_row("Delivered", format_date(delivered)))
        }
        (detail_row("Latest release list", html! {
            @match entry.release_published_year {
                Some(year) => {
                    "The Operators of " (year)
                    @if let Some(released) = entry.release_published_at.as_ref() { ", released " (format_date(released)) }
                    @if let Some(erased) = entry.release_erased_at.as_ref() {
                        ", erased " (format_date(erased))
                    }
                }
                None => { "Not published yet" }
            }
        }))
    }
}

fn business_listing_form(base: &str, csrf_token: &str, user_id: &str, business: bool) -> Markup {
    html! {
        details class="mt-4 rounded-lg border border-neutral-200" {
            summary class="cursor-pointer rounded-lg px-3 py-2 font-medium text-sm" {
                (if business { "Replace the business logo" } else { "List as a business" })
            }
            form method="post" enctype="multipart/form-data"
                action={(base) (WALL_PATH) "/users/" (user_id) "/business?_csrf=" (csrf_token)}
                class="flex flex-col gap-3 border-neutral-200 border-t p-3"
                onsubmit="return confirm('Show this company name and logo on the wall now?')" {
                p class="text-sm text-neutral-600" {
                    "Shows the company name and logo after the people on the wall, at the smallest size and with no link. It goes live at once and does not follow profile changes. The API checks that a purchase has a qualifying tax ID and was paid at least 7 days ago. Put back an earlier personal revision to make it a personal listing again."
                }
                (text_field("name", "Company name", 32))
                label class="flex flex-col gap-1" {
                    span class=(LABEL_CLASS) { "Logo" }
                    input type="file" name="logo" required accept="image/png,image/jpeg,image/webp" class="text-sm"
                        onchange=(format!("this.setCustomValidity(this.files[0] && this.files[0].size > {BUSINESS_LOGO_MAX_BYTES} ? 'The logo must be under 2 MB' : '')"));
                    span class="text-neutral-600 text-sm" {
                        "A square PNG, JPEG or WebP image of at least 256 pixels, under 2 MB and not animated, on an opaque background. Pad a logo that isn't square before you upload it."
                    }
                }
                fieldset class="flex flex-col gap-2" {
                    legend class={(LABEL_CLASS) " mb-1"} { "Checks" }
                    @for (key, label) in BUSINESS_CHECKS {
                        label class="flex items-start gap-2 text-sm" {
                            input type="checkbox" name=(key) value="yes" required class="mt-1";
                            (label)
                        }
                    }
                }
                div { (submit_button(if business { "Replace the logo" } else { "List the business" })) }
            }
        }
    }
}

fn erasable_release_years(events: &[WallTimelineEvent]) -> Vec<i64> {
    let years_of = |kind: &str| -> BTreeSet<i64> {
        events
            .iter()
            .filter(|event| event.kind == kind)
            .filter_map(|event| event.release_year)
            .collect()
    };
    let erased = years_of("erased");
    years_of("release_published")
        .difference(&erased)
        .copied()
        .collect()
}

#[allow(clippy::too_many_arguments)]
pub fn operator_pass_wall_user_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    flash: Option<&FlashMessage>,
    user_id: &str,
    data: Option<&AdminOperatorPassUserResponse>,
    timeline: Option<&WallTimeline>,
    can_moderate: bool,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header("Operators' Wall", None))
        p class="mb-4 text-sm" {
            (resource_link(base, ResourceType::User, user_id, html! { "Open account " (user_id) }))
            " | "
            a href={(base) (WALL_PATH)} class="underline" { "Back to the review queue" }
        }
        @match data {
            None => { (error_alert("Could not load the Operator Pass of this account.")) }
            Some(data) => {
                @let removal = data.removal.as_ref().map(|removal| removal.state);
                @let removed = removal == Some(AdminOperatorPassRemovalStateSchema::Removed);
                div class="flex flex-col gap-6" {
                    (card_with_header("Pass and delivery consent", html! {
                        (detail_row("Active passes", html! {
                            @match &data.holder {
                                Some(holder) => { (holder.count) }
                                None if removed => { "None while the bonus is removed" }
                                None => { "None" }
                            }
                        }))
                        @match &data.pass {
                            Some(pass) => { (consent_rows(pass)) }
                            None => { p class="text-sm text-neutral-600" { "This account has no stored pass." } }
                        }
                    }))
                    (card_with_header("Name on the wall", html! {
                        @match &data.wall {
                            Some(entry) => {
                                (entry_rows(base, entry, removal))
                                @if can_moderate && !removed && entry.listed_at.is_some() && !entry.wall_opt_out {
                                    div class="mt-4" { (listing_actions(base, csrf_token, entry)) }
                                }
                                @if can_moderate {
                                    @for year in erasable_release_years(timeline.map_or(&[][..], |timeline| &timeline.events)) {
                                        form method="post" data-admin-result-form="true" hx-push-url="false" action={(base) (RELEASES_PATH) "/" (year) "/erase"}
                                            class="mt-4 flex flex-col gap-2"
                                            onsubmit="return confirm('Erase this name from the published list? This cannot be undone.')" {
                                            (csrf_input(csrf_token))
                                            input type="hidden" name="user_id" value=(user_id);
                                            (danger_button(&format!("Erase from The Operators of {year}")))
                                        }
                                    }
                                }
                            }
                            None => { p class="text-sm text-neutral-600" { "This account was never listed." } }
                        }
                        @if can_moderate && !removed && data.holder.is_some() {
                            (business_listing_form(base, csrf_token, user_id, data.wall.as_ref().is_some_and(|entry| entry.business)))
                        }
                    }))
                    (removal_section(base, csrf_token, user_id, data.removal.as_ref(), &data.removal_history, can_moderate))
                    @match timeline {
                        Some(timeline) => { (timeline_section(base, csrf_token, user_id, timeline, can_moderate)) }
                        None => { (error_alert("Could not load the wall timeline of this account.")) }
                    }
                    (card_with_header("Purchases", purchases_section(base, csrf_token, user_id, &data.purchases, can_moderate)))
                    (card_with_header("Passes", units_table(base, &data.units)))
                    (card_with_header("Pass it on offers", offers_table(base, &data.offers)))
                    @if let Some(activation) = &data.visionary_activation {
                        (card_with_header("Pass that comes with Visionary", visionary_activation_card(activation)))
                    }
                }
            }
        }
    };
    admin_layout(
        config,
        auth,
        "Operators' Wall",
        "operator-pass-wall",
        flash,
        html! { div id=(REFRESH_ID) { (content) } },
    )
}

fn tier_px(tier: i64) -> f64 {
    let index = usize::try_from(tier.clamp(1, 4) - 1).unwrap_or(0);
    BASE_AVATAR_PX * TIER_SCALES[index]
}

fn time_travel_grid(base: &str, view: &AdminOperatorPassWallAsOfResponse) -> Markup {
    if view.entries.is_empty() {
        return html! { p class="text-sm text-neutral-600" { "The wall was empty at that time." } };
    }
    html! {
        p class="mb-3 text-sm text-neutral-600" {
            "The wall as it was at " (format_time(Some(&view.at))) ", " (view.entries.len()) " Operators, largest first."
        }
        div class="flex flex-wrap items-end gap-3" {
            @for entry in &view.entries {
                @let size = tier_px(entry.tier);
                div class="flex flex-col items-center gap-1 text-center" style={"width: " (size + 16.0) "px"} {
                    @if entry.deleted {
                        span class="inline-block shrink-0 rounded-full bg-neutral-200" style={"width: " (size) "px; height: " (size) "px"} {}
                        span class="text-xs text-neutral-500" { "Deleted account" }
                    } @else {
                        @match proxied_avatar(base, entry.avatar_url.as_deref()) {
                            Some(src) => { img src=(src) alt="" class="shrink-0 rounded-full object-cover" style={"width: " (size) "px; height: " (size) "px"} loading="lazy"; }
                            None => { span class="inline-block shrink-0 rounded-full bg-neutral-300" style={"width: " (size) "px; height: " (size) "px"} {} }
                        }
                        span class="w-full truncate text-xs" title=(entry.name) { (entry.name) }
                        @if entry.count >= 2 { span class="text-xs text-neutral-500" { (entry.count) " passes" } }
                    }
                }
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn operator_pass_wall_tools_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    flash: Option<&FlashMessage>,
    state: Option<&AdminOperatorPassWallStateResponse>,
    at: Option<&str>,
    view: Option<&AdminOperatorPassWallAsOfResponse>,
    can_moderate: bool,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header("Operators' Wall Tools", Some("Publishing state and time travel. Order and size come only from the number of passes and the first pass date, so staff cannot reorder or resize.")))
        div class="flex flex-col gap-6" {
            p class="text-sm" { a href={(base) (WALL_PATH)} class="underline" { "Back to the review queue" } }
            (card_with_header("Publishing", html! {
                @match state {
                    None => { (error_alert("Could not load the wall state.")) }
                    Some(state) => {
                        (detail_row("Version", html! { (state.version) }))
                        (detail_row("Publishing", html! { (if state.frozen { "Frozen, the current snapshot keeps serving" } else { "Live" }) }))
                        @if state.frozen {
                            (detail_row("Frozen", html! { (format_time(state.frozen_at.as_ref())) }))
                        }
                        (detail_row("Last published", html! { (format_time(state.published_at.as_ref())) }))
                        (detail_row("Entries served", html! { (state.entry_count) }))
                        @if can_moderate {
                            div class="mt-4 flex flex-wrap gap-2" {
                                form method="post" data-admin-refresh-on-success=(REFRESH_TARGET) action={(base) (TOOLS_PATH) "/" (if state.frozen { "unfreeze" } else { "freeze" })}
                                    onsubmit="return confirm('Change the publishing state of the wall?')" {
                                    (csrf_input(csrf_token))
                                    @if state.frozen { (secondary_button("Unfreeze and publish")) } @else { (danger_button("Freeze publishing")) }
                                }
                                @if !state.frozen {
                                    form method="post" data-admin-refresh-on-success=(REFRESH_TARGET) action={(base) (TOOLS_PATH) "/republish"} {
                                        (csrf_input(csrf_token))
                                        (secondary_button("Re-publish"))
                                    }
                                }
                            }
                        }
                    }
                }
            }))
            (card_with_header("Time travel", html! {
                form method="get" action={(base) (TOOLS_PATH)} class="mb-4 flex flex-wrap items-end gap-2" {
                    label class="flex flex-col gap-1" {
                        span class=(LABEL_CLASS) { "Show the wall as it was at (UTC)" }
                        input type="datetime-local" name="at" required value=(at.unwrap_or("")) class=(FORM_INPUT_CLASS);
                    }
                    (secondary_button("Show"))
                }
                @match (at, view) {
                    (Some(_), Some(view)) => { (time_travel_grid(base, view)) }
                    (Some(_), None) => { (error_alert("Could not rebuild the wall for that time.")) }
                    _ => {}
                }
            }))
        }
    };
    admin_layout(
        config,
        auth,
        "Operators' Wall Tools",
        "operator-pass-wall-tools",
        flash,
        html! { div id=(REFRESH_ID) { (content) } },
    )
}

fn published_rows(release: &AdminOperatorPassStoredReleaseResponse) -> Markup {
    html! {
        (detail_row("Tag", html! { (release.tag) }))
        (detail_row("Released", html! { (format_time(Some(&release.released_at))) }))
        (detail_row("Published", html! { (format_time(Some(&release.published_at))) }))
        (detail_row("Cut-off", html! { (format_time(Some(&release.cutoff_at))) }))
        (detail_row("Names", html! { (release.names.len()) }))
        @if !release.erased_names.is_empty() {
            (detail_row("Erased names", html! { (release.erased_names.join(", ")) }))
        }
    }
}

fn release_file_card(result: &AdminOperatorPassReleaseFileResponse) -> Markup {
    let filename = format!("OPERATORS-{}", result.release.year);
    html! {
        (card_with_header("OPERATORS file", html! {
            p class="mb-2 text-sm" {
                a href=(operators_file_data_url(&result.operators_file)) download=(filename) class="underline" {
                    "Download the OPERATORS file"
                }
            }
            pre class="overflow-x-auto rounded-lg bg-neutral-100 p-3 text-sm" { (result.operators_file) }
            @if !result.tasks.is_empty() {
                h3 class="mt-4 font-semibold text-sm" { "Edits to make by hand" }
                ul class="list-disc pl-5 text-sm" {
                    @for task in &result.tasks { li { (task) } }
                }
            }
        }))
    }
}

#[allow(clippy::too_many_arguments)]
pub fn operator_pass_releases_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    flash: Option<&FlashMessage>,
    year: i64,
    preview: Option<&AdminOperatorPassReleasePreviewResponse>,
    result: Option<&AdminOperatorPassReleaseFileResponse>,
    can_moderate: bool,
) -> Markup {
    let base = &config.base_path;
    let title = format!("The Operators of {year}");
    let content = html! {
        (page_header(
            "Operators Release List",
            Some("Each year's list goes into a tagged Fluxer release and the homepage feature. Passes delivered by the cut-off are in it."),
        ))
        div class="flex flex-col gap-6" {
            form method="get" action={(base) (RELEASES_PATH)} class="flex flex-wrap items-end gap-2" {
                label class="flex flex-col gap-1" {
                    span class=(LABEL_CLASS) { "Year" }
                    input type="number" name="year" min="2026" max="9999" value=(year) class=(FORM_INPUT_CLASS);
                }
                (secondary_button("Show"))
            }
            @if let Some(result) = result { (release_file_card(result)) }
            @match preview {
                None => { (error_alert("Could not load this list. The API request failed or this account lacks the user:lookup ACL.")) }
                Some(preview) => {
                    (card_with_header(&title, html! {
                        (detail_row("Cut-off", html! { (format_time(Some(&preview.cutoff_at))) }))
                        (detail_row("Publish by", html! { (format_time(Some(&preview.publish_by))) }))
                        @match &preview.published {
                            Some(release) => { (published_rows(release)) }
                            None => { (detail_row("Status", html! { (if preview.can_publish { "Ready to publish" } else { "Waiting for the cut-off" }) })) }
                        }
                    }))
                    @if let Some(release) = &preview.published {
                        (card_with_header("Published names", html! {
                            @if release.names.is_empty() {
                                p class="text-sm text-neutral-600" { "No names in this list." }
                            } @else {
                                ul class="list-disc pl-5 text-sm" { @for name in &release.names { li { (name) } } }
                            }
                            @if can_moderate {
                                form method="post" data-admin-result-form="true" hx-push-url="false" action={(base) (RELEASES_PATH) "/" (year) "/erase"}
                                    class="mt-4 flex flex-wrap items-end gap-2"
                                    onsubmit="return confirm('Erase this name from the published list? This cannot be undone.')" {
                                    (csrf_input(csrf_token))
                                    label class="flex flex-col gap-1" {
                                        span class=(LABEL_CLASS) { "User ID to erase" }
                                        input type="text" name="user_id" required inputmode="numeric" class=(FORM_INPUT_CLASS);
                                    }
                                    (danger_button("Erase"))
                                }
                            }
                        }))
                    } @else {
                        (card_with_header("Names in this list", html! {
                            @if preview.candidates.is_empty() {
                                p class="text-sm text-neutral-600" { "No names qualify yet." }
                            } @else {
                                (data_table(&["Name", "Account", "Delivered"], html! {
                                    @for candidate in &preview.candidates {
                                        (table_row(html! {
                                            (table_cell(false, html! { (candidate.name) }))
                                            (table_cell(false, resource_link(base, ResourceType::User, &candidate.user_id, html! { (&*candidate.user_id) })))
                                            (table_cell(true, format_optional_date(candidate.delivered_at.as_ref(), "")))
                                        }))
                                    }
                                }))
                            }
                        }))
                        @if can_moderate && preview.can_publish {
                            (card_with_header("Publish", html! {
                                form method="post" data-admin-result-form="true" hx-push-url="false" action={(base) (RELEASES_PATH) "/" (year) "/publish"}
                                    class="flex flex-col gap-3"
                                    onsubmit="return confirm('Publish this list? It cannot be changed afterwards, only erased name by name.')" {
                                    (csrf_input(csrf_token))
                                    (text_field("tag", "Release tag", 100))
                                    label class="flex flex-col gap-1" {
                                        span class=(LABEL_CLASS) { "Release date" }
                                        input type="date" name="released_on" required class=(FORM_INPUT_CLASS);
                                    }
                                    (submit_button(&format!("Publish {title}")))
                                }
                            }))
                        }
                    }
                }
            }
        }
    };
    admin_layout(
        config,
        auth,
        "Operators Release List",
        "operator-pass-releases",
        flash,
        html! { div id=(REFRESH_ID) { (content) } },
    )
}
