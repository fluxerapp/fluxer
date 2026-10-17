// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    admin_flags::{self, EnforcementHandling},
    api::{
        client::ApiResult,
        generated::types::{
            EnforcementDecisionListResponse, EnforcementDecisionListResponseDecisionsItem,
            EnforcementDecisionResponse, EnforcementDecisionResponseEvidenceItem,
            EnforcementDecisionResponseNotice, EnforcementKindType,
        },
        types::AdminUser,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            enforcement_fields::{
                FIELD_HELP_CLASS, FIELD_INPUT_CLASS, FIELD_LABEL_CLASS, field_label,
                internal_note_field, statement_field,
            },
            form::{csrf_input, form_actions, submit_button},
            page_container::{page_header, page_header_with_back},
            section_card::section_card_simple,
            staff_notes::{StaffNote, StaffNoteForm, staff_notes_section},
            user_display::{SYSTEM_USER_ID, user_link},
        },
        layout::admin_layout,
    },
    utils::timestamps::format_admin_timestamp,
};
use maud::{Markup, html};

const CORRECTION_LINE: &str = "If this decision was reported to an authority, file a correction.";
const DECISIONS_PAGE_KEY: &str = "decisions";
const PERMANENT_SUSPENSION_THRESHOLD_YEARS: i32 = 50;

pub fn kind_label(kind: EnforcementKindType) -> &'static str {
    match kind {
        EnforcementKindType::Warning => "Warning",
        EnforcementKindType::Suspension => "Suspension",
        EnforcementKindType::Closure => "Closure",
        EnforcementKindType::FeatureLimit => "Feature limit",
        EnforcementKindType::ContentRemoval => "Content removal",
        EnforcementKindType::ProfileReset => "Profile reset",
        EnforcementKindType::CommunityRemoval => "Community removal",
    }
}

pub fn state_label(
    status: &str,
    end_outcome: Option<&str>,
    replaced: bool,
    expired: bool,
) -> &'static str {
    match (status, end_outcome) {
        ("ended", Some("lifted")) => "Lifted",
        ("ended", Some("reversed")) => "Reversed",
        ("ended", Some("expired")) => "Expired",
        ("ended", _) => "Ended",
        _ if replaced => "Replaced",
        _ if expired => "Expired",
        _ => "Active",
    }
}

fn source_label(source: &str) -> &'static str {
    match source {
        "report" => "A report",
        _ => "Our own review",
    }
}

fn automation_label(automation: &str) -> &'static str {
    match automation {
        "flagged" => "Flagged this for review",
        "automated" => "Decided by automated tools, waiting for a person",
        _ => "Not involved",
    }
}

fn notice_status_label(status: &str) -> &'static str {
    match status {
        "sent" => "Sent",
        "undelivered" => "Not delivered. The user sees it when they sign in",
        _ => "Pending",
    }
}

fn notice_channel_label(channel: &str) -> &'static str {
    match channel {
        "email" => "Email",
        "system_dm" => "System DM",
        _ => "Sign-in screen",
    }
}

fn limit_label(limit: &str) -> &'static str {
    match limit {
        "direct_messages" => "Direct messages",
        "reports" => "Reports",
        _ => "Purchases",
    }
}

fn end_acl(kind: EnforcementKindType, removed_all_messages: bool) -> &'static str {
    match kind {
        EnforcementKindType::Suspension => acl::USER_TEMP_BAN,
        EnforcementKindType::Closure => acl::USER_DELETE,
        EnforcementKindType::Warning | EnforcementKindType::FeatureLimit => acl::USER_UPDATE_FLAGS,
        EnforcementKindType::ContentRemoval if removed_all_messages => acl::MESSAGE_DELETE_ALL,
        EnforcementKindType::ContentRemoval => acl::MESSAGE_SHRED,
        EnforcementKindType::ProfileReset => acl::USER_UPDATE_PROFILE,
        EnforcementKindType::CommunityRemoval => acl::GUILD_DELETE,
    }
}

pub fn can_end(
    admin_acls: &[String],
    kind: EnforcementKindType,
    removed_all_messages: bool,
) -> bool {
    acl::has_permission(admin_acls, acl::ENFORCEMENT_VIEW)
        && acl::has_permission(admin_acls, end_acl(kind, removed_all_messages))
}

pub fn can_lift(kind: EnforcementKindType) -> bool {
    matches!(
        kind,
        EnforcementKindType::Suspension
            | EnforcementKindType::Closure
            | EnforcementKindType::FeatureLimit
    )
}

fn is_restricted(reason_code: i32) -> bool {
    admin_flags::enforcement_reason(reason_code)
        .is_some_and(|reason| reason.handling == EnforcementHandling::Restricted)
}

pub fn ends_label(
    kind: EnforcementKindType,
    ends_at: Option<&str>,
    deletion_at: Option<&str>,
) -> String {
    match (kind, ends_at, deletion_at) {
        (EnforcementKindType::Closure, _, Some(at)) => {
            format!("Deleted on {}", format_admin_timestamp(at))
        }
        (_, Some(at), _) => format!("Until {}", format_admin_timestamp(at)),
        (EnforcementKindType::Suspension, None, _) => "Permanent".to_owned(),
        (EnforcementKindType::FeatureLimit, None, _) => "No end date".to_owned(),
        _ => String::new(),
    }
}

pub fn suspension_label(until: &str) -> String {
    let permanent =
        time::OffsetDateTime::parse(until, &time::format_description::well_known::Rfc3339)
            .is_ok_and(|until| {
                until.year() - time::OffsetDateTime::now_utc().year()
                    >= PERMANENT_SUSPENSION_THRESHOLD_YEARS
            });
    if permanent {
        "Banned permanently".to_owned()
    } else {
        format!("Suspended until {}", format_admin_timestamp(until))
    }
}

pub struct EndDecisionForm<'a> {
    pub base: &'a str,
    pub decision_id: &'a str,
    pub can_lift: bool,
    pub reason_code: i32,
    pub csrf_token: &'a str,
    pub refresh_target: &'a str,
}

pub fn end_decision_form(form: &EndDecisionForm<'_>) -> Markup {
    let prefix = format!("end-{}", form.decision_id);
    let restricted = is_restricted(form.reason_code);
    let lifted_id = format!("{prefix}-outcome-lifted");
    let reversed_id = format!("{prefix}-outcome-reversed");
    html! {
        form method="post"
            action={(form.base) "/decisions/" (form.decision_id) "?action=end"}
            data-admin-refresh-on-success=(form.refresh_target)
            data-end-decision-form="" {
            (csrf_input(form.csrf_token))
            div class="space-y-4" {
                fieldset class="space-y-1" {
                    legend class=(FIELD_LABEL_CLASS) { "Outcome" }
                    @if form.can_lift {
                        div class="flex items-center gap-2" {
                            input type="radio" id=(lifted_id) name="outcome" value="lifted" required
                                class="h-4 w-4 accent-neutral-900";
                            label for=(lifted_id) class="text-sm text-neutral-900" { "Lifted" }
                        }
                    }
                    div class="flex flex-wrap items-center gap-2" {
                        input type="radio" id=(reversed_id) name="outcome" value="reversed" required
                            class="peer h-4 w-4 accent-neutral-900";
                        label for=(reversed_id) class="text-sm text-neutral-900" { "Reversed" }
                        @if restricted {
                            p class="hidden w-full text-sm text-amber-800 peer-checked:block" data-correction-line="" {
                                (CORRECTION_LINE)
                            }
                        }
                    }
                    p class=(FIELD_HELP_CLASS) {
                        @if form.can_lift { "Lifted ends a correct decision early. " }
                        "Reversed says the decision was wrong."
                    }
                }
                (statement_field(&prefix, "Shown to the user in the email. Never name or describe other people."))
                (internal_note_field(&prefix, "Staff only. Saved in the audit log."))
                (form_actions(submit_button("End decision")))
            }
        }
    }
}

fn field(label: &str, value: Markup) -> Markup {
    html! {
        div class="flex min-w-0 flex-col gap-1" {
            dt class="text-xs text-neutral-500" { (label) }
            dd class="break-words text-sm text-neutral-900" { (value) }
        }
    }
}

fn subject_link(base: &str, decision: &EnforcementDecisionResponse, users: &[AdminUser]) -> Markup {
    let id = decision.subject_id.as_str();
    if decision.subject_type.to_string() == "guild" {
        return html! {
            a href={(base) "/guilds/" (id)} class="text-blue-600 hover:underline" {
                (decision.subject_name.as_deref().unwrap_or(id))
            }
        };
    }
    user_link(base, users, id)
}

fn summary_section(
    base: &str,
    decision: &EnforcementDecisionResponse,
    users: &[AdminUser],
) -> Markup {
    let ends = ends_label(
        decision.kind,
        decision.ends_at.as_deref(),
        decision.deletion_at.as_deref(),
    );
    let end_outcome = decision.end_outcome.map(|outcome| outcome.to_string());
    html! {
        (section_card_simple("Decision", html! {
            dl class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                (field("Status", html! {
                    (state_label(&decision.status.to_string(), end_outcome.as_deref(), decision.replaced, decision.expired))
                }))
                (field("Kind", html! { (kind_label(decision.kind)) }))
                (field("Subject", subject_link(base, decision, users)))
                @if let Some(recipient) = decision.recipient_user_id.as_deref().filter(|id| *id != decision.subject_id.as_str()) {
                    (field("Notice goes to", user_link(base, users, recipient)))
                }
                (field("Reason", html! {
                    (admin_flags::deletion_reason_label(*decision.reason_code))
                    @if let Some(rule) = &decision.notice.rule {
                        " ("
                        @if let Some(url) = &rule.url {
                            a href=(url) target="_blank" rel="noopener noreferrer" class="text-blue-600 hover:underline" { (rule.title) }
                        } @else {
                            (rule.title)
                        }
                        ")"
                    }
                }))
                (field("Decided", html! { (format_admin_timestamp(&decision.created_at)) }))
                (field("Decided by", user_link(base, users, &decision.decided_by)))
                @if !ends.is_empty() {
                    (field("Ends", html! { (ends) }))
                }
                (field("How we found this", html! { (source_label(&decision.source.to_string())) }))
                (field("Automated tools", html! { (automation_label(&decision.automation.to_string())) }))
                @if let Some(limit) = decision.limit {
                    (field("Limit", html! { (limit_label(&limit.to_string())) }))
                }
                @if !decision.profile_fields.is_empty() {
                    (field("Profile fields", html! { (decision.profile_fields.join(", ")) }))
                }
                @if decision.removed_all_messages {
                    (field("Removed messages", html! { "All messages" }))
                } @else if let Some(count) = &decision.removed_message_count {
                    (field("Removed messages", html! { (**count) }))
                }
                @if let Some(at) = &decision.payment_at {
                    (field("Charge date", html! { (format_admin_timestamp(at)) }))
                }
                @if let Some(rights_holder) = &decision.rights_holder {
                    (field("Rights holder or right concerned", html! { (rights_holder) }))
                }
                @if let Some(statement) = &decision.statement {
                    (field("Note to the user", html! { span class="whitespace-pre-wrap" { (statement) } }))
                }
                @if let Some(at) = &decision.ended_at {
                    (field("Ended", html! {
                        (format_admin_timestamp(at))
                        @if let Some(by) = &decision.ended_by {
                            " by " (user_link(base, users, by))
                        }
                    }))
                }
                @if let Some(statement) = &decision.end_statement {
                    (field("Note to the user on ending", html! { span class="whitespace-pre-wrap" { (statement) } }))
                }
            }
        }))
    }
}

fn text_list(items: &[String]) -> Markup {
    html! {
        @if !items.is_empty() {
            ul class="list-disc space-y-1 pl-5" {
                @for item in items { li { (item) } }
            }
        }
    }
}

fn current_notice(notice: &EnforcementDecisionResponseNotice) -> Markup {
    html! {
        div class="space-y-3 rounded-lg border border-neutral-200 bg-neutral-50 p-4 text-sm text-neutral-800" data-current-notice="" {
            p class="font-medium" { (notice.summary) }
            (text_list(&notice.effects))
            @if let Some(rule) = &notice.rule {
                p { "Rule: " (rule.title) }
            }
            (text_list(&notice.facts))
            @if let Some(statement) = &notice.statement {
                p class="whitespace-pre-wrap" { (statement) }
            }
            (text_list(&notice.how_we_decided))
            @if let Some(appeal) = &notice.appeal {
                p {
                    "Appeals: " (appeal.email)
                    @if let Some(deadline) = &appeal.deadline {
                        " by " (deadline)
                    }
                }
            }
            (text_list(&notice.redress))
        }
    }
}

fn notice_section(decision: &EnforcementDecisionResponse) -> Markup {
    let channels = decision
        .notice_channels
        .iter()
        .map(|channel| notice_channel_label(&channel.to_string()))
        .collect::<Vec<_>>();
    html! {
        (section_card_simple("Notice", html! {
            div class="space-y-4" {
                dl class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                    (field("Status", html! { (notice_status_label(&decision.notice_status.to_string())) }))
                    (field("Channels", html! {
                        @if channels.is_empty() { "None" } @else { (channels.join(", ")) }
                    }))
                    @if let Some(locale) = &decision.notice_locale {
                        (field("Locale", html! { (locale) }))
                    }
                    @if let Some(at) = &decision.notice_sent_at {
                        (field("Received", html! { (format_admin_timestamp(at)) }))
                    }
                }
                div class="space-y-1" {
                    p class=(FIELD_LABEL_CLASS) { "Text as sent" }
                    @if let Some(text) = &decision.notice_text {
                        pre class="whitespace-pre-wrap break-words rounded-lg bg-neutral-50 p-4 font-sans text-sm text-neutral-800" data-notice-text="" { (text) }
                    } @else {
                        p class="text-sm text-neutral-500" { "Nothing was sent yet." }
                    }
                }
                div class="space-y-1" {
                    p class=(FIELD_LABEL_CLASS) { "What the user sees now" }
                    (current_notice(&decision.notice))
                }
            }
        }))
    }
}

fn evidence_row(
    base: &str,
    item: &EnforcementDecisionResponseEvidenceItem,
    restricted: bool,
) -> Markup {
    html! {
        li class="space-y-1 py-3" data-evidence-message=(item.message_id.as_str()) {
            div class="flex flex-wrap items-baseline gap-x-3 text-xs text-neutral-500" {
                span { (format_admin_timestamp(&item.timestamp)) }
                span { "Message " (item.message_id.as_str()) }
                @if let Some(channel_id) = &item.channel_id {
                    a href={(base) "/messages?channel_id=" (channel_id.as_str()) "&message_id=" (item.message_id.as_str()) "&context_limit=50"}
                        class="text-blue-600 hover:underline" { "Open in message tools" }
                }
            }
            @if let Some(content) = &item.content {
                p class="whitespace-pre-wrap break-words text-sm text-neutral-900" { (content) }
            } @else if restricted {
                p class="text-sm text-neutral-500" { "Content is not stored for this reason." }
            } @else {
                p class="text-sm text-neutral-500" { "Content is hidden. Needs the message:lookup permission." }
            }
            @if let Some(attachments) = &item.attachments {
                @for attachment in attachments {
                    p class="text-xs text-neutral-600" { "Attachment: " (attachment.filename) }
                }
            } @else if *item.attachment_count > 0 {
                p class="text-xs text-neutral-600" { "Attachments: " (*item.attachment_count) }
            }
        }
    }
}

fn evidence_section(base: &str, decision: &EnforcementDecisionResponse) -> Markup {
    let restricted = is_restricted(*decision.reason_code);
    html! {
        (section_card_simple("Evidence", html! {
            @if decision.evidence.is_empty() {
                p class="text-sm text-neutral-500" { "No messages were recorded." }
            } @else {
                ul class="divide-y divide-neutral-100" {
                    @for item in &decision.evidence {
                        (evidence_row(base, item, restricted))
                    }
                }
            }
            @if let Some(values) = &decision.profile_values {
                @if !values.is_empty() {
                    dl class="mt-4 grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        @for (name, value) in values {
                            (field(&format!("Previous {name}"), html! { (value) }))
                        }
                    }
                }
            }
        }))
    }
}

fn reports_section(base: &str, decision: &EnforcementDecisionResponse) -> Markup {
    html! {
        @if !decision.report_ids.is_empty() {
            (section_card_simple("Reports", html! {
                ul class="space-y-1 text-sm" {
                    @for report_id in &decision.report_ids {
                        li {
                            a href={(base) "/reports/" (report_id.as_str())} class="text-blue-600 hover:underline" {
                                (report_id.as_str())
                            }
                        }
                    }
                }
            }))
        }
    }
}

fn network_blocks_section(decision: &EnforcementDecisionResponse) -> Markup {
    html! {
        @if !decision.network_blocks.is_empty() {
            (section_card_simple("Network blocks", html! {
                ul class="space-y-1 text-sm text-neutral-900" {
                    @for block in &decision.network_blocks {
                        li {
                            span class="font-mono" { (block.ip) }
                            @if let Some(expires_at) = &block.expires_at {
                                span class="text-neutral-500" { ", until " (format_admin_timestamp(expires_at)) }
                            }
                        }
                    }
                }
                p class="mt-2 text-xs text-neutral-500" {
                    "Ending the decision removes these blocks. Deleting the account removes them too."
                }
            }))
        }
    }
}

fn notes_section(
    base: &str,
    decision: &EnforcementDecisionResponse,
    users: &[AdminUser],
    can_annotate: bool,
    csrf_token: &str,
) -> Markup {
    let notes: Vec<StaffNote> = decision
        .notes
        .iter()
        .map(|note| StaffNote {
            note_id: note.note_id.as_str(),
            author: user_link(base, users, &note.author_id),
            body: &note.body,
            created_at: &note.created_at,
        })
        .collect();
    staff_notes_section(
        &notes,
        can_annotate.then(|| StaffNoteForm {
            action: format!(
                "{base}/decisions/{}?action=add_note",
                decision.decision_id.as_str()
            ),
            refresh_target: "#decision-detail",
            field_id: format!("note-{}", decision.decision_id.as_str()),
            help: "Staff only. Notes cannot be changed and expire after a year.",
            csrf_token,
        }),
    )
}

pub fn decision_user_ids(decision: &EnforcementDecisionResponse) -> Vec<String> {
    let mut ids: Vec<String> = [
        Some(&decision.decided_by),
        decision.recipient_user_id.as_ref(),
        decision.ended_by.as_ref(),
    ]
    .into_iter()
    .flatten()
    .chain(decision.notes.iter().map(|note| &note.author_id))
    .map(|id| id.as_str().to_owned())
    .collect();
    if decision.subject_type.to_string() == "user" {
        ids.push(decision.subject_id.as_str().to_owned());
    }
    ids.retain(|id| id != SYSTEM_USER_ID);
    ids.sort_unstable();
    ids.dedup();
    ids
}

pub fn decision_detail(
    config: &AdminConfig,
    admin_acls: &[String],
    decision: &EnforcementDecisionResponse,
    users: &[AdminUser],
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    let decision_id = decision.decision_id.as_str();
    let expired_end = decision
        .end_outcome
        .map(|outcome| outcome.to_string())
        .as_deref()
        == Some("expired");
    let open = (decision.status.to_string() == "active" && !decision.replaced) || expired_end;
    html! {
        div id="decision-detail" class="space-y-6" {
            (summary_section(base, decision, users))
            @if open
                && can_end(admin_acls, decision.kind, decision.removed_all_messages)
                && (decision.network_blocks.is_empty() || acl::has_permission(admin_acls, acl::BAN_IP_REMOVE))
            {
                (section_card_simple("End decision", end_decision_form(&EndDecisionForm {
                    base,
                    decision_id,
                    can_lift: can_lift(decision.kind) && !expired_end,
                    reason_code: *decision.reason_code,
                    csrf_token,
                    refresh_target: "#decision-detail",
                })))
            }
            (notice_section(decision))
            (evidence_section(base, decision))
            (reports_section(base, decision))
            (network_blocks_section(decision))
            (notes_section(
                base,
                decision,
                users,
                acl::has_permission(admin_acls, acl::ENFORCEMENT_ANNOTATE),
                csrf_token,
            ))
        }
    }
}

pub fn enforcement_decision_page(
    config: &AdminConfig,
    auth: &AuthContext,
    decision_id: &str,
    decision: Option<&EnforcementDecisionResponse>,
    users: &[AdminUser],
    csrf_token: &str,
    is_htmx: bool,
) -> Markup {
    let base = &config.base_path;
    let title = format!("Decision {decision_id}");
    let admin_acls = auth
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let content = html! {
        (page_header_with_back(&title, None, &format!("{base}/decisions"), Some("Back to decisions")))
        @match decision {
            Some(decision) => { (decision_detail(config, admin_acls, decision, users, csrf_token)) }
            None => {
                (section_card_simple("Decision", html! {
                    p class="text-sm text-neutral-500" { "This decision was not found or could not be loaded." }
                }))
            }
        }
    };
    if is_htmx {
        content
    } else {
        admin_layout(config, auth, &title, DECISIONS_PAGE_KEY, None, content)
    }
}

pub fn decisions_page(
    config: &AdminConfig,
    auth: &AuthContext,
    lookup_error: Option<&str>,
    review_queue: Option<&ApiResult<EnforcementDecisionListResponse>>,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header("Decisions", Some("Look up a decision by its ID, or work through the review queue.")))
        (section_card_simple("Find a decision", html! {
            form method="get" action={(base) "/decisions"} class="space-y-3" {
                div class="space-y-1" {
                    (field_label("decision-lookup", "Decision ID"))
                    input type="text" id="decision-lookup" name="id" required inputmode="numeric"
                        class=(FIELD_INPUT_CLASS);
                    p class=(FIELD_HELP_CLASS) { "The ID from the notice or the appeal email." }
                    @if let Some(error) = lookup_error {
                        p class="text-sm text-red-700" { (error) }
                    }
                }
                (form_actions(submit_button("Open decision")))
            }
        }))
        @if let Some(queue) = review_queue {
            (section_card_simple("Review queue", html! {
                p class=(FIELD_HELP_CLASS) {
                    "Automated limits waiting for a person, such as purchase limits after a chargeback. End the limit, or record a warning, suspension or closure on the account, to take it out of the queue."
                }
                @match queue {
                    Ok(response) => { (review_queue_table(base, &response.decisions)) }
                    Err(_) => { p class="text-sm text-red-700" { "Failed to load the review queue." } }
                }
            }))
        }
    };
    admin_layout(config, auth, "Decisions", DECISIONS_PAGE_KEY, None, content)
}

fn review_queue_table(
    base: &str,
    decisions: &[EnforcementDecisionListResponseDecisionsItem],
) -> Markup {
    html! {
        @if decisions.is_empty() {
            p class="text-sm text-neutral-500" { "Nothing is waiting for review." }
        } @else {
            div class="overflow-x-auto" {
                table class="min-w-full divide-y divide-neutral-200 text-sm" data-review-queue="" {
                    thead {
                        tr class="text-left text-xs text-neutral-500" {
                            @for header in ["ID", "Account", "Reason", "Payment", "Date"] {
                                th class="px-3 py-2 font-medium" { (header) }
                            }
                        }
                    }
                    tbody class="divide-y divide-neutral-100" {
                        @for decision in decisions {
                            tr {
                                td class="px-3 py-2" {
                                    a href={(base) "/decisions/" (decision.decision_id.as_str())} class="text-blue-600 hover:underline" {
                                        (decision.decision_id.as_str())
                                    }
                                }
                                td class="px-3 py-2" {
                                    a href={(base) "/users/" (decision.subject_id.as_str()) "?tab=moderation"} class="text-blue-600 hover:underline" {
                                        (decision.subject_id.as_str())
                                    }
                                }
                                td class="px-3 py-2" { (admin_flags::deletion_reason_label(*decision.reason_code)) }
                                td class="whitespace-nowrap px-3 py-2" {
                                    (decision.payment_at.as_deref().map(format_admin_timestamp).unwrap_or_default())
                                }
                                td class="whitespace-nowrap px-3 py-2" { (format_admin_timestamp(&decision.created_at)) }
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn decisions_table(
    base: &str,
    decisions: &[EnforcementDecisionListResponseDecisionsItem],
) -> Markup {
    html! {
        @if decisions.is_empty() {
            p class="text-sm text-neutral-500" { "No decisions." }
        } @else {
            div class="overflow-x-auto" {
                table class="min-w-full divide-y divide-neutral-200 text-sm" data-decisions-table="" {
                    thead {
                        tr class="text-left text-xs text-neutral-500" {
                            @for header in ["ID", "Kind", "Reason", "Status", "Date"] {
                                th class="px-3 py-2 font-medium" { (header) }
                            }
                        }
                    }
                    tbody class="divide-y divide-neutral-100" {
                        @for decision in decisions {
                            @let end_outcome = decision.end_outcome.map(|outcome| outcome.to_string());
                            tr {
                                td class="px-3 py-2" {
                                    a href={(base) "/decisions/" (decision.decision_id.as_str())} class="text-blue-600 hover:underline" {
                                        (decision.decision_id.as_str())
                                    }
                                }
                                td class="px-3 py-2" { (kind_label(decision.kind)) }
                                td class="px-3 py-2" { (admin_flags::deletion_reason_label(*decision.reason_code)) }
                                td class="px-3 py-2" {
                                    (state_label(&decision.status.to_string(), end_outcome.as_deref(), decision.replaced, decision.expired))
                                }
                                td class="whitespace-nowrap px-3 py-2" { (format_admin_timestamp(&decision.created_at)) }
                            }
                        }
                    }
                }
            }
        }
    }
}
