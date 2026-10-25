// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::admin_flags::{ENFORCEMENT_LIMITS, deletion_reason_label, user_flag_bits};
use crate::utils::plural::{count_noun, noun_for};
use crate::{
    acl,
    api::{
        client::{ApiError, ApiResult},
        generated::types::{
            EnforcementDecisionListResponse, EnforcementDecisionListResponseDecisionsItem,
            EnforcementKindType, NetworkBlockCandidatesResponse,
        },
        types::{AdminUser, AuditLogEntry, MessageShredStatusResponse},
    },
    config::AdminConfig,
    templates::{
        components::{
            enforcement_fields::{
                BasisFields, DecisionFields, FIELD_HELP_CLASS, FIELD_INPUT_CLASS,
                FIELD_LABEL_CLASS, basis_fields, deletion_days_field, enforcement_fields,
                field_label, internal_note_field, network_block_fields, required_mark,
            },
            form::{Checkbox, csrf_input, danger_button, form_actions, submit_button},
            page_container::card_with_header,
        },
        pages::enforcement_decision::{
            EndDecisionForm, can_end, can_lift, decisions_table, end_decision_form, ends_label,
            kind_label, suspension_label,
        },
    },
    utils::timestamps::format_admin_timestamp,
};
use maud::{Markup, html};

const REFRESH_TARGET: &str = "#user-tab-content";
const REPORTS_LIMIT_DEFAULT_DAYS: i64 = 90;

const CONTENT_BASIS_CHOICES: &[(&str, &str)] = &[
    ("enforcement", "A rule was broken"),
    ("holder_request", "The account holder asked"),
    ("housekeeping", "Housekeeping"),
];

const SUSPENSION_DURATIONS: &[(&str, &str)] = &[
    ("1", "1 hour"),
    ("12", "12 hours"),
    ("24", "1 day"),
    ("72", "3 days"),
    ("120", "5 days"),
    ("168", "1 week"),
    ("336", "2 weeks"),
    ("720", "30 days"),
    ("0", "Permanent"),
];

pub struct LegacySuspension<'a> {
    pub entry: &'a AuditLogEntry,
}

#[derive(Default)]
pub struct DecisionPrefill {
    pub report_ids: Vec<String>,
    pub messages: Vec<String>,
}

#[derive(Default)]
pub struct ModerationContext<'a> {
    pub deletion_scheduler: Option<&'a AdminUser>,
    pub legacy_suspension: Option<LegacySuspension<'a>>,
    pub decisions: Option<&'a ApiResult<EnforcementDecisionListResponse>>,
    pub network_candidates: Option<&'a ApiResult<NetworkBlockCandidatesResponse>>,
    pub prefill: DecisionPrefill,
}

pub fn find_legacy_suspension<'a>(
    user: &AdminUser,
    logs: &'a [AuditLogEntry],
) -> Option<LegacySuspension<'a>> {
    let banned_until = user.temp_banned_until.as_deref()?;
    let entry = logs.iter().find(|log| {
        log.action == "temp_ban"
            && log.target_id == user.id
            && log.metadata.get("banned_until").map(String::as_str) == Some(banned_until)
    })?;
    Some(LegacySuspension { entry })
}

fn admin_name(entry: &AuditLogEntry) -> String {
    entry.admin_user.as_ref().map_or_else(
        || entry.admin_user_id.clone(),
        |admin| admin.username.clone(),
    )
}

#[allow(clippy::too_many_arguments)]
pub fn moderation_tab(
    config: &AdminConfig,
    user: &AdminUser,
    csrf_token: &str,
    admin_acls: &[String],
    context: &ModerationContext<'_>,
    message_shred_job_id: Option<&str>,
    message_shred_status: Option<&ApiResult<MessageShredStatusResponse>>,
    delete_all_messages_dry_run: Option<(u64, u64)>,
) -> Markup {
    let base = &config.base_path;
    let can_limit = !user.bot && acl::has_permission(admin_acls, acl::USER_UPDATE_FLAGS);
    let can_delete_all_messages = acl::has_permission(admin_acls, acl::MESSAGE_DELETE_ALL);
    let can_shred_messages = acl::has_permission(admin_acls, acl::MESSAGE_SHRED);
    let decisions = context
        .decisions
        .and_then(|result| result.as_ref().ok())
        .map(|response| response.decisions.as_slice())
        .unwrap_or_default();
    html! {
        div class="space-y-6" {
            (current_restriction_card(base, user, csrf_token, admin_acls, context, decisions))
            div class="grid grid-cols-1 gap-6 md:grid-cols-2" {
                @if acl::has_permission(admin_acls, acl::USER_TEMP_BAN) {
                    (suspend_card(base, user, csrf_token, context))
                }
                @if acl::has_permission(admin_acls, acl::USER_DELETE) && !deletion_complete(user) {
                    (close_card(base, config.self_hosted, user, csrf_token, context))
                }
                @if can_limit {
                    (limit_card(base, user, csrf_token, &context.prefill))
                }
                @if can_limit {
                    (warn_card(base, user, csrf_token, &context.prefill))
                }
            }
            @if let Some(result) = context.decisions {
                (card_with_header("Decisions", html! {
                    @match result {
                        Ok(response) => { (decisions_table(base, &response.decisions)) }
                        Err(_) => { p class="text-sm text-red-700" { "Failed to load decisions." } }
                    }
                }))
            }
            @if can_delete_all_messages {
                (delete_all_messages_card(base, user, csrf_token, &context.prefill, delete_all_messages_dry_run))
            }
            @if can_shred_messages {
                (message_shred_card(base, user, csrf_token, &context.prefill, message_shred_job_id, message_shred_status))
            }
        }
    }
}

fn deletion_complete(user: &AdminUser) -> bool {
    user.flags & user_flag_bits::DELETED != 0 && user.pending_deletion_at.is_none()
}

fn current_restriction_card(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    admin_acls: &[String],
    context: &ModerationContext<'_>,
    decisions: &[EnforcementDecisionListResponseDecisionsItem],
) -> Markup {
    html! {
        (card_with_header("Current restriction", html! {
            div data-current-restriction="" {
                @if let Some(decision_id) = &user.enforcement_decision_id {
                    (decision_restriction(base, user, csrf_token, admin_acls, decision_id, decisions))
                } @else if let Some(until) = &user.temp_banned_until {
                    (legacy_suspension_restriction(base, user, csrf_token, admin_acls, until, context.legacy_suspension.as_ref()))
                } @else if let Some(pending) = &user.pending_deletion_at {
                    (legacy_deletion_restriction(base, user, csrf_token, admin_acls, pending, context.deletion_scheduler))
                } @else if deletion_complete(user) {
                    p class="text-sm text-neutral-900" { "This account was deleted." }
                } @else {
                    p class="text-sm text-neutral-500" { "No current restriction." }
                }
            }
        }))
    }
}

fn decision_restriction(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    admin_acls: &[String],
    decision_id: &str,
    decisions: &[EnforcementDecisionListResponseDecisionsItem],
) -> Markup {
    let decision = decisions
        .iter()
        .find(|decision| decision.decision_id.as_str() == decision_id);
    let state = match (decision, user.temp_banned_until.as_deref()) {
        (Some(decision), _) => {
            let ends = ends_label(
                decision.kind,
                decision.ends_at.as_deref(),
                decision.deletion_at.as_deref(),
            );
            format!(
                "{} for {}. {ends}",
                kind_label(decision.kind),
                deletion_reason_label(*decision.reason_code)
            )
        }
        (None, Some(until)) => suspension_label(until),
        (None, None) => "Restricted by a decision.".to_owned(),
    };
    html! {
        div class="space-y-4" {
            p class="text-sm text-neutral-900" {
                (state.trim_end())
                " "
                a href={(base) "/decisions/" (decision_id)} class="text-blue-600 hover:underline" {
                    "Decision " (decision_id)
                }
            }
            @match decision {
                Some(decision) if can_end(admin_acls, decision.kind, decision.removed_all_messages) => {
                    (end_decision_form(&EndDecisionForm {
                        base,
                        decision_id,
                        can_lift: can_lift(decision.kind),
                        reason_code: *decision.reason_code,
                        csrf_token,
                        refresh_target: REFRESH_TARGET,
                    }))
                }
                Some(_) => {}
                None => {
                    p class="text-sm text-neutral-500" { "Open the decision to see or end it." }
                }
            }
        }
    }
}

fn legacy_suspension_restriction(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    admin_acls: &[String],
    until: &str,
    legacy_suspension: Option<&LegacySuspension<'_>>,
) -> Markup {
    html! {
        div class="space-y-4" {
            p class="text-sm text-neutral-900" {
                (suspension_label(until)) ". Recorded before decisions, so it has no decision."
            }
            (legacy_suspension_details(base, legacy_suspension))
            @if acl::has_permission(admin_acls, acl::USER_TEMP_BAN) {
                form method="post"
                    action={(base) "/users/" (user.id) "?action=unban&tab=moderation"}
                    data-admin-refresh-on-success=(REFRESH_TARGET) {
                    (csrf_input(csrf_token))
                    div class="space-y-3" {
                        (internal_note_field("lift", "Staff only. Saved in the audit log."))
                        p class=(FIELD_HELP_CLASS) { "The user is told by email that the suspension was lifted." }
                        (form_actions(submit_button("Lift suspension")))
                    }
                }
            }
        }
    }
}

fn legacy_suspension_details(
    base: &str,
    legacy_suspension: Option<&LegacySuspension<'_>>,
) -> Markup {
    let Some(suspension) = legacy_suspension else {
        return html! {
            p class="text-sm text-neutral-500" {
                "The audit log entry of this suspension could not be loaded."
            }
        };
    };
    html! {
        dl class="space-y-1 text-sm text-neutral-700" {
            div {
                dt class="inline font-medium" { "Suspended by: " }
                dd class="inline" {
                    a href={(base) "/users/" (suspension.entry.admin_user_id)} class="underline" {
                        (admin_name(suspension.entry))
                    }
                    " on " (format_admin_timestamp(&suspension.entry.created_at))
                }
            }
            div {
                dt class="inline font-medium" { "Internal note: " }
                dd class="inline" { (suspension.entry.audit_log_reason.as_deref().unwrap_or("None recorded")) }
            }
        }
    }
}

fn suspend_card(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    context: &ModerationContext<'_>,
) -> Markup {
    let prefill = &context.prefill;
    html! {
        (card_with_header("Suspend account", html! {
            form method="post"
                action={(base) "/users/" (user.id) "?action=suspend&tab=moderation"}
                data-admin-refresh-on-success=(REFRESH_TARGET) {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    div class="space-y-1" {
                        (field_label("suspend-duration", "Duration"))
                        select id="suspend-duration" name="duration_hours" required class=(FIELD_INPUT_CLASS) {
                            @for &(value, label) in SUSPENSION_DURATIONS {
                                option value=(value) { (label) }
                            }
                        }
                    }
                    (enforcement_fields(&DecisionFields {
                        id_prefix: "suspend",
                        kind: EnforcementKindType::Suspension,
                        report_ids: &prefill.report_ids,
                        messages: Some(&prefill.messages),
                        user_flags: Some(user.flags),
                        allow_restricted: true,
                    }))
                    (remove_evidence_field())
                    @if let Some(candidates) = context.network_candidates {
                        fieldset data-show-for-duration_hours="0" hidden disabled {
                            (network_block_fields("suspend", candidates))
                        }
                    }
                    (form_actions(submit_button("Suspend account")))
                }
            }
        }))
    }
}

fn warn_card(base: &str, user: &AdminUser, csrf_token: &str, prefill: &DecisionPrefill) -> Markup {
    html! {
        (card_with_header("Warn", html! {
            form method="post"
                action={(base) "/users/" (user.id) "?action=warn&tab=moderation"}
                data-admin-refresh-on-success=(REFRESH_TARGET) {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (enforcement_fields(&DecisionFields {
                        id_prefix: "warn",
                        kind: EnforcementKindType::Warning,
                        report_ids: &prefill.report_ids,
                        messages: Some(&prefill.messages),
                        user_flags: Some(user.flags),
                        allow_restricted: true,
                    }))
                    (form_actions(submit_button("Send warning")))
                }
            }
        }))
    }
}

fn pending_deletion_summary(
    base: &str,
    user: &AdminUser,
    scheduler: Option<&AdminUser>,
) -> (String, Markup) {
    let scheduler_name = match (user.deletion_scheduled_by.as_deref(), scheduler) {
        (None, _) => "an unrecorded source".to_owned(),
        (Some(id), _) if id == user.id => "the user".to_owned(),
        (Some(_), Some(scheduler)) => scheduler.username.clone(),
        (Some(id), None) => id.to_owned(),
    };
    let reason = user
        .deletion_reason_code
        .map_or_else(|| "no reason code".to_owned(), deletion_reason_label);
    let due = user
        .pending_deletion_at
        .as_deref()
        .map(format_admin_timestamp)
        .unwrap_or_default();
    let markup = html! {
        dl class="space-y-1 text-sm text-neutral-700" {
            div {
                dt class="inline font-medium" { "Scheduled by: " }
                dd class="inline" {
                    @match user.deletion_scheduled_by.as_deref() {
                        Some(id) => {
                            a href={(base) "/users/" (id)} class="underline" { (scheduler_name) }
                        }
                        None => { (scheduler_name) }
                    }
                    @if let Some(at) = user.deletion_scheduled_at.as_deref() {
                        " on " (format_admin_timestamp(at))
                    }
                }
            }
            div {
                dt class="inline font-medium" { "Due: " }
                dd class="inline" { (due) }
            }
            div {
                dt class="inline font-medium" { "Reason: " }
                dd class="inline" { (reason) }
            }
            @if let Some(public_reason) = &user.deletion_public_reason {
                div {
                    dt class="inline font-medium" { "Public reason: " }
                    dd class="inline" { (public_reason) }
                }
            }
            @if let Some(private_reason) = &user.deletion_audit_log_reason {
                div {
                    dt class="inline font-medium" { "Private reason: " }
                    dd class="inline" { (private_reason) }
                }
            }
        }
    };
    (
        format!("Cancel {scheduler_name}'s deletion ({reason}, due {due})"),
        markup,
    )
}

fn legacy_deletion_restriction(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    admin_acls: &[String],
    pending: &str,
    scheduler: Option<&AdminUser>,
) -> Markup {
    let (confirmation, summary) = pending_deletion_summary(base, user, scheduler);
    html! {
        div class="space-y-4" {
            p class="text-sm text-neutral-900" {
                "Scheduled for deletion. No decision owns it, so it is cancelled here."
            }
            (summary)
            @if acl::has_permission(admin_acls, acl::USER_DELETE) {
                form method="post"
                    action={(base) "/users/" (user.id) "?action=cancel_deletion&tab=moderation"}
                    data-admin-refresh-on-success=(REFRESH_TARGET) {
                    (csrf_input(csrf_token))
                    input type="hidden" name="expected_pending_deletion_at" value=(pending);
                    div class="space-y-3" {
                        (internal_note_field("cancel-deletion", "Staff only. Saved in the audit log."))
                        p class=(FIELD_HELP_CLASS) { "The user is told by email that the deletion was cancelled." }
                        (Checkbox {
                            name: "confirm",
                            value: "true",
                            label: &confirmation,
                            checked: false,
                            enabled: true,
                        }.render())
                        (form_actions(danger_button("Cancel deletion")))
                    }
                }
            }
        }
    }
}

fn close_card(
    base: &str,
    self_hosted: bool,
    user: &AdminUser,
    csrf_token: &str,
    context: &ModerationContext<'_>,
) -> Markup {
    let prefill = &context.prefill;
    html! {
        (card_with_header("Close account", html! {
            form method="post"
                action={(base) "/users/" (user.id) "?action=close&tab=moderation"}
                data-admin-refresh-on-success=(REFRESH_TARGET) {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (basis_fields(&BasisFields {
                        decision: DecisionFields {
                            id_prefix: "close",
                            kind: EnforcementKindType::Closure,
                            report_ids: &prefill.report_ids,
                            messages: Some(&prefill.messages),
                            user_flags: Some(user.flags),
                            allow_restricted: true,
                        },
                        choices: &[
                            ("enforcement", "A rule was broken"),
                            ("holder_request", "The account holder asked"),
                            ("housekeeping", "Inactivity"),
                        ],
                        plain_note_help: "Staff only. Saved in the audit log. The holder can cancel the deletion by signing in.",
                        enforcement_extra: html! {
                            (remove_evidence_field())
                            @if let Some(candidates) = context.network_candidates {
                                (network_block_fields("close", candidates))
                            }
                        },
                    }))
                    (deletion_days_field("close-days", self_hosted))
                    @if self_hosted {
                        (delete_immediately_fields())
                    }
                    @if let Some(pending) = &user.pending_deletion_at {
                        input type="hidden" name="replace_pending_deletion_at" value=(pending);
                        p class=(FIELD_HELP_CLASS) { "This replaces the deletion already scheduled." }
                    }
                    (form_actions(danger_button("Close account")))
                }
            }
        }))
    }
}

fn delete_immediately_fields() -> Markup {
    html! {
        div class="space-y-2 rounded-lg border border-red-200 bg-red-50 p-3" {
            (Checkbox {
                name: "delete_immediately",
                value: "true",
                label: "Delete the account now instead",
                checked: false,
                enabled: true,
            }.render())
            p class=(FIELD_HELP_CLASS) {
                "Skips the waiting period and sends no email. The account and its data are deleted \
                 within a minute and cannot be restored."
            }
            div class="space-y-1" {
                label for="close-delete-confirm" class=(FIELD_LABEL_CLASS) { "Type DELETE to confirm" }
                input type="text" id="close-delete-confirm" name="delete_immediately_confirm"
                    autocomplete="off" class=(FIELD_INPUT_CLASS);
            }
        }
    }
}

fn remove_evidence_field() -> Markup {
    html! {
        div class="space-y-1" {
            (Checkbox {
                name: "remove_evidence",
                value: "true",
                label: "Remove these messages",
                checked: false,
                enabled: true,
            }.render())
            p class=(FIELD_HELP_CLASS) { "Removes the messages listed above after they are saved as evidence." }
        }
    }
}

fn content_basis_fields<'a>(
    id_prefix: &'a str,
    user: &'a AdminUser,
    prefill: &'a DecisionPrefill,
    snapshot_entries: bool,
) -> BasisFields<'a> {
    BasisFields {
        decision: DecisionFields {
            id_prefix,
            kind: EnforcementKindType::ContentRemoval,
            report_ids: &prefill.report_ids,
            messages: (!snapshot_entries).then_some(prefill.messages.as_slice()),
            user_flags: Some(user.flags),
            allow_restricted: snapshot_entries,
        },
        choices: CONTENT_BASIS_CHOICES,
        plain_note_help: "Staff only. Saved in the audit log.",
        enforcement_extra: html! {},
    }
}

fn limit_card(base: &str, user: &AdminUser, csrf_token: &str, prefill: &DecisionPrefill) -> Markup {
    let reports_end = (time::OffsetDateTime::now_utc()
        + time::Duration::days(REPORTS_LIMIT_DEFAULT_DAYS))
    .date()
    .to_string();
    html! {
        (card_with_header("Limit features", html! {
            form method="post"
                action={(base) "/users/" (user.id) "?action=limit_features&tab=moderation"}
                data-admin-refresh-on-success=(REFRESH_TARGET) {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (enforcement_fields(&DecisionFields {
                        id_prefix: "limit",
                        kind: EnforcementKindType::FeatureLimit,
                        report_ids: &prefill.report_ids,
                        messages: Some(&prefill.messages),
                        user_flags: Some(user.flags),
                        allow_restricted: true,
                    }))
                    fieldset class="space-y-1" {
                        legend class=(FIELD_LABEL_CLASS) { "Limit" (required_mark()) }
                        div class="flex flex-wrap gap-x-4 gap-y-1" {
                            @for limit in ENFORCEMENT_LIMITS {
                                @let id = format!("limit-{}", limit.value);
                                @let codes = limit.codes.iter().map(ToString::to_string).collect::<Vec<_>>().join(" ");
                                div class="flex items-center gap-2" data-show-for-codes=(codes) hidden {
                                    input type="radio" id=(id) name="limit" value=(limit.value) required disabled
                                        class="h-4 w-4 accent-neutral-900";
                                    label for=(id) class="text-sm text-neutral-900" { (limit.label) }
                                }
                            }
                        }
                        p class=(FIELD_HELP_CLASS) { "Each reason offers only the limits it allows." }
                    }
                    fieldset class="space-y-1" data-show-for-limit="reports" hidden disabled {
                        (field_label("limit-ends-on", "Ends on"))
                        input type="date" id="limit-ends-on" name="ends_on" value=(reports_end) required class=(FIELD_INPUT_CLASS);
                        p class=(FIELD_HELP_CLASS) { "At most 365 days from now." }
                    }
                    (form_actions(submit_button("Limit features")))
                }
            }
        }))
    }
}

fn delete_all_messages_card(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    prefill: &DecisionPrefill,
    dry_run: Option<(u64, u64)>,
) -> Markup {
    let action = format!(
        "{base}/users/{}?action=delete_all_messages&tab=moderation",
        user.id
    );
    html! {
        (card_with_header("Delete all messages", html! {
            div class="space-y-4" {
                p class="text-sm text-neutral-600" {
                    "Removes every message this user has sent. Preview the count first. Child safety and violent extremism reasons can't be used here, because the messages can't all be saved as evidence first. Use the message shredder with the messages instead."
                }
                form method="post" action=(action) data-admin-result-form="true" {
                    (csrf_input(csrf_token))
                    input type="hidden" name="dry_run" value="true";
                    (form_actions(submit_button("Preview deletion")))
                }
                @if let Some((channel_count, message_count)) = dry_run {
                    form method="post" action=(action)
                        class="space-y-4 rounded-lg border border-neutral-200 bg-neutral-50 p-4" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="dry_run" value="false";
                        p class="text-sm text-neutral-700" {
                            (count_noun(message_count, "message", "messages")) " in " (count_noun(channel_count, "channel", "channels")) "."
                        }
                        (basis_fields(&content_basis_fields("delete-all", user, prefill, false)))
                        (form_actions(danger_button("Delete all messages")))
                    }
                }
            }
        }))
    }
}

fn message_shred_card(
    base: &str,
    user: &AdminUser,
    csrf_token: &str,
    prefill: &DecisionPrefill,
    job_id: Option<&str>,
    status: Option<&ApiResult<MessageShredStatusResponse>>,
) -> Markup {
    html! {
        (card_with_header("Message shredder", html! {
            div class="space-y-4" {
                p class="text-sm text-neutral-600" {
                    "Upload a CSV file with one channel_id,message_id pair per row. When a rule was broken, the first 50 messages are saved as evidence before they are removed, and every message for child safety and violent extremism reasons."
                }
                form method="post"
                    action={(base) "/users/" (user.id) "?action=message_shred&tab=moderation"}
                    id="message-shred-form" {
                    (csrf_input(csrf_token))
                    input type="hidden" name="csv_data" id="message-shred-csv-data";
                    div class="space-y-4" {
                        div class="space-y-2" {
                            (field_label("message-shred-file", "CSV file"))
                            input id="message-shred-file" type="file" accept=".csv" class="hidden";
                            div class="flex items-center gap-2" {
                                button type="button"
                                    class="inline-flex items-center justify-center gap-2 rounded-lg border border-neutral-300 bg-neutral-50 px-3 py-1.5 text-sm font-medium text-neutral-700 transition-all duration-150 hover:border-neutral-400 hover:text-neutral-900 focus:outline-none focus:ring-2 focus:ring-brand-primary/20"
                                    onclick="document.getElementById('message-shred-file').click()" {
                                    span { "Choose file" }
                                }
                                span class="text-sm text-neutral-500" id="message-shred-file-name" {
                                    "No file chosen"
                                }
                            }
                        }
                        (basis_fields(&content_basis_fields("shred", user, prefill, true)))
                        button type="submit"
                            id="message-shred-submit"
                            class="inline-flex w-full items-center justify-center gap-2 rounded-lg bg-neutral-900 px-4 py-2 text-base font-medium text-white transition-all duration-150 hover:bg-neutral-800 focus:outline-none focus:ring-2 focus:ring-neutral-900 focus:ring-offset-2 focus:ring-offset-white disabled:cursor-not-allowed disabled:opacity-50" {
                            span { "Shred messages" }
                        }
                    }
                }
                (message_shred_status_section(base, user, job_id, status))
                (message_shred_form_script())
            }
        }))
    }
}

fn message_shred_status_section(
    base: &str,
    user: &AdminUser,
    job_id: Option<&str>,
    status: Option<&ApiResult<MessageShredStatusResponse>>,
) -> Markup {
    let Some(_) = job_id.filter(|job_id| !job_id.trim().is_empty()) else {
        return html! {};
    };
    html! {
        div class="space-y-3 rounded-lg border border-neutral-200 bg-white p-4" {
            div class="flex items-center justify-between gap-3" {
                h3 class="text-sm font-medium text-neutral-900" { "Message shred status" }
                a href={(base) "/users/" (user.id) "?tab=moderation"}
                    class="text-sm text-neutral-500 hover:text-neutral-900" {
                    "Clear"
                }
            }
            @match status {
                Some(Ok(response)) => {
                    (message_shred_status_content(&response.data))
                }
                Some(Err(ApiError::Http { status: 404, .. })) | None => {
                    p class="text-sm text-neutral-700" { "Preparing job... check back in a moment." }
                }
                Some(Err(error)) => {
                    (message_shred_status_error(error))
                }
            }
        }
    }
}

fn message_shred_status_content(status: &serde_json::Value) -> Markup {
    let status_value = value_str(status, "status").unwrap_or("not_found");
    if status_value == "not_found" {
        return html! {
            div class="space-y-3" {
                p class="text-sm text-neutral-700" {
                    "Status: " (message_shred_status_label(status_value))
                }
            }
        };
    }
    let requested = value_u64(status, "requested").unwrap_or(0);
    let total = value_u64(status, "total").unwrap_or(0);
    let processed = value_u64(status, "processed").unwrap_or(0);
    let skipped = value_u64(status, "skipped").unwrap_or(0);
    let percentage = (u128::from(processed) * 100)
        .checked_div(u128::from(total))
        .unwrap_or(0)
        .min(100);
    html! {
        div class="space-y-3" {
            p class="text-sm text-neutral-700" {
                "Status: " (message_shred_status_label(status_value))
            }
            p class="text-sm text-neutral-700" {
                "Requested " (count_noun(requested, "entry", "entries")) ", skipped " (count_noun(skipped, "entry", "entries"))
            }
            @if status_value == "in_progress" {
                div class="space-y-2" {
                    div class="text-sm text-neutral-700" {
                        span { (processed) " / " (total) " (" (percentage) "%)" }
                    }
                    div class="h-2 w-full overflow-hidden rounded-full bg-neutral-200" {
                        div class="h-2 bg-neutral-900 transition-[width] duration-300"
                            style={ "width: " (percentage) "%;" } {}
                    }
                }
            }
            @if status_value == "completed" {
                p class="text-sm text-neutral-700" {
                    "Deleted " (processed) " / " (total) " " (noun_for(total, "entry", "entries"))
                }
            }
            @if let Some(started_at) = value_str(status, "started_at") {
                p class="text-xs text-neutral-500" { "Started " (format_admin_timestamp(started_at)) }
            }
            @if let Some(completed_at) = value_str(status, "completed_at") {
                p class="text-xs text-neutral-500" { "Completed " (format_admin_timestamp(completed_at)) }
            }
            @if let Some(failed_at) = value_str(status, "failed_at") {
                p class="text-xs text-red-600" { "Failed " (format_admin_timestamp(failed_at)) }
            }
            @if let Some(error) = value_str(status, "error") {
                p class="text-sm text-red-600" { (error) }
            }
        }
    }
}

fn message_shred_status_error(error: &ApiError) -> Markup {
    let message = match error {
        ApiError::Network(message) | ApiError::Parse(message) => message.as_str(),
        ApiError::Http { message, .. } => message.as_str(),
    };
    html! {
        div class="rounded-lg border border-red-200 bg-red-50 p-4 text-sm text-red-800" {
            "Failed to load message shred status"
            @if !message.is_empty() {
                div class="mt-1 text-xs" { (message) }
            }
        }
    }
}

fn message_shred_status_label(status: &str) -> &'static str {
    match status {
        "in_progress" => "In progress",
        "completed" => "Completed",
        "failed" => "Failed",
        "not_found" => "Preparing",
        _ => "Unknown",
    }
}

fn value_str<'a>(value: &'a serde_json::Value, key: &str) -> Option<&'a str> {
    value.get(key).and_then(serde_json::Value::as_str)
}

fn value_u64(value: &serde_json::Value, key: &str) -> Option<u64> {
    value.get(key).and_then(|value| {
        value
            .as_u64()
            .or_else(|| value.as_i64().and_then(|n| n.try_into().ok()))
    })
}

fn message_shred_form_script() -> Markup {
    html! {
        script defer {
            (maud::PreEscaped(MESSAGE_SHRED_FORM_SCRIPT))
        }
    }
}

const MESSAGE_SHRED_FORM_SCRIPT: &str = r#"
(function () {
	var form = document.getElementById('message-shred-form');
	if (!form) return;
	var file = document.getElementById('message-shred-file');
	var csvInput = document.getElementById('message-shred-csv-data');
	var submitButton = document.getElementById('message-shred-submit');
	var fileName = document.getElementById('message-shred-file-name');
	if (!file || !csvInput || !submitButton) return;
	var processing = false;
	function toast(level, message) {
		document.body.dispatchEvent(new CustomEvent('showFlash', {detail: {level: level, message: message}}));
	}
	function toastFromResponse(response) {
		var raw = response.headers.get('X-Fluxer-Admin-Toast');
		if (!raw) return null;
		try {
			var parsed = JSON.parse(raw);
			return {
				level: parsed && parsed.level ? String(parsed.level) : 'info',
				message: parsed && parsed.message ? String(parsed.message) : ''
			};
		} catch (error) {
			return null;
		}
	}
	function csrfToken() {
		var input = form.querySelector('input[name="_csrf"]');
		return input ? input.value || '' : '';
	}
	function resetSubmit(label) {
		processing = false;
		submitButton.disabled = false;
		submitButton.querySelector('span').textContent = label || 'Shred messages';
	}
	file.addEventListener('change', function () {
		if (fileName) {
			fileName.textContent = file.files && file.files[0]
				? file.files[0].name
				: 'No file chosen';
		}
	});
	form.addEventListener('submit', function (event) {
		if (processing) {
			event.preventDefault();
			return;
		}
		var selected = file.files && file.files[0];
		if (!selected) {
			event.preventDefault();
			toast('error', 'Please select a CSV file to continue.');
			return;
		}
		event.preventDefault();
		processing = true;
		submitButton.disabled = true;
		submitButton.querySelector('span').textContent = 'Processing...';
		var reader = new FileReader();
		reader.onload = function () {
			csvInput.value = reader.result || '';
			toast('info', 'Queueing message shred...');
			fetch(form.getAttribute('action') || window.location.href, {
				method: 'POST',
				credentials: 'same-origin',
				headers: {
					'Content-Type': 'application/x-www-form-urlencoded',
					'HX-Request': 'true',
					'HX-Target': 'flash-container',
					'X-CSRF-Token': csrfToken()
				},
				body: new URLSearchParams(new FormData(form)).toString()
			}).then(function (response) {
				var parsed = toastFromResponse(response);
				if (!response.ok && !parsed) throw new Error('Failed to queue message shred.');
				toast(
					parsed ? parsed.level : response.ok ? 'success' : 'error',
					parsed ? parsed.message : response.ok ? 'Message shred queued.' : 'Failed to queue message shred.'
				);
				resetSubmit();
				if (response.ok) {
					file.value = '';
					if (fileName) fileName.textContent = 'No file chosen';
				}
			}).catch(function (error) {
				toast('error', error && error.message ? error.message : 'Failed to queue message shred.');
				resetSubmit();
			});
		};
		reader.onerror = function () {
			resetSubmit();
			toast('error', 'Failed to read the CSV file. Please try again.');
		};
		reader.readAsText(selected);
	});
})();
"#;
