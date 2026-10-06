// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            auto_refresh::auto_refresh,
            data_field::{data_field_mono, data_field_text, data_grid},
            form::{csrf_input, danger_button, form_field_group},
            page_container::{page_header_with_actions, page_header_with_back},
            section_card::section_card_simple,
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

use super::jobs_list_helpers::status_badge;

fn val_str<'a>(job: &'a serde_json::Value, key: &str) -> &'a str {
    job.get(key).and_then(|v| v.as_str()).unwrap_or("")
}

fn val_id(job: &serde_json::Value, key: &str) -> String {
    job.get(key)
        .map(|v| match v {
            serde_json::Value::String(s) => s.clone(),
            serde_json::Value::Number(n) => n.to_string(),
            serde_json::Value::Null => "\u{2014}".to_owned(),
            _ => v.to_string(),
        })
        .unwrap_or_else(|| "\u{2014}".to_owned())
}

fn pretty_json(raw: &str) -> String {
    serde_json::from_str::<serde_json::Value>(raw)
        .ok()
        .and_then(|v| serde_json::to_string_pretty(&v).ok())
        .unwrap_or_else(|| raw.to_owned())
}

fn progress_bar(job: &serde_json::Value) -> Markup {
    let current = job.get("progress_current").and_then(|v| v.as_u64());
    let total = job.get("progress_total").and_then(|v| v.as_u64());
    let message = job
        .get("progress_message")
        .and_then(|v| v.as_str())
        .unwrap_or("");

    match (current, total) {
        (_, None | Some(0)) => {
            html! {
                p class="text-sm text-neutral-500" { "\u{2014}" }
                @if !message.is_empty() {
                    p class="text-sm text-neutral-500 mt-1" { (message) }
                }
            }
        }
        (cur, Some(tot)) => {
            let c = cur.unwrap_or(0);
            let pct = (u128::from(c) * 100 / u128::from(tot)).min(100);
            html! {
                div role="progressbar" aria-valuenow=(pct) aria-valuemin="0"
                    aria-valuemax="100"
                    aria-label={"Job progress: " (c) " of " (tot)}
                    class="w-full" {
                    div class="h-2 w-full rounded bg-neutral-200" {
                        div class="h-2 rounded bg-blue-500 transition-all"
                            style={"width: " (pct) "%"} {}
                    }
                    p class="text-xs text-neutral-500 mt-1" {
                        (c) " / " (tot) " (" (pct) "%)"
                    }
                }
                @if !message.is_empty() {
                    p class="text-sm text-neutral-500 mt-1" { (message) }
                }
            }
        }
    }
}

fn overview_section(job: &serde_json::Value) -> Markup {
    let task_type = val_str(job, "task_type");
    let lane = val_str(job, "jet_stream_lane");
    let created_at = val_str(job, "created_at");
    let started_at = val_str(job, "started_at");
    let completed_at = val_str(job, "completed_at");
    let run_at = job
        .get("run_at")
        .and_then(|v| v.as_str())
        .unwrap_or("immediate");
    let attempts = job.get("attempts").and_then(|v| v.as_u64()).unwrap_or(0);
    let max_attempts = job
        .get("max_attempts")
        .and_then(|v| v.as_u64())
        .unwrap_or(0);
    let requester = val_id(job, "requested_by_user_id");
    let requester_display = if requester == "null" || requester.is_empty() {
        "cron"
    } else {
        &requester
    };
    let audit_reason = val_str(job, "audit_log_reason");
    let js_seq = val_id(job, "jet_stream_seq");

    section_card_simple(
        "Overview",
        data_grid(
            2,
            html! {
                (data_field_mono("Task", task_type))
                (data_field_mono("Lane", if lane.is_empty() { "\u{2014}" } else { lane }))
                (data_field_text("Created", if created_at.is_empty() { "\u{2014}" } else { created_at }))
                (data_field_text("Started", if started_at.is_empty() { "\u{2014}" } else { started_at }))
                (data_field_text("Completed", if completed_at.is_empty() { "\u{2014}" } else { completed_at }))
                (data_field_text("Run-at", run_at))
                (data_field_text("Attempts", &format!("{attempts}/{max_attempts}")))
                (data_field_mono("Scheduled by", requester_display))
                (data_field_text("Audit reason", if audit_reason.is_empty() { "\u{2014}" } else { audit_reason }))
                (data_field_mono("JetStream seq", &js_seq))
            },
        ),
    )
}

fn context_link_section(base: &str, job: &serde_json::Value) -> Markup {
    let link = val_str(job, "context_link");
    if link.is_empty() {
        return html! {};
    }
    section_card_simple(
        "Affected resources",
        html! {
            a href={(base) (link)}
                class="break-all rounded text-blue-600 text-sm hover:underline" {
                (link)
            }
        },
    )
}

fn cancel_section(base: &str, job: &serde_json::Value, job_id: &str, csrf_token: &str) -> Markup {
    let status = val_str(job, "status");
    if status != "queued" && status != "running" {
        return html! {};
    }
    let cancel_requested = job
        .get("cancel_requested")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    if cancel_requested {
        return section_card_simple(
            "Cancel job",
            html! {
                p class="text-sm text-neutral-500" {
                    "Cancellation requested. Waiting for the handler to acknowledge \
                     at its next checkpoint."
                }
            },
        );
    }
    section_card_simple(
        "Cancel job",
        html! {
            p class="text-sm text-neutral-500 mb-4" {
                "Cooperative \u{2014} the handler will see "
                code class="rounded bg-neutral-100 px-1 text-xs" {
                    "shouldCancel()"
                }
                " become true at its next checkpoint. Tasks without checkpoints \
                 will continue to completion."
            }
            form method="post"
                action={(base) "/jobs/" (job_id) "?action=cancel&_csrf=" (csrf_token)} {
                (csrf_input(csrf_token))
                div class="space-y-3" {
                    (form_field_group("Audit reason", "audit_log_reason", true, None, None, html! {
                        input type="text" id="audit_log_reason" name="audit_log_reason" required
                            class="w-full rounded-lg border border-neutral-300 bg-white \
                                   text-neutral-900 text-sm h-8 px-3 py-1.5 \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-2 focus:ring-brand-primary/20";
                    }))
                    div class="w-full" { (danger_button("Request cancel")) }
                }
            }
        },
    )
}

pub fn job_detail_page(
    config: &AdminConfig,
    auth: &AuthContext,
    job_id: &str,
    job: Option<&serde_json::Value>,
    csrf_token: &str,
    is_htmx: bool,
) -> Markup {
    let base = &config.base_path;
    let title = format!("Job {job_id}");
    let admin_acls = auth
        .admin_user
        .as_ref()
        .map(|u| u.acls.as_slice())
        .unwrap_or(&[]);
    let can_cancel = acl::has_permission(admin_acls, acl::JOBS_CANCEL);

    let content = match job {
        Some(job) => {
            let status = val_str(job, "status");
            let error_message = val_str(job, "error_message");
            let payload_raw = val_str(job, "payload");
            let payload = if payload_raw.is_empty() {
                "\u{2014}".to_owned()
            } else {
                pretty_json(payload_raw)
            };
            let result_raw = val_str(job, "result");
            let should_auto_refresh = status == "queued" || status == "running";

            html! {
                (page_header_with_actions(&title, None, status_badge(status)))
                div class="space-y-6" {
                    (context_link_section(base, job))
                    (overview_section(job))
                    (section_card_simple("Progress", progress_bar(job)))
                    @if !error_message.is_empty() {
                        (section_card_simple("Error", html! {
                            pre class="overflow-x-auto rounded bg-red-50 p-3 text-red-900 text-xs" {
                                (error_message)
                            }
                        }))
                    }
                    @if can_cancel {
                        (cancel_section(base, job, job_id, csrf_token))
                    }
                    (section_card_simple("Payload", html! {
                        pre class="overflow-x-auto rounded bg-neutral-50 p-3 text-xs" {
                            (payload)
                        }
                    }))
                    @if !result_raw.is_empty() {
                        (section_card_simple("Result", html! {
                            pre class="overflow-x-auto rounded bg-neutral-50 p-3 text-xs" {
                                (pretty_json(result_raw))
                            }
                        }))
                    }
                }
                (auto_refresh(should_auto_refresh, 3000))
            }
        }
        None => {
            html! {
                (page_header_with_back(&title, None, &format!("{base}/jobs"), Some("Back to Jobs")))
                div class="space-y-6" {
                    (section_card_simple("Overview", html! {
                        p class="text-sm text-neutral-500" {
                            "Job not found or could not be loaded. Job ID: "
                            span class="" { (job_id) }
                        }
                    }))
                }
            }
        }
    };

    if is_htmx {
        content
    } else {
        admin_layout(config, auth, &title, "jobs", None, content)
    }
}
