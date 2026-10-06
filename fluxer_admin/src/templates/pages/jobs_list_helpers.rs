// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::templates::components::{
    badge::{BadgeVariant, badge},
    table::{data_table, table_cell, table_row},
};
use maud::{Markup, html};

pub(crate) fn status_badge(status: &str) -> Markup {
    let variant = match status {
        "queued" => BadgeVariant::Default,
        "running" => BadgeVariant::Info,
        "succeeded" => BadgeVariant::Success,
        "failed" | "deadletter" => BadgeVariant::Danger,
        "cancelled" => BadgeVariant::Warning,
        _ => BadgeVariant::Default,
    };
    badge(status, variant)
}

pub(crate) fn format_progress(job: &serde_json::Value) -> String {
    let current = job.get("progress_current").and_then(|v| v.as_u64());
    let total = job.get("progress_total").and_then(|v| v.as_u64());
    let status = job.get("status").and_then(|v| v.as_str()).unwrap_or("");
    match (current, total) {
        (None, None) => {
            if status == "running" {
                "Running...".to_owned()
            } else {
                "\u{2014}".to_owned()
            }
        }
        (Some(cur), None | Some(0)) => format!("{cur}"),
        (cur, Some(tot)) => {
            let c = cur.unwrap_or(0);
            let pct = (u128::from(c) * 100)
                .checked_div(u128::from(tot))
                .unwrap_or(0);
            format!("{c} / {tot} ({pct}%)")
        }
    }
}

fn job_id_text(value: &serde_json::Value) -> String {
    value
        .get("job_id")
        .map(|v| match v {
            serde_json::Value::String(s) => s.clone(),
            _ => v.to_string(),
        })
        .unwrap_or_default()
}

pub(crate) fn job_row(base: &str, job: &serde_json::Value) -> Markup {
    let job_id = job_id_text(job);
    let task_type = job
        .get("task_type")
        .and_then(|v| v.as_str())
        .unwrap_or("\u{2014}");
    let status = job
        .get("status")
        .and_then(|v| v.as_str())
        .unwrap_or("unknown");
    let created_at = job
        .get("created_at")
        .and_then(|v| v.as_str())
        .unwrap_or("\u{2014}");
    let attempts = job.get("attempts").and_then(|v| v.as_u64()).unwrap_or(0);
    let max_attempts = job
        .get("max_attempts")
        .and_then(|v| v.as_u64())
        .unwrap_or(0);
    let requester = job
        .get("requested_by_user_id")
        .and_then(|v| {
            v.as_str()
                .map(|s| s.to_owned())
                .or_else(|| v.as_u64().map(|n| n.to_string()))
        })
        .unwrap_or_else(|| "cron".to_owned());
    let progress = format_progress(job);

    table_row(html! {
        (table_cell(true, html! {
            span class="whitespace-nowrap text-sm" { (created_at) }
        }))
        (table_cell(false, html! {
            a href={(base) "/jobs/" (job_id)}
                hx-target="#main-content"
                hx-swap="innerHTML"
                hx-push-url="true"
                class="text-blue-600 text-sm hover:underline" {
                (job_id)
            }
        }))
        (table_cell(false, html! {
            span class="text-sm" { (task_type) }
        }))
        (table_cell(false, html! { (status_badge(status)) }))
        (table_cell(false, html! {
            span class="text-sm" data-job-progress=(job_id) { (progress) }
        }))
        (table_cell(true, html! {
            span class="text-sm" { (attempts) "/" (max_attempts) }
        }))
        (table_cell(true, html! {
            span class="text-sm" { (requester) }
        }))
        (table_cell(false, html! {
            a href={(base) "/jobs/" (job_id)}
                hx-target="#main-content"
                hx-swap="innerHTML"
                hx-push-url="true"
                class="text-blue-600 text-sm hover:underline" {
                "Details"
            }
        }))
    })
}

pub(crate) fn jobs_table(base: &str, jobs: &[serde_json::Value]) -> Markup {
    data_table(
        &[
            "Created",
            "Job ID",
            "Task",
            "Status",
            "Progress",
            "Attempts",
            "Scheduled by",
            "",
        ],
        html! {
            @for job in jobs {
                (job_row(base, job))
            }
        },
    )
}

pub(crate) fn next_page_link(
    base: &str,
    cursor: &serde_json::Value,
    status: &str,
    task_type: &str,
    requester: &str,
    max_lookback_days: u32,
) -> Markup {
    let bucket_day = cursor
        .get("bucket_day")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    let created_at = cursor
        .get("created_at")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    let job_id = job_id_text(cursor);

    let mut params = vec![
        format!("cursor_bucket_day={bucket_day}"),
        format!("cursor_created_at={created_at}"),
        format!("cursor_job_id={job_id}"),
        format!("max_lookback_days={max_lookback_days}"),
    ];
    if !status.is_empty() {
        params.push(format!("status={status}"));
    }
    if !task_type.is_empty() {
        params.push(format!("task_type={task_type}"));
    }
    if !requester.is_empty() {
        params.push(format!("requested_by_user_id={requester}"));
    }
    let qs = params.join("&");

    html! {
        div class="flex justify-center mt-4" {
            a href={(base) "/jobs?" (qs)}
                class="inline-flex items-center justify-center gap-2 font-medium \
                       rounded-lg transition-all duration-150 bg-neutral-50 \
                       text-neutral-700 hover:text-neutral-900 border \
                       border-neutral-300 hover:border-neutral-400 px-4 py-2 \
                       text-sm" {
                "Next page"
            }
        }
    }
}
