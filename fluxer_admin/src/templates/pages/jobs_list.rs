// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            form::{
                FORM_INPUT_CLASS, FORM_LABEL_CLASS, secondary_button_link, select_input,
                submit_button,
            },
            page_container::page_header,
            table::empty_state,
        },
        layout::admin_layout,
        pages::jobs_list_helpers::{jobs_table, next_page_link},
    },
};
use maud::{Markup, html};

fn filter_bar(base: &str, p: &JobsListParams) -> Markup {
    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm mb-6" {
            form method="get" action={(base) "/jobs"} class="space-y-4" {
                div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4" {
                    (select_input("status", "Status", &[
                        ("", "Any"), ("queued", "Queued"), ("running", "Running"),
                        ("succeeded", "Succeeded"), ("cancelled", "Cancelled"),
                        ("deadletter", "Dead-letter"),
                    ], p.status_filter))
                    div class="flex flex-col gap-2" {
                        label for="task_type" class=(FORM_LABEL_CLASS) { "Task type" }
                        input type="text" id="task_type" name="task_type"
                            value=(p.task_type_filter) placeholder="syncDisposableEmailDomains"
                            class=(FORM_INPUT_CLASS);
                    }
                    div class="flex flex-col gap-2" {
                        label for="requested_by_user_id" class=(FORM_LABEL_CLASS) {
                            "Scheduled by (admin user ID)"
                        }
                        input type="text" id="requested_by_user_id" name="requested_by_user_id"
                            value=(p.requester_filter) placeholder="Snowflake" class=(FORM_INPUT_CLASS);
                    }
                    div class="flex flex-col gap-2" {
                        label for="max_lookback_days" class=(FORM_LABEL_CLASS) { "Lookback (days)" }
                        input type="number" id="max_lookback_days" name="max_lookback_days"
                            value=(p.max_lookback_days) class=(FORM_INPUT_CLASS);
                    }
                }
                div class="flex gap-2" {
                    (submit_button("Filter"))
                    (secondary_button_link("Clear", &format!("{base}/jobs")))
                }
            }
        }
    }
}

pub struct JobsListParams<'a> {
    pub status_filter: &'a str,
    pub task_type_filter: &'a str,
    pub requester_filter: &'a str,
    pub max_lookback_days: u32,
    pub current_url: &'a str,
    pub jobs: &'a [serde_json::Value],
    pub next_cursor: Option<&'a serde_json::Value>,
}

pub fn jobs_list_page(config: &AdminConfig, auth: &AuthContext, params: &JobsListParams) -> Markup {
    let base = &config.base_path;
    let count_label = format!(
        "{} jobs{}",
        params.jobs.len(),
        if params.next_cursor.is_some() {
            " (more available)"
        } else {
            ""
        }
    );
    let content = html! {
        (page_header("Jobs", Some(&count_label)))
        (filter_bar(base, params))
        (jobs_results(config, params))
    };
    admin_layout(config, auth, "Jobs", "jobs", None, content)
}

pub fn jobs_results(config: &AdminConfig, params: &JobsListParams) -> Markup {
    let base = &config.base_path;
    html! {
        div id="jobs-results"
            hx-get=(params.current_url)
            hx-trigger="every 3s"
            hx-target="#jobs-results"
            hx-swap="outerHTML" {
        @if params.jobs.is_empty() {
            (empty_state(
                "No background jobs match these filters within the lookback window."
            ))
        } @else {
            (jobs_table(base, params.jobs))
            @if let Some(cursor) = params.next_cursor {
                (next_page_link(
                    base, cursor, params.status_filter, params.task_type_filter,
                    params.requester_filter, params.max_lookback_days,
                ))
            }
        }
        }
    }
}
