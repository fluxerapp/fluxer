// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::utils::timestamps::format_admin_timestamp;
use crate::{
    api::types::Archive,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            error_display::error_alert,
            form::submit_button,
            page_container::page_header,
            table::{data_table, empty_state, table_cell, table_row},
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

fn archive_status_label(archive: &Archive) -> (&'static str, BadgeVariant) {
    if archive.failed_at.is_some() {
        ("Failed", BadgeVariant::Danger)
    } else if archive.completed_at.is_some() {
        ("Completed", BadgeVariant::Success)
    } else {
        ("In Progress", BadgeVariant::Info)
    }
}

fn tab_button(base: &str, current: &str, value: &str, label: &str) -> Markup {
    let is_active = current == value;
    let classes = if is_active {
        "px-4 py-2 rounded-md text-sm font-medium bg-neutral-800 text-white"
    } else {
        "px-4 py-2 rounded-md text-sm font-medium bg-neutral-100 \
         text-neutral-600 hover:bg-neutral-200"
    };
    html! {
        a href={(base) "/archives?subject_type=" (value)} class=(classes) {
            (label)
        }
    }
}

fn archive_table(archives: &[Archive], base: &str) -> Markup {
    data_table(
        &[
            "Subject",
            "Requested By",
            "Requested At",
            "Status",
            "Actions",
        ],
        html! {
            @for archive in archives {
                @let (status_label, status_variant) = archive_status_label(archive);
                (table_row(html! {
                    (table_cell(false, html! {
                        div class="flex flex-col whitespace-nowrap" {
                            span class="font-semibold text-sm" {
                                (archive.subject_type) " " (archive.subject_id)
                            }
                            span class="text-xs text-neutral-500" {
                                "Archive ID: " (archive.archive_id)
                            }
                        }
                    }))
                    (table_cell(false, html! {
                        span class="text-sm" { (archive.requested_by) }
                    }))
                    (table_cell(false, html! {
                        span class="text-sm" { (format_admin_timestamp(&archive.requested_at)) }
                    }))
                    (table_cell(false, html! {
                        div class="flex flex-col gap-1" {
                            div class="flex items-center gap-2" {
                                (badge(status_label, status_variant))
                                span class="text-xs text-neutral-500" {
                                    (archive.progress_percent) "%"
                                }
                            }
                            @if let Some(ref step) = archive.progress_step {
                                @if archive.completed_at.is_none() && archive.failed_at.is_none() {
                                    span class="text-xs text-neutral-500" { (step) }
                                }
                            }
                        }
                    }))
                    (table_cell(false, html! {
                        @if archive.completed_at.is_some() {
                            a href={
                                (base) "/archives/download?subject_type="
                                (archive.subject_type)
                                "&subject_id=" (archive.subject_id)
                                "&archive_id=" (archive.archive_id)
                            }
                            class="inline-flex items-center justify-center gap-2 \
                                   font-medium rounded-lg bg-neutral-900 text-white \
                                   hover:bg-neutral-800 px-3 py-1.5 text-sm" {
                                "Download"
                            }
                        } @else {
                            span class="text-sm text-neutral-500" { "Not ready" }
                        }
                    }))
                }))
            }
        },
    )
}

pub fn archives_page(
    config: &AdminConfig,
    auth: &AuthContext,
    subject_type: &str,
    subject_id: Option<&str>,
    archives: &[Archive],
    error: Option<&str>,
) -> Markup {
    let base = &config.base_path;
    let filter_hint = subject_id
        .map(|id| format!(" for {subject_type} {id}"))
        .unwrap_or_default();
    let title = format!("Archives{filter_hint}");

    let content = html! {
        (page_header(&title, None))

        div class="space-y-6" {
            div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm" {
                div class="flex gap-2 mb-4" {
                    (tab_button(base, subject_type, "all", "All"))
                    (tab_button(base, subject_type, "user", "User"))
                    (tab_button(base, subject_type, "guild", "Guild"))
                }

                form method="get" action={(base) "/archives"} class="flex gap-2" {
                    input type="hidden" name="subject_type" value=(subject_type);
                    input type="text" name="subject_id"
                        value=[subject_id]
                        placeholder="Subject ID (user or guild)"
                        class="flex-1 rounded-lg border border-neutral-300 bg-white \
                               text-neutral-900 text-sm px-3 py-2 \
                               focus:border-brand-primary focus:outline-none \
                               focus:ring-2 focus:ring-brand-primary/20"
                        autocomplete="off";
                    (submit_button("Search"))
                }
            }

            @if let Some(err) = error {
                (error_alert(err))
            } @else if archives.is_empty() {
                (empty_state(&format!(
                    "No archives found{filter_hint}. Request an archive from a \
                     user or guild detail page."
                )))
            } @else {
                (archive_table(archives, base))
            }
        }
    };
    admin_layout(config, auth, "Archives", "archives", None, content)
}
