// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::Archive,
    templates::components::form::{checkbox, csrf_input, submit_button},
};
use maud::{Markup, html};

pub fn archives_tab(
    base: &str,
    title: &str,
    action: &str,
    empty_message: &str,
    archives: &[Archive],
    csrf_token: &str,
) -> Markup {
    html! {
        div class="space-y-6" {
            div class="flex flex-wrap items-center justify-between gap-3" {
                h2 class="text-xl font-semibold text-neutral-900" {
                    (title)
                }
                form method="post"
                    action=(action)
                    class="flex flex-wrap items-center gap-3" {
                    (csrf_input(csrf_token))
                    (checkbox("include_attachments", "true", "Include attachments", false, true))
                    (submit_button("Trigger Archive"))
                }
            }
            @if archives.is_empty() {
                div class="flex flex-col items-center justify-center py-12 text-center" {
                    p class="text-sm font-medium text-neutral-900" {
                        (empty_message)
                    }
                }
            } @else {
                (archives_table(base, archives))
            }
        }
    }
}

fn status_text(archive: &Archive) -> &str {
    if archive.failed_at.is_some() {
        return "Failed";
    }
    if archive.completed_at.is_some() {
        return "Completed";
    }
    archive.progress_step.as_deref().unwrap_or("In Progress")
}

fn archives_table(base: &str, archives: &[Archive]) -> Markup {
    html! {
        div class="overflow-x-auto rounded-lg border border-neutral-200 bg-white" {
            table class="min-w-full divide-y divide-neutral-200" {
                thead class="bg-neutral-50" {
                    tr {
                        th class="whitespace-nowrap px-4 py-2 text-left text-xs font-medium uppercase tracking-wider text-neutral-700" {
                            "Requested At"
                        }
                        th class="whitespace-nowrap px-4 py-2 text-left text-xs font-medium uppercase tracking-wider text-neutral-700" {
                            "Status"
                        }
                        th class="whitespace-nowrap px-4 py-2 text-left text-xs font-medium uppercase tracking-wider text-neutral-700" {
                            "Actions"
                        }
                    }
                }
                tbody class="divide-y divide-neutral-200" {
                    @for archive in archives {
                        tr {
                            td class="whitespace-nowrap px-4 py-3 text-sm text-neutral-900" {
                                (archive.requested_at)
                            }
                            td class="px-4 py-3 text-sm text-neutral-900" {
                                (status_text(archive))
                                " (" (archive.progress_percent) "%)"
                            }
                            td class="px-4 py-3 text-sm" {
                                @if archive.completed_at.is_some() {
                                    a href={(base) "/archives/download?subject_type=" (archive.subject_type) "&subject_id=" (archive.subject_id) "&archive_id=" (archive.archive_id)}
                                        class="inline-flex items-center rounded-md bg-brand-primary px-3 py-1.5 text-sm font-medium text-white hover:bg-brand-primary-dark" {
                                        "Download"
                                    }
                                } @else {
                                    span class="text-sm text-neutral-500" {
                                        "Pending"
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
