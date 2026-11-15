// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    templates::components::{
        enforcement_fields::{FIELD_HELP_CLASS, FIELD_INPUT_CLASS, field_label},
        form::{csrf_input, form_actions, submit_button},
        section_card::section_card_simple,
    },
    utils::timestamps::format_admin_timestamp,
};
use maud::{Markup, html};

const NOTE_MAX_CHARS: usize = 2000;

pub struct StaffNote<'a> {
    pub note_id: &'a str,
    pub author: Markup,
    pub body: &'a str,
    pub created_at: &'a str,
}

pub struct StaffNoteForm<'a> {
    pub action: String,
    pub refresh_target: &'a str,
    pub field_id: String,
    pub help: &'a str,
    pub csrf_token: &'a str,
}

pub fn staff_notes_section(notes: &[StaffNote], form: Option<StaffNoteForm>) -> Markup {
    html! {
        (section_card_simple("Notes", html! {
            div class="space-y-4" {
                @if notes.is_empty() {
                    p class="text-sm text-neutral-500" { "No notes." }
                } @else {
                    ol class="space-y-3" {
                        @for note in notes {
                            li class="space-y-1" data-note=(note.note_id) {
                                p class="text-xs text-neutral-500" {
                                    (note.author)
                                    " on " (format_admin_timestamp(note.created_at))
                                }
                                p class="whitespace-pre-wrap break-words text-sm text-neutral-900" { (note.body) }
                            }
                        }
                    }
                }
                @if let Some(form) = form {
                    form method="post" action=(form.action)
                        data-admin-refresh-on-success=(form.refresh_target) {
                        (csrf_input(form.csrf_token))
                        div class="space-y-3" {
                            div class="space-y-1" {
                                (field_label(&form.field_id, "Add a note"))
                                textarea id=(form.field_id) name="body" rows="2" required maxlength=(NOTE_MAX_CHARS)
                                    class=(FIELD_INPUT_CLASS) {}
                                p class=(FIELD_HELP_CLASS) { (form.help) }
                            }
                            (form_actions(submit_button("Add note")))
                        }
                    }
                }
            }
        }))
    }
}
