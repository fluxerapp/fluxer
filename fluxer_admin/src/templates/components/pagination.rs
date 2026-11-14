// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

pub fn pager(prev: Option<String>, label: Option<String>, next: Option<String>) -> Markup {
    pager_with_missing(prev, label, next, false)
}

pub fn pager_with_missing(
    prev: Option<String>,
    label: Option<String>,
    next: Option<String>,
    omit_missing: bool,
) -> Markup {
    html! {
        div class="mt-4 flex items-center justify-between gap-3" {
            @if let Some(url) = prev {
                a href=(url) class="rounded-md border border-neutral-300 bg-white px-3 py-2 text-sm font-medium text-neutral-700 hover:bg-neutral-50" { "← Previous" }
            } @else if !omit_missing {
                span class="rounded-md border border-neutral-200 bg-neutral-50 px-3 py-2 text-sm text-neutral-400" { "← Previous" }
            }
            @if let Some(text) = label { span class="text-sm text-neutral-500" { (text) } }
            @if let Some(url) = next {
                a href=(url) class="rounded-md border border-neutral-300 bg-white px-3 py-2 text-sm font-medium text-neutral-700 hover:bg-neutral-50" { "Next →" }
            } @else if !omit_missing {
                span class="rounded-md border border-neutral-200 bg-neutral-50 px-3 py-2 text-sm text-neutral-400" { "Next →" }
            }
        }
    }
}
