// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

pub fn table_container(content: Markup) -> Markup {
    html! {
        div class="-mx-3 sm:mx-0" {
            div class="table-scroll relative overflow-x-auto rounded-none border-neutral-200 border-y bg-white sm:rounded-lg sm:border"
                role="region" aria-label="Scrollable table" {
                (content)
            }
        }
    }
}

pub fn table(content: Markup) -> Markup {
    html! {
        table class="min-w-full divide-y divide-neutral-200" {
            (content)
        }
    }
}

pub fn table_head(content: Markup) -> Markup {
    html! { thead class="bg-neutral-50" { (content) } }
}

pub fn table_body(content: Markup) -> Markup {
    html! { tbody class="divide-y divide-neutral-200 bg-white" { (content) } }
}

pub fn table_row(content: Markup) -> Markup {
    html! { tr class="transition-colors hover:bg-neutral-50" { (content) } }
}

pub fn table_header_cell(label: &str) -> Markup {
    html! {
        th class="whitespace-nowrap px-3 py-3 text-left text-neutral-600 text-xs uppercase tracking-wider sm:px-6" {
            (label)
        }
    }
}

pub fn table_cell(muted: bool, content: Markup) -> Markup {
    html! { td class=(table_cell_class(muted)) { (content) } }
}

fn table_cell_class(muted: bool) -> &'static str {
    if muted {
        "px-3 py-3 text-sm sm:px-6 sm:py-4 text-neutral-600"
    } else {
        "px-3 py-3 text-sm sm:px-6 sm:py-4 text-neutral-900"
    }
}

pub fn data_table(headers: &[&str], rows: Markup) -> Markup {
    table_container(table(html! {
        (table_head(html! {
            tr {
                @for header in headers {
                    (table_header_cell(header))
                }
            }
        }))
        (table_body(html! {
            (rows)
        }))
    }))
}

pub fn empty_state(message: &str) -> Markup {
    html! {
        div class="rounded-lg bg-white transition-all border border-neutral-200 p-12 text-center" {
            div class="flex flex-col items-center gap-4" {
                p class="text-neutral-600 text-sm" { (message) }
            }
        }
    }
}
