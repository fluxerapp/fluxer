// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

const EMPTY_FIELD_VALUE: &str = "\u{2014}";
const DATA_FIELD_EMPTY_CLASS: &str = "text-sm text-neutral-500 italic";
const DATA_FIELD_TEXT_CLASS: &str = "text-sm text-gray-900 break-words";
const DATA_FIELD_MONO_CLASS: &str = "text-sm text-gray-900 break-all";
const DATA_FIELD_MUTED_CLASS: &str = "text-sm text-neutral-500 break-words";
const DATA_FIELD_LINK_CLASS: &str = "rounded text-blue-600 text-sm transition-colors \
     hover:text-blue-800 hover:underline focus:outline-none focus-visible:ring-2 \
     focus-visible:ring-brand-primary focus-visible:ring-offset-2";

fn data_field_value(value: &str, class: &str) -> Markup {
    if value.is_empty() {
        html! { p class=(DATA_FIELD_EMPTY_CLASS) { (EMPTY_FIELD_VALUE) } }
    } else {
        html! { p class=(class) { (value) } }
    }
}

fn data_field_link_value(href: &str, display: &str, break_class: &str) -> Markup {
    html! {
        a href=(href) class={(DATA_FIELD_LINK_CLASS) " " (break_class)} {
            (display)
        }
    }
}

pub fn data_field(label: &str, value: Markup) -> Markup {
    html! {
        div class="flex min-w-0 flex-col gap-1" {
            p class="text-xs text-gray-500" { (label) }
            (value)
        }
    }
}

pub fn data_field_text(label: &str, value: &str) -> Markup {
    data_field(label, data_field_value(value, DATA_FIELD_TEXT_CLASS))
}

pub fn data_field_mono(label: &str, value: &str) -> Markup {
    data_field(label, data_field_value(value, DATA_FIELD_MONO_CLASS))
}

pub fn data_field_link_mono(label: &str, href: &str, display: &str) -> Markup {
    data_field(label, data_field_link_value(href, display, "break-all"))
}

pub fn data_field_muted(label: &str, value: &str) -> Markup {
    data_field(label, data_field_value(value, DATA_FIELD_MUTED_CLASS))
}

pub fn data_grid(cols: u8, content: Markup) -> Markup {
    let col_class = match cols {
        1 => "grid grid-cols-1 gap-4",
        3 => "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4",
        _ => "grid grid-cols-1 sm:grid-cols-2 gap-4",
    };
    html! {
        div class=(col_class) { (content) }
    }
}
