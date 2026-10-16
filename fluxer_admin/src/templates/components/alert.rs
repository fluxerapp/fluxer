// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

#[derive(Clone, Copy)]
pub enum AlertVariant {
    Info,
    Success,
}

pub fn alert(variant: AlertVariant, title: Option<&str>, content: Markup) -> Markup {
    let variant_classes = match variant {
        AlertVariant::Info => "bg-blue-50 border-blue-200 text-blue-700",
        AlertVariant::Success => "bg-green-50 border-green-200 text-green-700",
    };
    html! {
        div class={"rounded-lg border p-4 " (variant_classes)} {
            @if let Some(title_text) = title {
                div class="mb-2 font-bold" { (title_text) }
            }
            div { (content) }
        }
    }
}

pub fn alert_info(content: Markup) -> Markup {
    alert(AlertVariant::Info, None, content)
}
