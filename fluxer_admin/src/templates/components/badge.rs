// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

#[derive(Clone, Copy)]
pub enum BadgeVariant {
    Default,
    Success,
    Warning,
    Danger,
    Info,
}

const BADGE_BASE_CLASS: &str =
    "inline-flex items-center rounded-full px-2.5 py-0.5 font-medium text-xs ";

pub fn badge(label: &str, variant: BadgeVariant) -> Markup {
    let variant_class = match variant {
        BadgeVariant::Default => "bg-neutral-100 text-neutral-700 border-neutral-200",
        BadgeVariant::Success => "bg-green-100 text-green-700 border-green-200",
        BadgeVariant::Warning => "bg-yellow-100 text-yellow-700 border-yellow-200",
        BadgeVariant::Danger => "bg-red-100 text-red-700 border-red-200",
        BadgeVariant::Info => "bg-blue-100 text-blue-700 border-blue-200",
    };
    let classes = format!("{BADGE_BASE_CLASS}{variant_class}");
    html! {
        span class=(classes) {
            (label)
        }
    }
}
