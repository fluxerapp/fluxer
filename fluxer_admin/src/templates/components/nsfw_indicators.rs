// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

use super::badge::{BadgeVariant, badge};

const CONTENT_WARNING_LEVEL: i32 = 1;

pub fn adult_content_badge(is_adult: bool, label: Option<&str>) -> Markup {
    if !is_adult {
        return html! {};
    }
    let display = label.unwrap_or("18+");
    html! {
        span class="self-start" title="Adult content (18+)" {
            (badge(display, BadgeVariant::Danger))
        }
    }
}

fn truncate(text: &str, max: usize) -> String {
    if text.chars().nth(max).is_none() {
        text.to_owned()
    } else {
        let boundary = max.saturating_sub(1);
        let truncated: String = text.chars().take(boundary).collect();
        format!("{truncated}\u{2026}")
    }
}

pub fn content_warning_badge(level: Option<i32>, text: Option<&str>, inline_text: bool) -> Markup {
    if level != Some(CONTENT_WARNING_LEVEL) {
        return html! {};
    }
    let default_text = "This contains sensitive content.";
    let tooltip = text
        .filter(|t| !t.trim().is_empty())
        .unwrap_or(default_text);
    let inline = if inline_text {
        truncate(tooltip, 60)
    } else {
        "Content warning".to_owned()
    };
    html! {
        span class="self-start" title=(tooltip) {
            span class="inline-flex items-center gap-1 rounded bg-amber-100 \
                        px-2 py-0.5 font-medium text-amber-800 text-xs" {
                span aria-hidden="true" { "\u{26A0}" }
                span { (inline) }
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum NsfwSource {
    Channel,
    Category,
    Community,
    None,
}

fn resolve_nsfw_source(
    channel_override: Option<bool>,
    category_override: Option<bool>,
    guild_nsfw: Option<bool>,
) -> (NsfwSource, Option<bool>) {
    if let Some(v) = channel_override {
        return (NsfwSource::Channel, Some(v));
    }
    if let Some(v) = category_override {
        return (NsfwSource::Category, Some(v));
    }
    if let Some(v) = guild_nsfw {
        return (NsfwSource::Community, Some(v));
    }
    (NsfwSource::None, None)
}

fn source_label(source: NsfwSource, explicit: Option<bool>) -> &'static str {
    match source {
        NsfwSource::Channel if explicit == Some(true) => "(override on)",
        NsfwSource::Channel => "(override off)",
        NsfwSource::Category => "(from category)",
        NsfwSource::Community => "(from community)",
        NsfwSource::None => "",
    }
}

pub fn channel_nsfw_state_badge(
    is_nsfw: bool,
    nsfw_override: Option<bool>,
    category_override: Option<bool>,
    guild_nsfw: Option<bool>,
    content_warning_level: Option<i32>,
    content_warning_text: Option<&str>,
    hide_source: bool,
) -> Markup {
    let show_cw = content_warning_level == Some(CONTENT_WARNING_LEVEL);
    if !is_nsfw && !show_cw {
        return html! {};
    }
    let has_context =
        nsfw_override.is_some() || category_override.is_some() || guild_nsfw.is_some();
    let (source, explicit) = resolve_nsfw_source(nsfw_override, category_override, guild_nsfw);
    html! {
        span class="inline-flex flex-wrap items-center gap-1" {
            @if is_nsfw {
                span {
                    (badge("NSFW", BadgeVariant::Danger))
                }
            }
            @if show_cw {
                (content_warning_badge(content_warning_level, content_warning_text, false))
            }
            @if is_nsfw && has_context && !hide_source {
                span class="text-neutral-500 text-xs" {
                    (source_label(source, explicit))
                }
            }
        }
    }
}

pub fn attachment_nsfw_badge(is_nsfw: bool) -> Markup {
    if !is_nsfw {
        return html! {};
    }
    html! {
        span class="self-start" {
            (badge("Media NSFW", BadgeVariant::Danger))
        }
    }
}
