// SPDX-License-Identifier: AGPL-3.0-or-later

use super::icons::paperclip_icon;
use maud::{Markup, html};
use std::sync::atomic::{AtomicUsize, Ordering};

static HINT_TOGGLE_ID: AtomicUsize = AtomicUsize::new(0);

pub struct HintLink<'a> {
    href: &'a str,
    label: &'a str,
}

impl<'a> HintLink<'a> {
    pub fn new(href: &'a str, label: &'a str) -> Self {
        debug_assert!(
            href.starts_with('/'),
            "hint link href must be admin-absolute: {href:?}"
        );
        debug_assert!(
            href.contains('#'),
            "hint link href should point at an anchor: {href:?}"
        );
        debug_assert!(!label.trim().is_empty(), "hint link needs a label");
        Self { href, label }
    }
}

pub struct Hint<'a> {
    pub name: Option<&'a str>,
    pub body: &'a str,
    pub link: Option<HintLink<'a>>,
}

pub fn info(base: &str, hint: &Hint<'_>) -> Markup {
    let aria_label = match hint.name {
        Some(name) => format!("About {name}"),
        None => "More information".to_owned(),
    };
    let toggle_id = format!(
        "hint-toggle-{}",
        HINT_TOGGLE_ID.fetch_add(1, Ordering::Relaxed)
    );
    html! {
        span class="group relative inline-flex items-center" {
            input type="checkbox" id=(toggle_id) class="peer sr-only";
            label for=(toggle_id) tabindex="0" aria-label=(aria_label)
                class="flex h-4 w-4 shrink-0 cursor-pointer items-center justify-center rounded-full \
                       font-semibold text-brand-primary leading-none active:scale-97 \
                       hover:text-brand-primary-dark" {
                "?"
            }
            label for=(toggle_id) aria-hidden="true"
                class="invisible fixed inset-0 z-20 cursor-default peer-checked:visible" {}
            div class="invisible absolute bottom-full left-2 z-30 w-64 pb-3 pl-2 opacity-0 \
                      transition-[opacity,visibility] duration-200 ease-out motion-reduce:transition-none \
                      group-hover:visible group-hover:opacity-100 \
                      group-focus-within:visible group-focus-within:opacity-100 \
                      peer-checked:visible peer-checked:opacity-100" {
                div class="rounded-lg border border-neutral-200 bg-white p-3 text-neutral-600 \
                          text-xs shadow-lg" {
                    @if let Some(name) = hint.name {
                        p class="font-semibold text-neutral-900" { (name) }
                    }
                    p class=[hint.name.is_some().then_some("mt-1")] { (hint.body) }
                    @if let Some(link) = &hint.link {
                        a href={(base) (link.href)} hx-boost="false"
                            class="mt-2 inline-flex items-center gap-1 text-blue-600 hover:underline" {
                            (paperclip_icon(""))(link.label)
                        }
                    }
                }
            }
        }
    }
}
