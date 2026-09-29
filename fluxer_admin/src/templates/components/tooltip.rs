// SPDX-License-Identifier: AGPL-3.0-or-later

use super::icons::paperclip_icon;
use maud::{Markup, html};

pub struct HintLink<'a> {
    href: &'a str,
    label: &'a str,
}

// Validation for links
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

pub fn info(hint: &Hint<'_>, base_path: &str) -> Markup {
    let aria_label = match hint.name {
        Some(name) => format!("About {name}"),
        None => "More information".to_owned(),
    };
    html! {
        span class="group relative inline-flex items-center" {
            button type="button" aria-label=(aria_label)
                class="flex h-4 w-4 shrink-0 cursor-help items-center justify-center rounded-full \
                       font-semibold text-brand-primary text:lg leading-none \
                                             hover:text-brand-primary-dark hover:ring-brand-primary/60" {
                "?"
            }
            div class="pointer-events-none invisible absolute bottom-full left-2 z-30 w-64 \
                      translate-y-1 pb-1 opacity-0 transition-[opacity,transform,visibility] \
                      duration-150 ease-out motion-reduce:transition-none \
                      group-hover:pointer-events-auto group-hover:visible group-hover:translate-y-0 \
                      group-hover:opacity-100 group-focus-within:pointer-events-auto \
                      group-focus-within:visible group-focus-within:translate-y-0 \
                      group-focus-within:opacity-100" {
                div class="rounded-lg border border-neutral-200 bg-white p-3 text-neutral-600 \
                          text-xs shadow-lg" {
                    @if let Some(name) = hint.name {
                        p class="font-semibold text-neutral-900" { (name) }
                    }
                    p class=[hint.name.is_some().then_some("mt-1")] { (hint.body) }
                    @if let Some(link) = &hint.link {
                        a href={(base_path) (link.href)} hx-boost="false"
                            class="mt-2 inline-block text-blue-600 hover:underline" {
                            (paperclip_icon(""))(link.label)
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::HintLink;

    #[test]
    #[should_panic(expected = "anchor")]
    fn rejects_a_link_that_points_at_no_anchor() {
        let _ = HintLink::new("/instance-config", "Instance policy");
    }

    #[test]
    #[should_panic(expected = "label")]
    fn rejects_a_link_with_no_label() {
        let _ = HintLink::new("/instance-config#community-creation", "  ");
    }
}
