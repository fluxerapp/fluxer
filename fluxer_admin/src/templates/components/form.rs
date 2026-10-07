// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, PreEscaped, html};

pub const FORM_CONTROL_CLASS: &str = "w-full rounded-lg border border-neutral-300 bg-white \
     text-neutral-900 text-sm transition-all placeholder:text-neutral-400 \
     focus:border-brand-primary focus:outline-none focus:ring-2 \
     focus:ring-brand-primary/20 disabled:cursor-not-allowed \
     disabled:bg-neutral-50 disabled:opacity-50";
const FORM_INPUT_SIZE_CLASS: &str = "h-8 px-3 py-1.5";
pub const FORM_SEARCH_INPUT_SIZE_CLASS: &str = "h-10 px-3 py-1.5";
const FORM_SELECT_SIZE_CLASS: &str = "h-8 appearance-none px-3 py-1.5 pr-10";
const FORM_TEXTAREA_SIZE_CLASS: &str = "px-3 py-1.5";
pub const FORM_LABEL_CLASS: &str = "font-semibold text-neutral-500 text-xs uppercase tracking-wide";
pub const FORM_INPUT_CLASS: &str = "w-full rounded-lg border border-neutral-300 bg-white \
     text-neutral-900 text-sm transition-all placeholder:text-neutral-400 \
     focus:border-brand-primary focus:outline-none focus:ring-2 \
     focus:ring-brand-primary/20 disabled:cursor-not-allowed \
     disabled:bg-neutral-50 disabled:opacity-50 h-8 px-3 py-1.5";
pub const FORM_TEXTAREA_CLASS: &str = "w-full rounded-lg border border-neutral-300 bg-white \
     text-neutral-900 text-sm transition-all placeholder:text-neutral-400 \
     focus:border-brand-primary focus:outline-none focus:ring-2 \
     focus:ring-brand-primary/20 disabled:cursor-not-allowed \
     disabled:bg-neutral-50 disabled:opacity-50 px-3 py-1.5";
pub const FORM_SELECT_CLASS: &str = "w-full rounded-lg border border-neutral-300 bg-white \
     text-neutral-900 text-sm transition-all placeholder:text-neutral-400 \
     focus:border-brand-primary focus:outline-none focus:ring-2 \
     focus:ring-brand-primary/20 disabled:cursor-not-allowed \
     disabled:bg-neutral-50 disabled:opacity-50 h-8 appearance-none px-3 py-1.5 pr-10";
const BUTTON_BASE_CLASS: &str = "inline-flex items-center justify-center gap-2 font-medium \
     rounded-lg transition-all duration-150 focus:outline-none focus:ring-2 \
     focus:ring-offset-2 w-fit px-4 py-2 text-base";
const PRIMARY_BUTTON_CLASS: &str =
    "bg-neutral-900 text-white hover:bg-neutral-800 focus:ring-offset-white";
const DANGER_BUTTON_CLASS: &str = "bg-red-600 text-white hover:bg-red-700 focus:ring-offset-white";
const SECONDARY_BUTTON_CLASS: &str = "bg-neutral-50 text-neutral-700 hover:text-neutral-900 \
     border border-neutral-300 hover:border-neutral-400";

fn button_markup(label: &str, button_type: &str, variant_class: &str) -> Markup {
    html! {
        button type=(button_type) class={(BUTTON_BASE_CLASS) " " (variant_class)} {
            span { (label) }
        }
    }
}

pub fn text_input(name: &str, label: &str, value: &str, placeholder: &str) -> Markup {
    html! {
        div class="flex flex-col gap-2" {
            label for=(name) class=(FORM_LABEL_CLASS) {
                (label)
            }
            input type="text" id=(name) name=(name) value=(value)
                placeholder=(placeholder)
                class={(FORM_CONTROL_CLASS) " " (FORM_INPUT_SIZE_CLASS)};
        }
    }
}

pub fn search_input(
    name: &str,
    value: &str,
    placeholder: &str,
    action: &str,
    target_id: &str,
) -> Markup {
    search_input_ext(name, value, placeholder, action, target_id, None)
}

pub fn search_input_ext(
    name: &str,
    value: &str,
    placeholder: &str,
    action: &str,
    target_id: &str,
    hx_swap: Option<&str>,
) -> Markup {
    html! {
        form method="get" action=(action)
            class="flex flex-col gap-3 sm:flex-row sm:items-center" {
            div class="flex flex-1 flex-col gap-2 sm:flex-row" {
                div class="flex-1" {
                    input type="search" name=(name) value=(value) placeholder=(placeholder)
                        class={(FORM_CONTROL_CLASS) " " (FORM_SEARCH_INPUT_SIZE_CLASS)}
                        autocomplete="off"
                        hx-get=(action)
                        hx-trigger="input changed delay:300ms, search"
                        hx-target={"#" (target_id)}
                        hx-push-url="true"
                        hx-include="this"
                        hx-swap=[hx_swap];
                }
            }
            div class="flex flex-col gap-2 sm:shrink-0 sm:flex-row" {
                (submit_button("Search"))
            }
        }
    }
}

pub fn select_chevron() -> Markup {
    html! {
        svg class="pointer-events-none absolute top-1/2 right-3 h-4 w-4 -translate-y-1/2 text-neutral-500"
            xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 20 20" {
            path stroke="currentColor" stroke-linecap="round"
                stroke-linejoin="round" stroke-width="1.5"
                d="m6 8 4 4 4-4" {}
        }
    }
}

pub fn select_input(name: &str, label: &str, options: &[(&str, &str)], selected: &str) -> Markup {
    html! {
        div class="flex flex-col gap-2" {
            label for=(name) class=(FORM_LABEL_CLASS) {
                (label)
            }
            div class="relative" {
                select id=(name) name=(name) class={(FORM_CONTROL_CLASS) " " (FORM_SELECT_SIZE_CLASS)} {
                    @for (value, display) in options {
                        option value=(value) selected[*value == selected] {
                            (display)
                        }
                    }
                }
                (select_chevron())
            }
        }
    }
}

pub fn csrf_input(token: &str) -> Markup {
    html! {
        input type="hidden" name="_csrf" value=(token);
    }
}

pub fn form_actions(content: Markup) -> Markup {
    html! {
        div class="flex flex-wrap justify-start gap-2" {
            (content)
        }
    }
}

pub fn submit_button(label: &str) -> Markup {
    button_markup(label, "submit", PRIMARY_BUTTON_CLASS)
}

pub fn danger_button(label: &str) -> Markup {
    button_markup(label, "submit", DANGER_BUTTON_CLASS)
}

pub fn secondary_button(label: &str) -> Markup {
    button_markup(label, "button", SECONDARY_BUTTON_CLASS)
}

pub fn form_field_group(
    label: &str,
    name: &str,
    required: bool,
    error: Option<&str>,
    helper: Option<&str>,
    content: Markup,
) -> Markup {
    html! {
        div class="flex flex-col gap-2" {
            label for=(name) class=(FORM_LABEL_CLASS) {
                (label)
                @if required {
                    span class="text-red-500 ml-0.5" { "*" }
                }
            }
            (content)
            @if let Some(helper_text) = helper {
                p class="text-xs text-neutral-500" { (helper_text) }
            }
            @if let Some(err) = error {
                p class="text-xs text-red-600" { (err) }
            }
        }
    }
}

pub fn textarea_input(
    name: &str,
    label: &str,
    placeholder: &str,
    value: &str,
    rows: u32,
    required: bool,
) -> Markup {
    html! {
        div class="flex flex-col gap-2" {
            label for=(name) class=(FORM_LABEL_CLASS) {
                (label)
                @if required {
                    span class="text-red-500 ml-0.5" { "*" }
                }
            }
            textarea id=(name) name=(name) placeholder=(placeholder) rows=(rows)
                class={(FORM_CONTROL_CLASS) " " (FORM_TEXTAREA_SIZE_CLASS)}
                required[required] {
                (value)
            }
        }
    }
}

const CHECKMARK_SVG: &str = r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" style="stroke-width:32"><polyline points="40 144 96 200 224 72" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round"/></svg>"#;

pub fn checkbox(name: &str, value: &str, label: &str, checked: bool, enabled: bool) -> Markup {
    html! {
        label class={
            "group flex w-full items-center gap-2 "
            (if enabled { "cursor-pointer" } else { "cursor-not-allowed opacity-60" })
        } {
            input type="checkbox" name=(name) value=(value)
                checked[checked]
                disabled[!enabled]
                class="hidden";
            div class="checkbox-custom" {
                (PreEscaped(CHECKMARK_SVG))
            }
            span class="min-w-0 [overflow-wrap:anywhere] text-sm leading-5 text-neutral-900" { (label) }
        }
    }
}

pub fn opt_out_checkbox(name: &str, label: &str) -> Markup {
    html! {
        input type="hidden" name={(name) "_present"} value="1";
        (checkbox(name, "true", label, true, true))
    }
}

pub fn secondary_button_link(label: &str, href: &str) -> Markup {
    html! {
        a href=(href) role="button"
            class={(BUTTON_BASE_CLASS) " " (SECONDARY_BUTTON_CLASS)} {
            span { (label) }
        }
    }
}
