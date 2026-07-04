// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, PreEscaped, html};

pub fn page_container(spacing: &str, content: Markup) -> Markup {
    let class = match spacing {
        "sm" => "space-y-4",
        "lg" => "space-y-8",
        _ => "space-y-6",
    };
    html! {
        div class=(class) { (content) }
    }
}

pub fn page_container_md(content: Markup) -> Markup {
    page_container("md", content)
}

pub fn page_header(title: &str, description: Option<&str>) -> Markup {
    page_header_full(title, description, None, None, None, maud::html! {})
}

pub fn page_header_with_actions(title: &str, description: Option<&str>, actions: Markup) -> Markup {
    page_header_full(
        title,
        description,
        Some(actions),
        None,
        None,
        maud::html! {},
    )
}

pub fn page_header_with_back(
    title: &str,
    description: Option<&str>,
    back_href: &str,
    back_label: Option<&str>,
) -> Markup {
    page_header_full(
        title,
        description,
        None,
        Some(back_href),
        back_label,
        maud::html! {},
    )
}

pub fn page_header_full(
    title: &str,
    description: Option<&str>,
    actions: Option<Markup>,
    back_href: Option<&str>,
    back_label: Option<&str>,
    children: Markup,
) -> Markup {
    let mb_class = if description.is_some() {
        "mb-2"
    } else {
        "mb-0"
    };
    html! {
        div {
            @if let Some(href) = back_href {
                a href=(href) class="-mt-1 mb-2 inline-flex items-center gap-1 rounded text-neutral-600 text-sm transition-colors hover:text-neutral-900 focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-primary focus-visible:ring-offset-2" {
                    span aria-hidden="true" { (PreEscaped("&larr;")) }
                    span { (back_label.unwrap_or("Back")) }
                }
            }
            div class={"flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between sm:gap-4 " (mb_class)} {
                div class="flex min-w-0 flex-1 flex-col gap-2" {
                    h1 class="break-words font-bold text-2xl text-gray-900 sm:text-3xl" { (title) }
                    @if let Some(desc) = description {
                        p class="text-base text-gray-600" { (desc) }
                    }
                }
                @if let Some(actions) = actions {
                    div class="flex flex-shrink-0 flex-wrap items-center gap-2 sm:ml-4" {
                        (actions)
                    }
                }
            }
            (children)
        }
    }
}

pub fn card(content: Markup) -> Markup {
    html! {
        div class="rounded-lg bg-white transition-all border border-neutral-200 p-6" {
            (content)
        }
    }
}

pub fn card_with_header(title: &str, content: Markup) -> Markup {
    html! {
        div class="rounded-lg bg-white transition-all border border-neutral-200" {
            div class="border-b border-neutral-200 px-6 py-3" {
                h3 class="font-medium text-lg text-neutral-900" {
                    (title)
                }
            }
            div class="p-6" {
                (content)
            }
        }
    }
}

pub fn detail_row(label: &str, value: Markup) -> Markup {
    html! {
        div class="flex flex-col gap-1 py-3 sm:flex-row sm:gap-4" {
            dt class="w-48 flex-shrink-0 text-sm font-medium text-neutral-500" {
                (label)
            }
            dd class="min-w-0 flex-1 text-sm text-neutral-900" {
                (value)
            }
        }
    }
}

pub fn tabs(tabs: &[(&str, &str, bool)], base_url: &str) -> Markup {
    html! {
        div class="border-b border-neutral-200 mb-6" {
            nav class="-mb-px flex gap-6" aria-label="Tabs" {
                @for (id, label, active) in tabs {
                    @let classes = if *active {
                        "border-brand-primary text-brand-primary whitespace-nowrap border-b-2 \
                         pb-3 pt-1 text-sm font-medium"
                    } else {
                        "border-transparent text-neutral-500 hover:border-neutral-300 \
                         hover:text-neutral-700 whitespace-nowrap border-b-2 pb-3 pt-1 \
                         text-sm font-medium"
                    };
                    a href={(base_url) "?tab=" (id)} class=(classes) {
                        (label)
                    }
                }
            }
        }
    }
}
