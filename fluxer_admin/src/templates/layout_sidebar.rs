// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{acl, config::AdminConfig, templates::components::badge::badge};
use maud::{Markup, PreEscaped, html};

use super::layout_scripts::SIDEBAR_ACTIVE_SCROLL_SCRIPT;
pub use super::layout_sidebar_nav::{NAV_SECTIONS, NavItem, NavSection};

pub fn render_sidebar(
    config: &AdminConfig,
    admin_acls: &[String],
    active_page: &str,
    inspected_voice_region_id: Option<&str>,
    pending_user_registrations: Option<i32>,
) -> Markup {
    let base = &config.base_path;
    html! {
        aside id="admin-sidebar" data-sidebar="" class="fixed inset-y-0 left-0 z-40 flex h-[100dvh] w-72 max-w-[85vw] -translate-x-full transform flex-col bg-neutral-900 text-white shadow-2xl transition-transform duration-200 ease-in-out lg:static lg:inset-auto lg:w-64 lg:max-w-none lg:translate-x-0 lg:shadow-none" role="navigation" aria-label="Admin navigation" tabindex="-1" {
            div class="flex items-center justify-between gap-3 border-neutral-800 border-b px-5 py-4 lg:px-6 lg:py-6" {
                a href={(base) "/users"} class="rounded focus:outline-none focus-visible:ring-2 focus-visible:ring-white/70 focus-visible:ring-offset-2 focus-visible:ring-offset-neutral-900" {
                    h1 class="font-semibold text-base" { "Fluxer Admin" }
                }
                button type="button" data-sidebar-close="" class="inline-flex h-11 w-11 items-center justify-center rounded-md text-neutral-200 transition-colors hover:bg-neutral-800 hover:text-white focus:outline-none focus-visible:ring-2 focus-visible:ring-white/70 lg:hidden" aria-label="Close sidebar" {
                    svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" class="h-5 w-5" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" {
                        line x1="18" y1="6" x2="6" y2="18" {}
                        line x1="6" y1="6" x2="18" y2="18" {}
                    }
                }
            }
            nav data-sidebar-nav="" class="sidebar-scrollbar flex-1 space-y-4 overflow-y-auto p-4" aria-label="Admin sections" {
                @for section in NAV_SECTIONS {
                    @let visible_items: Vec<_> = section.items.iter()
                        .filter(|item| !(item.hosted_only && config.self_hosted))
                        .filter(|item| acl::has_any_permission(admin_acls, item.required_acls))
                        .filter(|item| {
                            item.active_key != "voice-servers" || inspected_voice_region_id.is_some()
                        })
                        .collect();
                    @if !visible_items.is_empty() {
                        div {
                            div class="mb-2 text-neutral-400 text-xs uppercase" { (section.title) }
                            div class="space-y-1" {
                                @for item in &visible_items {
                                    @let active = active_page == item.active_key;
                                    @let base_classes = "block min-h-[44px] px-3 py-2.5 rounded text-sm transition-colors focus:outline-none focus-visible:ring-2 focus-visible:ring-white/70 focus-visible:ring-offset-2 focus-visible:ring-offset-neutral-900";
                                    @let item_path = if item.active_key == "voice-servers" {
                                        if let Some(region_id) = inspected_voice_region_id {
                                            let encoded = urlencoding::encode(region_id);
                                            format!("/voice-servers?region_id={encoded}")
                                        } else {
                                            item.path.to_owned()
                                        }
                                    } else {
                                        item.path.to_owned()
                                    };
                                    @let classes = if active {
                                        format!("{base_classes} bg-neutral-800 text-white")
                                    } else {
                                        format!("{base_classes} text-neutral-300 hover:bg-neutral-800 hover:text-white")
                                    };
                                    a href={(base) (item_path)} class=(classes) aria-current=[active.then_some("page")] data-active=[active.then_some("")] {
                                        (item.title)
                                        @if item.active_key == "pending" && pending_user_registrations.unwrap_or(0) > 0 {
                                            div class="float-right" {(badge(&pending_user_registrations.unwrap().to_string(), crate::templates::components::badge::BadgeVariant::Info))}
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            script defer { (PreEscaped(SIDEBAR_ACTIVE_SCROLL_SCRIPT)) }
        }
    }
}
