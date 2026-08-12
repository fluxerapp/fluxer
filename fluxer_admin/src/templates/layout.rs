// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{api::types::FlashLevel, config::AdminConfig, middleware::auth::AuthContext};
use maud::{DOCTYPE, Markup, PreEscaped, html};

use super::components::drawer::drawer_controller_script;
use super::layout_header::render_header;
use super::layout_scripts::{
    ADMIN_ACTION_FORM_SCRIPT, COPY_TO_CLIPBOARD_SCRIPT, HTMX_FLASH_SCRIPT,
    HTMX_SCROLL_PRESERVER_SCRIPT, SH_LINK_REWRITE_SCRIPT, SIDEBAR_SCRIPT,
};
use super::layout_sidebar::render_sidebar;

fn cache_busted_asset(base_path: &str, asset_version: &str, path: &str) -> String {
    format!("{base_path}{path}?t={asset_version}")
}

#[derive(Default)]
pub struct LayoutOptions<'a> {
    pub auto_refresh: bool,
    pub extra_scripts: Option<&'a str>,
    pub inspected_voice_region_id: Option<&'a str>,
    pub csrf_token: &'a str,
}

pub fn admin_layout(
    config: &AdminConfig,
    auth: &AuthContext,
    title: &str,
    active_page: &str,
    flash: Option<&crate::api::types::FlashMessage>,
    content: Markup,
) -> Markup {
    admin_layout_ext(
        config,
        auth,
        title,
        active_page,
        flash,
        content,
        LayoutOptions::default(),
    )
}

pub fn admin_layout_ext(
    config: &AdminConfig,
    auth: &AuthContext,
    title: &str,
    active_page: &str,
    flash: Option<&crate::api::types::FlashMessage>,
    content: Markup,
    options: LayoutOptions<'_>,
) -> Markup {
    let admin_acls = auth
        .admin_user
        .as_ref()
        .map(|u| u.acls.as_slice())
        .unwrap_or(&[]);
    let base = &config.base_path;
    let asset_version = &config.build_version;
    let csrf_token = options.csrf_token;

    html! {
        (DOCTYPE)
        html lang="en" data-base-path=(base) {
            head {
                meta charset="UTF-8";
                meta name="viewport" content="width=device-width, initial-scale=1.0";
                @if options.auto_refresh {
                    meta http-equiv="refresh" content="3";
                }
                title { (title) " ~ Fluxer Admin" }
                link rel="stylesheet" href={(base) "/static/fonts/" (crate::fonts::STYLESHEET_FILE_NAME)};
                link rel="stylesheet" href=(cache_busted_asset(base, asset_version, "/static/app.css"));
                link rel="icon" type="image/x-icon" href={(config.static_cdn_endpoint) "/web/favicon.ico"};
                link rel="apple-touch-icon" href={(config.static_cdn_endpoint) "/web/apple-touch-icon.png"};
                link rel="icon" type="image/png" sizes="32x32" href={(config.static_cdn_endpoint) "/web/favicon-32x32.png"};
                link rel="icon" type="image/png" sizes="16x16" href={(config.static_cdn_endpoint) "/web/favicon-16x16.png"};
                script src=(cache_busted_asset(base, asset_version, "/static/htmx.min.js")) defer {}
            }
            body class="min-h-screen overflow-hidden bg-neutral-50" hx-boost="true" {
                a href="#main-content" class="skip-link" { "Skip to main content" }
                div class="flex h-[100dvh]" {
                    (render_sidebar(config, admin_acls, active_page, options.inspected_voice_region_id))
                    div data-sidebar-overlay="" class="pointer-events-none fixed inset-0 z-30 bg-black/50 opacity-0 transition-opacity duration-200 ease-in-out lg:hidden" aria-hidden="true" {}
                    div class="flex h-[100dvh] w-full flex-1 flex-col overflow-y-auto" {
                        (render_header(config, auth, csrf_token))
                        main id="main-content" tabindex="-1" class="flex-1 px-3 py-4 sm:px-6 sm:py-6 lg:px-8 lg:py-8" {
                            div class="mx-auto w-full max-w-7xl" {
                                div id="flash-container" class="empty:hidden" {
                                    @if let Some(flash) = flash {
                                        div class="mb-4 sm:mb-6" {
                                            (render_flash(flash))
                                        }
                                    }
                                }
                                (content)
                            }
                        }
                    }
                }
                script defer { (PreEscaped(SIDEBAR_SCRIPT)) }
                script defer { (PreEscaped(SH_LINK_REWRITE_SCRIPT)) }
                script defer { (PreEscaped(ADMIN_ACTION_FORM_SCRIPT)) }
                script defer { (PreEscaped(HTMX_SCROLL_PRESERVER_SCRIPT)) }
                script defer { (PreEscaped(HTMX_FLASH_SCRIPT)) }
                script defer { (PreEscaped(COPY_TO_CLIPBOARD_SCRIPT)) }
                (drawer_controller_script())
                @if let Some(extra) = options.extra_scripts {
                    script defer { (PreEscaped(extra)) }
                }
            }
        }
    }
}

fn render_flash(flash: &crate::api::types::FlashMessage) -> Markup {
    let classes = match flash.level {
        FlashLevel::Success => "bg-green-50 text-green-800 border-green-200",
        FlashLevel::Error => "bg-red-50 text-red-800 border-red-200",
        FlashLevel::Info => "bg-blue-50 text-blue-800 border-blue-200",
    };
    html! {
        div class={"rounded-lg border px-4 py-3 text-sm " (classes)} {
            div { (flash.message) }
        }
    }
}

pub fn flash_container() -> Markup {
    html! {
        div id="flash-container" class="mb-4 sm:mb-6 empty:hidden" {}
    }
}
