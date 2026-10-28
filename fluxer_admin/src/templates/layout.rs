// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{FlashLevel, PremiumBranding},
    config::AdminConfig,
    middleware::auth::AuthContext,
};
use maud::{DOCTYPE, Markup, PreEscaped, html};

use super::components::drawer::drawer_controller_script;
use super::layout_header::render_header;
use super::layout_scripts::{
    ADMIN_ACTION_FORM_SCRIPT, COPY_TO_CLIPBOARD_SCRIPT, ENFORCEMENT_FIELDS_SCRIPT,
    HTMX_FLASH_SCRIPT, HTMX_SCROLL_PRESERVER_SCRIPT, SH_LINK_REWRITE_SCRIPT, SIDEBAR_SCRIPT,
    THEME_INIT_SCRIPT, THEME_TOGGLE_SCRIPT,
};
use super::layout_sidebar::render_sidebar;

const DEFAULT_PRODUCT_NAME: &str = "Fluxer";
const LIGHT_SCHEME_MEDIA: &str = "(prefers-color-scheme: light)";
const DARK_SCHEME_MEDIA: &str = "(prefers-color-scheme: dark)";

pub fn product_name(branding: Option<&PremiumBranding>) -> &str {
    branding
        .and_then(|branding| branding.product_name.as_deref())
        .unwrap_or(DEFAULT_PRODUCT_NAME)
}

pub fn admin_title(branding: Option<&PremiumBranding>) -> String {
    format!("{} Admin", product_name(branding))
}

pub fn favicon_links(config: &AdminConfig, branding: Option<&PremiumBranding>) -> Markup {
    let cdn = &config.static_cdn_endpoint;
    let favicon = branding.and_then(|branding| branding.favicon_url.as_deref());
    let favicon_dark = branding.and_then(|branding| branding.favicon_dark_url.as_deref());
    html! {
        @match (favicon, favicon_dark) {
            (Some(favicon_url), None) => {
                link rel="icon" href=(favicon_url);
            }
            (None, None) => {
                link rel="icon" type="image/x-icon" href={(cdn) "/web/favicon.ico"};
                link rel="apple-touch-icon" href={(cdn) "/web/apple-touch-icon.png"};
                link rel="icon" type="image/png" sizes="32x32" href={(cdn) "/web/favicon-32x32.png"};
                link rel="icon" type="image/png" sizes="16x16" href={(cdn) "/web/favicon-16x16.png"};
            }
            (favicon_url, Some(favicon_dark_url)) => {
                @if let Some(favicon_url) = favicon_url {
                    link rel="icon" href=(favicon_url) media=(LIGHT_SCHEME_MEDIA);
                } @else {
                    link rel="icon" type="image/png" sizes="32x32" href={(cdn) "/web/favicon-32x32.png"} media=(LIGHT_SCHEME_MEDIA);
                    link rel="apple-touch-icon" href={(cdn) "/web/apple-touch-icon.png"};
                }
                link rel="icon" href=(favicon_dark_url) media=(DARK_SCHEME_MEDIA);
            }
        }
    }
}

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
    let admin_acls = auth.acls();
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
                title { (title) " ~ " (admin_title(auth.branding.as_ref())) }
                script { (PreEscaped(THEME_INIT_SCRIPT)) }
                link rel="stylesheet" href={(base) "/static/fonts/" (crate::fonts::STYLESHEET_FILE_NAME)};
                link rel="stylesheet" href=(cache_busted_asset(base, asset_version, "/static/app.css"));
                (favicon_links(config, auth.branding.as_ref()))
                script src=(cache_busted_asset(base, asset_version, "/static/htmx.min.js")) defer {}
            }
            body class="min-h-screen overflow-hidden bg-neutral-50" hx-boost="true" {
                a href="#main-content" class="skip-link" { "Skip to main content" }
                div class="flex h-[100dvh]" {
                    (render_sidebar(config, &admin_title(auth.branding.as_ref()), admin_acls, active_page, options.inspected_voice_region_id, auth.pending_registration_count))
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
                script defer { (PreEscaped(THEME_TOGGLE_SCRIPT)) }
                script defer { (PreEscaped(ENFORCEMENT_FIELDS_SCRIPT)) }
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
