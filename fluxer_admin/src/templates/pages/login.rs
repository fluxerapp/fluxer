// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::AdminConfig;
use maud::{DOCTYPE, Markup, html};

pub fn login_page(config: &AdminConfig, error_message: Option<&str>) -> Markup {
    let base = &config.base_path;
    html! {
        (DOCTYPE)
        html lang="en" {
            head {
                meta charset="UTF-8";
                meta name="viewport" content="width=device-width, initial-scale=1.0";
                title { "Login ~ Fluxer Admin" }
                link rel="stylesheet" href={(base) "/static/fonts/" (crate::fonts::STYLESHEET_FILE_NAME)};
                link rel="stylesheet" href={(base) "/static/app.css"};
                link rel="icon" type="image/x-icon" href={(config.static_cdn_endpoint) "/web/favicon.ico"};
            }
            body class="flex min-h-[100dvh] items-center justify-center bg-neutral-50 p-4" {
                main class="w-full max-w-sm" {
                    div class="flex flex-col gap-4 items-stretch" {
                        div class="rounded-lg bg-white transition-all border border-neutral-200 p-8" {
                            div class="flex flex-col gap-8 items-center" {
                                div class="flex flex-col gap-2 items-center" {
                                    h1 class="text-gray-900 tracking-tight text-xl" {
                                        "Fluxer Admin"
                                    }
                                }
                                @if let Some(error) = error_message {
                                    div class="rounded-lg border p-4 bg-red-50 border-red-200 text-red-700" {
                                        div { (error) }
                                    }
                                }
                                a href={(base) "/auth/start"} role="button"
                                    class="inline-flex items-center justify-center gap-2 \
                                           font-medium rounded-lg transition-all duration-150 \
                                           focus:outline-none focus:ring-2 focus:ring-offset-2 \
                                           bg-neutral-900 text-white hover:bg-neutral-800 \
                                           px-4 py-2 text-base w-full sm:w-fit \
                                           focus:ring-offset-white" {
                                    span { "Sign in with Fluxer" }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
