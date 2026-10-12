// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::Application, config::AdminConfig,
    templates::components::page_container::card_with_header,
};
use maud::{Markup, html};

pub fn applications_tab(
    config: &AdminConfig,
    applications: &[Application],
    empty_title: &str,
    empty_body: &str,
) -> Markup {
    let base = &config.base_path;
    html! {
        (card_with_header(
            &format!("Applications ({})", applications.len()),
            html! {
                @if applications.is_empty() {
                    div class="flex flex-col items-center justify-center py-12 text-center" {
                        p class="text-sm font-medium text-neutral-900" {
                            (empty_title)
                        }
                        p class="mt-1 text-sm text-neutral-500" {
                            (empty_body)
                        }
                    }
                } @else {
                    div class="space-y-4" {
                        @for app in applications {
                            (application_card(base, app))
                        }
                    }
                }
            },
        ))
    }
}

fn application_card(base: &str, app: &Application) -> Markup {
    let bot_display = match (&app.bot_user_id, &app.bot_username, &app.bot_discriminator) {
        (Some(bid), Some(bname), Some(bdisc)) => {
            let display = match &app.bot_global_name {
                Some(gn) if !gn.trim().is_empty() => {
                    format!("{} ({}#{})", gn, bname, bdisc)
                }
                _ => format!("{}#{}", bname, bdisc),
            };
            Some((bid.clone(), display))
        }
        (Some(bid), _, _) => Some((bid.clone(), bid.clone())),
        _ => None,
    };

    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-4" {
            div class="flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between" {
                div class="min-w-0 flex-1 space-y-2" {
                    div class="flex flex-wrap items-center gap-2" {
                        h3 class="text-base font-medium text-neutral-900" {
                            (app.name)
                        }
                        @if app.bot_user_id.is_some() {
                            span class="rounded bg-blue-100 px-2 py-0.5 text-xs \
                                        font-medium uppercase text-blue-700" {
                                "Bot"
                            }
                        } @else {
                            span class="rounded bg-neutral-200 px-2 py-0.5 text-xs \
                                        font-medium uppercase text-neutral-700" {
                                "OAuth2 App"
                            }
                        }
                    }
                    p class="break-all text-sm text-neutral-500" {
                        (app.id)
                    }
                    @if let Some((ref bid, ref display)) = bot_display {
                        p class="text-sm text-neutral-500" {
                            "Bot user: "
                            a href={(base) "/users/" (bid)}
                                class="transition-colors hover:text-blue-600 \
                                       hover:underline" {
                                (display)
                            }
                        }
                    } @else {
                        p class="text-sm text-neutral-500" {
                            "No bot user (OAuth2 only)"
                        }
                    }
                    p class="text-sm text-neutral-500" {
                        (app.oauth2_redirect_uris.len()) " redirect URI"
                        @if app.oauth2_redirect_uris.len() != 1 { "s" }
                        " \u{00b7} Client secret "
                        @if app.has_client_secret { "set" } @else { "not set" }
                        " \u{00b7} Bot token "
                        @if app.has_bot_token { "set" } @else { "not set" }
                    }
                }
                a href={(base) "/applications/" (app.id)}
                    class="inline-flex items-center rounded-md bg-brand-primary px-4 \
                           py-2 text-sm font-medium text-white shadow-sm \
                           hover:bg-brand-primary-dark" {
                    "View Details"
                }
            }
        }
    }
}
