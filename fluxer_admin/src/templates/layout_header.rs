// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::components::media::user_avatar_url,
    utils::{bigint::format_discriminator, user_tag::user_tag},
};
use maud::{Markup, html};

pub fn render_header(config: &AdminConfig, auth: &AuthContext, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    html! {
        header class="sticky top-0 z-10 flex items-center justify-between gap-3 border-neutral-200 border-b bg-white px-3 py-3 sm:gap-4 sm:px-6 sm:py-4 lg:px-8" {
            div class="flex min-w-0 flex-1 items-center gap-2 sm:gap-3" {
                button type="button" data-sidebar-toggle="" aria-label="Open navigation menu" aria-expanded="false" aria-controls="admin-sidebar" class="inline-flex h-11 w-11 flex-shrink-0 items-center justify-center rounded-md border border-neutral-300 text-neutral-700 transition-colors hover:bg-neutral-100 focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-primary lg:hidden" {
                    svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" class="h-5 w-5" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" {
                        line x1="3" y1="6" x2="21" y2="6" {}
                        line x1="3" y1="12" x2="21" y2="12" {}
                        line x1="3" y1="18" x2="21" y2="18" {}
                    }
                }
                @if let Some(ref admin) = auth.admin_user {
                    a href={(base) "/users/" (auth.session.user_id)} class="flex min-w-0 items-center gap-2 rounded transition-opacity hover:opacity-80 focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-primary focus-visible:ring-offset-2 sm:gap-3" {
                        img
                            src=(user_avatar_url(config, &admin.id, admin.avatar.as_deref(), 160, true))
                            alt=(format!("{}'s avatar", admin.username))
                            class="h-9 w-9 flex-shrink-0 rounded-full sm:h-10 sm:w-10";
                        div class="flex min-w-0 flex-col" {
                            div class="truncate text-neutral-900 text-sm" {
                                @let display = admin.global_name.as_deref()
                                    .filter(|n| !n.trim().is_empty())
                                    .unwrap_or(&admin.username);
                                (display)
                            }
                            div class="truncate text-neutral-500 text-xs" {
                                (user_tag(&admin.username, &format_discriminator(&admin.discriminator), admin.bot))
                            }
                        }
                    }
                } @else {
                    div class="min-w-0 truncate text-neutral-600 text-sm" {
                        span class="hidden sm:inline" { "Logged in as: " }
                        a href={(base) "/users/" (auth.session.user_id)} class="rounded text-blue-600 hover:text-blue-800 hover:underline focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-primary" {
                            (auth.session.user_id)
                        }
                    }
                }
            }
            form method="post" action={(base) "/logout"} class="flex-shrink-0" hx-boost="false" {
                input type="hidden" name="_csrf" value=(csrf_token);
                button type="submit" aria-label="Log out" class="inline-flex h-11 items-center justify-center rounded border border-neutral-300 px-3 font-medium text-neutral-700 text-sm transition-colors hover:border-neutral-400 hover:text-neutral-900 focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-primary sm:px-4" {
                    svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" class="h-5 w-5 sm:hidden" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" {
                        path d="M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4" {}
                        polyline points="16 17 21 12 16 7" {}
                        line x1="21" y1="12" x2="9" y2="12" {}
                    }
                    span class="hidden sm:inline" { "Logout" }
                }
            }
        }
    }
}
