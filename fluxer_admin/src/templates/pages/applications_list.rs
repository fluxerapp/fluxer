// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::Application,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            form::search_input,
            page_container::{card, page_header, page_header_with_actions},
            table::empty_state,
            user_display::format_user_display,
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

pub struct ApplicationsListParams<'a> {
    pub app_id_query: &'a str,
    pub owner_id_query: &'a str,
    pub applications: Option<&'a [Application]>,
    pub owner_count: Option<usize>,
    pub error: Option<&'a str>,
    pub is_htmx: bool,
}

pub fn applications_list_page(
    config: &AdminConfig,
    auth: &AuthContext,
    params: &ApplicationsListParams<'_>,
) -> Markup {
    let ApplicationsListParams {
        app_id_query,
        owner_id_query,
        applications,
        owner_count,
        error,
        is_htmx,
    } = params;
    let base = &config.base_path;
    if *is_htmx {
        return render_results(config, app_id_query, owner_id_query, *applications, *error);
    }

    let results_markup =
        render_results(config, app_id_query, owner_id_query, *applications, *error);
    let content = html! {
        @if let Some(count) = owner_count {
            (page_header_with_actions("Applications", None, html! {
                p class="text-sm text-neutral-500" {
                    (count) " owned by "
                    a href={(base) "/users/" (owner_id_query)}
                        class="transition-colors hover:text-blue-600 hover:underline" {
                        (owner_id_query)
                    }
                }
            }))
        } @else {
            (page_header("Applications", None))
        }

        div class="space-y-4 mb-6" {
            div class="space-y-2" {
                h3 class="text-sm font-medium text-neutral-900" {
                    "Look up by application ID"
                }
                (search_input(
                    "application_id",
                    app_id_query,
                    "Application ID (snowflake)",
                    &format!("{base}/applications"),
                    "applications-results",
                ))
            }
            div class="space-y-2" {
                h3 class="text-sm font-medium text-neutral-900" {
                    "List applications owned by user"
                }
                (search_input(
                    "owner_id",
                    owner_id_query,
                    "User ID (snowflake)",
                    &format!("{base}/applications"),
                    "applications-results",
                ))
            }
        }

        div id="applications-results" {
            (results_markup)
        }
    };

    admin_layout(config, auth, "Applications", "applications", None, content)
}

fn render_results(
    config: &AdminConfig,
    app_id_query: &str,
    owner_id_query: &str,
    applications: Option<&[Application]>,
    error: Option<&str>,
) -> Markup {
    html! {
        @if let Some(err) = error {
            div class="rounded-lg border border-red-200 bg-red-50 p-4" {
                p class="text-sm text-red-700" { (err) }
            }
        } @else if let Some(apps) = applications {
            @if apps.is_empty() {
                @if owner_id_query.is_empty() {
                    (empty_state(
                        "Application not found."
                    ))
                } @else {
                    (empty_state(&format!(
                        "User {} does not own any applications.",
                        owner_id_query
                    )))
                }
            } @else {
                (render_applications_list(config, apps))
            }
        } @else if !app_id_query.is_empty() || !owner_id_query.is_empty() {
            (empty_state("No applications found."))
        } @else {
            (empty_state(
                "Enter an application ID to inspect a single application, \
                 or a user ID to list all of their applications."
            ))
        }
    }
}

fn render_applications_list(config: &AdminConfig, apps: &[Application]) -> Markup {
    let base = &config.base_path;
    html! {
        div class="space-y-4" {
            @for app in apps {
                (render_application_card(config, base, app))
            }
        }
    }
}

fn render_application_card(config: &AdminConfig, base: &str, app: &Application) -> Markup {
    let owner_display = format_owner_display(app);
    let owner_id = app.owner_user_id.as_str();
    card(html! {
        div class="flex flex-col gap-4 sm:flex-row sm:items-start sm:justify-between" {
            div class="min-w-0 flex-1 space-y-1" {
                h2 class="text-base font-semibold text-neutral-900" {
                    (app.name)
                }
                p class="text-sm text-neutral-500 break-all" {
                    "ID: " (app.id)
                }
                p class="text-sm text-neutral-500" {
                    "Owner: "
                    a href={(base) "/users/" (owner_id)}
                        class="transition-colors hover:text-blue-600 hover:underline" {
                        (owner_display)
                    }
                }
                @if let Some(ref bot_id) = app.bot_user_id {
                    p class="text-sm text-neutral-500" {
                        "Bot user: "
                        a href={(base) "/users/" (bot_id)}
                            class="transition-colors hover:text-blue-600 hover:underline" {
                            (format_bot_display(app))
                        }
                    }
                } @else {
                    p class="text-sm text-neutral-500" {
                        "No bot user (OAuth2 only)"
                    }
                }
            }
            a href={(&config.base_path) "/applications/" (app.id)}
                class="inline-flex items-center rounded-md bg-brand-primary px-4 py-2 \
                       text-sm font-medium text-white shadow-sm \
                       hover:bg-brand-primary-dark" {
                "View Details"
            }
        }
    })
}

fn format_owner_display(app: &Application) -> String {
    if let (Some(un), Some(disc)) = (&app.owner_username, &app.owner_discriminator) {
        format_user_display(
            app.owner_global_name.as_deref(),
            Some(un),
            Some(disc),
            false,
        )
    } else {
        app.owner_user_id.clone()
    }
}

fn format_bot_display(app: &Application) -> String {
    if let (Some(_bid), Some(un), Some(disc)) =
        (&app.bot_user_id, &app.bot_username, &app.bot_discriminator)
    {
        format_user_display(app.bot_global_name.as_deref(), Some(un), Some(disc), true)
    } else {
        app.bot_user_id.clone().unwrap_or_default()
    }
}
