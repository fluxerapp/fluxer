// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::Application,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            data_field::{data_field, data_field_mono, data_field_text},
            empty_state::not_found_state,
            form::{FORM_INPUT_CLASS, csrf_input, danger_button, form_field_group},
            page_container::page_header_with_back,
            section_card::{section_card_simple, section_card_with_description},
            user_display::format_user_display,
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

pub struct ApplicationDetailPage<'a> {
    pub config: &'a AdminConfig,
    pub auth: &'a AuthContext,
    pub application: Option<&'a Application>,
    pub application_id: &'a str,
    pub is_htmx: bool,
    pub csrf_token: &'a str,
    pub can_transfer_ownership: bool,
    pub can_list_by_owner: bool,
}

pub fn application_detail_page(page: ApplicationDetailPage<'_>) -> Markup {
    let content = match page.application {
        Some(app) => render_detail(
            page.config,
            app,
            page.csrf_token,
            page.can_transfer_ownership,
            page.can_list_by_owner,
        ),
        None => not_found_state(
            "Application",
            page.application_id,
            Some(&format!("{}/applications", page.config.base_path)),
            Some("Back to Applications"),
        ),
    };

    let title = page
        .application
        .map(|a| format!("{} - Application", a.name))
        .unwrap_or_else(|| format!("{} - Application", page.application_id));

    if page.is_htmx {
        content
    } else {
        admin_layout(
            page.config,
            page.auth,
            &title,
            "applications",
            None,
            content,
        )
    }
}

fn render_detail(
    config: &AdminConfig,
    app: &Application,
    csrf_token: &str,
    can_transfer_ownership: bool,
    can_list_by_owner: bool,
) -> Markup {
    let base = &config.base_path;
    html! {
        (page_header_with_back(
            &app.name,
            None,
            &format!("{base}/applications"),
            Some("Back to Applications"),
        ))
        div class="flex flex-wrap items-center gap-2 mb-6" {
            @if app.bot_user_id.is_some() {
                span class="rounded bg-blue-100 px-2 py-0.5 font-medium text-blue-700 \
                            text-xs uppercase" {
                    "Bot"
                }
            } @else {
                span class="rounded bg-neutral-200 px-2 py-0.5 font-medium text-neutral-700 \
                            text-xs uppercase" {
                    "OAuth2 App"
                }
            }
            p class="text-sm text-neutral-500 break-all" {
                (app.id)
            }
        }

        div class="space-y-6" {
            (overview_card(config, app, can_list_by_owner))
            (credentials_card(app))
            (redirect_uris_card(app))
            @if can_transfer_ownership {
                (transfer_ownership_card(config, app, csrf_token))
            }
        }
    }
}

fn overview_card(config: &AdminConfig, app: &Application, can_list_by_owner: bool) -> Markup {
    let base = &config.base_path;
    let owner_display = format_user_display(
        app.owner_global_name.as_deref(),
        app.owner_username.as_deref(),
        app.owner_discriminator.as_deref(),
        false,
    );
    section_card_simple(
        "Overview",
        html! {
            div class="grid grid-cols-1 sm:grid-cols-2 gap-4" {
                (data_field_mono("Application ID", &app.id))
                (data_field_text("Name", &app.name))
                (data_field("Owner", html! {
                    div class="space-y-1" {
                        a href={(base) "/users/" (app.owner_user_id)}
                            class="rounded text-blue-600 text-sm hover:underline" {
                            (owner_display)
                        }
                        p class="text-xs text-neutral-500 break-all" {
                            (app.owner_user_id)
                        }
                        @if can_list_by_owner {
                            a href={(base) "/applications?owner_id=" (app.owner_user_id)}
                                class="rounded text-blue-600 text-xs hover:underline" {
                                "List all applications owned by this user \u{2192}"
                            }
                        }
                    }
                }))
                (data_field("Bot user", bot_display_markup(config, app)))
                (data_field_text("Bot is public",
                    if app.bot_is_public { "Yes" } else { "No" }))
                (data_field_text("Bot requires code grant",
                    if app.bot_require_code_grant { "Yes" } else { "No" }))
                (data_field_text("Version", &app.version.to_string()))
            }
        },
    )
}

fn bot_display_markup(config: &AdminConfig, app: &Application) -> Markup {
    let base = &config.base_path;
    match &app.bot_user_id {
        Some(bot_id) => {
            let bot_display = format_user_display(
                app.bot_global_name.as_deref(),
                app.bot_username.as_deref(),
                app.bot_discriminator.as_deref(),
                true,
            );
            html! {
                div class="space-y-1" {
                    a href={(base) "/users/" (bot_id)}
                        class="rounded text-blue-600 text-sm hover:underline" {
                        (bot_display)
                    }
                    p class="text-xs text-neutral-500 break-all" {
                        (bot_id)
                    }
                }
            }
        }
        None => html! {
            p class="text-sm text-neutral-500" { "No bot user (OAuth2 only)" }
        },
    }
}

fn credentials_card(app: &Application) -> Markup {
    let secret_label = if app.has_client_secret {
        "Set"
    } else {
        "Not set"
    };
    let token_label = if app.has_bot_token {
        match &app.bot_token_preview {
            Some(preview) => format!("Set (\u{2026}{preview})"),
            None => "Set".to_owned(),
        }
    } else {
        "Not set".to_owned()
    };
    let secret_created = app
        .client_secret_created_at
        .as_deref()
        .unwrap_or("\u{2014}");
    let token_created = app.bot_token_created_at.as_deref().unwrap_or("\u{2014}");
    section_card_simple(
        "Credentials",
        html! {
            div class="grid grid-cols-1 sm:grid-cols-2 gap-4" {
                (data_field_text("Client secret", secret_label))
                (data_field_text("Client secret created", secret_created))
                (data_field_text("Bot token", &token_label))
                (data_field_text("Bot token created", token_created))
            }
        },
    )
}

fn redirect_uris_card(app: &Application) -> Markup {
    section_card_simple(
        "OAuth2 redirect URIs",
        html! {
            @if app.oauth2_redirect_uris.is_empty() {
                p class="text-sm text-neutral-500" {
                    "No redirect URIs registered."
                }
            } @else {
                ul class="space-y-1 text-neutral-800 text-sm" {
                    @for uri in &app.oauth2_redirect_uris {
                        li class="break-all" { (uri) }
                    }
                }
            }
        },
    )
}

fn transfer_ownership_card(config: &AdminConfig, app: &Application, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    section_card_with_description(
        "Transfer ownership",
        "Transfers ownership of this application to another user. \
         This action is logged to the audit log.",
        html! {
            form method="post"
                action={(base) "/applications/" (app.id) "?action=transfer_ownership&_csrf=" (csrf_token)} {
                (csrf_input(csrf_token))
                div class="flex flex-col gap-3" {
                    (form_field_group(
                        "New owner user ID", "new_owner_id", true, None, None,
                        html! {
                            input type="text" id="new_owner_id" name="new_owner_id"
                                placeholder="New owner user ID" required
                                class=(FORM_INPUT_CLASS);
                        },
                    ))
                    (danger_button("Transfer Ownership"))
                }
            }
        },
    )
}
