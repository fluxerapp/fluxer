// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            error_display::error_alert,
            form::{
                FORM_TEXTAREA_CLASS, csrf_input, form_actions, form_field_group, submit_button,
            },
            page_container::{card, page_header},
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

pub struct SystemDmParams<'a> {
    pub form_error: Option<&'a str>,
    pub csrf_token: &'a str,
}

pub fn system_dm_page(
    config: &AdminConfig,
    auth: &AuthContext,
    params: &SystemDmParams<'_>,
) -> Markup {
    let base = &config.base_path;
    let jobs_url = format!("{base}/jobs?task_type=sendSystemDm");
    let content = html! {
        (page_header("System DMs", None))

        (card(html! {
            div class="flex flex-col gap-4" {
                h2 class="text-base font-semibold text-neutral-900" {
                    "Send a system DM"
                }
                p class="text-sm text-neutral-500" {
                    "Sent from the official Fluxer system account. Each recipient \
                     will receive the same content as a DM. Progress is observable \
                     on the "
                    a href=(jobs_url) class="font-medium text-neutral-900 hover:underline" {
                        "Jobs page"
                    }
                    " (filtered to "
                    code { "sendSystemDm" }
                    ")."
                }

                @if let Some(err) = params.form_error {
                    (error_alert(err))
                }

                form method="post" action={(base) "/system-dms"} {
                    (csrf_input(params.csrf_token))
                    div class="flex flex-col gap-4" {
                        (form_field_group(
                            "Recipient user IDs", "system-dm-user-ids",
                            true, None,
                            Some("One per line. Snowflake IDs only, or a single * to send to every user."),
                            html! {
                                textarea id="system-dm-user-ids" name="user_ids"
                                    required rows="10"
                                    placeholder="1234567890123456789\n9876543210987654321"
                                    class=(FORM_TEXTAREA_CLASS) {}
                            },
                        ))
                        (form_field_group(
                            "Content", "system-dm-content",
                            true, None, None,
                            html! {
                                textarea id="system-dm-content" name="content"
                                    required rows="6" maxlength="4000"
                                    class=(FORM_TEXTAREA_CLASS) {}
                            },
                        ))
                        (form_actions(html! {
                            (submit_button("Queue send"))
                        }))
                    }
                }
            }
        }))
    };
    admin_layout(config, auth, "System DMs", "system-dms", None, content)
}
