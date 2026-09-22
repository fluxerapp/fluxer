// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            form::{
                FORM_INPUT_CLASS, FORM_SELECT_CLASS, FORM_TEXTAREA_CLASS, csrf_input, form_actions,
                form_field_group, submit_button,
            },
            page_container::{card, page_header},
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

pub const MAX_GIFT_CODES: u32 = 100;
const DEFAULT_GIFT_COUNT: u32 = 10;

pub fn gift_codes_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    generated_codes: Option<&[String]>,
) -> Markup {
    let base = &config.base_path;
    let codes_value = generated_codes.map(|c| c.join("\n")).unwrap_or_default();

    let content = html! {
        (page_header(
            "Gift Codes",
            Some("Create one-use Plutonium gift URLs with a fixed positive \
                  duration. Lifetime gifts cannot be generated here."),
        ))

        (card(html! {
            div class="flex flex-col gap-4" {
                h2 class="text-xl font-semibold text-neutral-900" {
                    "Generate Gift Codes"
                }
                form id="gift-form" method="post" action={(base) "/gift-codes"} {
                    (csrf_input(csrf_token))
                    div class="flex flex-col gap-4" {
                        (form_field_group(
                            "Number of codes", "gift-count-slider", true, None,
                            Some(&format!("Range: 1-{MAX_GIFT_CODES}")),
                            html! {
                                input type="number" id="gift-count-slider" name="count"
                                    value=(DEFAULT_GIFT_COUNT) min="1"
                                    max=(MAX_GIFT_CODES) required
                                    class=(FORM_INPUT_CLASS);
                            },
                        ))

                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (form_field_group(
                                "Duration quantity", "gift-duration-quantity",
                                true, None,
                                Some("Use a positive whole number."),
                                html! {
                                    input type="number" id="gift-duration-quantity"
                                        name="duration_quantity" value="1" min="1"
                                        max="3650" required
                                        class=(FORM_INPUT_CLASS);
                                },
                            ))
                            (form_field_group(
                                "Duration unit", "gift-duration-type",
                                true, None,
                                Some("Days, weeks, months, or years. \
                                      No lifetime option is available."),
                                html! {
                                    select id="gift-duration-type" name="duration_type"
                                        required class=(FORM_SELECT_CLASS) {
                                        option value="days" { "Days" }
                                        option value="weeks" { "Weeks" }
                                        option value="months" selected { "Months" }
                                        option value="years" { "Years" }
                                    }
                                },
                            ))
                        }

                        (form_actions(html! {
                            (submit_button("Generate Codes"))
                        }))

                        (form_field_group(
                            "Generated URLs", "gift-generated-urls", false, None,
                            Some("Copy one URL per line. Codes are shown only \
                                  after generation."),
                            html! {
                                textarea id="gift-generated-urls" name="generated_urls"
                                    readonly rows="10"
                                    placeholder="Generated gift URLs will appear here."
                                    class=(FORM_TEXTAREA_CLASS) {
                                    (codes_value)
                                }
                            },
                        ))
                    }
                }
            }
        }))
    };
    admin_layout(config, auth, "Gift Codes", "gift-codes", None, content)
}
