// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::templates::pages::instance_config::ConfigForm;
use crate::{
    api::types::{
        CAPTCHA_COST_RANGE, CAPTCHA_MAX_COUNTER_RANGE, CaptchaConfigResponse, CaptchaProvider,
    },
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{NumberField, form_actions, submit_button},
        section_card::section_card_with_description,
    },
};
use maud::{Markup, html};

use super::{CAPTCHA_PROVIDER_CHOICES, choice_select};

const LOW_END_SOLVE_REFERENCE_SECONDS: f64 = 0.75;
const LOW_END_SOLVE_REFERENCE_WORK: f64 = 1_050_000.0;

pub(super) fn captcha_section(
    base: &str,
    config_form: ConfigForm<'_>,
    captcha: &CaptchaConfigResponse,
    instant_accounts_enabled: bool,
) -> Markup {
    let status = if captcha.provider != CaptchaProvider::None {
        ("On", BadgeVariant::Success)
    } else {
        ("Off", BadgeVariant::Default)
    };
    let estimate =
        LOW_END_SOLVE_REFERENCE_SECONDS * f64::from(captcha.cost) * f64::from(captcha.max_counter)
            / LOW_END_SOLVE_REFERENCE_WORK;
    section_card_with_description(
        "Proof-of-work check",
        "Clients solve it in the background. The API issues and verifies every challenge itself, \
         and no third party is involved.",
        html! {
            div class="space-y-6" {
                div class="flex flex-wrap items-center gap-2" {
                    (badge(status.0, status.1))
                }
                form method="post" action={(base) "/instance-config?action=update_captcha"} {
                    (config_form.fields())
                    div class="space-y-6" {
                        (choice_select(
                            "captcha_provider",
                            "Check",
                            CAPTCHA_PROVIDER_CHOICES,
                            &captcha.provider,
                        ))
                        p class="text-xs text-neutral-500" {
                            "On by default. Turning it off removes the check from every request."
                            @if instant_accounts_enabled {
                                " It cannot be turned off while people can start with just a \
                                 display name. Turn that off first under Sign-in & new accounts."
                            }
                        }
                        (NumberField {
                            name: "captcha_cost",
                            label: "Cost (PBKDF2 iterations per try)",
                            value: &captcha.cost.to_string(),
                            min: Some(*CAPTCHA_COST_RANGE.start()),
                            max: Some(*CAPTCHA_COST_RANGE.end()),
                            step: "1",
                            helper: Some("Default 5000."),
                            placeholder: None,
                        }.render())
                        (NumberField {
                            name: "captcha_max_counter",
                            label: "Maximum counter",
                            value: &captcha.max_counter.to_string(),
                            min: Some(*CAPTCHA_MAX_COUNTER_RANGE.start()),
                            max: Some(*CAPTCHA_MAX_COUNTER_RANGE.end()),
                            step: "1",
                            helper: Some("Default 1000. Solve time grows with cost times this value."),
                            placeholder: None,
                        }.render())
                        p class="text-sm text-neutral-700" {
                            (format!(
                                "Average solve: about {estimate:.1} s on a low-end Android device, \
                                 well under a second in desktop browsers."
                            ))
                        }
                        (form_actions(html! {
                            (submit_button("Save"))
                        }))
                    }
                }
            }
        },
    )
}
