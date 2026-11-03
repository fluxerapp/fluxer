// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{format_decimal, inherit_select};
use crate::templates::pages::instance_config::ConfigForm;
use crate::{
    api::types::InstanceMediaResponse,
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{NumberField, form_actions, submit_button},
        section_card::section_card_with_description,
    },
};
use maud::{Markup, html};

const MAX_ATTACHMENT_SIZE_MB: u32 = 1_000_000;
const MAX_ATTACHMENT_LIFETIME_DAYS: u32 = 36_500;
const MAX_ATTACHMENT_RENEWAL_DAYS: u32 = 365;

pub(super) enum HostingMode {
    SelfHosted,
    Hosted,
}

pub(super) fn media_config_section(
    base: &str,
    config_form: ConfigForm<'_>,
    media: &InstanceMediaResponse,
    hosting: HostingMode,
) -> Markup {
    let decay = &media.attachment_decay;
    let effective = &decay.effective;
    let stored_decimal = |value: Option<f64>| value.map(format_decimal).unwrap_or_default();
    let stored_days = |value: Option<u32>| value.map(|days| days.to_string()).unwrap_or_default();
    let min_size_mb = stored_decimal(decay.min_size_mb);
    let max_size_mb = stored_decimal(decay.max_size_mb);
    let max_eligible_size_mb = stored_decimal(decay.max_eligible_size_mb);
    let min_lifetime_days = stored_days(decay.min_lifetime_days);
    let max_lifetime_days = stored_days(decay.max_lifetime_days);
    let curve = stored_decimal(decay.curve);
    let renew_threshold_days = stored_days(decay.renew_threshold_days);
    let renew_window_days = stored_days(decay.renew_window_days);
    let default_min_size_mb = format_decimal(effective.min_size_mb);
    let default_max_size_mb = format_decimal(effective.max_size_mb);
    let default_max_eligible_size_mb = format_decimal(effective.max_eligible_size_mb);
    let default_min_lifetime_days = effective.min_lifetime_days.to_string();
    let default_max_lifetime_days = effective.max_lifetime_days.to_string();
    let default_curve = format_decimal(effective.curve);
    let default_renew_threshold_days = effective.renew_threshold_days.to_string();
    let default_renew_window_days = effective.renew_window_days.to_string();
    let description = match hosting {
        HostingMode::SelfHosted => {
            "Expire eligible attachments using size-based lifetimes. This is enabled by default and applies at runtime. On a self-hosted instance, changes also apply to attachments uploaded earlier, which are kept for at least the renew window before a new or shorter expiry deletes them."
        }
        HostingMode::Hosted => {
            "Expire eligible attachments using size-based lifetimes. This is enabled by default and applies at runtime. Changes apply to attachments uploaded from then on."
        }
    };
    section_card_with_description(
        "Media Expiry",
        description,
        html! {
            form method="post" action={(base) "/instance-config?action=update_media"} {
                (config_form.fields())
                div class="space-y-6" {
                    div class="flex flex-wrap items-center gap-2" {
                        @if effective.enabled {
                            (badge("Effective: enabled", BadgeVariant::Success))
                        } @else {
                            (badge("Effective: disabled", BadgeVariant::Default))
                        }
                    }
                    (inherit_select(
                        "media_attachment_decay_enabled",
                        "Attachment expiry",
                        decay.enabled,
                    ))
                    p class="text-sm text-neutral-500" {
                        "Leave a field empty to use the value in effect, which is shown in the field."
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3" {
                        (NumberField {
                            name: "media_attachment_decay_min_size_mb",
                            label: "Small file threshold (MB)",
                            value: &min_size_mb,
                            min: Some(1),
                            max: Some(MAX_ATTACHMENT_SIZE_MB),
                            step: "0.1",
                            helper: Some("Attachments at or below this size keep the maximum lifetime."),
                            placeholder: Some(&default_min_size_mb),
                        }.render())
                        (NumberField {
                            name: "media_attachment_decay_max_size_mb",
                            label: "Large file threshold (MB)",
                            value: &max_size_mb,
                            min: Some(1),
                            max: Some(MAX_ATTACHMENT_SIZE_MB),
                            step: "0.1",
                            helper: Some("Attachments at or above this size get the minimum lifetime."),
                            placeholder: Some(&default_max_size_mb),
                        }.render())
                        (NumberField {
                            name: "media_attachment_decay_max_eligible_size_mb",
                            label: "Maximum eligible size (MB)",
                            value: &max_eligible_size_mb,
                            min: Some(1),
                            max: Some(MAX_ATTACHMENT_SIZE_MB),
                            step: "0.1",
                            helper: Some("Attachments above this size are not managed by expiry."),
                            placeholder: Some(&default_max_eligible_size_mb),
                        }.render())
                        (NumberField {
                            name: "media_attachment_decay_min_lifetime_days",
                            label: "Minimum lifetime (days)",
                            value: &min_lifetime_days,
                            min: Some(1),
                            max: Some(MAX_ATTACHMENT_LIFETIME_DAYS),
                            step: "1",
                            helper: None,
                            placeholder: Some(&default_min_lifetime_days),
                        }.render())
                        (NumberField {
                            name: "media_attachment_decay_max_lifetime_days",
                            label: "Maximum lifetime (days)",
                            value: &max_lifetime_days,
                            min: Some(1),
                            max: Some(MAX_ATTACHMENT_LIFETIME_DAYS),
                            step: "1",
                            helper: None,
                            placeholder: Some(&default_max_lifetime_days),
                        }.render())
                        (NumberField {
                            name: "media_attachment_decay_curve",
                            label: "Expiry curve",
                            value: &curve,
                            min: Some(0),
                            max: Some(1),
                            step: "0.05",
                            helper: Some("0 is linear. 1 is logarithmic."),
                            placeholder: Some(&default_curve),
                        }.render())
                        (NumberField {
                            name: "media_attachment_decay_renew_threshold_days",
                            label: "Renew threshold (days)",
                            value: &renew_threshold_days,
                            min: Some(1),
                            max: Some(MAX_ATTACHMENT_RENEWAL_DAYS),
                            step: "1",
                            helper: Some("Viewed attachments renew when expiry is this close."),
                            placeholder: Some(&default_renew_threshold_days),
                        }.render())
                        (NumberField {
                            name: "media_attachment_decay_renew_window_days",
                            label: "Renew window (days)",
                            value: &renew_window_days,
                            min: Some(1),
                            max: Some(MAX_ATTACHMENT_RENEWAL_DAYS),
                            step: "1",
                            helper: Some("Renewed attachments are extended by this many days."),
                            placeholder: Some(&default_renew_window_days),
                        }.render())
                    }
                    (form_actions(html! {
                        (submit_button("Save media expiry"))
                    }))
                }
            }
        },
    )
}
