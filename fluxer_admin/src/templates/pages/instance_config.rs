// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{
        AppPublicConfigResponse, GatewayRolloutConfigResponse, InstanceConfigResponse,
        InstanceIntegrationsResponse, InstanceMediaResponse, InstancePolicyResponse,
        InstanceRegistrationResponse, LimitConfigResponse, PendingRegistrationResponse,
        RegistrationUrlResponse, SsoConfigResponse,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            form::{
                FORM_INPUT_CLASS, checkbox, csrf_input, danger_button, form_actions,
                form_field_group, secondary_button_link, select_input, submit_button, text_input,
                textarea_input,
            },
            page_container::page_header,
            section_card::{section_card_simple, section_card_with_description},
        },
        layout::admin_layout,
    },
    utils::timestamps::format_admin_timestamp,
};
use maud::{Markup, html};

fn format_percent(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{}", value as u64)
    } else {
        value.to_string()
    }
}

fn format_decimal(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{}", value as u64)
    } else {
        value.to_string()
    }
}

fn number_field(
    name: &str,
    label: &str,
    value: &str,
    min: Option<u32>,
    max: Option<u32>,
    step: &str,
    helper: Option<&str>,
) -> Markup {
    form_field_group(
        label,
        name,
        false,
        None,
        helper,
        html! {
            input type="number" id=(name) name=(name) step=(step)
                value=(value)
                min=[min] max=[max] class=(FORM_INPUT_CLASS);
        },
    )
}

pub fn instance_config_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    instance_config: Option<&InstanceConfigResponse>,
    limit_config: Option<&LimitConfigResponse>,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header("Instance Configuration", Some("Manage instance-wide settings")))
        div class="space-y-10" {
            @if let Some(instance_config) = instance_config {
                @if instance_config.self_hosted {
                    (config_group(
                        "Identity & branding",
                        "How the instance presents itself to clients and the public discovery document.",
                        html! {
                            (app_public_config_section(
                                base,
                                csrf_token,
                                &instance_config.app_public,
                                instance_config.self_hosted,
                            ))
                        },
                    ))
                }
                (config_group(
                    "Access & accounts",
                    "Who can sign in and create accounts on this instance.",
                    html! {
                        (registration_config_section(
                            config,
                            csrf_token,
                            &instance_config.registration,
                            instance_config.app_public.registration.collect_date_of_birth,
                            instance_config.self_hosted,
                        ))
                        (sso_config_section(base, csrf_token, &instance_config.sso))
                    },
                ))
                @if instance_config.self_hosted {
                    (config_group(
                        "Community & policy",
                        "Community shape, direct messaging, the premium model, and optional embed services.",
                        html! {
                            (policy_config_section(base, csrf_token, &instance_config.policy))
                        },
                    ))
                }
                (config_group(
                    "Runtime integrations",
                    "Credentials and provider choices that override environment variables at runtime.",
                    html! {
                        (integrations_config_section(base, csrf_token, &instance_config.integrations))
                    },
                ))
                (config_group(
                    "Media & retention",
                    "Attachment expiry rules that can be changed without editing environment variables.",
                    html! {
                        (media_config_section(base, csrf_token, &instance_config.media))
                    },
                ))
                (config_group(
                    "Infrastructure & limits",
                    "Gateway rollout behavior and the limit rules applied to users and guilds.",
                    html! {
                        (gateway_rollout_section(base, csrf_token, &instance_config.gateway_rollout))
                        @if let Some(limit_config) = limit_config {
                            (limit_config_section(base, limit_config))
                        } @else {
                            (section_card_simple("Limit Configuration", html! {
                                p class="text-sm text-red-600" { "Failed to load limit configuration." }
                            }))
                        }
                    },
                ))
            } @else {
                (section_card_simple("Instance Configuration", html! {
                    p class="text-sm text-red-600" { "Failed to load instance configuration." }
                }))
                @if let Some(limit_config) = limit_config {
                    (limit_config_section(base, limit_config))
                } @else {
                    (section_card_simple("Limit Configuration", html! {
                        p class="text-sm text-red-600" { "Failed to load limit configuration." }
                    }))
                }
            }
        }
    };
    admin_layout(
        config,
        auth,
        "Instance Configuration",
        "instance-config",
        None,
        content,
    )
}

fn config_group(title: &str, description: &str, content: Markup) -> Markup {
    html! {
        section class="space-y-4" {
            div class="space-y-1 border-b border-neutral-200 pb-3" {
                h2 class="text-lg font-semibold tracking-tight text-neutral-900" { (title) }
                p class="text-sm text-neutral-500" { (description) }
            }
            div class="space-y-6" {
                (content)
            }
        }
    }
}

fn policy_config_section(base: &str, csrf_token: &str, policy: &InstancePolicyResponse) -> Markup {
    section_card_with_description(
        "Community & Policy",
        "Control whether this instance runs as a single community, whether direct messages and \
         friends are available, which premium model applies, and which optional embed services \
         are enabled.",
        html! {
            div class="space-y-8" {
                (single_community_form(base, csrf_token, policy))
                (direct_messages_form(base, csrf_token, policy))
                (premium_mode_form(base, csrf_token, policy))
                (services_form(base, csrf_token, policy))
            }
        },
    )
}

fn single_community_form(base: &str, csrf_token: &str, policy: &InstancePolicyResponse) -> Markup {
    let status = if policy.single_community_enabled {
        ("Enabled", BadgeVariant::Success)
    } else {
        ("Disabled", BadgeVariant::Default)
    };
    html! {
        div class="space-y-4" {
            div class="flex flex-wrap items-center gap-2" {
                h3 class="text-sm font-semibold text-neutral-900" { "Single community" }
                (badge(status.0, status.1))
                @if policy.single_community_locked {
                    (badge("Locked", BadgeVariant::Warning))
                }
            }
            @if let Some(guild_id) = policy.single_community_guild_id.as_deref() {
                p class="break-all text-xs text-neutral-500" { "Community guild ID: " (guild_id) }
            }
            @if policy.single_community_enabled && !policy.single_community_locked {
                p class="text-sm text-neutral-500" {
                    "This instance funnels every member into a single community. Disabling it is \
                     permanent: single-community mode can only be enabled again from the \
                     self-host setup wizard, never from this panel."
                }
                form method="post" action={(base) "/instance-config?action=disable_single_community"} {
                    (csrf_input(csrf_token))
                    (form_actions(html! {
                        (danger_button("Disable single-community mode"))
                    }))
                }
            } @else if policy.single_community_enabled {
                p class="text-sm text-neutral-500" {
                    "Single-community mode is enabled and locked for this instance. It cannot be \
                     changed from the admin panel."
                }
            } @else {
                p class="text-sm text-neutral-500" {
                    "Single-community mode is off. It can only be turned on from the self-host \
                     setup wizard, not from this panel."
                }
            }
        }
    }
}

fn direct_messages_form(base: &str, csrf_token: &str, policy: &InstancePolicyResponse) -> Markup {
    html! {
        div class="space-y-4 border-t border-neutral-200 pt-6" {
            div class="flex flex-wrap items-center gap-2" {
                h3 class="text-sm font-semibold text-neutral-900" { "Direct messages & friends" }
                @if policy.direct_messages_locked {
                    (badge("Locked", BadgeVariant::Warning))
                }
            }
            @if policy.direct_messages_locked {
                p class="text-sm text-neutral-500" {
                    @if policy.direct_messages_disabled {
                        "Direct messages and friends are disabled and locked for this instance."
                    } @else {
                        "Direct messages and friends are enabled and locked for this instance."
                    }
                }
            } @else {
                form method="post" action={(base) "/instance-config?action=update_policy"} {
                    (csrf_input(csrf_token))
                    div class="space-y-4" {
                        (select_input("policy_direct_messages_disabled", "Direct messages & friends", &[
                            ("false", "Enabled"),
                            ("true", "Disabled"),
                        ], if policy.direct_messages_disabled { "true" } else { "false" }))
                        p class="text-xs text-neutral-500" {
                            "Re-enabling direct messages after they have been disabled locks this \
                             setting permanently."
                        }
                        (form_actions(html! {
                            (submit_button("Save direct message policy"))
                        }))
                    }
                }
            }
        }
    }
}

fn premium_mode_form(base: &str, csrf_token: &str, policy: &InstancePolicyResponse) -> Markup {
    html! {
        div class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Premium model" }
            form method="post" action={(base) "/instance-config?action=update_policy"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (select_input("policy_premium_mode", "Premium model", &[
                        ("mirror", "Mirror (Free and Premium tiers)"),
                        ("everyone", "Everyone (every member gets Plutonium limits)"),
                    ], policy.premium_mode.as_str()))
                    (form_actions(html! {
                        (submit_button("Save premium model"))
                    }))
                }
            }
        }
    }
}

fn service_select(name: &str, label: &str, override_value: Option<bool>, resolved: bool) -> Markup {
    let selected = match override_value {
        None => "inherit",
        Some(true) => "on",
        Some(false) => "off",
    };
    let resolved_text = if resolved {
        "Effective: enabled"
    } else {
        "Effective: disabled"
    };
    html! {
        div class="space-y-2" {
            (select_input(name, label, &[
                ("inherit", "Inherit env default"),
                ("on", "Force on"),
                ("off", "Force off"),
            ], selected))
            p class="text-xs text-neutral-500" { (resolved_text) }
        }
    }
}

fn services_form(base: &str, csrf_token: &str, policy: &InstancePolicyResponse) -> Markup {
    let available = &policy.services_available;
    if !available.gif && !available.youtube && !available.bluesky {
        return html! {
            div class="space-y-4 border-t border-neutral-200 pt-6" {
                h3 class="text-sm font-semibold text-neutral-900" { "Optional services" }
                p class="text-sm text-neutral-500" {
                    "No optional embed services are available in this environment."
                }
            }
        };
    }
    html! {
        div class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Optional services" }
            p class="text-sm text-neutral-500" {
                "Override the environment default for each optional embed service. \
                 \"Inherit env default\" keeps the value configured for this deployment."
            }
            form method="post" action={(base) "/instance-config?action=update_policy"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-3" {
                        @if available.gif {
                            (service_select(
                                "policy_service_gif",
                                "GIF picker",
                                policy.services.gif_enabled,
                                policy.services_resolved.gif_enabled,
                            ))
                        }
                        @if available.youtube {
                            (service_select(
                                "policy_service_youtube",
                                "YouTube embeds",
                                policy.services.youtube_enabled,
                                policy.services_resolved.youtube_enabled,
                            ))
                        }
                        @if available.bluesky {
                            (service_select(
                                "policy_service_bluesky",
                                "Bluesky embeds",
                                policy.services.bluesky_enabled,
                                policy.services_resolved.bluesky_enabled,
                            ))
                        }
                    }
                    (form_actions(html! {
                        (submit_button("Save optional services"))
                    }))
                }
            }
        }
    }
}

fn secret_badge(label: &str, is_set: bool) -> Markup {
    if is_set {
        badge(&format!("{label} set"), BadgeVariant::Success)
    } else {
        badge(&format!("{label} missing"), BadgeVariant::Default)
    }
}

fn password_input(name: &str, label: &str, helper: Option<&str>) -> Markup {
    form_field_group(
        label,
        name,
        false,
        None,
        helper,
        html! {
            input type="password" id=(name) name=(name) value="" class=(FORM_INPUT_CLASS)
                autocomplete="new-password";
        },
    )
}

fn integrations_config_section(
    base: &str,
    csrf_token: &str,
    integrations: &InstanceIntegrationsResponse,
) -> Markup {
    let captcha_provider = integrations
        .captcha
        .provider
        .as_deref()
        .unwrap_or(integrations.captcha.effective_provider.as_str());
    let smtp_port = integrations
        .email
        .smtp
        .port
        .map(|port| port.to_string())
        .unwrap_or_else(|| "587".to_owned());
    section_card_with_description(
        "Runtime Integrations",
        "Configure optional providers without changing environment variables. Blank secret fields keep the current value.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_integrations"} {
                (csrf_input(csrf_token))
                div class="space-y-8" {
                    div class="space-y-4" {
                        div class="flex flex-wrap items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" { "GIF provider" }
                            (secret_badge("KLIPY key", integrations.gif.klipy_api_key_set))
                        }
                        div class="grid grid-cols-1 gap-4" {
                            (password_input("integration_klipy_api_key", "KLIPY API key", Some("Leave blank to keep the current key.")))
                        }
                    }

                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        div class="flex flex-wrap items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" { "YouTube Data API" }
                            (secret_badge("API key", integrations.youtube.api_key_set))
                        }
                        (password_input("integration_youtube_api_key", "YouTube API key", Some("Leave blank to keep the current key.")))
                    }

                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        div class="flex flex-wrap items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" { "Bot protection" }
                            (secret_badge("hCaptcha secret", integrations.captcha.hcaptcha_secret_key_set))
                            (secret_badge("Turnstile secret", integrations.captcha.turnstile_secret_key_set))
                        }
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3" {
                            (select_input("integration_captcha_provider", "Provider", &[
                                ("none", "Disabled"),
                                ("hcaptcha", "hCaptcha"),
                                ("turnstile", "Cloudflare Turnstile"),
                            ], captcha_provider))
                            (text_input(
                                "integration_hcaptcha_site_key",
                                "hCaptcha site key",
                                integrations.captcha.hcaptcha_site_key.as_deref().unwrap_or(""),
                                "",
                            ))
                            (password_input("integration_hcaptcha_secret_key", "hCaptcha secret key", Some("Leave blank to keep the current secret.")))
                            (text_input(
                                "integration_turnstile_site_key",
                                "Turnstile site key",
                                integrations.captcha.turnstile_site_key.as_deref().unwrap_or(""),
                                "",
                            ))
                            (password_input("integration_turnstile_secret_key", "Turnstile secret key", Some("Leave blank to keep the current secret.")))
                        }
                    }

                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        div class="flex flex-wrap items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" { "Email delivery" }
                            @if integrations.email.effective_enabled {
                                (badge("Effective: enabled", BadgeVariant::Success))
                            } @else {
                                (badge("Effective: disabled", BadgeVariant::Default))
                            }
                            @if integrations.email.effective_disable_new_ip_authorization {
                                (badge("IP auth disabled", BadgeVariant::Warning))
                            } @else {
                                (badge("IP auth required", BadgeVariant::Default))
                            }
                            (secret_badge("SMTP password", integrations.email.smtp.password_set))
                        }
                        (checkbox("integration_email_enabled", "true", "Enable email delivery", integrations.email.effective_enabled, true))
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (text_input(
                                "integration_email_from_email",
                                "From email",
                                integrations.email.from_email.as_deref().unwrap_or(""),
                                "notifications@example.com",
                            ))
                            (text_input(
                                "integration_email_from_name",
                                "From name",
                                integrations.email.from_name.as_deref().unwrap_or(""),
                                "Fluxer",
                            ))
                            (text_input(
                                "integration_smtp_host",
                                "SMTP host",
                                integrations.email.smtp.host.as_deref().unwrap_or(""),
                                "smtp.example.com",
                            ))
                            (text_input(
                                "integration_smtp_port",
                                "SMTP port",
                                &smtp_port,
                                "587",
                            ))
                            (text_input(
                                "integration_smtp_username",
                                "SMTP username",
                                integrations.email.smtp.username.as_deref().unwrap_or(""),
                                "user@example.com",
                            ))
                            (password_input("integration_smtp_password", "SMTP password", Some("Leave blank to keep the current password.")))
                        }
                        (checkbox("integration_smtp_secure", "true", "Use TLS", integrations.email.smtp.secure.unwrap_or(true), true))
                        (checkbox("integration_email_disable_new_ip_authorization", "true", "Disable new IP login authorisation", integrations.email.disable_new_ip_authorization, true))
                        div class="flex flex-wrap gap-2" {
                            button type="submit"
                                formaction={(base) "/instance-config?action=test_smtp"}
                                class="inline-flex w-fit items-center justify-center gap-2 rounded-lg border border-neutral-300 bg-neutral-50 px-4 py-2 font-medium text-base text-neutral-700 transition-all duration-150 hover:border-neutral-400 hover:text-neutral-900 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-white" {
                                span { "Test SMTP connection" }
                            }
                        }
                    }

                    div class="space-y-4 border-t border-neutral-200 pt-6" {
                        div class="flex flex-wrap items-center gap-2" {
                            h3 class="text-sm font-semibold text-neutral-900" { "Bluesky OAuth" }
                            @if integrations.bluesky.effective_enabled {
                                (badge("Effective: enabled", BadgeVariant::Success))
                            } @else {
                                (badge("Effective: disabled", BadgeVariant::Default))
                            }
                            (badge(&format!("{} signing key(s)", integrations.bluesky.key_count), BadgeVariant::Default))
                        }
                        (checkbox("integration_bluesky_enabled", "true", "Enable Bluesky OAuth", integrations.bluesky.effective_enabled, true))
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (text_input(
                                "integration_bluesky_client_name",
                                "Client name",
                                integrations.bluesky.client_name.as_deref().unwrap_or(""),
                                "Fluxer",
                            ))
                            (text_input(
                                "integration_bluesky_client_uri",
                                "Client URL",
                                integrations.bluesky.client_uri.as_deref().unwrap_or(""),
                                "https://example.com",
                            ))
                            (text_input(
                                "integration_bluesky_logo_uri",
                                "Logo URL",
                                integrations.bluesky.logo_uri.as_deref().unwrap_or(""),
                                "https://example.com/logo.png",
                            ))
                            (text_input(
                                "integration_bluesky_tos_uri",
                                "Terms URL",
                                integrations.bluesky.tos_uri.as_deref().unwrap_or(""),
                                "https://example.com/terms",
                            ))
                            (text_input(
                                "integration_bluesky_policy_uri",
                                "Privacy URL",
                                integrations.bluesky.policy_uri.as_deref().unwrap_or(""),
                                "https://example.com/privacy",
                            ))
                            (text_input("integration_bluesky_key_id", "New signing key ID", "", "atproto-key-1"))
                            (password_input("integration_bluesky_private_key", "New private key", Some("Provide this only when adding or replacing runtime Bluesky keys.")))
                        }
                    }

                    (form_actions(html! {
                        (submit_button("Save runtime integrations"))
                    }))
                }
            }
        },
    )
}

fn media_config_section(base: &str, csrf_token: &str, media: &InstanceMediaResponse) -> Markup {
    let decay = &media.attachment_decay;
    let effective = &decay.effective;
    let enabled = decay.enabled.unwrap_or(effective.enabled);
    let min_size_mb = format_decimal(decay.min_size_mb.unwrap_or(effective.min_size_mb));
    let max_size_mb = format_decimal(decay.max_size_mb.unwrap_or(effective.max_size_mb));
    let max_eligible_size_mb = format_decimal(
        decay
            .max_eligible_size_mb
            .unwrap_or(effective.max_eligible_size_mb),
    );
    let min_lifetime_days = decay
        .min_lifetime_days
        .unwrap_or(effective.min_lifetime_days)
        .to_string();
    let max_lifetime_days = decay
        .max_lifetime_days
        .unwrap_or(effective.max_lifetime_days)
        .to_string();
    let curve = format_decimal(decay.curve.unwrap_or(effective.curve));
    let renew_threshold_days = decay
        .renew_threshold_days
        .unwrap_or(effective.renew_threshold_days)
        .to_string();
    let renew_window_days = decay
        .renew_window_days
        .unwrap_or(effective.renew_window_days)
        .to_string();
    section_card_with_description(
        "Media Expiry",
        "Expire eligible attachments using size-based lifetimes. This is disabled by default and applies at runtime.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_media"} {
                (csrf_input(csrf_token))
                div class="space-y-6" {
                    div class="flex flex-wrap items-center gap-2" {
                        @if effective.enabled {
                            (badge("Effective: enabled", BadgeVariant::Success))
                        } @else {
                            (badge("Effective: disabled", BadgeVariant::Default))
                        }
                    }
                    (checkbox("media_attachment_decay_enabled", "true", "Enable attachment expiry", enabled, true))
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3" {
                        (number_field(
                            "media_attachment_decay_min_size_mb",
                            "Small file threshold (MB)",
                            &min_size_mb,
                            Some(1),
                            Some(1_000_000),
                            "0.1",
                            Some("Attachments at or below this size keep the maximum lifetime."),
                        ))
                        (number_field(
                            "media_attachment_decay_max_size_mb",
                            "Large file threshold (MB)",
                            &max_size_mb,
                            Some(1),
                            Some(1_000_000),
                            "0.1",
                            Some("Attachments at or above this size get the minimum lifetime."),
                        ))
                        (number_field(
                            "media_attachment_decay_max_eligible_size_mb",
                            "Maximum eligible size (MB)",
                            &max_eligible_size_mb,
                            Some(1),
                            Some(1_000_000),
                            "0.1",
                            Some("Attachments above this size are not managed by expiry."),
                        ))
                        (number_field(
                            "media_attachment_decay_min_lifetime_days",
                            "Minimum lifetime (days)",
                            &min_lifetime_days,
                            Some(1),
                            Some(36_500),
                            "1",
                            None,
                        ))
                        (number_field(
                            "media_attachment_decay_max_lifetime_days",
                            "Maximum lifetime (days)",
                            &max_lifetime_days,
                            Some(1),
                            Some(36_500),
                            "1",
                            None,
                        ))
                        (number_field(
                            "media_attachment_decay_curve",
                            "Expiry curve",
                            &curve,
                            Some(0),
                            Some(1),
                            "0.05",
                            Some("0 is linear. 1 is logarithmic."),
                        ))
                        (number_field(
                            "media_attachment_decay_renew_threshold_days",
                            "Renew threshold (days)",
                            &renew_threshold_days,
                            Some(1),
                            Some(365),
                            "1",
                            Some("Viewed attachments renew when expiry is this close."),
                        ))
                        (number_field(
                            "media_attachment_decay_renew_window_days",
                            "Renew window (days)",
                            &renew_window_days,
                            Some(1),
                            Some(365),
                            "1",
                            Some("Renewed attachments are extended by this many days."),
                        ))
                    }
                    (form_actions(html! {
                        (submit_button("Save media expiry"))
                    }))
                }
            }
        },
    )
}

fn app_public_config_section(
    base: &str,
    csrf_token: &str,
    app_public: &AppPublicConfigResponse,
    self_hosted: bool,
) -> Markup {
    let setup_helper = if self_hosted {
        "When this is unchecked, the web app shows the self-host setup wizard."
    } else {
        "Hosted instances are treated as already configured by clients."
    };
    section_card_with_description(
        "Public App Identity",
        "Configure the public product name, client-visible brand assets, and initial setup state returned by the instance discovery document.",
        html! {
            div class="space-y-8" {
                form method="post" action={(base) "/instance-config?action=update_app_public"} {
                    (csrf_input(csrf_token))
                    div class="space-y-6" {
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (text_input(
                                "app_product_name",
                                "Product Name",
                                &app_public.branding.product_name,
                                "Fluxer",
                            ))
                            (text_input(
                                "app_theme_color",
                                "Theme Color",
                                app_public.branding.theme_color.as_deref().unwrap_or(""),
                                "#5865f2",
                            ))
                        }
                        div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                            (text_input(
                                "app_icon_url",
                                "Icon URL",
                                app_public.branding.icon_url.as_deref().unwrap_or(""),
                                "https://example.com/icon.png",
                            ))
                            (text_input(
                                "app_symbol_url",
                                "Symbol URL",
                                app_public.branding.symbol_url.as_deref().unwrap_or(""),
                                "https://example.com/symbol.png",
                            ))
                            (text_input(
                                "app_logo_url",
                                "Logo URL",
                                app_public.branding.logo_url.as_deref().unwrap_or(""),
                                "https://example.com/logo.png",
                            ))
                            (text_input(
                                "app_wordmark_url",
                                "Wordmark URL",
                                app_public.branding.wordmark_url.as_deref().unwrap_or(""),
                                "https://example.com/wordmark.png",
                            ))
                            (text_input(
                                "app_favicon_url",
                                "Favicon URL",
                                app_public.branding.favicon_url.as_deref().unwrap_or(""),
                                "https://example.com/favicon.ico",
                            ))
                        }
                        div class="space-y-2" {
                            (checkbox(
                                "app_setup_configured",
                                "true",
                                "Setup complete",
                                app_public.setup.configured,
                                true,
                            ))
                            p class="text-xs text-neutral-500" { (setup_helper) }
                        }
                        (form_actions(html! {
                            (submit_button("Save Public App Identity"))
                        }))
                    }
                }
                div class="space-y-4 border-t border-neutral-200 pt-6" {
                    h3 class="text-sm font-semibold text-neutral-900" { "Legal Documents" }
                    form method="post" action={(base) "/instance-config?action=update_app_legal"} {
                        (csrf_input(csrf_token))
                        div class="space-y-4" {
                            div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                                (text_input(
                                    "app_terms_url",
                                    "Terms URL",
                                    app_public.legal.terms_url.as_deref().unwrap_or(""),
                                    "https://example.com/terms",
                                ))
                                (text_input(
                                    "app_privacy_url",
                                    "Privacy URL",
                                    app_public.legal.privacy_url.as_deref().unwrap_or(""),
                                    "https://example.com/privacy",
                                ))
                            }
                            p class="text-xs text-neutral-500" {
                                "Registration asks for agreement only to the documents configured here. Leave both blank to hide legal consent on self-hosted registration."
                            }
                            (form_actions(html! {
                                (submit_button("Save Legal Documents"))
                            }))
                        }
                    }
                }
            }
        },
    )
}

fn gateway_rollout_section(
    base: &str,
    csrf_token: &str,
    gateway_rollout: &GatewayRolloutConfigResponse,
) -> Markup {
    section_card_with_description(
        "Gateway Rollout Configuration",
        "Control session and guild rollout percentages, NATS timeouts, and \
         concurrency limits for gateway nodes. Changes propagate instantly \
         via NATS.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_gateway_rollout"} {
                (csrf_input(csrf_token))
                div class="space-y-6" {
                    h3 class="text-sm font-semibold text-neutral-900" { "Rollout" }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (number_field(
                            "gateway_rollout_session_rollout_percentage",
                            "Session Rollout (%)", &format_percent(gateway_rollout.session_rollout_percentage),
                            Some(0), Some(100), "1",
                            Some("Percentage of sessions allowed to start (0-100)."),
                        ))
                        (select_input("gateway_rollout_session_rollout_mode", "Session Rollout Mode", &[
                            ("modulo", "Modulo"),
                            ("random", "Random"),
                        ], gateway_rollout.session_rollout_mode.as_str()))
                    }
                    (number_field(
                        "gateway_rollout_guild_rollout_percentage",
                        "Guild Rollout (%)", &format_percent(gateway_rollout.guild_rollout_percentage),
                        Some(0), Some(100), "1",
                        Some("Percentage of guilds allowed to start (0-100). Uses modulo mode."),
                    ))

                    h3 class="text-sm font-semibold text-neutral-900" { "RPC and Concurrency" }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-3" {
                        (number_field(
                            "gateway_rollout_rpc_request_timeout_ms",
                            "RPC Request Timeout (ms)", &gateway_rollout.rpc_request_timeout_ms.to_string(),
                            Some(1000), Some(60000), "1",
                            Some("Timeout for gateway-to-API RPC calls."),
                        ))
                        (number_field(
                            "gateway_rollout_max_concurrent_session_starts",
                            "Max Concurrent Session Starts", &gateway_rollout.max_concurrent_session_starts.to_string(),
                            Some(1), None, "1",
                            Some("Per-node concurrency limit."),
                        ))
                        (number_field(
                            "gateway_rollout_max_concurrent_guild_starts",
                            "Max Concurrent Guild Starts", &gateway_rollout.max_concurrent_guild_starts.to_string(),
                            Some(1), None, "1",
                            Some("Per-node concurrency limit."),
                        ))
                    }

                    h3 class="text-sm font-semibold text-neutral-900" { "Voice E2EE" }
                    (select_input("gateway_rollout_voice_e2ee_scope", "Enforcement scope", &[
                        ("guild_feature_only", "Guild feature only (require VOICE_E2EE on the guild; DMs/GDMs never E2EE)"),
                        ("platform_wide", "Platform-wide (every guild voice channel, DM, and GDM)"),
                    ], gateway_rollout.voice_e2ee_scope.as_str()))

                    (form_actions(html! {
                        (submit_button("Save Gateway Rollout Configuration"))
                    }))
                }
            }
        },
    )
}

fn registration_config_section(
    config: &AdminConfig,
    csrf_token: &str,
    registration: &InstanceRegistrationResponse,
    collect_date_of_birth: bool,
    self_hosted: bool,
) -> Markup {
    let base = &config.base_path;
    section_card_with_description(
        "Registration Controls",
        "Control public registration, admin-issued registration URLs, and pending approval requests.",
        html! {
            div class="space-y-8" {
                (registration_policy_form(base, csrf_token, registration))
                @if self_hosted {
                    (registration_fields_form(base, csrf_token, collect_date_of_birth))
                }
                (create_registration_url_form(base, csrf_token))
                (registration_url_list(config, csrf_token, &registration.urls))
                (pending_registration_list(config, csrf_token, &registration.pending_registrations))
            }
        },
    )
}

fn registration_policy_form(
    base: &str,
    csrf_token: &str,
    registration: &InstanceRegistrationResponse,
) -> Markup {
    html! {
        div class="space-y-4" {
            h3 class="text-sm font-semibold text-neutral-900" { "Registration Policy" }
            form method="post" action={(base) "/instance-config?action=update_registration"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (select_input("registration_mode", "Mode", &[
                            ("open", "Open"),
                            ("approval", "Approval required"),
                            ("closed", "Closed"),
                        ], registration.mode.as_str()))
                        div class="flex items-end pb-1" {
                            (checkbox(
                                "admin_registration_urls_enabled",
                                "true",
                                "Admin registration URLs enabled",
                                registration.admin_registration_urls_enabled,
                                true,
                            ))
                        }
                    }
                    (form_actions(html! {
                        (submit_button("Save Registration Policy"))
                    }))
                }
            }
        }
    }
}

fn registration_fields_form(base: &str, csrf_token: &str, collect_date_of_birth: bool) -> Markup {
    html! {
        div class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Registration Fields" }
            form method="post" action={(base) "/instance-config?action=update_app_registration"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (checkbox(
                        "app_collect_date_of_birth",
                        "true",
                        "Collect date of birth during registration",
                        collect_date_of_birth,
                        true,
                    ))
                    p class="text-xs text-neutral-500" {
                        "When disabled, new self-hosted accounts do not store a date of birth and are treated as adults."
                    }
                    (form_actions(html! {
                        (submit_button("Save Registration Fields"))
                    }))
                }
            }
        }
    }
}

fn create_registration_url_form(base: &str, csrf_token: &str) -> Markup {
    let action = format!("{base}/instance-config?action=create_registration_url");
    html! {
        div class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Create Admin-Issued Registration URL" }
            form method="post" action=(&action)
                hx-post=(&action)
                hx-target="#registration-url-list"
                hx-swap="outerHTML"
                hx-push-url="false"
                data-admin-allow-swap="true" {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4" {
                        (text_input(
                            "registration_url_label",
                            "Label",
                            "",
                            "Invite batch, partner, or support case",
                        ))
                        (number_field(
                            "registration_url_expires_in_days",
                            "Expires In Days",
                            "",
                            Some(1),
                            None,
                            "1",
                            Some("Leave blank for no expiration."),
                        ))
                        (number_field(
                            "registration_url_max_uses",
                            "Max Uses",
                            "",
                            Some(1),
                            Some(1_000_000),
                            "1",
                            Some("Leave blank for unlimited uses."),
                        ))
                        div class="flex items-end pb-1" {
                            (checkbox(
                                "registration_url_approval_required",
                                "true",
                                "Approval required",
                                false,
                                true,
                            ))
                        }
                    }
                    (form_actions(html! {
                        (submit_button("Create Registration URL"))
                    }))
                }
            }
        }
    }
}

pub fn registration_url_list(
    config: &AdminConfig,
    csrf_token: &str,
    urls: &[RegistrationUrlResponse],
) -> Markup {
    let base = &config.base_path;
    html! {
        div id="registration-url-list" class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Existing Registration URLs" }
            @if urls.is_empty() {
                p class="text-sm text-neutral-500" { "No admin-issued registration URLs." }
            } @else {
                div class="overflow-x-auto rounded-lg border border-neutral-200" {
                    table class="min-w-[1100px] divide-y divide-neutral-200 text-sm" {
                        thead class="bg-neutral-50" {
                            tr {
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Label / URL" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Created" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Expires" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Uses" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Status" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Approval" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Action" }
                            }
                        }
                        tbody class="divide-y divide-neutral-200 bg-white" {
                            @for url in urls {
                                (registration_url_row(config, base, csrf_token, url))
                            }
                        }
                    }
                }
            }
        }
    }
}

fn registration_url_row(
    config: &AdminConfig,
    base: &str,
    csrf_token: &str,
    url: &RegistrationUrlResponse,
) -> Markup {
    let label = url.label.as_deref().unwrap_or("Unlabeled");
    let full_url = admin_issued_registration_url(config, &url.id);
    let created_at = format_admin_timestamp(&url.created_at);
    let expires_at = format_optional_admin_timestamp(url.expires_at.as_deref(), "Never");
    let uses = registration_url_uses_text(url);
    let status = registration_url_status(url);
    let approval = if url.approval_required {
        "Required"
    } else {
        "Not required"
    };
    let revoke_action = format!("{base}/instance-config?action=revoke_registration_url");
    html! {
        tr {
            td class="min-w-[28rem] px-4 py-3 align-top" {
                p class="font-medium text-neutral-900" { (label) }
                (copyable_registration_url(&full_url, "Copy URL"))
                p class="mt-1 whitespace-nowrap text-xs text-neutral-500" { "ID: " (&url.id) }
            }
            td class="px-4 py-3 align-top text-neutral-700" {
                p class="whitespace-nowrap" { (created_at) }
                p class="whitespace-nowrap text-xs text-neutral-500" { "By: " (&url.created_by_user_id) }
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (expires_at)
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (uses)
            }
            td class="px-4 py-3 align-top whitespace-nowrap" {
                (registration_url_status_badge(status))
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (approval)
            }
            td class="px-4 py-3 align-top" {
                @if status == RegistrationUrlStatus::Active {
                    form method="post" action=(&revoke_action)
                        hx-post=(&revoke_action)
                        hx-target="#registration-url-list"
                        hx-swap="outerHTML"
                        hx-push-url="false"
                        data-admin-allow-swap="true" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="registration_url_id" value=(&url.id);
                        (compact_button("Revoke", true))
                    }
                } @else {
                    span class="text-sm text-neutral-400" { "No action" }
                }
            }
        }
    }
}

pub fn pending_registration_list(
    config: &AdminConfig,
    csrf_token: &str,
    pending_registrations: &[PendingRegistrationResponse],
) -> Markup {
    let base = &config.base_path;
    html! {
        div id="pending-registration-list" class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Pending Registrations" }
            @if pending_registrations.is_empty() {
                p class="text-sm text-neutral-500" { "No pending registrations." }
            } @else {
                div class="overflow-x-auto rounded-lg border border-neutral-200" {
                    table class="min-w-[940px] divide-y divide-neutral-200 text-sm" {
                        thead class="bg-neutral-50" {
                            tr {
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Applicant" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Requested" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Registration URL" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "IP" }
                                th scope="col" class="px-4 py-3 text-left font-semibold text-neutral-600" { "Action" }
                            }
                        }
                        tbody class="divide-y divide-neutral-200 bg-white" {
                            @for pending in pending_registrations {
                                (pending_registration_row(config, base, csrf_token, pending))
                            }
                        }
                    }
                }
            }
        }
    }
}

fn pending_registration_row(
    config: &AdminConfig,
    base: &str,
    csrf_token: &str,
    pending: &PendingRegistrationResponse,
) -> Markup {
    let account_name = pending_registration_account_name(pending);
    let requested_at = format_admin_timestamp(&pending.requested_at);
    let email = pending.email.as_deref().unwrap_or("None");
    let link_id = pending.registration_url_id.as_deref().unwrap_or("None");
    let client_ip = pending.client_ip.as_deref().unwrap_or("None");
    let approve_action = format!("{base}/instance-config?action=approve_pending_registration");
    let reject_action = format!("{base}/instance-config?action=reject_pending_registration");
    html! {
        tr {
            td class="max-w-[20rem] px-4 py-3 align-top" {
                p class="font-medium text-neutral-900" { (account_name) }
                @if let Some(global_name) = pending.global_name.as_deref() {
                    p class="truncate text-xs text-neutral-500" title=(global_name) { (global_name) }
                }
                p class="truncate text-xs text-neutral-500" title=(email) { (email) }
                p class="whitespace-nowrap text-xs text-neutral-500" { "ID: " (&pending.user_id) }
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (requested_at)
            }
            td class="px-4 py-3 align-top text-neutral-700" {
                @if let Some(registration_url_id) = pending.registration_url_id.as_deref() {
                    @let full_url = admin_issued_registration_url(config, registration_url_id);
                    p class="whitespace-nowrap text-xs text-neutral-500" { "ID: " (registration_url_id) }
                    div class="mt-1" {
                        (compact_copy_button(&full_url, "Copy URL"))
                    }
                } @else {
                    span class="text-neutral-500" { (link_id) }
                }
            }
            td class="px-4 py-3 align-top text-neutral-700 whitespace-nowrap" {
                (client_ip)
            }
            td class="px-4 py-3 align-top" {
                div class="flex flex-nowrap gap-2" {
                    form method="post" action=(&approve_action)
                        hx-post=(&approve_action)
                        hx-target="#pending-registration-list"
                        hx-swap="outerHTML"
                        hx-push-url="false"
                        data-admin-allow-swap="true" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="user_id" value=(&pending.user_id);
                        (compact_button("Approve", false))
                    }
                    form method="post" action=(&reject_action)
                        hx-post=(&reject_action)
                        hx-target="#pending-registration-list"
                        hx-swap="outerHTML"
                        hx-push-url="false"
                        data-admin-allow-swap="true" {
                        (csrf_input(csrf_token))
                        input type="hidden" name="user_id" value=(&pending.user_id);
                        (compact_button("Reject", true))
                    }
                }
            }
        }
    }
}

fn admin_issued_registration_url(config: &AdminConfig, code: &str) -> String {
    format!(
        "{}/register?registration_url={}",
        config.web_app_endpoint.trim_end_matches('/'),
        urlencoding::encode(code)
    )
}

fn copyable_registration_url(value: &str, label: &str) -> Markup {
    html! {
        div class="mt-2 flex items-center gap-2" {
            input type="url" readonly value=(value)
                aria-label="Registration URL"
                class="h-8 min-w-0 flex-1 rounded-lg border border-neutral-300 bg-neutral-50 px-3 py-1.5 text-xs text-neutral-800";
            (compact_copy_button(value, label))
        }
    }
}

fn compact_copy_button(value: &str, label: &str) -> Markup {
    html! {
        button type="button"
            class="inline-flex h-8 shrink-0 items-center justify-center rounded-lg border border-neutral-300 bg-neutral-50 px-3 text-xs font-medium text-neutral-700 transition-all hover:border-neutral-400 hover:text-neutral-900 focus:outline-none focus:ring-2 focus:ring-brand-primary/20"
            data-copy-value=(value)
            onclick="window.__adminCopyToClipboard && window.__adminCopyToClipboard(this.dataset.copyValue, this, 'Copied')" {
            (label)
        }
    }
}

fn compact_button(label: &str, danger: bool) -> Markup {
    let variant_class = if danger {
        "bg-red-600 text-white hover:bg-red-700 focus:ring-red-500/30"
    } else {
        "bg-neutral-900 text-white hover:bg-neutral-800 focus:ring-brand-primary/20"
    };
    html! {
        button type="submit"
            class={"inline-flex h-8 shrink-0 items-center justify-center rounded-lg px-3 text-xs font-medium transition-all focus:outline-none focus:ring-2 " (variant_class)} {
            (label)
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum RegistrationUrlStatus {
    Active,
    Revoked,
    Expired,
    Exhausted,
}

impl RegistrationUrlStatus {
    fn label(self) -> &'static str {
        match self {
            Self::Active => "Active",
            Self::Revoked => "Revoked",
            Self::Expired => "Expired",
            Self::Exhausted => "Exhausted",
        }
    }

    fn variant(self) -> BadgeVariant {
        match self {
            Self::Active => BadgeVariant::Success,
            Self::Revoked => BadgeVariant::Danger,
            Self::Expired | Self::Exhausted => BadgeVariant::Warning,
        }
    }
}

fn registration_url_status(url: &RegistrationUrlResponse) -> RegistrationUrlStatus {
    if url.revoked_at.is_some() {
        return RegistrationUrlStatus::Revoked;
    }
    if url.expires_at.as_deref().is_some_and(timestamp_is_past) {
        return RegistrationUrlStatus::Expired;
    }
    if url
        .max_uses
        .is_some_and(|max_uses| url.use_count >= max_uses)
    {
        return RegistrationUrlStatus::Exhausted;
    }
    RegistrationUrlStatus::Active
}

fn registration_url_status_badge(status: RegistrationUrlStatus) -> Markup {
    badge(status.label(), status.variant())
}

fn timestamp_is_past(iso: &str) -> bool {
    time::OffsetDateTime::parse(iso, &time::format_description::well_known::Rfc3339)
        .or_else(|_| {
            time::OffsetDateTime::parse(
                iso,
                &time::format_description::well_known::Iso8601::DEFAULT,
            )
        })
        .map(|timestamp| timestamp < time::OffsetDateTime::now_utc())
        .unwrap_or(false)
}

fn format_optional_admin_timestamp(value: Option<&str>, fallback: &str) -> String {
    value
        .map(format_admin_timestamp)
        .unwrap_or_else(|| fallback.to_owned())
}

fn registration_url_uses_text(url: &RegistrationUrlResponse) -> String {
    match url.max_uses {
        Some(max_uses) => format!("{} / {}", url.use_count, max_uses),
        None => format!("{} / unlimited", url.use_count),
    }
}

fn pending_registration_account_name(pending: &PendingRegistrationResponse) -> String {
    if pending.discriminator == 0 {
        pending.username.clone()
    } else {
        format!("{}#{:04}", pending.username, pending.discriminator)
    }
}

fn sso_config_section(base: &str, csrf_token: &str, sso: &SsoConfigResponse) -> Markup {
    let allowed_domains = sso.allowed_domains.join("\n");
    let secret_caption = if sso.client_secret_set {
        "A secret is set. Check to clear, or enter a new value to rotate."
    } else {
        "No secret set yet."
    };
    section_card_with_description(
        "Single Sign-On (SSO)",
        "Configure OIDC-style SSO for the admin and client apps. When enabled, \
         users can sign in through your SSO provider. Require SSO to block local \
         password registration and login.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_sso"} {
                (csrf_input(csrf_token))
                div class="space-y-6" {
                    div class="flex flex-col gap-2" {
                        (checkbox("sso_enabled", "true", "Enable SSO", sso.enabled, true))
                        (checkbox("sso_enforced", "true", "Require SSO (disables local password login and registration)", sso.enforced, true))
                        (checkbox("sso_auto_provision", "true", "Automatically provision users on first SSO login", sso.auto_provision, true))
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (text_input("sso_display_name", "Display Name", sso.display_name.as_deref().unwrap_or(""), "Example Identity Provider"))
                        (text_input("sso_issuer", "Issuer", sso.issuer.as_deref().unwrap_or(""), "https://idp.example.com"))
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (text_input("sso_authorization_url", "Authorization URL", sso.authorization_url.as_deref().unwrap_or(""), "https://idp.example.com/oauth/authorize"))
                        (text_input("sso_token_url", "Token URL", sso.token_url.as_deref().unwrap_or(""), "https://idp.example.com/oauth/token"))
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (text_input("sso_userinfo_url", "User Info URL", sso.userinfo_url.as_deref().unwrap_or(""), "https://idp.example.com/oauth/userinfo"))
                        (text_input("sso_jwks_url", "JWKS URL", sso.jwks_url.as_deref().unwrap_or(""), "https://idp.example.com/.well-known/jwks.json"))
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (text_input("sso_client_id", "Client ID", sso.client_id.as_deref().unwrap_or(""), "client-id"))
                        div class="space-y-2" {
                            (form_field_group("Client Secret", "sso_client_secret", false, None, None, html! {
                                input type="password" id="sso_client_secret" name="sso_client_secret"
                                    placeholder="Leave blank to keep existing"
                                    class=(FORM_INPUT_CLASS);
                            }))
                            (checkbox("sso_clear_client_secret", "true", "Clear secret", false, true))
                            p class="text-xs text-neutral-500" {
                                (secret_caption)
                            }
                        }
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (text_input("sso_scope", "Scope", sso.scope.as_deref().unwrap_or(""), "openid email profile"))
                        (form_field_group("Redirect URI", "sso_redirect_uri", false, None,
                            Some("Configure this exact URI in your IdP. It is derived from the public gateway URL."),
                            html! {
                                input type="url" id="sso_redirect_uri" name="sso_redirect_uri"
                                    value=(sso.redirect_uri.as_deref().unwrap_or(""))
                                    disabled class=(FORM_INPUT_CLASS);
                            },
                        ))
                    }
                    div class="space-y-2" {
                        (textarea_input(
                            "sso_allowed_domains",
                            "Allowed Email Domains",
                            "example.com\nexample.org",
                            &allowed_domains,
                            3,
                            false,
                        ))
                        p class="text-xs text-neutral-500" {
                            "Limit SSO logins to these domains (one per line). Leave empty to allow any verified email."
                        }
                    }
                    (form_actions(html! {
                        (submit_button("Save SSO Settings"))
                    }))
                }
            }
        },
    )
}

fn limit_config_section(base: &str, limit_config: &LimitConfigResponse) -> Markup {
    let description = if limit_config.self_hosted.unwrap_or(false) {
        "Self-hosted instance with all premium features enabled. Configure user and guild limits."
    } else {
        "Configure limit rules that control user and guild restrictions based on traits and features."
    };
    section_card_simple(
        "Limit Configuration",
        html! {
            div class="flex flex-col gap-2 sm:flex-row sm:items-center sm:justify-between" {
                div class="space-y-1" {
                    p class="text-sm text-neutral-500" {
                        (description)
                    }
                }
                (secondary_button_link("Configure Limits", &format!("{base}/limit-config")))
            }
        },
    )
}
