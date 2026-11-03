// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{
        AccountIdentitySettings, AppPublicConfigResponse, CaptchaProvider, CleanupMode,
        DateOfBirthPolicy, EXPERIMENT_MAX_ROLLOUT_COUNTRY_CODES, EXPERIMENT_MAX_TARGETED_USERS,
        EmailDeliveryMode, EmailDeliveryState, ExperimentDeliveryConfigResponse,
        GUILD_VISITS_DEFAULT_SALT, GatewayRolloutConfigResponse, GuildJoinGate,
        GuildVisitsConfigResponse, IdentifierFirstMode, InstanceConfigResponse,
        InstanceNoticeResponse, InteractionTierMode, LastSessionCleanupMode, LimitConfigResponse,
        PushRelayConfigResponse, VerificationBasis,
    },
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            form::{
                Checkbox, FORM_INPUT_CLASS, FORM_LABEL_CLASS, FORM_TEXTAREA_CLASS, NumberField,
                csrf_input, form_actions, form_field_group, secondary_button_link, select_input,
                submit_button, text_input, textarea_input,
            },
            page_container::page_header,
            section_card::{section_card_simple, section_card_with_description},
        },
        layout::admin_layout,
        pages::{
            instance_billing::premium_billing_section, instance_branding::branding_section,
            instance_registration::registration_config_section,
        },
    },
    utils::timestamps::format_admin_timestamp,
};
use maud::{Markup, html};

mod accounts;
mod captcha;
mod experiments;
mod integrations;
mod media;
mod policy;
mod sso;

use self::{
    accounts::{AccountsSectionInput, account_identity_section, accounts_section},
    captcha::captcha_section,
    experiments::{domain_migration_section, voice_p2p_section},
    integrations::integrations_config_section,
    media::{HostingMode, media_config_section},
    policy::policy_config_section,
    sso::sso_config_section,
};

pub const LOADED_CONFIG_FIELD: &str = "loaded_config";

#[derive(Clone, Copy)]
pub(crate) struct ConfigForm<'a> {
    pub(crate) csrf_token: &'a str,
    pub(crate) loaded: &'a str,
}

impl ConfigForm<'_> {
    pub(crate) fn fields(&self) -> Markup {
        html! {
            (csrf_input(self.csrf_token))
            input type="hidden" name=(LOADED_CONFIG_FIELD) value=(self.loaded);
        }
    }
}

pub const REGISTRATION_ALLOWED_EMAIL_DOMAINS_FIELD: &str = "registration_allowed_email_domains";
pub const SSO_ALLOWED_DOMAINS_FIELD: &str = "sso_allowed_domains";

fn inherit_select(name: &str, label: &str, override_value: Option<bool>) -> Markup {
    let selected = match override_value {
        None => "inherit",
        Some(true) => "on",
        Some(false) => "off",
    };
    select_input(
        name,
        label,
        &[
            ("inherit", "Inherit env default"),
            ("on", "Force on"),
            ("off", "Force off"),
        ],
        selected,
    )
}

fn format_decimal(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{}", value as u64)
    } else {
        value.to_string()
    }
}

fn entry_count_hint(count: usize, cap: usize) -> Markup {
    html! {
        p class="text-xs text-neutral-500" {
            (count) " of " (cap) " stored"
            @if count >= cap {
                " (at the cap; remove an entry before adding another)"
            }
        }
    }
}

fn experiment_rollout_country_codes_field(name: &str, country_codes: &[String]) -> Markup {
    html! {
        div class="flex flex-col gap-2" {
            (text_input(name, "Rollout Countries", &country_codes.join(", "), "SE, NO, FI"))
            (entry_count_hint(country_codes.len(), EXPERIMENT_MAX_ROLLOUT_COUNTRY_CODES))
            p class="text-xs text-neutral-500" {
                "Two-letter ISO 3166-1 country codes, separated by commas or spaces. When set, \
                 the user rollout percentage only applies to requests from these countries, and \
                 a request with an unknown country is left out. Leave empty to apply the \
                 percentage everywhere. The always-on lists, the premium switch and the \
                 never-on list ignore the country. Invalid entries prevent the save."
            }
        }
    }
}

pub fn instance_config_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    instance_config: Option<&InstanceConfigResponse>,
    limit_config: Option<&LimitConfigResponse>,
) -> Markup {
    let base = &config.base_path;
    let loaded = instance_config
        .map(InstanceConfigResponse::loaded_marker)
        .unwrap_or_default();
    let config_form = ConfigForm {
        csrf_token,
        loaded: &loaded,
    };
    let content = html! {
        (page_header("Instance Configuration", Some("Manage instance-wide settings")))
        div class="space-y-10" {
            @if let Some(instance_config) = instance_config {
                @if instance_config.self_hosted {
                    (config_group(
                        "Identity & branding",
                        "How the instance presents itself to clients and the public discovery document.",
                        app_public_config_section(
                                config,
                                config_form,
                                &instance_config.app_public,
                            ),
                    ))
                }
                (config_group(
                    "Notice",
                    "A message for everyone on the instance, shown in the app.",
                    notice_section(base, config_form, instance_config.notice.as_ref()),
                ))
                (config_group(
                    "Access & accounts",
                    "Who can sign in and create accounts on this instance.",
                    html! {
                        @if instance_config.self_hosted {
                            (account_identity_section(&instance_config.account_identity))
                        }
                        (registration_config_section(
                            config,
                            config_form,
                            &instance_config.registration,
                            instance_config.account_identity.mode,
                        ))
                        (sso_config_section(
                            base,
                            config_form,
                            &instance_config.sso,
                            instance_config.account_identity.mode,
                        ))
                        (accounts_section(
                            base,
                            config_form,
                            &AccountsSectionInput {
                                auth: &instance_config.auth,
                                accounts: &instance_config.accounts,
                                identity: AccountIdentitySettings {
                                    mode: instance_config.account_identity.mode,
                                    tag_style: instance_config.account_identity.tag_style,
                                    emails_enabled: instance_config.integrations.email.delivery_state
                                        == EmailDeliveryState::On,
                                    username_from_provider: instance_config.sso.enabled
                                        && instance_config.sso.username_from_provider,
                                },
                                delivery_state: instance_config.integrations.email.delivery_state,
                                captcha_provider: instance_config.captcha.provider,
                                guild_visits_enabled: instance_config.guild_visits.enabled,
                            },
                        ))
                        (guild_visits_section(base, config_form, &instance_config.guild_visits))
                    },
                ))
                (config_group(
                    "Bot protection",
                    "A proof-of-work check on sign-up, login, password reset and a few other abuse-prone actions.",
                    captcha_section(
                            base,
                            config_form,
                            &instance_config.captcha,
                            instance_config.auth.instant_accounts.enabled,
                        ),
                ))
                @if instance_config.self_hosted {
                    (config_group(
                        "Community & policy",
                        "Community shape, direct messaging, the premium model, and optional embed services.",
                        policy_config_section(
                                base,
                                config_form,
                                &instance_config.policy,
                                &instance_config.app_public.branding.premium_product_name,
                                instance_config.billing.enabled == Some(true),
                            ),
                    ))
                    (config_group(
                        "Premium & billing",
                        "The premium tier's name, Stripe credentials and the prices members pay.",
                        premium_billing_section(
                                base,
                                config_form,
                                &instance_config.app_public.branding,
                                &instance_config.billing,
                                instance_config.policy.premium_mode,
                            ),
                    ))
                }
                (config_group(
                    "Runtime integrations",
                    "Credentials and provider choices that override environment variables at runtime.",
                    integrations_config_section(
                            base,
                            config_form,
                            &instance_config.integrations,
                            instance_config.account_identity.mode,
                            &instance_config.app_public.branding.product_name,
                        ),
                ))
                (config_group(
                    "Push notifications",
                    "Consent for the relay that delivers official mobile app notifications.",
                    push_relay_section(base, config_form, &instance_config.push_relay),
                ))
                (config_group(
                    "Media & retention",
                    "Attachment expiry rules that can be changed without editing environment variables.",
                    media_config_section(
                            base,
                            config_form,
                            &instance_config.media,
                            if instance_config.self_hosted {
                                HostingMode::SelfHosted
                            } else {
                                HostingMode::Hosted
                            },
                        ),
                ))
                (config_group(
                    "Infrastructure & limits",
                    "Gateway rollout behavior and the limit rules applied to users and guilds.",
                    html! {
                        (gateway_rollout_section(base, config_form, &instance_config.gateway_rollout))
                        @if !instance_config.self_hosted {
                            (domain_migration_section(base, config_form, &instance_config.domain_migration))
                        }
                        (voice_p2p_section(base, config_form, &instance_config.voice_p2p))
                        (experiment_delivery_section(base, config_form, &instance_config.experiment_delivery))
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

fn notice_section(
    base: &str,
    config_form: ConfigForm<'_>,
    notice: Option<&InstanceNoticeResponse>,
) -> Markup {
    let local_value = |value: Option<&str>| {
        value
            .map(|iso| iso.chars().take(16).collect::<String>())
            .unwrap_or_default()
    };
    let starts_at = local_value(notice.and_then(|notice| notice.starts_at.as_deref()));
    let ends_at = local_value(notice.and_then(|notice| notice.ends_at.as_deref()));
    section_card_with_description(
        "Instance notice",
        "A short message shown to everyone in the app, such as planned maintenance. It appears \
         from the start time until the end time, and people can dismiss it.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_notice"} {
                (config_form.fields())
                div class="space-y-4" {
                    (textarea_input(
                        "notice_message",
                        "Message",
                        "We are upgrading the server on Saturday at 10:00 UTC. Expect about 15 minutes of downtime.",
                        notice.map(|notice| notice.message.as_str()).unwrap_or(""),
                        3,
                        false,
                    ))
                    (text_input(
                        "notice_url",
                        "Link (optional)",
                        notice.and_then(|notice| notice.url.as_deref()).unwrap_or(""),
                        "https://status.example.com",
                    ))
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (form_field_group("Starts (UTC, optional)", "notice_starts_at", false, None, None, html! {
                            input type="datetime-local" id="notice_starts_at" name="notice_starts_at"
                                value=(starts_at) class=(FORM_INPUT_CLASS);
                        }))
                        (form_field_group("Ends (UTC, optional)", "notice_ends_at", false, None, None, html! {
                            input type="datetime-local" id="notice_ends_at" name="notice_ends_at"
                                value=(ends_at) class=(FORM_INPUT_CLASS);
                        }))
                    }
                    @if notice.is_some() {
                        (Checkbox {
                            name: "notice_clear",
                            value: "true",
                            label: "Remove the notice",
                            checked: false,
                            enabled: true,
                        }.render())
                    }
                    (form_actions(html! {
                        (submit_button("Save notice"))
                    }))
                }
            }
        },
    )
}

fn app_public_config_section(
    config: &AdminConfig,
    config_form: ConfigForm<'_>,
    app_public: &AppPublicConfigResponse,
) -> Markup {
    let base = &config.base_path;
    let host = web_app_host(&config.web_app_endpoint);
    section_card_with_description(
        "Public App Identity",
        "The name, color and images people see on the sign-in page, in the apps and in the browser. The preview shows each place they are used.",
        html! {
            div class="space-y-8" {
                (branding_section(config, config_form, app_public, &host))
                div class="space-y-4 border-t border-neutral-200 pt-6" {
                    h3 class="text-sm font-semibold text-neutral-900" { "Legal Documents" }
                    form method="post" action={(base) "/instance-config?action=update_app_legal"} {
                        (config_form.fields())
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
                                (text_input(
                                    "app_guidelines_url",
                                    "Community Guidelines URL",
                                    app_public.legal.guidelines_url.as_deref().unwrap_or(""),
                                    "https://example.com/guidelines",
                                ))
                            }
                            p class="text-xs text-neutral-500" {
                                "Registration asks for agreement only to the terms and privacy documents configured here. Leave both blank to hide legal consent on self-hosted registration."
                            }
                            p class="text-xs text-neutral-500" {
                                "The community guidelines URL is linked from the app, the report flow and enforcement emails. Use an absolute http or https URL. Leave it blank to hide those links."
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

fn web_app_host(endpoint: &str) -> String {
    endpoint
        .split_once("://")
        .map_or(endpoint, |(_, rest)| rest)
        .split('/')
        .next()
        .unwrap_or_default()
        .to_owned()
}

fn gateway_rollout_section(
    base: &str,
    config_form: ConfigForm<'_>,
    gateway_rollout: &GatewayRolloutConfigResponse,
) -> Markup {
    section_card_with_description(
        "Gateway Rollout Configuration",
        "Control session and guild rollout percentages, NATS timeouts, and \
         concurrency limits for gateway nodes. Changes propagate instantly \
         via NATS.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_gateway_rollout"} {
                (config_form.fields())
                div class="space-y-6" {
                    h3 class="text-sm font-semibold text-neutral-900" { "Rollout" }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (NumberField {
                            name: "gateway_rollout_session_rollout_percentage",
                            label: "Session Rollout (%)",
                            value: &format_decimal(gateway_rollout.session_rollout_percentage),
                            min: Some(0),
                            max: Some(100),
                            step: "1",
                            helper: Some("Percentage of sessions allowed to start (0-100)."),
                            placeholder: None,
                        }.render())
                        (select_input("gateway_rollout_session_rollout_mode", "Session Rollout Mode", &[
                            ("modulo", "Modulo"),
                            ("random", "Random"),
                        ], gateway_rollout.session_rollout_mode.as_str()))
                    }
                    (NumberField {
                        name: "gateway_rollout_guild_rollout_percentage",
                        label: "Guild Rollout (%)",
                        value: &format_decimal(gateway_rollout.guild_rollout_percentage),
                        min: Some(0),
                        max: Some(100),
                        step: "1",
                        helper: Some("Percentage of guilds allowed to start (0-100). Uses modulo mode."),
                        placeholder: None,
                    }.render())

                    h3 class="text-sm font-semibold text-neutral-900" { "RPC and Concurrency" }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-3" {
                        (NumberField {
                            name: "gateway_rollout_rpc_request_timeout_ms",
                            label: "RPC Request Timeout (ms)",
                            value: &gateway_rollout.rpc_request_timeout_ms.to_string(),
                            min: Some(1000),
                            max: Some(60000),
                            step: "1",
                            helper: Some("Timeout for gateway-to-API RPC calls."),
                            placeholder: None,
                        }.render())
                        (NumberField {
                            name: "gateway_rollout_max_concurrent_session_starts",
                            label: "Max Concurrent Session Starts",
                            value: &gateway_rollout.max_concurrent_session_starts.to_string(),
                            min: Some(1),
                            max: None,
                            step: "1",
                            helper: Some("Per-node concurrency limit."),
                            placeholder: None,
                        }.render())
                        (NumberField {
                            name: "gateway_rollout_max_concurrent_guild_starts",
                            label: "Max Concurrent Guild Starts",
                            value: &gateway_rollout.max_concurrent_guild_starts.to_string(),
                            min: Some(1),
                            max: None,
                            step: "1",
                            helper: Some("Per-node concurrency limit."),
                            placeholder: None,
                        }.render())
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

fn push_relay_section(
    base: &str,
    config_form: ConfigForm<'_>,
    push_relay: &PushRelayConfigResponse,
) -> Markup {
    let status = if push_relay.relay_consent_accepted {
        ("Accepted", BadgeVariant::Success)
    } else {
        ("Not accepted", BadgeVariant::Default)
    };
    let accepted_at =
        format_optional_admin_timestamp(push_relay.relay_consent_accepted_at.as_deref(), "Never");
    let accepted_by = push_relay
        .relay_consent_accepted_by
        .as_deref()
        .unwrap_or("Nobody");
    section_card_with_description(
        "Push Relay",
        "Official mobile app notifications travel through Fluxer's relay to Apple and Google. \
         The relay delivers them only after an operator accepts its privacy notice.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_push_relay"} {
                (config_form.fields())
                div class="space-y-6" {
                    div class="flex flex-wrap items-center gap-2" {
                        h3 class="text-sm font-semibold text-neutral-900" { "Relay consent" }
                        (badge(status.0, status.1))
                    }
                    (Checkbox {
                        name: "push_relay_consent_accepted",
                        value: "true",
                        label: "Accept the push relay supplemental privacy notice",
                        checked: push_relay.relay_consent_accepted,
                        enabled: true,
                    }.render())
                    p class="text-xs text-neutral-500" {
                        "Until this is accepted official mobile app notifications are dropped. \
                         Self-hosted UnifiedPush and ntfy endpoints never reach the relay and are \
                         unaffected. "
                        a href="https://fluxer.com/push-relay" target="_blank" rel="noreferrer"
                            class="text-neutral-900 underline decoration-neutral-300 hover:text-neutral-600 hover:decoration-neutral-500" {
                            "Read the notice"
                        }
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (form_field_group("Accepted at", "push_relay_consent_accepted_at", false, None, None,
                            html! {
                                input type="text" id="push_relay_consent_accepted_at"
                                    value=(accepted_at)
                                    disabled class=(FORM_INPUT_CLASS);
                            },
                        ))
                        (form_field_group("Accepted by user ID", "push_relay_consent_accepted_by", false, None, None,
                            html! {
                                input type="text" id="push_relay_consent_accepted_by"
                                    value=(accepted_by)
                                    disabled class=(FORM_INPUT_CLASS);
                            },
                        ))
                    }

                    (form_actions(html! {
                        (submit_button("Save Push Relay Settings"))
                    }))
                }
            }
        },
    )
}

const IDENTIFIER_FIRST_CHOICES: &[(IdentifierFirstMode, &str)] = &[
    (
        IdentifierFirstMode::Auto,
        "Automatic, on while the instance sends email",
    ),
    (IdentifierFirstMode::On, "On"),
    (IdentifierFirstMode::Off, "Off"),
];

const DATE_OF_BIRTH_CHOICES: &[(DateOfBirthPolicy, &str)] = &[
    (DateOfBirthPolicy::Off, "Don't ask"),
    (DateOfBirthPolicy::Ask, "Ask, but let people skip it"),
    (
        DateOfBirthPolicy::RequireNewAccounts,
        "Require it from accounts created without one",
    ),
    (
        DateOfBirthPolicy::RequireAll,
        "Require it from every account without one",
    ),
];

const INTERACTION_TIER_CHOICES: &[(InteractionTierMode, &str)] = &[
    (InteractionTierMode::Legacy, "Keep the older limits"),
    (
        InteractionTierMode::Enforced,
        "Only allow what does not reach other people",
    ),
];

const GUILD_JOIN_GATE_CHOICES: &[(GuildJoinGate, &str)] = &[
    (GuildJoinGate::Off, "Never ask"),
    (
        GuildJoinGate::VisitsCapable,
        "Ask only on clients that can preview communities",
    ),
    (GuildJoinGate::Always, "Ask on every client"),
];

const VERIFICATION_BASIS_CHOICES: &[(VerificationBasis, &str)] = &[
    (
        VerificationBasis::Auto,
        "Automatic, a confirmed email while the instance sends email, otherwise a password or passkey",
    ),
    (VerificationBasis::ConfirmedEmail, "A confirmed email"),
    (VerificationBasis::LocalCredential, "A password or passkey"),
];

const CLEANUP_MODE_CHOICES: &[(CleanupMode, &str)] = &[
    (CleanupMode::Off, "Off"),
    (
        CleanupMode::AfterInactivity,
        "After the days below without use",
    ),
];

const LAST_SESSION_CLEANUP_CHOICES: &[(LastSessionCleanupMode, &str)] = &[
    (LastSessionCleanupMode::Off, "Keep the account"),
    (
        LastSessionCleanupMode::AfterGrace,
        "Delete the account after the grace period",
    ),
];

const CAPTCHA_PROVIDER_CHOICES: &[(CaptchaProvider, &str)] = &[
    (CaptchaProvider::Altcha, "Proof-of-work check"),
    (CaptchaProvider::None, "Off"),
];

const EMAIL_DELIVERY_CHOICES: &[(EmailDeliveryMode, &str)] = &[
    (EmailDeliveryMode::Smtp, "Send email through SMTP"),
    (EmailDeliveryMode::Off, "Off"),
];

fn choice_options<T: std::fmt::Display>(
    choices: &[(T, &'static str)],
) -> Vec<(String, &'static str)> {
    choices
        .iter()
        .map(|(value, label)| (value.to_string(), *label))
        .collect()
}

fn choice_select<T: std::fmt::Display>(
    name: &str,
    label: &str,
    choices: &[(T, &'static str)],
    selected: &T,
) -> Markup {
    let options = choice_options(choices);
    let options: Vec<(&str, &str)> = options
        .iter()
        .map(|(value, label)| (value.as_str(), *label))
        .collect();
    select_input(name, label, &options, &selected.to_string())
}

fn guild_visits_section(
    base: &str,
    config_form: ConfigForm<'_>,
    guild_visits: &GuildVisitsConfigResponse,
) -> Markup {
    let status = if guild_visits.enabled {
        ("On", BadgeVariant::Success)
    } else {
        ("Off", BadgeVariant::Default)
    };
    section_card_with_description(
        "Community previews",
        "Lets assigned people preview a community before joining it. For them, joining follows \
         the \u{201c}Joining a community\u{201d} setting under Sign-in & new accounts while the \
         stricter limits are on.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_guild_visits"} {
                (config_form.fields())
                div class="space-y-6" {
                    div class="flex flex-wrap items-center gap-2" {
                        (badge(status.0, status.1))
                    }
                    (Checkbox {
                        name: "guild_visits_enabled",
                        value: "true",
                        label: "Allow community previews",
                        checked: guild_visits.enabled,
                        enabled: true,
                    }.render())
                    (NumberField {
                        name: "guild_visits_user_basis_points",
                        label: "Rollout (basis points)",
                        value: &guild_visits.user_basis_points.to_string(),
                        min: Some(0),
                        max: Some(10000),
                        step: "1",
                        helper: Some("Share of people assigned: 0 is nobody, 100 is 1%, 10000 is everybody."),
                        placeholder: None,
                    }.render())
                    (text_input(
                        "guild_visits_user_salt",
                        "Rollout Salt",
                        &guild_visits.user_salt,
                        GUILD_VISITS_DEFAULT_SALT,
                    ))
                    div class="flex flex-col gap-2" {
                        (textarea_input(
                            "guild_visits_included_user_ids",
                            "Always-on User IDs",
                            "1500000000000000001\n1500000000000000002",
                            &guild_visits.included_user_ids.join("\n"),
                            4,
                            false,
                        ))
                        (entry_count_hint(
                            guild_visits.included_user_ids.len(),
                            EXPERIMENT_MAX_TARGETED_USERS,
                        ))
                    }
                    div class="flex flex-col gap-2" {
                        (textarea_input(
                            "guild_visits_excluded_user_ids",
                            "Never-on User IDs",
                            "1500000000000000003",
                            &guild_visits.excluded_user_ids.join("\n"),
                            4,
                            false,
                        ))
                        (entry_count_hint(
                            guild_visits.excluded_user_ids.len(),
                            EXPERIMENT_MAX_TARGETED_USERS,
                        ))
                    }
                    (form_actions(html! {
                        (submit_button("Save"))
                    }))
                }
            }
        },
    )
}

fn experiment_delivery_section(
    base: &str,
    config_form: ConfigForm<'_>,
    experiment_delivery: &ExperimentDeliveryConfigResponse,
) -> Markup {
    section_card_with_description(
        "Experiment Delivery",
        "How often every client revalidates its experiment assignments. This is instance-wide \
         and covers every experiment, not just the ones above. Raising the interval sheds \
         request volume and makes a change take longer to reach a client. Raising the jitter \
         spreads a fleet that has synchronized on one tick back out across the interval.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_experiment_delivery"} {
                (config_form.fields())
                div class="space-y-6" {
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (NumberField {
                            name: "experiment_delivery_poll_interval_seconds",
                            label: "Assignment Poll Interval (s)",
                            value: &experiment_delivery.poll_interval_seconds.to_string(),
                            min: Some(60),
                            max: Some(86400),
                            step: "1",
                            helper: Some("How often a client re-reads its assignments, 60 to 86400 seconds. Lower values pick up changes sooner at the cost of more requests."),
                            placeholder: None,
                        }.render())
                        (NumberField {
                            name: "experiment_delivery_poll_jitter_percent",
                            label: "Assignment Poll Jitter (%)",
                            value: &experiment_delivery.poll_jitter_percent.to_string(),
                            min: Some(0),
                            max: Some(50),
                            step: "1",
                            helper: Some("How far each client spreads its poll around the interval, 0 to 50 percent. Raise it to break up a fleet that polls on the same tick, set it to 0 for an exact interval."),
                            placeholder: None,
                        }.render())
                    }

                    (form_actions(html! {
                        (submit_button("Save Experiment Delivery Configuration"))
                    }))
                }
            }
        },
    )
}

pub(super) fn format_optional_admin_timestamp(value: Option<&str>, fallback: &str) -> String {
    value
        .map(format_admin_timestamp)
        .unwrap_or_else(|| fallback.to_owned())
}

pub(super) fn allowed_domains_input(name: &str, value: &str, hint: &str) -> Markup {
    let hint_id = format!("{name}-hint");
    let error_id = format!("{name}-error");
    html! {
        div class="space-y-2" {
            div class="flex flex-col gap-2" {
                label for=(name) class=(FORM_LABEL_CLASS) { "Allowed Email Domains" }
                textarea id=(name) name=(name) placeholder="example.com\nexample.org" rows="3"
                    aria-describedby={(error_id) " " (hint_id)}
                    class=(FORM_TEXTAREA_CLASS) {
                    (value)
                }
            }
            p id=(error_id) class="text-xs leading-5 text-red-600"
                data-field-error=(name) hidden {}
            p id=(hint_id) class="text-xs text-neutral-500" { (hint) }
        }
    }
}

fn limit_config_section(base: &str, limit_config: &LimitConfigResponse) -> Markup {
    let description = if limit_config.self_hosted.unwrap_or(false) {
        "Self-hosted instance with all premium features enabled by default. Configure user and guild limits."
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
