// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::templates::pages::instance_config::ConfigForm;
use crate::{
    api::types::{AccountIdentityMode, SsoConfigResponse},
    templates::components::{
        form::{
            Checkbox, FORM_INPUT_CLASS, form_actions, form_field_group, submit_button, text_input,
            textarea_input,
        },
        section_card::section_card_with_description,
    },
};
use maud::{Markup, html};

use super::{SSO_ALLOWED_DOMAINS_FIELD, allowed_domains_input};

const SSO_BUTTON_LABEL_MAX_CHARS: usize = 64;

pub(super) fn sso_config_section(
    base: &str,
    config_form: ConfigForm<'_>,
    sso: &SsoConfigResponse,
    identity: AccountIdentityMode,
) -> Markup {
    let allowed_domains = sso.allowed_domains.join("\n");
    let group_role_mappings = sso
        .group_role_mappings
        .iter()
        .map(|mapping| format!("{} = {}", mapping.group, mapping.role_id))
        .collect::<Vec<_>>()
        .join("\n");
    let button_label_placeholder = match sso.display_name.as_deref().map(str::trim) {
        Some(name) if !name.is_empty() => format!("Continue with {name}"),
        _ => "Continue with SSO".to_string(),
    };
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
                (config_form.fields())
                div class="space-y-6" {
                    div class="flex flex-col gap-2" {
                        (Checkbox {
                            name: "sso_enabled",
                            value: "true",
                            label: "Enable SSO",
                            checked: sso.enabled,
                            enabled: true,
                        }.render())
                        (Checkbox {
                            name: "sso_enforced",
                            value: "true",
                            label: "Require SSO (disables local password login and registration)",
                            checked: sso.enforced,
                            enabled: true,
                        }.render())
                        (Checkbox {
                            name: "sso_auto_provision",
                            value: "true",
                            label: "Automatically provision users on first SSO login",
                            checked: sso.auto_provision,
                            enabled: true,
                        }.render())
                        (Checkbox {
                            name: "sso_username_from_provider",
                            value: "true",
                            label: "Usernames come from your sign-in provider",
                            checked: sso.username_from_provider,
                            enabled: identity.is_username(),
                        }.render())
                        p class="text-xs text-neutral-500" {
                            @if identity.is_username() {
                                "Every SSO sign-in sets the person's username from the provider, and people can't change it in Fluxer."
                            } @else {
                                "Only for instances where people sign in with a username."
                            }
                        }
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (text_input("sso_display_name", "Display Name", sso.display_name.as_deref().unwrap_or(""), "Example Identity Provider"))
                        (text_input("sso_issuer", "Issuer", sso.issuer.as_deref().unwrap_or(""), "https://idp.example.com"))
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (form_field_group("Button Text", "sso_button_label", false, None,
                            Some("Shown on the sign-in button. Leave empty to use \"Continue with\" and the display name, or \"Continue with SSO\" if there's no display name."),
                            html! {
                                input type="text" id="sso_button_label" name="sso_button_label"
                                    value=(sso.button_label.as_deref().unwrap_or(""))
                                    placeholder=(button_label_placeholder)
                                    maxlength=(SSO_BUTTON_LABEL_MAX_CHARS)
                                    class=(FORM_INPUT_CLASS);
                            },
                        ))
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
                            (Checkbox {
                                name: "sso_clear_client_secret",
                                value: "true",
                                label: "Clear secret",
                                checked: false,
                                enabled: true,
                            }.render())
                            p class="text-xs text-neutral-500" {
                                (secret_caption)
                            }
                        }
                    }
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (text_input("sso_scope", "Scope", sso.scope.as_deref().unwrap_or(""), "openid email profile"))
                        (form_field_group("Redirect URI", "sso_redirect_uri", false, None,
                            Some("Register this exact URI with your identity provider. It is the web app address followed by /auth/sso/callback. The desktop and mobile apps sign in through the same URI."),
                            html! {
                                input type="url" id="sso_redirect_uri" name="sso_redirect_uri"
                                    value=(sso.redirect_uri.as_deref().unwrap_or(""))
                                    disabled class=(FORM_INPUT_CLASS);
                            },
                        ))
                    }
                    (allowed_domains_input(
                        SSO_ALLOWED_DOMAINS_FIELD,
                        &allowed_domains,
                        "Limit SSO logins to these domains (one per line). Leave empty to allow any verified email.",
                    ))
                    div class="space-y-2" {
                        (text_input("sso_groups_claim", "Groups Claim", sso.groups_claim.as_deref().unwrap_or(""), "groups"))
                        (textarea_input(
                            "sso_group_role_mappings",
                            "Community Roles From Groups",
                            "staff = 1400000000000000001\n/engineering/devs = 1400000000000000002",
                            &group_role_mappings,
                            4,
                            false,
                        ))
                        p class="text-xs text-neutral-500" {
                            "Only used in single-community mode. Write one mapping per line, as the group name the provider sends, \"=\" and the ID of a role in the single community. \
                             At every SSO sign-in, members get each listed role whose group they are in and lose each listed role whose group they left. \
                             Roles not listed here are never changed. If the provider sends no groups claim, roles stay as they are. \
                             Leave the claim empty to use \"groups\"."
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
