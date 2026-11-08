// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::ConnectionProviderResponse,
    templates::{
        components::{
            form::{
                Checkbox, FORM_INPUT_CLASS, form_actions, form_field_group, select_input,
                submit_button, text_input,
            },
            section_card::section_card_with_description,
        },
        pages::{instance_config::ConfigForm, instance_registration::compact_copy_button},
    },
};
use maud::{Markup, html};

pub const CONNECTION_PROVIDER_COUNT_FIELD: &str = "connection_provider_count";

pub fn connection_provider_field(index: usize, name: &str) -> String {
    format!("connection_provider_{index}_{name}")
}

const KIND_CHOICES: &[(&str, &str)] = &[("oidc", "OpenID Connect"), ("oauth2", "OAuth 2.0")];

fn secret_input(name: &str, secret_set: bool) -> Markup {
    let helper = if secret_set {
        "A secret is saved. Leave this blank to keep it."
    } else {
        "Required."
    };
    form_field_group(
        "Client secret",
        name,
        false,
        None,
        Some(helper),
        html! {
            input type="password" id=(name) name=(name) value="" class=(FORM_INPUT_CLASS)
                autocomplete="new-password";
        },
    )
}

fn helped_input(name: &str, label: &str, value: &str, placeholder: &str, helper: &str) -> Markup {
    form_field_group(
        label,
        name,
        false,
        None,
        Some(helper),
        html! {
            input type="text" id=(name) name=(name) value=(value) placeholder=(placeholder)
                class=(FORM_INPUT_CLASS);
        },
    )
}

fn provider_card(index: usize, provider: Option<&ConnectionProviderResponse>) -> Markup {
    let field = |name: &str| connection_provider_field(index, name);
    let text = |value: Option<&String>| value.map(String::as_str).unwrap_or("").to_owned();
    let kind = provider.map(|p| p.kind.as_str()).unwrap_or("oidc");
    html! {
        div class="space-y-4 border-t border-neutral-200 pt-6 first:border-t-0 first:pt-0" {
            h3 class="text-sm font-semibold text-neutral-900" {
                @match provider {
                    Some(provider) => (provider.name),
                    None => "Add provider",
                }
            }
            div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                (helped_input(
                    &field("id"),
                    "ID",
                    provider.map(|p| p.id.as_str()).unwrap_or(""),
                    "github",
                    "Lowercase letters, digits, hyphens and underscores. It is part of the callback URL.",
                ))
                (text_input(&field("name"), "Name", provider.map(|p| p.name.as_str()).unwrap_or(""), "GitHub"))
                (select_input(&field("kind"), "Kind", KIND_CHOICES, kind))
                (text_input(&field("scope"), "Scope", provider.map(|p| p.scope.as_str()).unwrap_or(""), "openid profile"))
                (helped_input(
                    &field("issuer"),
                    "Issuer URL",
                    &text(provider.and_then(|p| p.issuer.as_ref())),
                    "https://id.example.com/realms/main",
                    "OpenID Connect only. Fluxer finds the other URLs from it.",
                ))
                (helped_input(
                    &field("authorization_url"),
                    "Authorization URL",
                    &text(provider.and_then(|p| p.authorization_url.as_ref())),
                    "https://github.com/login/oauth/authorize",
                    "Required for OAuth 2.0.",
                ))
                (helped_input(
                    &field("token_url"),
                    "Token URL",
                    &text(provider.and_then(|p| p.token_url.as_ref())),
                    "https://github.com/login/oauth/access_token",
                    "Required for OAuth 2.0.",
                ))
                (helped_input(
                    &field("userinfo_url"),
                    "User URL",
                    &text(provider.and_then(|p| p.userinfo_url.as_ref())),
                    "https://api.github.com/user",
                    "Required for OAuth 2.0. It returns the signed-in account as JSON.",
                ))
                (text_input(&field("client_id"), "Client ID", provider.map(|p| p.client_id.as_str()).unwrap_or(""), ""))
                (secret_input(&field("client_secret"), provider.is_some_and(|p| p.client_secret_set)))
                (helped_input(
                    &field("name_claim"),
                    "Name claim",
                    &text(provider.and_then(|p| p.name_claim.as_ref())),
                    "preferred_username",
                    "OpenID Connect only. Empty uses preferred_username, then name.",
                ))
                (helped_input(
                    &field("id_path"),
                    "Account ID path",
                    &text(provider.and_then(|p| p.id_path.as_ref())),
                    "id",
                    "OAuth 2.0 only. Dot path in the user JSON, for example data.0.id.",
                ))
                (helped_input(
                    &field("name_path"),
                    "Account name path",
                    &text(provider.and_then(|p| p.name_path.as_ref())),
                    "login",
                    "OAuth 2.0 only. Empty shows the account ID.",
                ))
                (helped_input(
                    &field("profile_url_template"),
                    "Profile link",
                    &text(provider.and_then(|p| p.profile_url_template.as_ref())),
                    "https://github.com/{name}",
                    "Optional. {id} and {name} are replaced with the account's ID and name.",
                ))
                (helped_input(
                    &field("icon_url"),
                    "Icon URL",
                    &text(provider.and_then(|p| p.icon_url.as_ref())),
                    "https://example.com/icon.png",
                    "Optional. People load it through the media proxy.",
                ))
            }
            @if let Some(provider) = provider {
                div class="flex flex-col gap-2" {
                    span class="text-sm font-medium text-neutral-700" { "Callback URL" }
                    div class="flex items-center gap-2" {
                        input type="url" readonly value=(provider.callback_url)
                            aria-label="Callback URL"
                            class="h-8 min-w-0 flex-1 rounded-lg border border-neutral-300 bg-neutral-50 px-3 py-1.5 text-xs text-neutral-800";
                        (compact_copy_button(&provider.callback_url, "Copy"))
                    }
                    p class="text-xs text-neutral-500" { "Register this URL as the redirect URL at the provider." }
                }
            } @else {
                p class="text-xs text-neutral-500" { "The callback URL to register at the provider shows here after you save." }
            }
            (Checkbox {
                name: &field("enabled"),
                value: "true",
                label: "People can connect this provider",
                checked: provider.is_none_or(|p| p.enabled),
                enabled: true,
            }.render())
            @if provider.is_some() {
                (Checkbox {
                    name: &field("remove"),
                    value: "true",
                    label: "Remove this provider",
                    checked: false,
                    enabled: true,
                }.render())
            }
        }
    }
}

pub(super) fn connection_providers_section(
    base: &str,
    config_form: ConfigForm<'_>,
    providers: &[ConnectionProviderResponse],
) -> Markup {
    let card_count = providers.len() + 1;
    section_card_with_description(
        "Profile connections",
        "Let people link accounts from your own OAuth 2.0 or OpenID Connect providers to their profiles. Leave the new provider's ID empty to add nothing.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_connection_providers"} {
                (config_form.fields())
                input type="hidden" name=(CONNECTION_PROVIDER_COUNT_FIELD) value=(card_count);
                div class="space-y-8" {
                    @for (index, provider) in providers.iter().enumerate() {
                        (provider_card(index, Some(provider)))
                    }
                    (provider_card(providers.len(), None))
                    (form_actions(html! {
                        (submit_button("Save profile connections"))
                    }))
                }
            }
        },
    )
}
