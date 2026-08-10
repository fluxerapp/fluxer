// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{CreateAdminApiKeyResponse, FlashMessage, ListAdminApiKeyEntry},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            alert::{AlertVariant, alert},
            badge::{BadgeVariant, badge},
            form::{checkbox, csrf_input, form_actions},
            page_container::page_header,
            section_card::section_card,
            table::empty_state,
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

fn created_key_banner(key: &CreateAdminApiKeyResponse) -> Markup {
    let key_value = &key.key;
    let key_id = &key.key_id;
    alert(
        AlertVariant::Success,
        Some("API Key created successfully."),
        html! {
            div class="flex flex-col gap-2" {
                p class="text-sm text-green-700" {
                    "Save this key now. You won't be able to see it again."
                }
                div class="flex flex-col gap-2 rounded-lg border border-green-200 \
                           bg-white p-3 sm:flex-row sm:items-center" {
                    code id="api-key-value"
                        class="min-w-0 flex-1 break-all text-green-900 text-sm" {
                        (key_value)
                    }
                    span class="text-xs text-green-600" {
                        "Key ID: " (key_id)
                    }
                }
                button type="button"
                    class="inline-flex items-center justify-center gap-2 font-medium \
                           rounded-lg bg-neutral-50 text-neutral-700 border \
                           border-neutral-300 px-3 py-1.5 text-sm w-fit"
                    onclick="navigator.clipboard.writeText(\
                        document.getElementById('api-key-value').innerText\
                    ).then(()=>document.body.dispatchEvent(new CustomEvent('showFlash', {detail: {level: 'success', message: 'API key copied.'}})))" {
                    "Copy Key"
                }
            }
        },
    )
}

fn create_form(base: &str, csrf_token: &str, acls: &[&str]) -> Markup {
    html! {
        (section_card(Some("Create Admin API Key"), None, None, html! {
            @if acls.is_empty() {
                p class="text-sm text-neutral-500" {
                    "No ACLs available. You can only grant permissions you have."
                }
            }
            form id="create-key-form" method="post"
                action={(base) "/admin-api-keys?action=create"}
                data-admin-result-form="true" hx-push-url="false" {
                (csrf_input(csrf_token))
                div class="flex flex-col gap-4" {
                    div class="flex flex-col gap-2" {
                        label for="api-key-name"
                            class="font-semibold text-neutral-500 text-xs uppercase tracking-wide" {
                            "Key Name"
                            span class="text-red-500 ml-0.5" { "*" }
                        }
                        input type="text" id="api-key-name" name="name" required
                            placeholder="Enter a descriptive name"
                            class="w-full rounded-lg border border-neutral-300 bg-white \
                                   text-neutral-900 text-sm h-8 px-3 py-1.5 \
                                   focus:border-brand-primary focus:outline-none \
                                   focus:ring-2 focus:ring-brand-primary/20";
                        p class="text-xs text-neutral-500" {
                            "A descriptive name to help you identify this API key."
                        }
                    }
                    div class="flex flex-col gap-3" {
                        div class="flex flex-col gap-1" {
                            label class="font-semibold text-neutral-500 text-xs \
                                         uppercase tracking-wide" {
                                "Permissions (ACLs)"
                            }
                            p class="text-xs text-neutral-500" {
                                "Select the permissions to grant this API key. \
                                 You can only grant permissions you have."
                            }
                        }
                        div class="grid grid-cols-1 gap-3 md:grid-cols-2" {
                            @for acl in acls {
                                (checkbox("acls", acl, acl, false, true))
                            }
                        }
                    }
                    (form_actions(html! {
                        button type="submit"
                            class="inline-flex items-center justify-center gap-2 font-medium \
                                   rounded-lg bg-neutral-900 text-white px-4 py-2 text-sm w-fit" {
                            "Create API Key"
                        }
                    }))
                }
            }
        }))
    }
}

fn api_key_item(base: &str, csrf_token: &str, key: &ListAdminApiKeyEntry) -> Markup {
    let name = &key.name;
    let key_id = &key.key_id;
    let created_at = &key.created_at;
    html! {
        div class="rounded-lg bg-white border border-neutral-200 p-4" {
            div class="flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between" {
                div class="min-w-0 flex-1 flex flex-col gap-1" {
                    h3 class="font-semibold text-neutral-900 text-lg break-words" {
                        (name)
                    }
                    p class="text-sm text-neutral-500 break-all" {
                        "Key ID: " (key_id)
                    }
                    p class="text-sm text-neutral-500" {
                        "Created: " (created_at)
                    }
                    p class="text-sm text-neutral-500" {
                        "Created by: " (key.created_by_user_id)
                    }
                    @if let Some(ref expires_at) = key.expires_at {
                        p class="text-sm text-neutral-500" {
                            "Expires: " (expires_at)
                        }
                    }
                    @if !key.acls.is_empty() {
                        div class="mt-2 flex flex-col gap-1" {
                            p class="text-xs font-medium text-neutral-500" {
                                "Permissions:"
                            }
                            div class="flex flex-wrap gap-1" {
                                @for acl in &key.acls {
                                    (badge(acl, BadgeVariant::Default))
                                }
                            }
                        }
                    }
                }
                form method="post"
                    action={(base) "/admin-api-keys?action=revoke"}
                    class="flex-shrink-0 self-stretch sm:self-start"
                    data-admin-result-form="true" hx-push-url="false" {
                    (csrf_input(csrf_token))
                    input type="hidden" name="key_id" value=(key_id);
                    button type="submit"
                        class="inline-flex w-full items-center justify-center gap-2 \
                               font-medium rounded-lg bg-red-600 text-white \
                               px-3 py-1.5 text-sm hover:bg-red-700" {
                        "Revoke"
                    }
                }
            }
        }
    }
}

fn key_list_section(base: &str, csrf_token: &str, keys: Option<&[ListAdminApiKeyEntry]>) -> Markup {
    match keys {
        None => html! {
            (section_card(Some("Existing API Keys"), None, None, html! {
                (empty_state("Loading API keys..."))
            }))
        },
        Some([]) => html! {
            (section_card(Some("Existing API Keys"), None, None, html! {
                (empty_state("No API keys found. Create one to get started."))
            }))
        },
        Some(keys) => html! {
            (section_card(Some("Existing API Keys"), None, None, html! {
                div class="flex flex-col gap-3" {
                    @for key in keys {
                        (api_key_item(base, csrf_token, key))
                    }
                }
            }))
        },
    }
}

pub fn admin_api_keys_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    flash: Option<&FlashMessage>,
    created_key: Option<&CreateAdminApiKeyResponse>,
    keys: Option<&[ListAdminApiKeyEntry]>,
    available_acls: &[&str],
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header("Admin API Keys", Some("Create and manage API keys for admin access")))
        div class="space-y-6" hx-history=[created_key.is_some().then_some("false")] {
            @if let Some(ck) = created_key {
                (created_key_banner(ck))
            }
            (create_form(base, csrf_token, available_acls))
            (key_list_section(base, csrf_token, keys))
        }
    };
    admin_layout(
        config,
        auth,
        "Admin API Keys",
        "admin-api-keys",
        flash,
        content,
    )
}
