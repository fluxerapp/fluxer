// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::{generated::types::EnforcementKindType, types::GuildInfo},
    config::AdminConfig,
    templates::components::{
        enforcement_fields::{BasisFields, DecisionFields, basis_fields},
        form::{csrf_input, danger_button, form_actions, submit_button},
        page_container::card_with_header,
    },
};
use maud::{Markup, html};

const MODERATION_ACLS: &[&str] = &[
    acl::GUILD_UPDATE_NAME,
    acl::GUILD_UPDATE_VANITY,
    acl::GUILD_TRANSFER_OWNERSHIP,
    acl::GUILD_FORCE_ADD_MEMBER,
    acl::GUILD_RELOAD,
    acl::GUILD_SHUTDOWN,
    acl::GUILD_DELETE,
];
const TEXT_INPUT_CLS: &str = "block w-full rounded-md border border-neutral-300 \
                               px-3 py-2 text-sm shadow-sm \
                               focus:border-brand-primary focus:outline-none \
                               focus:ring-1 focus:ring-brand-primary";

pub fn moderation_tab(
    config: &AdminConfig,
    guild: &GuildInfo,
    csrf_token: &str,
    admin_acls: &[String],
) -> Markup {
    if !acl::has_any_permission(admin_acls, MODERATION_ACLS) {
        return html! {
            div class="space-y-6" {
                div class="rounded-lg border border-neutral-200 bg-white p-8 text-center" {
                    p class="text-sm text-neutral-500" {
                        "You don't have permission to perform moderation actions."
                    }
                }
            }
        };
    }

    let base = &config.base_path;
    let can_update_name = acl::has_permission(admin_acls, acl::GUILD_UPDATE_NAME);
    let can_update_vanity = acl::has_permission(admin_acls, acl::GUILD_UPDATE_VANITY);
    let can_transfer = acl::has_permission(admin_acls, acl::GUILD_TRANSFER_OWNERSHIP);
    let can_force_add = acl::has_permission(admin_acls, acl::GUILD_FORCE_ADD_MEMBER);
    let can_reload = acl::has_permission(admin_acls, acl::GUILD_RELOAD);
    let can_shutdown = acl::has_permission(admin_acls, acl::GUILD_SHUTDOWN);
    let can_delete = acl::has_permission(admin_acls, acl::GUILD_DELETE);

    html! {
        div class="space-y-6" {
            @if can_update_name {
                (card_with_header("Update Guild Name", html! {
                    form method="post"
                        action={(base) "/guilds/" (guild.id) "?action=update_name&tab=moderation"} {
                        (csrf_input(csrf_token))
                        div class="space-y-3" {
                            input type="text" name="name"
                                placeholder="New guild name" required
                                class=(TEXT_INPUT_CLS);
                            (form_actions(html! {
                                (submit_button("Update Name"))
                            }))
                        }
                    }
                }))
            }

            @if can_update_vanity {
                (card_with_header("Update Vanity URL", html! {
                    form method="post"
                        action={(base) "/guilds/" (guild.id) "?action=update_vanity&tab=moderation"} {
                        (csrf_input(csrf_token))
                        div class="space-y-3" {
                            input type="text" name="vanity_url_code"
                                placeholder="vanity-code (leave empty to remove)"
                                class=(TEXT_INPUT_CLS);
                            (form_actions(html! {
                                (submit_button("Update Vanity URL"))
                            }))
                        }
                    }
                }))
            }

            @if can_transfer {
                (card_with_header("Transfer Ownership", html! {
                    form method="post"
                        action={(base) "/guilds/" (guild.id) "?action=transfer_ownership&tab=moderation"} {
                        (csrf_input(csrf_token))
                        div class="space-y-3" {
                            input type="text" name="new_owner_id"
                                placeholder="New owner user ID" required
                                class=(TEXT_INPUT_CLS);
                            (form_actions(html! {
                                (danger_button("Transfer Ownership"))
                            }))
                        }
                    }
                }))
            }

            @if can_force_add {
                (card_with_header("Force Add User to Guild", html! {
                    form method="post"
                        action={(base) "/guilds/" (guild.id) "?action=force_add_user&tab=moderation"} {
                        (csrf_input(csrf_token))
                        div class="space-y-3" {
                            input type="text" name="user_id"
                                placeholder="User ID to add" required
                                class=(TEXT_INPUT_CLS);
                            (form_actions(html! {
                                (submit_button("Add User"))
                            }))
                        }
                    }
                }))
            }

            @if can_reload || can_shutdown {
                (card_with_header("Guild Process Controls", html! {
                    div class="flex flex-wrap gap-3" {
                        @if can_reload {
                            form method="post"
                                action={(base) "/guilds/" (guild.id) "?action=reload&tab=moderation"} {
                                (csrf_input(csrf_token))
                                (submit_button("Reload Guild"))
                            }
                        }
                        @if can_shutdown {
                            form method="post"
                                action={(base) "/guilds/" (guild.id) "?action=shutdown&tab=moderation"} {
                                (csrf_input(csrf_token))
                                (danger_button("Shutdown Guild"))
                            }
                        }
                    }
                }))
            }

            @if can_delete {
                (card_with_header("Remove community", html! {
                    form method="post"
                        action={(base) "/guilds/" (guild.id) "?action=delete_guild&tab=moderation"} {
                        (csrf_input(csrf_token))
                        div class="space-y-4" {
                            p class="text-sm text-neutral-500" {
                                "Removes the community with its channels, messages and settings. It can't be undone. Child safety and violent extremism reasons can't be used, because the removal deletes the content."
                            }
                            (basis_fields(&BasisFields {
                                decision: DecisionFields {
                                    id_prefix: "remove-community",
                                    kind: EnforcementKindType::CommunityRemoval,
                                    report_ids: &[],
                                    messages: Some(&[]),
                                    user_flags: None,
                                    allow_restricted: false,
                                },
                                choices: &[
                                    ("enforcement", "A rule was broken"),
                                    ("holder_request", "The owner asked"),
                                ],
                                plain_note_help: "Staff only. Saved in the audit log.",
                                enforcement_extra: html! {},
                            }))
                            (form_actions(danger_button("Remove community")))
                        }
                    }
                }))
            }
        }
    }
}
