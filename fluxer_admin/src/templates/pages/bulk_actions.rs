// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::admin_flags::{DEPRECATED_GUILD_FEATURES, GUILD_FEATURES};
use crate::{
    acl,
    api::generated::types::EnforcementKindType,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            enforcement_fields::{
                FIELD_HELP_CLASS, FIELD_INPUT_CLASS, deletion_days_field, field_label,
                internal_note_field, radio_group, reason_select,
            },
            form::{
                Checkbox, csrf_input, danger_button, form_actions, form_field_group, submit_button,
                text_input, textarea_input,
            },
            page_container::page_header,
            section_card::section_card_simple,
        },
        layout::admin_layout,
    },
};
use maud::{Markup, html};

fn guild_feature_label(feature: &str) -> String {
    if DEPRECATED_GUILD_FEATURES.contains(&feature) {
        format!("{feature} (deprecated, removal only)")
    } else {
        feature.to_owned()
    }
}

pub fn bulk_actions_page(config: &AdminConfig, auth: &AuthContext, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    let admin_acls = auth.acls();

    let content = html! {
        (page_header("Bulk Actions", Some("Perform bulk operations on users and guilds")))
        div class="space-y-6" {
            @if acl::has_permission(admin_acls, acl::BULK_UPDATE_USER_FLAGS) {
                (bulk_update_user_flags_section(base, csrf_token))
            }
            @if acl::has_permission(admin_acls, acl::BULK_UPDATE_GUILD_FEATURES) {
                (bulk_update_guild_features_section(base, csrf_token))
            }
            @if acl::has_permission(admin_acls, acl::BULK_ADD_GUILD_MEMBERS) {
                (bulk_add_guild_members_section(base, csrf_token))
            }
            @if acl::has_permission(admin_acls, acl::BULK_DELETE_USERS) {
                (bulk_close_accounts_section(base, config.self_hosted, csrf_token))
            }
            @if acl::has_permission(admin_acls, acl::BULK_DELETE_USER_MESSAGES) {
                (bulk_delete_user_messages_section(base, csrf_token))
            }
        }
    };
    admin_layout(config, auth, "Bulk Actions", "bulk-actions", None, content)
}

fn guild_feature_checkbox_grid(prefix: &str, include_deprecated: bool) -> Markup {
    html! {
        div class="grid grid-cols-1 gap-3 sm:grid-cols-2" {
            @for feature in GUILD_FEATURES {
                @if include_deprecated || !DEPRECATED_GUILD_FEATURES.contains(feature) {
                    (Checkbox {
                        name: prefix,
                        value: feature,
                        label: &guild_feature_label(feature),
                        checked: false,
                        enabled: true,
                    }.render())
                }
            }
        }
    }
}

fn user_flag_checkbox_grid(prefix: &str) -> Markup {
    html! {
        div class="grid grid-cols-1 gap-3 sm:grid-cols-2" {
            @for flag in crate::admin_flags::USER_FLAGS {
                (Checkbox {
                    name: prefix,
                    value: &flag.value.to_string(),
                    label: flag.name,
                    checked: false,
                    enabled: true,
                }.render())
            }
        }
    }
}

fn bulk_update_user_flags_section(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Bulk Update User Flags",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-update-user-flags"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (textarea_input("user_ids", "User IDs (one per line)", "123456789\n987654321", "", 5, true))
                    div {
                        p class="font-semibold text-neutral-500 text-xs uppercase tracking-wide mb-2" {
                            "Flags to Add"
                        }
                        (user_flag_checkbox_grid("add_flags[]"))
                    }
                    div {
                        p class="font-semibold text-neutral-500 text-xs uppercase tracking-wide mb-2" {
                            "Flags to Remove"
                        }
                        (user_flag_checkbox_grid("remove_flags[]"))
                    }
                    (text_input("audit_log_reason", "Audit Log Reason (optional)", "", "Reason for this bulk operation"))
                    (form_actions(html! {
                        (submit_button("Update User Flags"))
                    }))
                }
            }
        },
    )
}

fn bulk_update_guild_features_section(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Bulk Update Guild Features",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-update-guild-features"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (textarea_input("guild_ids", "Guild IDs (one per line)", "123456789\n987654321", "", 5, true))
                    div {
                        p class="font-semibold text-neutral-500 text-xs uppercase tracking-wide mb-2" {
                            "Features to Add"
                        }
                        (guild_feature_checkbox_grid("add_features[]", false))
                    }
                    div {
                        p class="font-semibold text-neutral-500 text-xs uppercase tracking-wide mb-2" {
                            "Features to Remove"
                        }
                        (guild_feature_checkbox_grid("remove_features[]", true))
                    }
                    (form_field_group("Custom features to add", "custom_add_features", false, None,
                        Some("Comma-separated list of custom features not in the standard set."),
                        html! {
                            input type="text" id="custom_add_features" name="custom_add_features"
                                placeholder="CUSTOM_FEATURE_1, CUSTOM_FEATURE_2"
                                class="w-full rounded-lg border border-neutral-300 bg-white \
                                       text-neutral-900 text-sm h-8 px-3 py-1.5 \
                                       focus:border-brand-primary focus:outline-none \
                                       focus:ring-2 focus:ring-brand-primary/20";
                        },
                    ))
                    (form_field_group("Custom features to remove", "custom_remove_features", false, None,
                        Some("Comma-separated list of custom features to remove."),
                        html! {
                            input type="text" id="custom_remove_features" name="custom_remove_features"
                                placeholder="CUSTOM_FEATURE_1, CUSTOM_FEATURE_2"
                                class="w-full rounded-lg border border-neutral-300 bg-white \
                                       text-neutral-900 text-sm h-8 px-3 py-1.5 \
                                       focus:border-brand-primary focus:outline-none \
                                       focus:ring-2 focus:ring-brand-primary/20";
                        },
                    ))
                    (text_input("audit_log_reason", "Audit Log Reason (optional)", "", "Reason for this bulk operation"))
                    (form_actions(html! {
                        (submit_button("Update Guild Features"))
                    }))
                }
            }
        },
    )
}

fn bulk_add_guild_members_section(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Bulk Add Guild Members",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-add-guild-members"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (text_input("guild_id", "Guild ID", "", "123456789"))
                    (textarea_input("user_ids", "User IDs (one per line)", "123456789\n987654321", "", 5, true))
                    (text_input("audit_log_reason", "Audit Log Reason (optional)", "", "Reason for this bulk operation"))
                    (form_actions(html! {
                        (submit_button("Add Members"))
                    }))
                }
            }
        },
    )
}

fn bulk_decision_fields(
    id_prefix: &str,
    kind: EnforcementKindType,
    statement_help: &str,
) -> Markup {
    let statement_id = format!("{id_prefix}-statement");
    html! {
        fieldset class="space-y-4" data-show-for-basis="enforcement" hidden disabled {
            (reason_select(id_prefix, kind, false))
            (radio_group("Automated tools", "automation", id_prefix, &[
                ("none", "Not involved"),
                ("flagged", "Flagged this for review"),
            ]))
            div class="space-y-1" {
                (field_label(&statement_id, "Note to the user"))
                textarea id=(statement_id) name="statement" rows="2" maxlength="500" required
                    class=(FIELD_INPUT_CLASS) {}
                p class=(FIELD_HELP_CLASS) { (statement_help) }
            }
        }
    }
}

fn bulk_close_accounts_section(base: &str, self_hosted: bool, csrf_token: &str) -> Markup {
    section_card_simple(
        "Close accounts",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-schedule-user-deletion"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (textarea_input("user_ids", "User IDs (one per line)", "123456789\n987654321", "", 5, true))
                    (radio_group("Basis", "basis", "bulk-close", &[
                        ("enforcement", "A rule was broken"),
                        ("holder_request", "The account holders asked"),
                        ("housekeeping", "Inactivity"),
                    ]))
                    (deletion_days_field("bulk-close-days", self_hosted))
                    (bulk_decision_fields(
                        "bulk-close",
                        EnforcementKindType::Closure,
                        "Shown to every account in the job. It is the only fact a bulk closure can give, because messages and reports cannot be attached in bulk. Never name or describe other people.",
                    ))
                    (internal_note_field("bulk-close", "Staff only. Saved as the first note on each decision and in the audit log."))
                    p class=(FIELD_HELP_CLASS) {
                        "Accounts that already have a pending deletion are skipped and listed as failed. Each account closed for a broken rule gets its own decision and notice."
                    }
                    (form_actions(danger_button("Close accounts")))
                }
            }
        },
    )
}

fn bulk_delete_user_messages_section(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Delete all messages",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-delete-user-messages"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    p class="text-neutral-500 text-sm" {
                        "Deletes every message each account wrote, in every channel. This cannot be undone."
                    }
                    (textarea_input("user_ids", "User IDs (one per line)", "123456789\n987654321", "", 5, true))
                    (radio_group("Basis", "basis", "bulk-delete-messages", &[
                        ("enforcement", "A rule was broken"),
                        ("holder_request", "The account holders asked"),
                        ("housekeeping", "Housekeeping"),
                    ]))
                    (bulk_decision_fields(
                        "bulk-delete-messages",
                        EnforcementKindType::ContentRemoval,
                        "Shown to every account in the job. It is the only fact a bulk removal can give, because messages and reports cannot be attached in bulk. Never name or describe other people.",
                    ))
                    (internal_note_field("bulk-delete-messages", "Staff only. Saved as the first note on each decision and in the audit log."))
                    p class=(FIELD_HELP_CLASS) {
                        "Each account whose messages are removed for a broken rule gets its own decision and notice."
                    }
                    (form_actions(danger_button("Delete all messages")))
                }
            }
        },
    )
}
