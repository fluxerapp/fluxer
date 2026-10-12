// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::admin_flags::{DELETION_REASONS, DEPRECATED_GUILD_FEATURES, GUILD_FEATURES};
use crate::{
    acl,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            form::{
                FORM_SELECT_CLASS, checkbox, csrf_input, danger_button, form_actions,
                form_field_group, opt_out_checkbox, select_chevron, submit_button, text_input,
                textarea_input,
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

pub fn bulk_actions_page(
    config: &AdminConfig,
    auth: &AuthContext,
    csrf_token: &str,
    username_sign_in: bool,
) -> Markup {
    let base = &config.base_path;
    let admin_acls = auth
        .admin_user
        .as_ref()
        .map(|u| u.acls.as_slice())
        .unwrap_or(&[]);

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
                (bulk_schedule_deletion_section(base, csrf_token, username_sign_in))
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
                    (checkbox(prefix, feature, &guild_feature_label(feature), false, true))
                }
            }
        }
    }
}

fn user_flag_checkbox_grid(prefix: &str) -> Markup {
    html! {
        div class="grid grid-cols-1 gap-3 sm:grid-cols-2" {
            @for flag in crate::admin_flags::USER_FLAGS {
                (checkbox(prefix, &flag.value.to_string(), flag.name, false, true))
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

fn bulk_schedule_deletion_section(base: &str, csrf_token: &str, username_sign_in: bool) -> Markup {
    section_card_simple(
        "Bulk Schedule User Deletion",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-schedule-user-deletion"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    (textarea_input("user_ids", "User IDs (one per line)", "123456789\n987654321", "", 5, true))
                    (form_field_group("Deletion Reason", "reason_code", true, None,
                        Some("User requested skips identifier bans and pending report resolution. Every other reason applies them."),
                        html! {
                            div class="relative" {
                                select id="reason_code" name="reason_code" required
                                    class=(FORM_SELECT_CLASS) {
                                    option value="" selected { "Select a reason" }
                                    @for &(value, label) in DELETION_REASONS {
                                        option value=(value) { (label) }
                                    }
                                }
                                (select_chevron())
                            }
                        },
                    ))
                    p class="text-neutral-500 text-sm" {
                        "Users that already have a pending deletion are skipped and listed as failed with the reason already scheduled. Cancel those from the user page first to schedule them again."
                    }
                    (text_input("public_reason", "Public Reason (optional)", "", "Terms of service violation"))
                    (form_field_group("Days Until Deletion", "days_until_deletion", true, None,
                        Some("Moderation reasons are held for at least 60 days. Only User requested allows 14."),
                        html! {
                            input type="number" id="days_until_deletion" name="days_until_deletion"
                                value="60" min="14" max="365" required
                                class="w-full rounded-lg border border-neutral-300 bg-white \
                                       text-neutral-900 text-sm h-8 px-3 py-1.5 \
                                       focus:border-brand-primary focus:outline-none \
                                       focus:ring-2 focus:ring-brand-primary/20";
                        },
                    ))
                    (text_input("audit_log_reason", "Audit Log Reason (optional)", "", "Reason for this bulk operation"))
                    @if username_sign_in {
                        input type="hidden" name="notify_user_present" value="1";
                    } @else {
                        (opt_out_checkbox("notify_user", "Email each user about the scheduled deletion"))
                    }
                    (form_actions(html! {
                        (danger_button("Schedule Deletion"))
                    }))
                }
            }
        },
    )
}

fn bulk_delete_user_messages_section(base: &str, csrf_token: &str) -> Markup {
    section_card_simple(
        "Bulk Delete User Messages",
        html! {
            form method="post" action={(base) "/bulk-actions?action=bulk-delete-user-messages"} {
                (csrf_input(csrf_token))
                div class="space-y-4" {
                    p class="text-neutral-500 text-sm" {
                        "Deletes every message authored by each user across all channels. This cannot be undone."
                    }
                    (textarea_input("user_ids", "User IDs (one per line)", "123456789\n987654321", "", 5, true))
                    (text_input("audit_log_reason", "Audit Log Reason (optional)", "", "Reason for this bulk operation"))
                    (form_actions(html! {
                        (danger_button("Delete All Messages"))
                    }))
                }
            }
        },
    )
}
