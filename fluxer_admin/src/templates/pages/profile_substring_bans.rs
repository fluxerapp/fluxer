// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::page_container::page_header,
        layout::admin_layout,
        pages::blocklist_helpers::{
            BlocklistActionVariant, blocklist_action_card, blocklist_text_field,
        },
    },
};
use maud::{Markup, html};

const SCOPES: &[(&str, &str)] = &[
    ("username", "Username"),
    ("global_name", "Display Name (global_name)"),
    ("bio", "Bio"),
    ("pronouns", "Pronouns"),
    ("persona_tag", "Persona Tag"),
];

pub fn profile_substring_bans_page(
    config: &AdminConfig,
    auth: &AuthContext,
    flash: Option<&crate::api::types::FlashMessage>,
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header("Profile Substring Blocklist", None))
        div class="grid gap-6 lg:grid-cols-2" {
            (ban_card(base, csrf_token))
            (check_card(base, csrf_token))
        }
        div class="mt-6" {
            (unban_card(base, csrf_token))
        }
    };
    admin_layout(
        config,
        auth,
        "Profile Substring Blocklist",
        "profile-substring-bans",
        flash,
        content,
    )
}

fn scope_select() -> Markup {
    html! {
        div class="space-y-1" {
            label for="scope" class="block text-sm font-medium text-neutral-700" {
                "Scope"
            }
            select id="scope" name="scope" required
                class="block w-full rounded-md border border-neutral-300 px-3 py-2 \
                       text-sm shadow-sm focus:border-brand-primary focus:outline-none \
                       focus:ring-1 focus:ring-brand-primary" {
                @for (value, label) in SCOPES {
                    option value=(value) { (label) }
                }
            }
        }
    }
}

fn ban_card(base: &str, csrf_token: &str) -> Markup {
    let action_url = format!("{base}/profile-substring-bans?action=ban&_csrf={csrf_token}");
    blocklist_action_card(
        "Ban Profile Substring",
        &action_url,
        csrf_token,
        html! {
            (scope_select())
            (blocklist_text_field("substring", "Substring", "any substring to ban", true))
            (blocklist_text_field("audit_log_reason", "Private reason (audit log, optional)", "Why is this ban being applied?", false))
        },
        "Ban Substring",
        BlocklistActionVariant::Primary,
    )
}

fn check_card(base: &str, csrf_token: &str) -> Markup {
    let action_url = format!("{base}/profile-substring-bans?action=check&_csrf={csrf_token}");
    blocklist_action_card(
        "Check Profile Substring Ban Status",
        &action_url,
        csrf_token,
        html! {
            (scope_select())
            (blocklist_text_field("substring", "Substring", "substring to check", true))
        },
        "Check Status",
        BlocklistActionVariant::Primary,
    )
}

fn unban_card(base: &str, csrf_token: &str) -> Markup {
    let action_url = format!("{base}/profile-substring-bans?action=unban&_csrf={csrf_token}");
    blocklist_action_card(
        "Remove Profile Substring Ban",
        &action_url,
        csrf_token,
        html! {
            (scope_select())
            (blocklist_text_field("substring", "Substring", "substring to unban", true))
            (blocklist_text_field("audit_log_reason", "Private reason (audit log, optional)", "Why is this ban being removed?", false))
        },
        "Unban Substring",
        BlocklistActionVariant::Danger,
    )
}
