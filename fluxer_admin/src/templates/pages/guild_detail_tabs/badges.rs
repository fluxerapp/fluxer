// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::{GuildInfo, ListBadgesResponse},
    config::AdminConfig,
    templates::components::{
        form::{checkbox, csrf_input, form_actions, submit_button},
        page_container::card_with_header,
    },
};
use maud::{Markup, html};

pub fn badges_tab(
    config: &AdminConfig,
    guild: &GuildInfo,
    badges: &ListBadgesResponse,
    csrf_token: &str,
    admin_acls: &[String],
) -> Markup {
    let can_edit = acl::has_permission(admin_acls, acl::GUILD_UPDATE_BADGES);
    let mut guild_badges: Vec<_> = badges
        .badges
        .iter()
        .filter(|badge| badge.badge_type == "guild")
        .collect();
    guild_badges.sort_by_key(|badge| badge.position);
    html! {
        div class="space-y-6" {
            (card_with_header("Guild Badges", html! {
                @if guild_badges.is_empty() {
                    p class="text-sm text-neutral-500" { "No guild badges defined." }
                } @else {
                    form method="post"
                        action={(config.base_path) "/guilds/" (guild.id) "?action=update_badges&tab=badges"} {
                        (csrf_input(csrf_token))
                        div class="space-y-3" {
                            @for badge in guild_badges {
                                (checkbox(
                                    "badge_ids[]",
                                    &badge.id,
                                    &badge.name,
                                    guild.badge_ids.contains(&badge.id),
                                    can_edit,
                                ))
                            }
                        }
                        @if can_edit {
                            div class="mt-6 border-t border-neutral-200 pt-6" {
                                (form_actions(submit_button("Save Badges")))
                            }
                        } @else {
                            p class="mt-4 text-xs text-neutral-500" {
                                "Read-only \u{2014} " (acl::GUILD_UPDATE_BADGES) " permission required."
                            }
                        }
                    }
                }
            }))
        }
    }
}
