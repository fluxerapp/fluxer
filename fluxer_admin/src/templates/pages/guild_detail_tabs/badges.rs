// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::{AdminCustomBadgeList, CustomBadgeType, GuildDetailInfo},
    config::AdminConfig,
    templates::{
        components::{
            form::{csrf_input, form_actions, submit_button},
            page_container::card_with_header,
        },
        pages::badges::{custom_badge_list, custom_badge_picker, no_badges_defined},
    },
};
use maud::{Markup, html};

pub fn badges_tab(
    config: &AdminConfig,
    guild: &GuildDetailInfo,
    badges: &AdminCustomBadgeList,
    csrf_token: &str,
    admin_acls: &[String],
) -> Markup {
    let definitions: Vec<_> = badges.of_type(CustomBadgeType::Guild).collect();
    let assigned: Vec<_> = definitions
        .iter()
        .copied()
        .filter(|badge| guild.badge_ids.contains(&badge.id))
        .collect();
    let can_edit = acl::has_permission(admin_acls, acl::GUILD_UPDATE_BADGES);
    html! {
        div class="space-y-6" {
            (card_with_header("Badges", html! {
                p class="mb-4 text-sm text-neutral-500" {
                    "Badges show next to the guild name, after its verified, partnered or discoverable badge."
                }
                @if definitions.is_empty() {
                    (no_badges_defined(config, "guild badges"))
                } @else if can_edit {
                    form method="post"
                        action={(config.base_path) "/guilds/" (guild.id) "?action=update_badges&tab=badges"} {
                        (csrf_input(csrf_token))
                        div class="space-y-4" {
                            (custom_badge_picker(&definitions, &guild.badge_ids))
                            (form_actions(html! { (submit_button("Save badges")) }))
                        }
                    }
                } @else {
                    (custom_badge_list(&assigned))
                    p class="mt-2 text-xs text-neutral-500" { "You need the guild badges permission to change these." }
                }
            }))
        }
    }
}
