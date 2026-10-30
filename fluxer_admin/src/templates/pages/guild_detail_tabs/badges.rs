// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::types::{BadgeAssignment, CustomBadgeType, GuildDetailInfo},
    config::AdminConfig,
    templates::{
        components::page_container::card_with_header,
        pages::badges::{BadgeAssignmentTargets, custom_badge_assignment},
    },
};
use maud::{Markup, html};

pub fn badges_tab(
    config: &AdminConfig,
    guild: &GuildDetailInfo,
    assignment: &BadgeAssignment,
    csrf_token: &str,
    admin_acls: &[String],
) -> Markup {
    let can_edit = acl::has_permission(admin_acls, acl::GUILD_UPDATE_BADGES);
    let search_action = format!("{}/guilds/{}", config.base_path, guild.id);
    let save_action = format!(
        "{}/guilds/{}?action=update_badges&tab=badges",
        config.base_path, guild.id
    );
    html! {
        div class="space-y-6" {
            (card_with_header("Badges", html! {
                p class="mb-4 text-sm text-neutral-500" {
                    "Badges show next to the guild name, after its verified, partnered or discoverable badge."
                }
                (custom_badge_assignment(config, assignment, &BadgeAssignmentTargets {
                    badge_type: CustomBadgeType::Guild,
                    noun: "guild badges",
                    tab: "badges",
                    search_action: &search_action,
                    save_action: &save_action,
                    permission_note: "You need the guild badges permission to change these.",
                }, can_edit, csrf_token))
            }))
        }
    }
}
