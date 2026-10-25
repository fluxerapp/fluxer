// SPDX-License-Identifier: AGPL-3.0-or-later

use super::parse_tristate;
use crate::{
    api::types::{
        InstanceConfigUpdateRequest, InstancePolicyUpdateRequest, InstanceServicesUpdateRequest,
        PremiumMode,
    },
    utils::forms::MultiValueForm,
};

pub(super) enum CommunityAction {
    Enable,
    Disable,
    Update,
}

pub(super) fn build_policy_update(form: &MultiValueForm) -> InstanceConfigUpdateRequest {
    let direct_messages_disabled = form
        .first("policy_direct_messages_disabled")
        .map(|value| value == "true");
    let friends_disabled = form
        .first("policy_friends_disabled")
        .map(|value| value == "true");
    let personal_notes_enabled = form
        .first("policy_personal_notes_enabled")
        .map(|value| value == "true");
    let direct_messages_locked = (form.bool_value("policy_direct_messages_unlock")
        && direct_messages_disabled == Some(true))
    .then_some(false);
    let guild_create_access = form
        .first("policy_guild_create_access")
        .map(|value| value == "true");
    let premium_mode = match form.first("policy_premium_mode") {
        Some("mirror") => Some(PremiumMode::Mirror),
        Some("everyone") => Some(PremiumMode::Everyone),
        _ => None,
    };
    let services = {
        let gif_enabled = parse_tristate(form, "policy_service_gif");
        let youtube_enabled = parse_tristate(form, "policy_service_youtube");
        let bluesky_enabled = parse_tristate(form, "policy_service_bluesky");
        if gif_enabled.is_none() && youtube_enabled.is_none() && bluesky_enabled.is_none() {
            None
        } else {
            Some(InstanceServicesUpdateRequest {
                gif_enabled,
                youtube_enabled,
                bluesky_enabled,
            })
        }
    };
    InstanceConfigUpdateRequest {
        policy: Some(InstancePolicyUpdateRequest {
            direct_messages_disabled,
            direct_messages_locked,
            friends_disabled,
            personal_notes_enabled,
            guild_create_access,
            premium_mode,
            services,
            ..Default::default()
        }),
        ..Default::default()
    }
}

pub(super) fn build_single_community_update(
    form: &MultiValueForm,
    action: CommunityAction,
) -> InstanceConfigUpdateRequest {
    let enabled = match action {
        CommunityAction::Enable => Some(true),
        CommunityAction::Disable => Some(false),
        CommunityAction::Update => None,
    };
    let keeps_mode = !matches!(action, CommunityAction::Disable);
    InstanceConfigUpdateRequest {
        policy: Some(InstancePolicyUpdateRequest {
            single_community_enabled: enabled,
            single_community_name: form
                .clean("single_community_name")
                .filter(|_| matches!(action, CommunityAction::Enable)),
            single_community_guild_id: form
                .clean("single_community_guild_id")
                .filter(|_| keeps_mode),
            single_community_add_members: (keeps_mode
                && form.bool_value("single_community_add_members"))
            .then_some(true),
            ..Default::default()
        }),
        ..Default::default()
    }
}
