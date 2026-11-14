// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{entry_count_hint, experiment_rollout_country_codes_field};
use crate::templates::pages::instance_config::ConfigForm;
use crate::{
    api::types::{
        DOMAIN_MIGRATION_DEFAULT_SALT, DomainMigrationConfigResponse,
        EXPERIMENT_MAX_TARGETED_USERS, VOICE_P2P_DEFAULT_SALT, VOICE_P2P_MAX_PARTICIPANTS_RANGE,
        VoiceP2pConfigResponse,
    },
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{Checkbox, NumberField, form_actions, submit_button, text_input, textarea_input},
        section_card::section_card_with_description,
    },
};
use maud::{Markup, html};

fn targeted_ids_field(input: Markup, count: usize, hint: &str) -> Markup {
    html! {
        div class="flex flex-col gap-2" {
            (input)
            (entry_count_hint(count, EXPERIMENT_MAX_TARGETED_USERS))
            p class="text-xs text-neutral-500" { (hint) }
        }
    }
}

pub(super) fn domain_migration_section(
    base: &str,
    config_form: ConfigForm<'_>,
    domain_migration: &DomainMigrationConfigResponse,
) -> Markup {
    let status = if domain_migration.enabled {
        ("Live", BadgeVariant::Success)
    } else {
        ("Inert", BadgeVariant::Default)
    };
    let included_user_ids = domain_migration.included_user_ids.join("\n");
    let excluded_user_ids = domain_migration.excluded_user_ids.join("\n");
    section_card_with_description(
        "Domain Migration",
        "Moves web clients of the official instance from the legacy web app origin to the new \
         one. Selected accounts copy their local data across and continue on the new origin. \
         Clients of other instances read this configuration and ignore it.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_domain_migration"} {
                (config_form.fields())
                div class="space-y-6" {
                    div class="flex flex-wrap items-center gap-2" {
                        h3 class="text-sm font-semibold text-neutral-900" { "Master switch" }
                        (badge(status.0, status.1))
                        span class="text-xs text-neutral-500" {
                            "Config version " (domain_migration.config_version)
                        }
                    }
                    (Checkbox {
                        name: "domain_migration_enabled",
                        value: "true",
                        label: "Move selected web clients to the new origin",
                        checked: domain_migration.enabled,
                        enabled: true,
                    }.render())
                    p class="text-xs text-neutral-500" {
                        "Off is the safe state and the kill switch. With this unchecked no client \
                         starts a migration and clients that already migrated stop forwarding the \
                         legacy origin, so the rollout and targeting fields below have no effect at all."
                    }

                    h3 class="text-sm font-semibold text-neutral-900" { "Installed apps" }
                    (Checkbox {
                        name: "domain_migration_standalone_forwarding",
                        value: "true",
                        label: "Forward installed desktop web apps to the new origin",
                        checked: domain_migration.standalone_forwarding,
                        enabled: true,
                    }.render())
                    p class="text-xs text-neutral-500" {
                        "Leave this off until the manifest scope extension and the association file \
                         are live and verified. While it is off, installed Chromium desktop apps copy \
                         their data across but stay on the legacy origin and offer to install the new \
                         app. Installed mobile and Safari apps never forward either way."
                    }

                    h3 class="text-sm font-semibold text-neutral-900" { "Rollout" }
                    (NumberField {
                        name: "domain_migration_rollout_basis_points",
                        label: "Rollout (basis points)",
                        value: &domain_migration.rollout_basis_points.to_string(),
                        min: Some(0),
                        max: Some(10000),
                        step: "1",
                        helper: Some("Share of logged-in users bucketed into the migration, in basis points: 0 is nobody, 100 is 1%, 10000 is everybody."),
                        placeholder: None,
                    }.render())
                    (NumberField {
                        name: "domain_migration_anonymous_rollout_basis_points",
                        label: "Anonymous rollout (basis points)",
                        value: &domain_migration.anonymous_rollout_basis_points.to_string(),
                        min: Some(0),
                        max: Some(10000),
                        step: "1",
                        helper: Some("Share of logged-out devices sent to the new origin, in basis points. Each device is bucketed on its own random ID."),
                        placeholder: None,
                    }.render())
                    (experiment_rollout_country_codes_field(
                        "domain_migration_rollout_country_codes",
                        &domain_migration.rollout_country_codes,
                    ))
                    div class="flex flex-col gap-2" {
                        (text_input(
                            "domain_migration_rollout_salt",
                            "Rollout Salt",
                            &domain_migration.rollout_salt,
                            DOMAIN_MIGRATION_DEFAULT_SALT,
                        ))
                        p class="text-xs text-neutral-500" {
                            "Seeds the bucketing hash for users and devices. Changing it reshuffles \
                             which users and devices fall inside the percentages above. Leave it \
                             alone to keep the current cohort stable."
                        }
                    }
                    (targeted_ids_field(
                        textarea_input(
                            "domain_migration_included_user_ids",
                            "Always-on User IDs",
                            "1500000000000000001\n1500000000000000002",
                            &included_user_ids,
                            4,
                            false,
                        ),
                        domain_migration.included_user_ids.len(),
                        "One snowflake per line, or comma separated. These users are targeted \
                             regardless of the percentage above. IDs must contain 1 to 20 decimal \
                             digits. Invalid entries prevent the save. Blank entries and duplicate \
                             IDs are ignored.",
                    ))
                    div class="flex flex-col gap-2" {
                        (Checkbox {
                            name: "domain_migration_include_premium_users",
                            value: "true",
                            label: "Include premium users",
                            checked: domain_migration.include_premium_users,
                            enabled: true,
                        }.render())
                        p class="text-xs text-neutral-500" {
                            "Includes every account with active premium perks, regardless of the \
                             percentage above. The never-on list still wins."
                        }
                    }
                    (targeted_ids_field(
                        textarea_input(
                            "domain_migration_included_guild_ids",
                            "Always-on Guild IDs",
                            "1500000000000000005\n1500000000000000006",
                            &domain_migration.included_guild_ids.join("\n"),
                            4,
                            false,
                        ),
                        domain_migration.included_guild_ids.len(),
                        "Same format, with guild IDs. Every member of a listed guild is \
                             included regardless of the percentage above, unless the user is \
                             in the never-on list.",
                    ))
                    (targeted_ids_field(
                        textarea_input(
                            "domain_migration_excluded_user_ids",
                            "Never-on User IDs",
                            "1500000000000000003\n1500000000000000004",
                            &excluded_user_ids,
                            4,
                            false,
                        ),
                        domain_migration.excluded_user_ids.len(),
                        "Same format. Exclusion wins over both the always-on list and the \
                             percentage. It stops new migrations only. A user who already moved \
                             stays on the new origin.",
                    ))

                    (form_actions(html! {
                        (submit_button("Save Domain Migration Configuration"))
                    }))
                }
            }
        },
    )
}

pub(super) fn voice_p2p_section(
    base: &str,
    config_form: ConfigForm<'_>,
    voice_p2p: &VoiceP2pConfigResponse,
) -> Markup {
    let status = if voice_p2p.enabled {
        ("Live", BadgeVariant::Success)
    } else {
        ("Inert", BadgeVariant::Default)
    };
    let included_user_ids = voice_p2p.included_user_ids.join("\n");
    let excluded_user_ids = voice_p2p.excluded_user_ids.join("\n");
    section_card_with_description(
        "Peer-to-peer voice",
        "Lets selected users start small voice calls that connect the participants directly \
         instead of through the voice servers. A peer-to-peer call only moves to the voice \
         servers when a participant switches it or its region changes.",
        html! {
            form method="post" action={(base) "/instance-config?action=update_voice_p2p"} {
                (config_form.fields())
                div class="space-y-6" {
                    div class="flex flex-wrap items-center gap-2" {
                        h3 class="text-sm font-semibold text-neutral-900" { "Master switch" }
                        (badge(status.0, status.1))
                        span class="text-xs text-neutral-500" {
                            "Config version " (voice_p2p.config_version)
                        }
                    }
                    (Checkbox {
                        name: "voice_p2p_enabled",
                        value: "true",
                        label: "Let the selected users start peer-to-peer calls",
                        checked: voice_p2p.enabled,
                        enabled: true,
                    }.render())
                    p class="text-xs text-neutral-500" {
                        "Off is the safe state and the kill switch. With this unchecked every new \
                         call uses the voice servers and a running peer-to-peer call accepts no \
                         new joins, so the rollout and targeting fields below have no effect at \
                         all. While it is on, the fields below select who can start a \
                         peer-to-peer call, and anyone who agrees to a direct connection can \
                         join one."
                    }

                    h3 class="text-sm font-semibold text-neutral-900" { "Call size" }
                    (NumberField {
                        name: "voice_p2p_max_participants",
                        label: "Maximum participants",
                        value: &voice_p2p.max_participants.to_string(),
                        min: Some(*VOICE_P2P_MAX_PARTICIPANTS_RANGE.start()),
                        max: Some(*VOICE_P2P_MAX_PARTICIPANTS_RANGE.end()),
                        step: "1",
                        helper: Some("Most participants a peer-to-peer call can hold, 2 through 4. Clients only offer a peer-to-peer call in a direct message or group with at most this many members, and a join past it is refused as full. Lowering it never removes anyone from a running call."),
                        placeholder: None,
                    }.render())

                    h3 class="text-sm font-semibold text-neutral-900" { "Rollout" }
                    (NumberField {
                        name: "voice_p2p_rollout_basis_points",
                        label: "Rollout (basis points)",
                        value: &voice_p2p.rollout_basis_points.to_string(),
                        min: Some(0),
                        max: Some(10000),
                        step: "1",
                        helper: Some("Share of users who can start peer-to-peer calls, in basis points: 0 is nobody, 100 is 1%, 10000 is everybody."),
                        placeholder: None,
                    }.render())
                    (experiment_rollout_country_codes_field(
                        "voice_p2p_rollout_country_codes",
                        &voice_p2p.rollout_country_codes,
                    ))
                    div class="flex flex-col gap-2" {
                        (text_input(
                            "voice_p2p_rollout_salt",
                            "Rollout Salt",
                            &voice_p2p.rollout_salt,
                            VOICE_P2P_DEFAULT_SALT,
                        ))
                        p class="text-xs text-neutral-500" {
                            "Seeds the bucketing hash. Changing it reshuffles which users fall \
                             inside the percentage above. Leave it alone to keep the current \
                             cohort stable."
                        }
                    }
                    (targeted_ids_field(
                        textarea_input(
                            "voice_p2p_included_user_ids",
                            "Always-on User IDs",
                            "1500000000000000001\n1500000000000000002",
                            &included_user_ids,
                            4,
                            false,
                        ),
                        voice_p2p.included_user_ids.len(),
                        "One snowflake per line, or comma separated. These users are targeted \
                             regardless of the percentage above. IDs must contain 1 to 20 decimal \
                             digits. Invalid entries prevent the save. Blank entries and duplicate \
                             IDs are ignored.",
                    ))
                    div class="flex flex-col gap-2" {
                        (Checkbox {
                            name: "voice_p2p_include_premium_users",
                            value: "true",
                            label: "Include premium users",
                            checked: voice_p2p.include_premium_users,
                            enabled: true,
                        }.render())
                        p class="text-xs text-neutral-500" {
                            "Includes every account with active premium perks, regardless of the \
                             percentage above. The never-on list still wins."
                        }
                    }
                    (targeted_ids_field(
                        textarea_input(
                            "voice_p2p_included_guild_ids",
                            "Always-on Guild IDs",
                            "1500000000000000005\n1500000000000000006",
                            &voice_p2p.included_guild_ids.join("\n"),
                            4,
                            false,
                        ),
                        voice_p2p.included_guild_ids.len(),
                        "Same format, with guild IDs. Every member of a listed guild is \
                             included regardless of the percentage above, unless the user is \
                             in the never-on list.",
                    ))
                    (targeted_ids_field(
                        textarea_input(
                            "voice_p2p_excluded_user_ids",
                            "Never-on User IDs",
                            "1500000000000000003\n1500000000000000004",
                            &excluded_user_ids,
                            4,
                            false,
                        ),
                        voice_p2p.excluded_user_ids.len(),
                        "Same format. Exclusion wins over both the always-on list and the \
                             percentage.",
                    ))

                    (form_actions(html! {
                        (submit_button("Save Peer-to-Peer Voice Configuration"))
                    }))
                }
            }
        },
    )
}
