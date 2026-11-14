// SPDX-License-Identifier: AGPL-3.0-or-later

use super::inherit_select;
use crate::templates::pages::instance_config::ConfigForm;
use crate::{
    api::types::{InstancePolicyResponse, PremiumMode},
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{Checkbox, danger_button, form_actions, select_input, submit_button, text_input},
        section_card::section_card_with_description,
    },
};
use maud::{Markup, html};

pub(super) fn policy_config_section(
    base: &str,
    config_form: ConfigForm<'_>,
    policy: &InstancePolicyResponse,
    premium_name: &str,
    billing_enabled: bool,
) -> Markup {
    section_card_with_description(
        "Community & Policy",
        "Control whether this instance runs as a single community, whether direct messages and \
         friends are available, which premium model applies, and which optional embed services \
         are enabled.",
        html! {
            div class="space-y-8" {
                (single_community_form(base, config_form, policy))
                (direct_messages_form(base, config_form, policy))
                (premium_mode_form(base, config_form, policy, premium_name, billing_enabled))
                (community_creation_form(base, config_form, policy))
                (services_form(base, config_form, policy))
            }
        },
    )
}

fn single_community_form(
    base: &str,
    config_form: ConfigForm<'_>,
    policy: &InstancePolicyResponse,
) -> Markup {
    let status = if policy.single_community_enabled {
        ("Enabled", BadgeVariant::Success)
    } else {
        ("Disabled", BadgeVariant::Default)
    };
    let existing_members_checkbox = Checkbox {
        name: "single_community_add_members",
        value: "true",
        label: "Also add every existing account to the community",
        checked: false,
        enabled: true,
    }
    .render();
    html! {
        div class="space-y-4" {
            div class="flex flex-wrap items-center gap-2" {
                h3 class="text-sm font-semibold text-neutral-900" { "Single community" }
                (badge(status.0, status.1))
            }
            @if let Some(guild_id) = policy.single_community_guild_id.as_deref() {
                p class="break-all text-xs text-neutral-500" { "Community guild ID: " (guild_id) }
            }
            @if policy.single_community_enabled {
                p class="text-sm text-neutral-500" {
                    "This instance funnels every member into a single community. New accounts join \
                     it when they sign up. You can point the mode at another community, add the \
                     accounts that existed before, or turn it off. The community itself is kept \
                     either way."
                }
                form method="post" action={(base) "/instance-config?action=update_single_community"} {
                    (config_form.fields())
                    div class="space-y-4" {
                        (text_input(
                            "single_community_guild_id",
                            "Use another community (ID)",
                            "",
                            "Leave blank to keep the current community",
                        ))
                        (existing_members_checkbox)
                        (form_actions(html! {
                            (submit_button("Save single-community settings"))
                        }))
                    }
                }
                form method="post" action={(base) "/instance-config?action=disable_single_community"} {
                    (config_form.fields())
                    (form_actions(html! {
                        (danger_button("Disable single-community mode"))
                    }))
                }
            } @else {
                p class="text-sm text-neutral-500" {
                    "Single-community mode is off. To turn it on, enter the ID of an existing community. "
                    @if policy.single_community_guild_id.is_some() {
                        "Leave the ID blank to reuse the community above."
                    } @else {
                        "Before setup is finished, a blank ID creates a new community."
                    }
                }
                form method="post" action={(base) "/instance-config?action=enable_single_community"} {
                    (config_form.fields())
                    div class="space-y-4" {
                        (text_input("single_community_guild_id", "Community ID", "", "123456789012345678"))
                        (existing_members_checkbox)
                        (form_actions(html! {
                            (submit_button("Enable single-community mode"))
                        }))
                    }
                }
            }
        }
    }
}

fn direct_messages_form(
    base: &str,
    config_form: ConfigForm<'_>,
    policy: &InstancePolicyResponse,
) -> Markup {
    let enabled_options: &[(&str, &str)] = &[("false", "Enabled"), ("true", "Disabled")];
    let personal_notes_options: &[(&str, &str)] = &[("true", "Enabled"), ("false", "Disabled")];
    html! {
        div class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Direct messages, friends and personal notes" }
            form method="post" action={(base) "/instance-config?action=update_policy"} {
                (config_form.fields())
                @if policy.direct_messages_locked && !policy.direct_messages_disabled {
                    input type="hidden" name="policy_direct_messages_unlock" value="true";
                }
                div class="space-y-4" {
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-2" {
                        (select_input(
                            "policy_direct_messages_disabled",
                            "Direct messages",
                            enabled_options,
                            if policy.direct_messages_disabled { "true" } else { "false" },
                        ))
                        (select_input(
                            "policy_friends_disabled",
                            "Friend requests",
                            enabled_options,
                            if policy.friends_disabled { "true" } else { "false" },
                        ))
                        (select_input(
                            "policy_personal_notes_enabled",
                            "Personal notes",
                            personal_notes_options,
                            if policy.personal_notes_enabled { "true" } else { "false" },
                        ))
                    }
                    p class="text-xs text-neutral-500" {
                        "With direct messages off, people can still be friends and talk in communities. \
                         With friend requests off, nobody can send or accept new requests, and \
                         existing friends stay listed."
                    }
                    p class="text-xs text-neutral-500" {
                        "Personal notes are a private channel where each person can write messages to \
                         themselves. With personal notes off, nobody can open or write in them. Existing \
                         notes are kept and come back when you turn personal notes on again."
                    }
                    @if policy.direct_messages_locked {
                        p class="text-xs text-neutral-500" {
                            "Direct messages were turned off before and then back on. Turning them off \
                             again hides existing conversations until they are turned back on."
                        }
                    }
                    (form_actions(html! {
                        (submit_button("Save direct message, friend and personal notes settings"))
                    }))
                }
            }
        }
    }
}

fn premium_mode_form(
    base: &str,
    config_form: ConfigForm<'_>,
    policy: &InstancePolicyResponse,
    premium_name: &str,
    billing_enabled: bool,
) -> Markup {
    let mirror_label = format!("Mirror (Free and {premium_name} tiers)");
    let everyone_label = format!("Everyone (every member gets {premium_name} limits)");
    html! {
        div class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Premium model" }
            form method="post" action={(base) "/instance-config?action=update_policy"} {
                (config_form.fields())
                div class="space-y-4" {
                    (select_input("policy_premium_mode", "Premium model", &[
                        ("mirror", mirror_label.as_str()),
                        ("everyone", everyone_label.as_str()),
                    ], policy.premium_mode.as_str()))
                    p class="text-xs text-neutral-500" {
                        "Limits you have not edited follow the model. Limits you changed under Limit \
                         Config keep your values in both models."
                    }
                    @if billing_enabled && matches!(policy.premium_mode, PremiumMode::Mirror) {
                        p class="text-sm text-amber-700" {
                            "Billing is enabled. Disable it under Premium & billing before switching \
                             the premium model to Everyone."
                        }
                    }
                    (form_actions(html! {
                        (submit_button("Save premium model"))
                    }))
                }
            }
        }
    }
}

fn community_creation_form(
    base: &str,
    config_form: ConfigForm<'_>,
    policy: &InstancePolicyResponse,
) -> Markup {
    html! {
        div id="community-creation" class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Community creation" }
            form method="post" action={(base) "/instance-config?action=update_policy"} {
                (config_form.fields())
                div class="space-y-4" {
                    (select_input("policy_guild_create_access", "Who can create communities", &[
                        ("true", "Everyone"),
                        ("false", "Restricted"),
                    ], if policy.guild_create_access { "true" } else { "false" }))
                    p class="text-xs text-neutral-500" {
                        "When restricted, only admins with the wildcard ACL and users matched by a "
                        a href={(base) "/limit-config"} class="text-blue-600 hover:underline" {
                            "limit rule"
                        }
                        " that grants Community Creation Access can create communities."
                    }
                    (form_actions(html! {
                        (submit_button("Save community creation policy"))
                    }))
                }
            }
        }
    }
}

fn service_select(name: &str, label: &str, override_value: Option<bool>, resolved: bool) -> Markup {
    let resolved_text = if resolved {
        "Effective: enabled"
    } else {
        "Effective: disabled"
    };
    html! {
        div class="space-y-2" {
            (inherit_select(name, label, override_value))
            p class="text-xs text-neutral-500" { (resolved_text) }
        }
    }
}

fn services_form(
    base: &str,
    config_form: ConfigForm<'_>,
    policy: &InstancePolicyResponse,
) -> Markup {
    let available = &policy.services_available;
    if !available.gif && !available.youtube && !available.bluesky {
        return html! {
            div class="space-y-4 border-t border-neutral-200 pt-6" {
                h3 class="text-sm font-semibold text-neutral-900" { "Optional services" }
                p class="text-sm text-neutral-500" {
                    "No optional embed services are available in this environment."
                }
            }
        };
    }
    html! {
        div class="space-y-4 border-t border-neutral-200 pt-6" {
            h3 class="text-sm font-semibold text-neutral-900" { "Optional services" }
            p class="text-sm text-neutral-500" {
                "Override the environment default for each optional embed service. \
                 \"Inherit env default\" keeps the value configured for this deployment."
            }
            form method="post" action={(base) "/instance-config?action=update_policy"} {
                (config_form.fields())
                div class="space-y-4" {
                    div class="grid grid-cols-1 gap-4 sm:grid-cols-3" {
                        @if available.gif {
                            (service_select(
                                "policy_service_gif",
                                "GIF picker",
                                policy.services.gif_enabled,
                                policy.services_resolved.gif_enabled,
                            ))
                        }
                        @if available.youtube {
                            (service_select(
                                "policy_service_youtube",
                                "YouTube embeds",
                                policy.services.youtube_enabled,
                                policy.services_resolved.youtube_enabled,
                            ))
                        }
                        @if available.bluesky {
                            (service_select(
                                "policy_service_bluesky",
                                "Bluesky embeds",
                                policy.services.bluesky_enabled,
                                policy.services_resolved.bluesky_enabled,
                            ))
                        }
                    }
                    (form_actions(html! {
                        (submit_button("Save optional services"))
                    }))
                }
            }
        }
    }
}
