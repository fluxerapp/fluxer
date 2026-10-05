// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::AdminUser,
    templates::components::page_container::{card_with_header, detail_row},
    utils::{
        bigint::{format_discriminator, has_flag, list_flags},
        user_tag::user_tag,
    },
};
use maud::{Markup, html};

pub fn settings_tab(user: &AdminUser) -> Markup {
    html! {
        div class="space-y-6" {
            (card_with_header("Profile Settings", html! {
                dl class="divide-y divide-neutral-100" {
                    (detail_row("Username", html! {
                        (user_tag(&user.username, &format_discriminator(&user.discriminator), user.bot))
                    }))
                    (detail_row("Display Name", html! {
                        @if let Some(ref name) = user.global_name {
                            (name)
                        } @else {
                            span class="text-neutral-400" { "Not set" }
                        }
                    }))
                    (detail_row("Email", html! {
                        @if let Some(ref email) = user.email {
                            (email)
                        } @else {
                            span class="text-neutral-400" { "Hidden" }
                        }
                    }))
                    (detail_row("Avatar", html! {
                        @if let Some(ref hash) = user.avatar {
                            span class="text-xs" { (hash) }
                        } @else {
                            span class="text-neutral-400" { "Default" }
                        }
                    }))
                    (detail_row("Bot", html! {
                        @if user.bot { "Yes" } @else { "No" }
                    }))
                    (detail_row("System", html! {
                        @if user.system { "Yes" } @else { "No" }
                    }))
                }
            }))

            (card_with_header("Flags", html! {
                dl class="divide-y divide-neutral-100" {
                    (detail_row("User Flags (raw)", html! {
                        span class="text-xs" { (user.flags) }
                    }))
                    (detail_row("Premium Flags (raw)", html! {
                        span class="text-xs" { (user.premium_flags) }
                    }))
                    (detail_row("Active flag bits", html! {
                        @let bits = list_flags(user.flags);
                        @if bits.is_empty() {
                            span class="text-neutral-400" { "None" }
                        } @else {
                            div class="flex flex-wrap gap-2" {
                                @for bit in &bits {
                                    span class="inline-flex items-center rounded-full px-2 \
                                                py-0.5 text-xs font-medium bg-blue-100 \
                                                text-blue-700" {
                                        "bit " (bit)
                                    }
                                }
                            }
                        }
                    }))
                }
            }))

            (card_with_header("Known Flags", html! {
                div class="grid grid-cols-2 gap-2 md:grid-cols-3" {
                    (flag_row("Staff", user.flags, 0))
                    (flag_row("Partner", user.flags, 1))
                    (flag_row("HypeSquad Events", user.flags, 2))
                    (flag_row("Bug Hunter Lvl 1", user.flags, 3))
                    (flag_row("HypeSquad Bravery", user.flags, 6))
                    (flag_row("HypeSquad Brilliance", user.flags, 7))
                    (flag_row("HypeSquad Balance", user.flags, 8))
                    (flag_row("Early Supporter", user.flags, 9))
                    (flag_row("Bug Hunter Lvl 2", user.flags, 14))
                    (flag_row("Verified Bot", user.flags, 16))
                    (flag_row("Verified Developer", user.flags, 17))
                    (flag_row("Certified Moderator", user.flags, 18))
                }
            }))
        }
    }
}

fn flag_row(label: &str, flags: u64, bit: u32) -> Markup {
    let active = has_flag(flags, 1u64 << bit);
    let classes = if active {
        "rounded-lg border border-green-200 bg-green-50 px-3 py-2 text-sm text-green-700"
    } else {
        "rounded-lg border border-neutral-200 bg-neutral-50 px-3 py-2 text-sm text-neutral-500"
    };
    html! {
        div class=(classes) {
            (label)
        }
    }
}
