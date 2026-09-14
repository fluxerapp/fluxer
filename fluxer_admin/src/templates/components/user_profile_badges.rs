// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::admin_flags::user_flag_bits;
use maud::{Markup, html};

pub mod premium_types {
    pub const NONE: i32 = 0;
    pub const LIFETIME: i32 = 2;
}

struct BadgeDef {
    icon_url: String,
    tooltip: String,
}

pub fn user_profile_badges(
    static_cdn_endpoint: &str,
    flags: u64,
    premium_type: Option<i32>,
    premium_since: Option<&str>,
    is_self_hosted: bool,
    size_sm: bool,
) -> Markup {
    let cdn = static_cdn_endpoint.trim_end_matches('/');
    let mut badges: Vec<BadgeDef> = Vec::new();

    if flags & user_flag_bits::STAFF != 0 {
        badges.push(BadgeDef {
            icon_url: format!("{cdn}/badges/staff.svg?v=2"),
            tooltip: "Fluxer Staff".into(),
        });
    }
    if !is_self_hosted && flags & user_flag_bits::PARTNER != 0 {
        badges.push(BadgeDef {
            icon_url: format!("{cdn}/badges/partner.svg"),
            tooltip: "Fluxer Partner".into(),
        });
    }
    if !is_self_hosted && flags & user_flag_bits::BUG_HUNTER != 0 {
        badges.push(BadgeDef {
            icon_url: format!("{cdn}/badges/bug-hunter.svg"),
            tooltip: "Fluxer Bug Hunter".into(),
        });
    }
    if !is_self_hosted
        && let Some(pt) = premium_type
        && pt != premium_types::NONE
    {
        let tooltip = if pt == premium_types::LIFETIME {
            match premium_since {
                Some(since) => format!("Fluxer Visionary since {since}"),
                None => "Fluxer Visionary".into(),
            }
        } else {
            match premium_since {
                Some(since) => {
                    format!("Fluxer Plutonium subscriber since {since}")
                }
                None => "Fluxer Plutonium".into(),
            }
        };
        badges.push(BadgeDef {
            icon_url: format!("{cdn}/badges/plutonium.svg"),
            tooltip,
        });
    }

    if badges.is_empty() {
        return html! {};
    }

    let (badge_size, container_gap) = if size_sm {
        ("h-4 w-4 shrink-0", "flex items-center gap-1.5")
    } else {
        ("h-5 w-5 shrink-0", "flex items-center gap-2")
    };

    html! {
        div class=(container_gap) {
            @for b in &badges {
                img src=(b.icon_url) alt=(b.tooltip) title=(b.tooltip)
                    class=(badge_size);
            }
        }
    }
}
