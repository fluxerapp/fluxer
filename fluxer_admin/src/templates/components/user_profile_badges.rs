// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::admin_flags::user_flag_bits;
use crate::utils::timestamps::format_admin_timestamp;
use maud::{Markup, html};

pub mod premium_types {
    pub const NONE: i32 = 0;
    pub const LIFETIME: i32 = 2;
}

struct BadgeDef {
    icon_url: String,
    tooltip: String,
}

fn premium_tooltip(
    premium_type: i32,
    premium_since: Option<&str>,
    is_self_hosted: bool,
    self_hosted_premium_name: Option<&str>,
) -> Option<String> {
    if is_self_hosted {
        let name = self_hosted_premium_name?;
        return Some(match premium_since {
            Some(since) => format!("{name} subscriber since {}", format_admin_timestamp(since)),
            None => name.to_owned(),
        });
    }
    Some(if premium_type == premium_types::LIFETIME {
        match premium_since {
            Some(since) => format!("Fluxer Visionary since {}", format_admin_timestamp(since)),
            None => "Fluxer Visionary".into(),
        }
    } else {
        match premium_since {
            Some(since) => format!(
                "Fluxer Plutonium subscriber since {}",
                format_admin_timestamp(since)
            ),
            None => "Fluxer Plutonium".into(),
        }
    })
}

pub fn user_profile_badges(
    static_cdn_endpoint: &str,
    flags: u64,
    premium_type: Option<i32>,
    premium_since: Option<&str>,
    is_self_hosted: bool,
    self_hosted_premium_name: Option<&str>,
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
    if let Some(pt) = premium_type
        && pt != premium_types::NONE
        && let Some(tooltip) =
            premium_tooltip(pt, premium_since, is_self_hosted, self_hosted_premium_name)
    {
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
