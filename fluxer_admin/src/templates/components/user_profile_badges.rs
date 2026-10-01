// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{admin_flags::user_flag_bits, api::types::ListBadgesResponse};
use maud::{Markup, PreEscaped, html};

pub mod premium_types {
    pub const NONE: i32 = 0;
    pub const LIFETIME: i32 = 2;
}

#[derive(Clone, Copy, Default)]
pub struct ProfileBadges<'a> {
    pub premium_name: Option<&'a str>,
    pub definitions: Option<&'a ListBadgesResponse>,
}

enum BadgeIcon<'a> {
    Url(String),
    Svg(&'a str),
}

struct BadgeDef<'a> {
    icon: BadgeIcon<'a>,
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
            Some(since) => format!("{name} subscriber since {since}"),
            None => name.to_owned(),
        });
    }
    Some(if premium_type == premium_types::LIFETIME {
        match premium_since {
            Some(since) => format!("Fluxer Visionary since {since}"),
            None => "Fluxer Visionary".into(),
        }
    } else {
        match premium_since {
            Some(since) => format!("Fluxer Plutonium subscriber since {since}"),
            None => "Fluxer Plutonium".into(),
        }
    })
}

fn builtin_icon<'a>(icon: Option<&'a str>, cdn: &str, file: &str) -> BadgeIcon<'a> {
    icon.map_or_else(
        || BadgeIcon::Url(format!("{cdn}/badges/{file}")),
        BadgeIcon::Svg,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn user_profile_badges(
    static_cdn_endpoint: &str,
    flags: u64,
    profile_badges: ProfileBadges<'_>,
    badge_ids: &[String],
    premium_type: Option<i32>,
    premium_since: Option<&str>,
    is_self_hosted: bool,
    size_sm: bool,
) -> Markup {
    let cdn = static_cdn_endpoint.trim_end_matches('/');
    let definitions = profile_badges.definitions;
    let mut badges: Vec<BadgeDef> = Vec::new();
    if flags & user_flag_bits::STAFF != 0 {
        badges.push(BadgeDef {
            icon: builtin_icon(
                definitions.and_then(|d| d.builtin_icons.staff.as_deref()),
                cdn,
                "staff.svg?v=2",
            ),
            tooltip: "Fluxer Staff".into(),
        });
    }
    let mut assigned: Vec<_> = definitions
        .map(|d| d.badges.as_slice())
        .unwrap_or_default()
        .iter()
        .filter(|badge| badge.badge_type == "user" && badge_ids.contains(&badge.id))
        .collect();
    assigned.sort_by_key(|badge| badge.position);
    badges.extend(assigned.into_iter().map(|badge| BadgeDef {
        icon: BadgeIcon::Svg(&badge.icon),
        tooltip: badge.tooltip.clone(),
    }));
    if let Some(pt) = premium_type
        && pt != premium_types::NONE
        && let Some(tooltip) = premium_tooltip(
            pt,
            premium_since,
            is_self_hosted,
            profile_badges.premium_name,
        )
    {
        badges.push(BadgeDef {
            icon: builtin_icon(
                definitions.and_then(|d| d.builtin_icons.premium.as_deref()),
                cdn,
                "plutonium.svg",
            ),
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
                @match &b.icon {
                    BadgeIcon::Url(url) => {
                        img src=(url) alt=(b.tooltip) title=(b.tooltip) class=(badge_size);
                    }
                    BadgeIcon::Svg(svg) => {
                        span role="img" aria-label=(b.tooltip) title=(b.tooltip)
                            class={(badge_size) " [&>svg]:h-full [&>svg]:w-full"} {
                            (PreEscaped(*svg))
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::types::{Badge, BuiltinBadgeIcons};

    fn render(self_hosted: bool, name: Option<&str>, premium_type: i32) -> String {
        user_profile_badges(
            "https://static.example.com",
            0,
            ProfileBadges {
                premium_name: name,
                definitions: None,
            },
            &[],
            Some(premium_type),
            Some("2026-01-01"),
            self_hosted,
            false,
        )
        .into_string()
    }

    #[test]
    fn assigned_badges_render_in_position_order_with_icon_overrides() {
        let badge = |id: &str, badge_type: &str, position: i64| Badge {
            id: id.to_owned(),
            badge_type: badge_type.to_owned(),
            name: id.to_owned(),
            tooltip: format!("{id} tooltip"),
            icon: format!("<svg id=\"{id}\"></svg>"),
            url: None,
            position,
        };
        let definitions = ListBadgesResponse {
            version: 1,
            badges: vec![
                badge("second", "user", 2),
                badge("first", "user", 1),
                badge("guild", "guild", 0),
                badge("unassigned", "user", 0),
            ],
            builtin_icons: BuiltinBadgeIcons {
                staff: Some("<svg id=\"staff\"></svg>".to_owned()),
                premium: Some("<svg id=\"premium\"></svg>".to_owned()),
                ..Default::default()
            },
        };
        let markup = user_profile_badges(
            "https://static.example.com",
            user_flag_bits::STAFF,
            ProfileBadges {
                premium_name: None,
                definitions: Some(&definitions),
            },
            &["second".to_owned(), "first".to_owned(), "guild".to_owned()],
            Some(1),
            None,
            false,
            false,
        )
        .into_string();
        let first = markup.find("first tooltip").unwrap();
        let second = markup.find("second tooltip").unwrap();
        let staff = markup.find("Fluxer Staff").unwrap();
        assert!(staff < first && first < second);
        assert!(markup.contains("<svg id=\"staff\"></svg>"));
        assert!(!markup.contains("guild tooltip"));
        assert!(!markup.contains("unassigned"));
        assert!(markup.contains("<svg id=\"premium\"></svg>"));
        assert!(!markup.contains("plutonium.svg"));
    }

    #[test]
    fn hosted_premium_badges_keep_their_fluxer_labels() {
        assert!(
            render(false, Some("Gold"), 1).contains("Fluxer Plutonium subscriber since 2026-01-01")
        );
        assert!(render(false, None, 2).contains("Fluxer Visionary since 2026-01-01"));
    }

    #[test]
    fn self_hosted_premium_badges_use_the_configured_name() {
        let markup = render(true, Some("Gold"), 1);
        assert!(markup.contains("Gold subscriber since 2026-01-01"));
        assert!(!markup.contains("Plutonium"));
        assert!(render(true, Some("Gold"), 2).contains("Gold subscriber since"));
        assert!(!render(true, None, 1).contains("img"));
    }
}
