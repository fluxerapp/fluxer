// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::api::types::AdminProfileBadge;
use maud::{Markup, html};

#[derive(Clone, Copy)]
pub enum BadgeSize {
    Detail,
    Dense,
}

impl BadgeSize {
    fn image_class(self, aspect_ratio: Option<f64>) -> &'static str {
        match (self, aspect_ratio.is_some()) {
            (BadgeSize::Detail, false) => "h-6 w-6 shrink-0",
            (BadgeSize::Detail, true) => "h-6 w-auto shrink-0",
            (BadgeSize::Dense, false) => "h-5 w-5 shrink-0",
            (BadgeSize::Dense, true) => "h-5 w-auto shrink-0",
        }
    }

    fn asset(self, badge: &AdminProfileBadge) -> Option<&str> {
        match self {
            BadgeSize::Detail => badge.asset.as_deref(),
            BadgeSize::Dense => badge.small_asset.as_deref().or(badge.asset.as_deref()),
        }
    }

    fn caption_class(self) -> &'static str {
        match self {
            BadgeSize::Detail => "inline-flex h-6 items-center text-base",
            BadgeSize::Dense => "inline-flex h-5 items-center text-sm",
        }
    }
}

fn badge_title(badge: &AdminProfileBadge) -> String {
    if badge.suppressed {
        format!("{} (hidden from public)", badge.label)
    } else {
        badge.label.clone()
    }
}

pub fn user_profile_badges(
    static_cdn_endpoint: &str,
    badges: &[AdminProfileBadge],
    size: BadgeSize,
) -> Markup {
    if badges.is_empty() {
        return html! {};
    }
    let cdn = static_cdn_endpoint.trim_end_matches('/');
    html! {
        div class="flex items-center gap-1.5" {
            @for badge in badges {
                @let title = badge_title(badge);
                @let opacity = if badge.suppressed { " opacity-50" } else { "" };
                @if let Some(asset) = size.asset(badge) {
                    img src={(cdn) "/badges/" (asset) ".webp"}
                        alt=(badge.caption.as_deref().unwrap_or(&title)) title=(title)
                        class={(size.image_class(badge.aspect_ratio)) (opacity)}
                        style=[badge.aspect_ratio.map(|ratio| format!("aspect-ratio: {ratio}"))];
                } @else if let Some(asset_url) = &badge.asset_url {
                    img src={(asset_url) "?size=96"} alt=(title) title=(title)
                        class={(size.image_class(None)) " object-contain" (opacity)};
                } @else if let Some(caption) = &badge.caption {
                    span title=(title)
                        class={(size.caption_class()) " font-bold tabular-nums text-indigo-600" (opacity)} {
                        (caption)
                    }
                }
            }
        }
    }
}
