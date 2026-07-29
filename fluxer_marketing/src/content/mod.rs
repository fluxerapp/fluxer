// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::i18n::{Locale, MarketingI18n, MarketingMessageDescriptor, descriptors::*};
use ammonia::Builder;
use comrak::{
    Arena, Options,
    adapters::{HeadingAdapter, HeadingMeta},
    format_html_with_plugins,
    nodes::{Node, NodeValue, Sourcepos},
    options, parse_document,
};
use maud::{Markup, PreEscaped, html};
use std::{
    borrow::Cow,
    collections::HashMap,
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    sync::{Arc, LazyLock, Mutex},
};

#[derive(Clone, Copy, Debug)]
pub struct JobListing {
    pub slug: &'static str,
    pub title: MarketingMessageDescriptor,
    pub department: MarketingMessageDescriptor,
    pub location: MarketingMessageDescriptor,
    pub employment_type: MarketingMessageDescriptor,
    pub description: MarketingMessageDescriptor,
    pub posted_date: &'static str,
    pub body: &'static str,
}

#[derive(Clone, Copy, Debug)]
pub struct Policy {
    pub slug: &'static str,
    pub title: MarketingMessageDescriptor,
    pub description: MarketingMessageDescriptor,
    pub category: MarketingMessageDescriptor,
    pub last_updated: Option<&'static str>,
    pub body: &'static str,
}

#[derive(Clone, Copy, Debug)]
pub struct HelpCategory {
    pub slug: &'static str,
    pub title: MarketingMessageDescriptor,
    pub description: MarketingMessageDescriptor,
}

#[derive(Clone, Copy, Debug)]
pub struct HelpArticle {
    pub slug: &'static str,
    pub aliases: &'static [&'static str],
    pub title: MarketingMessageDescriptor,
    pub description: MarketingMessageDescriptor,
    pub category_slug: &'static str,
    pub last_updated: &'static str,
    pub body: &'static str,
}

#[derive(Clone, Copy, Debug)]
pub struct BlogPost {
    pub slug: &'static str,
    pub title: MarketingMessageDescriptor,
    pub description: MarketingMessageDescriptor,
    pub author: &'static str,
    pub published_at: &'static str,
    pub updated_at: &'static str,
    pub tags: &'static [&'static str],
    pub feature_image_path: &'static str,
    pub feature_image_base_path: &'static str,
    pub feature_image_alt: MarketingMessageDescriptor,
    pub feature_image_placeholder: &'static str,
    pub source_url: &'static str,
    pub body: &'static str,
}

pub const JOBS: &[JobListing] = &[
    JobListing {
        slug: "product-engineer",
        title: CONTENT_JOBS_PRODUCT_ENGINEER_TITLE_DESCRIPTOR,
        department: CONTENT_LABEL_ENGINEERING_DESCRIPTOR,
        location: CONTENT_LABEL_REMOTE_DESCRIPTOR,
        employment_type: CONTENT_LABEL_FULL_TIME_DESCRIPTOR,
        description: CONTENT_JOBS_PRODUCT_ENGINEER_DESCRIPTION_DESCRIPTOR,
        posted_date: "2026-03-01",
        body: include_str!("../../content/jobs/product-engineer.md"),
    },
    JobListing {
        slug: "platform-engineer",
        title: CONTENT_JOBS_PLATFORM_ENGINEER_TITLE_DESCRIPTOR,
        department: CONTENT_LABEL_ENGINEERING_DESCRIPTOR,
        location: CONTENT_LABEL_REMOTE_DESCRIPTOR,
        employment_type: CONTENT_LABEL_FULL_TIME_DESCRIPTOR,
        description: CONTENT_JOBS_PLATFORM_ENGINEER_DESCRIPTION_DESCRIPTOR,
        posted_date: "2026-03-01",
        body: include_str!("../../content/jobs/platform-engineer.md"),
    },
    JobListing {
        slug: "community-lead",
        title: CONTENT_JOBS_COMMUNITY_LEAD_TITLE_DESCRIPTOR,
        department: CONTENT_LABEL_COMMUNITY_DESCRIPTOR,
        location: CONTENT_LABEL_REMOTE_DESCRIPTOR,
        employment_type: CONTENT_LABEL_FULL_TIME_DESCRIPTOR,
        description: CONTENT_JOBS_COMMUNITY_LEAD_DESCRIPTION_DESCRIPTOR,
        posted_date: "2026-03-01",
        body: include_str!("../../content/jobs/community-lead.md"),
    },
    JobListing {
        slug: "support-specialist",
        title: CONTENT_JOBS_SUPPORT_SPECIALIST_TITLE_DESCRIPTOR,
        department: CONTENT_LABEL_SUPPORT_DESCRIPTOR,
        location: CONTENT_LABEL_REMOTE_DESCRIPTOR,
        employment_type: CONTENT_LABEL_FULL_TIME_DESCRIPTOR,
        description: CONTENT_JOBS_SUPPORT_SPECIALIST_DESCRIPTION_DESCRIPTOR,
        posted_date: "2026-05-13",
        body: include_str!("../../content/jobs/support-specialist.md"),
    },
    JobListing {
        slug: "trust-and-safety-specialist",
        title: CONTENT_JOBS_TRUST_AND_SAFETY_SPECIALIST_TITLE_DESCRIPTOR,
        department: CONTENT_LABEL_TRUST_AND_SAFETY_DESCRIPTOR,
        location: CONTENT_LABEL_REMOTE_DESCRIPTOR,
        employment_type: CONTENT_LABEL_FULL_TIME_DESCRIPTOR,
        description: CONTENT_JOBS_TRUST_AND_SAFETY_SPECIALIST_DESCRIPTION_DESCRIPTOR,
        posted_date: "2026-03-01",
        body: include_str!("../../content/jobs/trust-and-safety-specialist.md"),
    },
    JobListing {
        slug: "privacy-and-legal-counsel",
        title: CONTENT_JOBS_PRIVACY_AND_LEGAL_COUNSEL_TITLE_DESCRIPTOR,
        department: CONTENT_LABEL_LEGAL_DESCRIPTOR,
        location: CONTENT_LABEL_REMOTE_DESCRIPTOR,
        employment_type: CONTENT_LABEL_FULL_TIME_DESCRIPTOR,
        description: CONTENT_JOBS_PRIVACY_AND_LEGAL_COUNSEL_DESCRIPTION_DESCRIPTOR,
        posted_date: "2026-03-01",
        body: include_str!("../../content/jobs/privacy-and-legal-counsel.md"),
    },
];

pub const BLOG_TAGS: &[&str] = &["News"];

pub const BLOG_POSTS: &[BlogPost] = &[
    BlogPost {
        slug: "mobile-clients-and-fluxer-v2",
        title: BLOG_POST_MOBILE_CLIENTS_AND_FLUXER_V2_TITLE_DESCRIPTOR,
        description: BLOG_POST_MOBILE_CLIENTS_AND_FLUXER_V2_DESCRIPTION_DESCRIPTOR,
        author: "Hampus Kraft",
        published_at: "2026-06-15T12:00:00Z",
        updated_at: "2026-06-15T12:00:00Z",
        tags: &["News"],
        feature_image_path: "/blog/assets/mobile-clients-and-fluxer-v2-feature-image-1280.jpg",
        feature_image_base_path: "/blog/assets/mobile-clients-and-fluxer-v2-feature-image",
        feature_image_alt: BLOG_POST_MOBILE_CLIENTS_AND_FLUXER_V2_TITLE_DESCRIPTOR,
        feature_image_placeholder: "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEASABIAAD/2wBDABIMDRANCxIQDhAUExIVGywdGxgYGzYnKSAsQDlEQz85Pj1HUGZXR0thTT0+WXlaYWltcnNyRVV9hnxvhWZwcm7/2wBDARMUFBsXGzQdHTRuST5Jbm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm7/wAARCAASACADAREAAhEBAxEB/8QAGAAAAwEBAAAAAAAAAAAAAAAAAAEEAwX/xAAjEAABBAEDBAMAAAAAAAAAAAABAAIDERIEISIFMUGRUsHR/8QAFwEBAQEBAAAAAAAAAAAAAAAAAAMBBf/EABgRAQEAAwAAAAAAAAAAAAAAAAABAhES/9oADAMBAAIRAxEAPwDkBxJoAknwF3Okz51eDvRTqA5fF3op0EXEGjsR4KdDKHUOhlbIysmGxYsKFu5pq6LquvnzbHi/ichiO3b7U7jhGlJ1XXRFrJMWlvJoLBttX6kxxoim1DppXyPIyebNbKkupqMTqbQgEAg//9k=",
        source_url: "https://fluxer.app/blog/mobile-clients-and-fluxer-v2",
        body: include_str!("../../content/blog/mobile-clients-and-fluxer-v2.mdx"),
    },
    BlogPost {
        slug: "roadmap-2026",
        title: BLOG_POST_ROADMAP_2026_TITLE_DESCRIPTOR,
        description: BLOG_POST_ROADMAP_2026_DESCRIPTION_DESCRIPTOR,
        author: "Hampus Kraft",
        published_at: "2026-01-26T12:49:48Z",
        updated_at: "2026-05-20T23:55:00Z",
        tags: &["News"],
        feature_image_path: "/blog/assets/roadmap-2026-feature-image-1280.jpg",
        feature_image_base_path: "/blog/assets/roadmap-2026-feature-image",
        feature_image_alt: BLOG_POST_ROADMAP_2026_TITLE_DESCRIPTOR,
        feature_image_placeholder: "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQIAOAA4AAD/2wBDABIMDRANCxIQDhAUExIVGywdGxgYGzYnKSAsQDlEQz85Pj1HUGZXR0thTT0+WXlaYWltcnNyRVV9hnxvhWZwcm7/2wBDARMUFBsXGzQdHTRuST5Jbm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm7/wAARCAASACADASIAAhEBAxEB/8QAGQAAAwEBAQAAAAAAAAAAAAAAAAIDAQQF/8QAGRABAQADAQAAAAAAAAAAAAAAAAECAxES/8QAGAEBAAMBAAAAAAAAAAAAAAAAAAECAwX/xAAXEQEAAwAAAAAAAAAAAAAAAAAAAQMT/9oADAMBAAIRAxEAPwDy8Z1vlHDbxSbo6+ih/Jcpxl3RPPb0iwc0NAGSRS0AH//Z",
        source_url: "https://fluxer.app/blog/roadmap-2026",
        body: include_str!("../../content/blog/roadmap-2026.mdx"),
    },
    BlogPost {
        slug: "how-i-built-fluxer-a-discord-like-chat-app",
        title: BLOG_POST_HOW_I_BUILT_FLUXER_TITLE_DESCRIPTOR,
        description: PRODUCT_POSITIONING_INTRO_DESCRIPTOR,
        author: "Hampus Kraft",
        published_at: "2026-01-24T14:00:00Z",
        updated_at: "2026-05-20T23:55:00Z",
        tags: &["News"],
        feature_image_path: "/blog/assets/how-i-built-fluxer-cover-1280.jpg",
        feature_image_base_path: "/blog/assets/how-i-built-fluxer-cover",
        feature_image_alt: BLOG_POST_HOW_I_BUILT_FLUXER_TITLE_DESCRIPTOR,
        feature_image_placeholder: "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQIAOAA4AAD/2wBDABIMDRANCxIQDhAUExIVGywdGxgYGzYnKSAsQDlEQz85Pj1HUGZXR0thTT0+WXlaYWltcnNyRVV9hnxvhWZwcm7/2wBDARMUFBsXGzQdHTRuST5Jbm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm5ubm7/wAARCAASACADASIAAhEBAxEB/8QAGQAAAgMBAAAAAAAAAAAAAAAAAAMBAgQF/8QAGRABAQEBAQEAAAAAAAAAAAAAAAECAxET/8QAGAEAAgMAAAAAAAAAAAAAAAAAAAECAwX/xAAZEQADAAMAAAAAAAAAAAAAAAAAAQIDITH/2gAMAwEAAhEDEQA/AOPrfin2R0lpNxWlkuk9EUjROxmd+sczT+cp4rpvYhmlKAsvoEL5AEdA/9k=",
        source_url: "https://fluxer.app/blog/how-i-built-fluxer-a-discord-like-chat-app",
        body: include_str!("../../content/blog/how-i-built-fluxer-a-discord-like-chat-app.mdx"),
    },
];

#[derive(Clone, Copy, Debug)]
pub enum BlogBookmarkAsset {
    BskyMississippiThumb,
    CefRsThumb,
    DiscordAgeVerificationThumb,
    DiscordForumsThumb,
    DiscordIpoThumb,
    FluxerIcon,
    FluxerStatusIcon,
    FluxerStatusThumb,
    GithubFluxerThumb,
    GithubIcon,
    HnIcon,
    HowBuiltThumb,
    PatreonChatThumb,
    RoadmapThumb,
    TechcrunchIcon,
    ThevergeIcon,
}

impl BlogBookmarkAsset {
    pub fn from_file(file: &str) -> Option<Self> {
        match file {
            "bookmark-bsky-mississippi-thumb.jpg" => Some(Self::BskyMississippiThumb),
            "bookmark-cef-rs-thumb.jpg" => Some(Self::CefRsThumb),
            "bookmark-discord-age-verification-thumb.jpg" => {
                Some(Self::DiscordAgeVerificationThumb)
            }
            "bookmark-discord-forums-thumb.jpg" => Some(Self::DiscordForumsThumb),
            "bookmark-discord-ipo-thumb.jpg" => Some(Self::DiscordIpoThumb),
            "bookmark-fluxer-icon.jpg" => Some(Self::FluxerIcon),
            "bookmark-fluxer-status-icon.jpg" => Some(Self::FluxerStatusIcon),
            "bookmark-fluxer-status-thumb.jpg" => Some(Self::FluxerStatusThumb),
            "bookmark-github-fluxer-thumb.jpg" => Some(Self::GithubFluxerThumb),
            "bookmark-github-icon.svg" => Some(Self::GithubIcon),
            "bookmark-hn-icon.svg" => Some(Self::HnIcon),
            "bookmark-how-built-thumb.jpg" => Some(Self::HowBuiltThumb),
            "bookmark-patreon-chat-thumb.jpg" => Some(Self::PatreonChatThumb),
            "bookmark-roadmap-thumb.jpg" => Some(Self::RoadmapThumb),
            "bookmark-techcrunch-icon.jpg" => Some(Self::TechcrunchIcon),
            "bookmark-theverge-icon.jpg" => Some(Self::ThevergeIcon),
            _ => None,
        }
    }

    pub const fn path(self) -> &'static str {
        match self {
            Self::BskyMississippiThumb => "/blog/assets/bookmark-bsky-mississippi-thumb.jpg",
            Self::CefRsThumb => "/blog/assets/bookmark-cef-rs-thumb.jpg",
            Self::DiscordAgeVerificationThumb => {
                "/blog/assets/bookmark-discord-age-verification-thumb.jpg"
            }
            Self::DiscordForumsThumb => "/blog/assets/bookmark-discord-forums-thumb.jpg",
            Self::DiscordIpoThumb => "/blog/assets/bookmark-discord-ipo-thumb.jpg",
            Self::FluxerIcon => "/blog/assets/bookmark-fluxer-icon.jpg",
            Self::FluxerStatusIcon => "/blog/assets/bookmark-fluxer-status-icon.jpg",
            Self::FluxerStatusThumb => "/blog/assets/bookmark-fluxer-status-thumb.jpg",
            Self::GithubFluxerThumb => "/blog/assets/bookmark-github-fluxer-thumb.jpg",
            Self::GithubIcon => "/blog/assets/bookmark-github-icon.svg",
            Self::HnIcon => "/blog/assets/bookmark-hn-icon.svg",
            Self::HowBuiltThumb => "/blog/assets/bookmark-how-built-thumb.jpg",
            Self::PatreonChatThumb => "/blog/assets/bookmark-patreon-chat-thumb.jpg",
            Self::RoadmapThumb => "/blog/assets/bookmark-roadmap-thumb.jpg",
            Self::TechcrunchIcon => "/blog/assets/bookmark-techcrunch-icon.jpg",
            Self::ThevergeIcon => "/blog/assets/bookmark-theverge-icon.jpg",
        }
    }

    pub const fn content_type(self) -> &'static str {
        match self {
            Self::GithubIcon | Self::HnIcon => "image/svg+xml",
            _ => "image/jpeg",
        }
    }

    pub const fn bytes(self) -> &'static [u8] {
        match self {
            Self::BskyMississippiThumb => {
                include_bytes!("../../static/blog/bookmark-bsky-mississippi-thumb.jpg")
            }
            Self::CefRsThumb => include_bytes!("../../static/blog/bookmark-cef-rs-thumb.jpg"),
            Self::DiscordAgeVerificationThumb => {
                include_bytes!("../../static/blog/bookmark-discord-age-verification-thumb.jpg")
            }
            Self::DiscordForumsThumb => {
                include_bytes!("../../static/blog/bookmark-discord-forums-thumb.jpg")
            }
            Self::DiscordIpoThumb => {
                include_bytes!("../../static/blog/bookmark-discord-ipo-thumb.jpg")
            }
            Self::FluxerIcon => include_bytes!("../../static/blog/bookmark-fluxer-icon.jpg"),
            Self::FluxerStatusIcon => {
                include_bytes!("../../static/blog/bookmark-fluxer-status-icon.jpg")
            }
            Self::FluxerStatusThumb => {
                include_bytes!("../../static/blog/bookmark-fluxer-status-thumb.jpg")
            }
            Self::GithubFluxerThumb => {
                include_bytes!("../../static/blog/bookmark-github-fluxer-thumb.jpg")
            }
            Self::GithubIcon => include_bytes!("../../static/blog/bookmark-github-icon.svg"),
            Self::HnIcon => include_bytes!("../../static/blog/bookmark-hn-icon.svg"),
            Self::HowBuiltThumb => include_bytes!("../../static/blog/bookmark-how-built-thumb.jpg"),
            Self::PatreonChatThumb => {
                include_bytes!("../../static/blog/bookmark-patreon-chat-thumb.jpg")
            }
            Self::RoadmapThumb => include_bytes!("../../static/blog/bookmark-roadmap-thumb.jpg"),
            Self::TechcrunchIcon => {
                include_bytes!("../../static/blog/bookmark-techcrunch-icon.jpg")
            }
            Self::ThevergeIcon => include_bytes!("../../static/blog/bookmark-theverge-icon.jpg"),
        }
    }
}

pub const POLICIES: &[Policy] = &[
    Policy {
        slug: "terms",
        title: CONTENT_POLICIES_TERMS_TITLE_DESCRIPTOR,
        description: CONTENT_POLICIES_TERMS_DESCRIPTION_DESCRIPTOR,
        category: CONTENT_LABEL_LEGAL_DESCRIPTOR,
        last_updated: Some("2026-03-10"),
        body: include_str!("../../content/policies/terms.md"),
    },
    Policy {
        slug: "privacy",
        title: CONTENT_POLICIES_PRIVACY_TITLE_DESCRIPTOR,
        description: CONTENT_POLICIES_PRIVACY_DESCRIPTION_DESCRIPTOR,
        category: CONTENT_LABEL_LEGAL_DESCRIPTOR,
        last_updated: Some("2026-04-18"),
        body: include_str!("../../content/policies/privacy.md"),
    },
    Policy {
        slug: "guidelines",
        title: CONTENT_POLICIES_GUIDELINES_TITLE_DESCRIPTOR,
        description: CONTENT_POLICIES_GUIDELINES_DESCRIPTION_DESCRIPTOR,
        category: CONTENT_LABEL_COMMUNITY_DESCRIPTOR,
        last_updated: Some("2026-03-10"),
        body: include_str!("../../content/policies/guidelines.md"),
    },
    Policy {
        slug: "security",
        title: CONTENT_POLICIES_SECURITY_TITLE_DESCRIPTOR,
        description: CONTENT_POLICIES_SECURITY_DESCRIPTION_DESCRIPTOR,
        category: CONTENT_LABEL_SECURITY_DESCRIPTOR,
        last_updated: None,
        body: include_str!("../../content/policies/security.md"),
    },
    Policy {
        slug: "company-information",
        title: CONTENT_POLICIES_COMPANY_INFORMATION_TITLE_DESCRIPTOR,
        description: CONTENT_POLICIES_COMPANY_INFORMATION_DESCRIPTION_DESCRIPTOR,
        category: CONTENT_LABEL_LEGAL_DESCRIPTOR,
        last_updated: None,
        body: include_str!("../../content/policies/company-information.md"),
    },
    Policy {
        slug: "changelog",
        title: CONTENT_POLICIES_CHANGELOG_TITLE_DESCRIPTOR,
        description: CONTENT_POLICIES_CHANGELOG_DESCRIPTION_DESCRIPTOR,
        category: CONTENT_LABEL_LEGAL_DESCRIPTOR,
        last_updated: Some("2026-04-18"),
        body: include_str!("../../content/policies/changelog.md"),
    },
];

pub const HELP_CATEGORIES: &[HelpCategory] = &[
    HelpCategory {
        slug: "premium",
        title: CONTENT_HELP_CATEGORY_PREMIUM_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_CATEGORY_PREMIUM_DESCRIPTION_DESCRIPTOR,
    },
    HelpCategory {
        slug: "faqs",
        title: CONTENT_HELP_CATEGORY_FAQS_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_CATEGORY_FAQS_DESCRIPTION_DESCRIPTOR,
    },
    HelpCategory {
        slug: "account",
        title: CONTENT_HELP_CATEGORY_ACCOUNT_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_CATEGORY_ACCOUNT_DESCRIPTION_DESCRIPTOR,
    },
    HelpCategory {
        slug: "legal-policy",
        title: CONTENT_HELP_CATEGORY_LEGAL_POLICY_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_CATEGORY_LEGAL_POLICY_DESCRIPTION_DESCRIPTOR,
    },
];

pub const HELP_ARTICLES: &[HelpArticle] = &[
    HelpArticle {
        slug: "plutonium-promotion-march-2026",
        aliases: &[
            "march-2026-plutonium-promotion",
            "13984954-march-2026-plutonium-promotion",
        ],
        title: CONTENT_HELP_ARTICLE_PLUTONIUM_PROMOTION_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_PLUTONIUM_PROMOTION_DESCRIPTION_DESCRIPTOR,
        category_slug: "premium",
        last_updated: "2026-04-04",
        body: include_str!("../../content/help/premium/plutonium-promotion-march-2026.md"),
    },
    HelpArticle {
        slug: "visionary",
        aliases: &[
            "what-was-fluxer-visionary",
            "13985047-what-was-fluxer-visionary",
        ],
        title: CONTENT_HELP_ARTICLE_VISIONARY_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_VISIONARY_DESCRIPTION_DESCRIPTOR,
        category_slug: "premium",
        last_updated: "2026-03-30",
        body: include_str!("../../content/help/premium/visionary.md"),
    },
    HelpArticle {
        slug: "community-programmes",
        aliases: &["community-programs", "partners", "partner-program"],
        title: CONTENT_HELP_ARTICLE_COMMUNITY_PROGRAMMES_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_COMMUNITY_PROGRAMMES_DESCRIPTION_DESCRIPTOR,
        category_slug: "faqs",
        last_updated: "2026-07-27",
        body: include_str!("../../content/help/faqs/community-programmes.md"),
    },
    HelpArticle {
        slug: "attachment-expiry",
        aliases: &[
            "how-attachment-expiry-works",
            "13984638-how-attachment-expiry-works",
        ],
        title: CONTENT_HELP_ARTICLE_ATTACHMENT_EXPIRY_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_ATTACHMENT_EXPIRY_DESCRIPTION_DESCRIPTOR,
        category_slug: "faqs",
        last_updated: "2026-03-12",
        body: include_str!("../../content/help/faqs/attachment-expiry.md"),
    },
    HelpArticle {
        slug: "report-bug",
        aliases: &["reporting-a-bug", "13984986-reporting-a-bug"],
        title: CONTENT_HELP_ARTICLE_REPORT_BUG_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_REPORT_BUG_DESCRIPTION_DESCRIPTOR,
        category_slug: "faqs",
        last_updated: "2026-03-07",
        body: include_str!("../../content/help/faqs/report-bug.md"),
    },
    HelpArticle {
        slug: "change-date-of-birth",
        aliases: &[
            "how-to-change-your-date-of-birth",
            "13984710-how-to-change-your-date-of-birth",
        ],
        title: CONTENT_HELP_ARTICLE_CHANGE_DATE_OF_BIRTH_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_CHANGE_DATE_OF_BIRTH_DESCRIPTION_DESCRIPTOR,
        category_slug: "account",
        last_updated: "2026-03-12",
        body: include_str!("../../content/help/account/change-date-of-birth.md"),
    },
    HelpArticle {
        slug: "data-deletion",
        aliases: &[
            "requesting-data-deletion",
            "13984741-requesting-data-deletion",
        ],
        title: CONTENT_HELP_ARTICLE_DATA_DELETION_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_DATA_DELETION_DESCRIPTION_DESCRIPTOR,
        category_slug: "account",
        last_updated: "2026-03-12",
        body: include_str!("../../content/help/account/data-deletion.md"),
    },
    HelpArticle {
        slug: "data-export",
        aliases: &[
            "exporting-your-account-data",
            "13984751-exporting-your-account-data",
        ],
        title: CONTENT_HELP_ARTICLE_DATA_EXPORT_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_DATA_EXPORT_DESCRIPTION_DESCRIPTOR,
        category_slug: "account",
        last_updated: "2026-03-12",
        body: include_str!("../../content/help/account/data-export.md"),
    },
    HelpArticle {
        slug: "delete-account",
        aliases: &[
            "how-to-delete-or-disable-your-account",
            "13984865-how-to-delete-or-disable-your-account",
        ],
        title: CONTENT_HELP_ARTICLE_DELETE_ACCOUNT_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_DELETE_ACCOUNT_DESCRIPTION_DESCRIPTOR,
        category_slug: "account",
        last_updated: "2026-03-12",
        body: include_str!("../../content/help/account/delete-account.md"),
    },
    HelpArticle {
        slug: "minimum-age",
        aliases: &[
            "minimum-age-requirements",
            "13984933-minimum-age-requirements",
        ],
        title: CONTENT_HELP_ARTICLE_MINIMUM_AGE_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_MINIMUM_AGE_DESCRIPTION_DESCRIPTOR,
        category_slug: "account",
        last_updated: "2026-03-12",
        body: include_str!("../../content/help/account/minimum-age.md"),
    },
    HelpArticle {
        slug: "copyright",
        aliases: &[
            "copyright-and-intellectual-property-complaints-policy",
            "copyright-and-ip-policy",
            "13984728-copyright-and-intellectual-property-complaints-policy",
            "13984728-copyright-and-ip-policy",
        ],
        title: CONTENT_HELP_ARTICLE_COPYRIGHT_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_COPYRIGHT_DESCRIPTION_DESCRIPTOR,
        category_slug: "legal-policy",
        last_updated: "2026-03-12",
        body: include_str!("../../content/help/legal-policy/copyright.md"),
    },
    HelpArticle {
        slug: "data-retention",
        aliases: &[
            "how-long-fluxer-keeps-your-information",
            "13984762-how-long-fluxer-keeps-your-information",
        ],
        title: CONTENT_HELP_ARTICLE_DATA_RETENTION_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_DATA_RETENTION_DESCRIPTION_DESCRIPTOR,
        category_slug: "legal-policy",
        last_updated: "2026-03-17",
        body: include_str!("../../content/help/legal-policy/data-retention.md"),
    },
    HelpArticle {
        slug: "dsa-dispute-resolution",
        aliases: &[
            "eu-dsa-dispute-resolution-options",
            "13984923-eu-dsa-dispute-resolution-options",
        ],
        title: CONTENT_HELP_ARTICLE_DSA_DISPUTE_RESOLUTION_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_DSA_DISPUTE_RESOLUTION_DESCRIPTION_DESCRIPTOR,
        category_slug: "legal-policy",
        last_updated: "2026-03-12",
        body: include_str!("../../content/help/legal-policy/dsa-dispute-resolution.md"),
    },
    HelpArticle {
        slug: "regional-restrictions",
        aliases: &["13984975-regional-restrictions"],
        title: CONTENT_HELP_ARTICLE_REGIONAL_RESTRICTIONS_TITLE_DESCRIPTOR,
        description: CONTENT_HELP_ARTICLE_REGIONAL_RESTRICTIONS_DESCRIPTION_DESCRIPTOR,
        category_slug: "legal-policy",
        last_updated: "2026-03-29",
        body: include_str!("../../content/help/legal-policy/regional-restrictions.md"),
    },
];

pub fn get_job(slug: &str) -> Option<JobListing> {
    JOBS.iter().copied().find(|job| job.slug == slug)
}

pub fn get_policy(slug: &str) -> Option<Policy> {
    POLICIES.iter().copied().find(|policy| policy.slug == slug)
}

pub fn get_help_category(slug: &str) -> Option<HelpCategory> {
    HELP_CATEGORIES
        .iter()
        .copied()
        .find(|category| category.slug == slug)
}

pub fn get_help_article(slug: &str) -> Option<HelpArticle> {
    HELP_ARTICLES
        .iter()
        .copied()
        .find(|article| article.slug == slug || article.aliases.contains(&slug))
}

pub fn get_blog_post(slug: &str) -> Option<BlogPost> {
    let slug = slug.trim_matches('/');
    BLOG_POSTS.iter().copied().find(|post| post.slug == slug)
}

pub fn get_blog_tag(slug: &str) -> Option<&'static str> {
    BLOG_TAGS
        .iter()
        .copied()
        .find(|tag| blog_tag_slug(tag) == slug)
}

pub fn blog_tag_slug(tag: &str) -> String {
    create_slug(tag)
}

pub fn blog_tag_label(i18n: &MarketingI18n, locale: Locale, tag: &str) -> String {
    let descriptor = match tag {
        "News" => BLOG_TAG_NEWS_DESCRIPTOR,
        _ => return tag.to_owned(),
    };
    i18n.text(locale, descriptor)
}

pub fn blog_post_body(markdown: &str) -> &str {
    let Some(rest) = markdown.strip_prefix("---\n") else {
        return markdown;
    };
    let Some((_, body)) = rest.split_once("\n---\n") else {
        return markdown;
    };
    body.trim_start()
}

pub fn render_blog_markdown_with_copy_label(
    markdown: &str,
    base_url: &str,
    copy_link_label: &str,
    linked_article_label: &str,
) -> Markup {
    let body =
        rewrite_blog_bookmark_quotes(blog_post_body(markdown), base_url, linked_article_label);
    render_markdown_with_copy_label(&body, base_url, copy_link_label)
}

pub fn render_markdown(markdown: &str, base_url: &str) -> Markup {
    let (markup, _) = render_markdown_with_headings_and_copy_label(
        markdown,
        base_url,
        NAVIGATION_COPY_LINK_TO_SECTION_DESCRIPTOR.message(),
    );
    markup
}

pub fn render_markdown_with_copy_label(
    markdown: &str,
    base_url: &str,
    copy_link_label: &str,
) -> Markup {
    let (markup, _) =
        render_markdown_with_headings_and_copy_label(markdown, base_url, copy_link_label);
    markup
}

#[derive(Clone, Debug)]
pub struct HeadingEntry {
    pub id: String,
    pub title: String,
    pub level: u32,
}

pub fn render_markdown_with_headings(
    markdown: &str,
    base_url: &str,
) -> (Markup, Vec<HeadingEntry>) {
    render_markdown_with_headings_and_copy_label(
        markdown,
        base_url,
        NAVIGATION_COPY_LINK_TO_SECTION_DESCRIPTOR.message(),
    )
}

#[derive(Clone)]
struct RenderedMarkdown {
    html: String,
    headings: Vec<HeadingEntry>,
}

static MARKDOWN_RENDER_CACHE: LazyLock<moka::sync::Cache<u64, Arc<RenderedMarkdown>>> =
    LazyLock::new(|| moka::sync::Cache::new(2048));

fn render_cache_key(markdown: &str, base_url: &str, copy_link_label: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    markdown.hash(&mut hasher);
    base_url.hash(&mut hasher);
    copy_link_label.hash(&mut hasher);
    hasher.finish()
}

pub fn render_markdown_with_headings_and_copy_label(
    markdown: &str,
    base_url: &str,
    copy_link_label: &str,
) -> (Markup, Vec<HeadingEntry>) {
    let key = render_cache_key(markdown, base_url, copy_link_label);
    let rendered = MARKDOWN_RENDER_CACHE.get_with(key, || {
        Arc::new(render_markdown_uncached(
            markdown,
            base_url,
            copy_link_label,
        ))
    });
    let markup = html! {
        div class="prose-content" {
            (PreEscaped(rendered.html.clone()))
        }
    };
    (markup, rendered.headings.clone())
}

fn render_markdown_uncached(
    markdown: &str,
    base_url: &str,
    copy_link_label: &str,
) -> RenderedMarkdown {
    let arena = Arena::new();
    let options = markdown_options(base_url);
    let root = parse_document(&arena, markdown, &options);
    render_inline_code_as_strong(&arena, root);

    let heading_adapter = FluxerHeadingAdapter::new(copy_link_label);
    let mut plugins = options::Plugins::default();
    plugins.render.heading_adapter = Some(&heading_adapter);

    let mut html_output = String::new();
    format_html_with_plugins(root, &options, &mut html_output, &plugins)
        .expect("rendering markdown HTML into a String should not fail");

    let headings = heading_adapter.headings();
    let html = sanitize_markdown_html(&html_output, base_url);
    RenderedMarkdown { html, headings }
}

fn markdown_options(base_url: &str) -> Options<'static> {
    let mut options = Options::default();
    options.extension.table = true;
    options.extension.strikethrough = true;
    options.extension.tasklist = true;
    options.render.hardbreaks = true;
    options.render.r#unsafe = true;

    let link_base_url = normalized_base_url(base_url);
    options.extension.link_url_rewriter = Some(Arc::new(move |url: &str| {
        prefix_internal_url(&link_base_url, url)
    }));

    let image_base_url = normalized_base_url(base_url);
    options.extension.image_url_rewriter = Some(Arc::new(move |url: &str| {
        prefix_internal_url(&image_base_url, url)
    }));

    options
}

fn rewrite_blog_bookmark_quotes(
    markdown: &str,
    base_url: &str,
    linked_article_label: &str,
) -> String {
    let base_url = normalized_base_url(base_url);
    let mut out = String::with_capacity(markdown.len());
    let mut lines = markdown.lines().peekable();
    while let Some(line) = lines.next() {
        if !line.starts_with('>') {
            out.push_str(line);
            out.push('\n');
            continue;
        }

        let mut quote_lines = vec![line.to_owned()];
        while let Some(next) = lines.peek() {
            if !next.starts_with('>') {
                break;
            }
            quote_lines.push(lines.next().unwrap_or_default().to_owned());
        }

        let mut skipped_blank = None;
        if lines.peek().is_some_and(|next| next.trim().is_empty()) {
            skipped_blank = lines.next().map(str::to_owned);
        }

        let caption = lines
            .peek()
            .and_then(|next| parse_bookmark_caption(next))
            .inspect(|_caption| {
                let _ = lines.next();
            });

        if let Some(card) = blog_bookmark_card_html(
            &quote_lines,
            caption.as_deref(),
            &base_url,
            linked_article_label,
        ) {
            out.push_str(&card);
            out.push('\n');
            if caption.is_none()
                && let Some(blank) = skipped_blank
            {
                out.push_str(&blank);
                out.push('\n');
            }
        } else {
            if let Some(blank) = skipped_blank {
                quote_lines.push(blank);
            }
            for quote_line in quote_lines {
                out.push_str(&quote_line);
                out.push('\n');
            }
        }
    }
    out
}

fn blog_bookmark_card_html(
    quote_lines: &[String],
    caption: Option<&str>,
    base_url: &str,
    linked_article_label: &str,
) -> Option<String> {
    let lines = quote_lines
        .iter()
        .map(|line| line.trim_start_matches('>').trim_start())
        .collect::<Vec<_>>();
    let first = lines.iter().find(|line| !line.trim().is_empty())?.trim();
    let (title, href) = parse_markdown_link(first)?;
    let mut description = Vec::new();
    let mut source = None;
    for line in lines.iter().skip_while(|line| line.trim() != first).skip(1) {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if let Some(rest) = line.strip_prefix("Source:") {
            source = Some(rest.trim());
        } else {
            description.push(line);
        }
    }

    let source = source
        .filter(|value| !value.is_empty())
        .map(ToOwned::to_owned)
        .unwrap_or_else(|| bookmark_source_from_href(&href, linked_article_label));
    let (author, publisher) = split_bookmark_source(&source);
    let description = description.join(" ");
    let media = bookmark_media_for_href(&href);
    let icon_html = media.icon.map_or_else(String::new, |icon| {
        format!(
            r#"<img class="kg-bookmark-icon" src="{}" alt="">"#,
            escape_html_attr(&prefix_internal_url(base_url, icon.path()))
        )
    });
    let publisher_html = publisher.map_or_else(String::new, |publisher| {
        format!(
            r#"<span class="kg-bookmark-publisher">{}</span>"#,
            escape_html(publisher)
        )
    });
    let thumbnail_html = media.thumbnail.map_or_else(String::new, |thumbnail| {
        format!(
            r#"<div class="kg-bookmark-thumbnail"><img src="{}" alt=""></div>"#,
            escape_html_attr(&prefix_internal_url(base_url, thumbnail.path()))
        )
    });
    let caption_html = caption.map_or_else(String::new, |caption| {
        format!(
            r#"<figcaption>{}</figcaption>"#,
            render_basic_inline_html(caption, base_url)
        )
    });
    let href = prefix_internal_url(base_url, &href);
    Some(format!(
        r#"<figure class="kg-card kg-bookmark-card"><a class="kg-bookmark-container" href="{}"><div class="kg-bookmark-content"><div class="kg-bookmark-title">{}</div><div class="kg-bookmark-description">{}</div><div class="kg-bookmark-metadata">{}<span class="kg-bookmark-author">{}</span>{}</div></div>{}</a>{}</figure>"#,
        escape_html_attr(&href),
        escape_html(&title),
        escape_html(&description),
        icon_html,
        escape_html(&author),
        publisher_html,
        thumbnail_html,
        caption_html,
    ))
}

struct BookmarkMedia {
    icon: Option<BlogBookmarkAsset>,
    thumbnail: Option<BlogBookmarkAsset>,
}

fn bookmark_media_for_href(href: &str) -> BookmarkMedia {
    let href = href
        .trim_end_matches('/')
        .trim_start_matches("https://")
        .trim_start_matches("http://");
    if href.contains("theverge.com/tech/875309/discord-age-verification-global-roll-out") {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::ThevergeIcon),
            thumbnail: Some(BlogBookmarkAsset::DiscordAgeVerificationThumb),
        };
    }
    if href.contains("techcrunch.com/2026/01/07/discords-ipo-could-happen-in-march") {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::TechcrunchIcon),
            thumbnail: Some(BlogBookmarkAsset::DiscordIpoThumb),
        };
    }
    if href.contains("github.com/fluxerapp/fluxer") {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::GithubIcon),
            thumbnail: Some(BlogBookmarkAsset::GithubFluxerThumb),
        };
    }
    if href.contains("github.com/fluxerapp/flutter_client") {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::GithubIcon),
            thumbnail: Some(BlogBookmarkAsset::GithubFluxerThumb),
        };
    }
    if href.contains("github.com/tauri-apps/cef-rs") {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::GithubIcon),
            thumbnail: Some(BlogBookmarkAsset::CefRsThumb),
        };
    }
    if href.contains("techcrunch.com/2025/05/23/discord-seeks-to-solve-a-problem-that-it-created") {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::TechcrunchIcon),
            thumbnail: Some(BlogBookmarkAsset::DiscordForumsThumb),
        };
    }
    if href.contains("news.ycombinator.com/item?id=46376201") {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::HnIcon),
            thumbnail: Some(BlogBookmarkAsset::HnIcon),
        };
    }
    if href.contains(
        "theverge.com/internet-censorship/764697/bluesky-blocks-mississippi-age-verification-law",
    ) {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::ThevergeIcon),
            thumbnail: Some(BlogBookmarkAsset::BskyMississippiThumb),
        };
    }
    if href.contains("blog/roadmap-2026") {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::FluxerIcon),
            thumbnail: Some(BlogBookmarkAsset::RoadmapThumb),
        };
    }
    if href.contains("blog/how-i-built-fluxer-a-discord-like-chat-app") {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::FluxerIcon),
            thumbnail: Some(BlogBookmarkAsset::HowBuiltThumb),
        };
    }
    if href.contains("fluxerstatus.com/cmpiwlw5e057vpbi74zh7ohmh") {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::FluxerStatusIcon),
            thumbnail: Some(BlogBookmarkAsset::FluxerStatusThumb),
        };
    }
    if href.contains(
        "theverge.com/2023/9/7/23861171/patreon-community-chats-discord-chatroom-member-profile",
    ) {
        return BookmarkMedia {
            icon: Some(BlogBookmarkAsset::ThevergeIcon),
            thumbnail: Some(BlogBookmarkAsset::PatreonChatThumb),
        };
    }
    BookmarkMedia {
        icon: None,
        thumbnail: None,
    }
}

fn split_bookmark_source(source: &str) -> (String, Option<&str>) {
    source
        .split_once(" / ")
        .map(|(author, publisher)| (author.to_owned(), Some(publisher)))
        .unwrap_or_else(|| (source.to_owned(), None))
}

fn parse_bookmark_caption(line: &str) -> Option<String> {
    let trimmed = line.trim();
    let caption = trimmed.strip_prefix('*')?.strip_suffix('*')?;
    (!caption.starts_with('*') && !caption.ends_with('*') && !caption.is_empty())
        .then(|| caption.to_owned())
}

fn render_basic_inline_html(value: &str, base_url: &str) -> String {
    let mut out = String::new();
    let mut rest = value;
    while let Some(start) = rest.find('[') {
        out.push_str(&escape_html(&rest[..start]));
        let after_start = &rest[start + 1..];
        let Some((label, after_label)) = after_start.split_once("](") else {
            out.push_str(&escape_html(&rest[start..]));
            return out;
        };
        let Some(end) = after_label.find(')') else {
            out.push_str(&escape_html(&rest[start..]));
            return out;
        };
        let href = &after_label[..end];
        out.push_str(&format!(
            r#"<a href="{}">{}</a>"#,
            escape_html_attr(&prefix_internal_url(base_url, href)),
            escape_html(label)
        ));
        rest = &after_label[end + 1..];
    }
    out.push_str(&escape_html(rest));
    out
}

fn parse_markdown_link(line: &str) -> Option<(String, String)> {
    let rest = line.strip_prefix('[')?;
    let (title, rest) = rest.split_once("](")?;
    let href = rest.strip_suffix(')')?;
    if title.is_empty() || href.is_empty() {
        return None;
    }
    Some((title.to_owned(), href.to_owned()))
}

fn bookmark_source_from_href(href: &str, fallback_label: &str) -> String {
    href.strip_prefix("https://")
        .or_else(|| href.strip_prefix("http://"))
        .and_then(|rest| rest.split('/').next())
        .filter(|host| !host.is_empty())
        .unwrap_or(fallback_label)
        .to_owned()
}

fn escape_html(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn escape_html_attr(value: &str) -> String {
    escape_html(value).replace('\'', "&#39;")
}

static MARKDOWN_SANITIZER: LazyLock<Builder<'static>> = LazyLock::new(build_markdown_sanitizer);

fn build_markdown_sanitizer() -> Builder<'static> {
    let mut builder = Builder::default();
    configure_markdown_sanitizer(&mut builder);
    builder
}

fn sanitize_markdown_html(html: &str, base_url: &str) -> String {
    let base = normalized_base_url(base_url);
    if base.is_empty() {
        return MARKDOWN_SANITIZER.clean(html).to_string();
    }
    let mut builder = Builder::default();
    configure_markdown_sanitizer(&mut builder);
    builder.attribute_filter(move |_element, attribute, value| match attribute {
        "src" | "poster" | "href" => Some(Cow::Owned(prefix_internal_url(&base, value))),
        "srcset" => Some(Cow::Owned(prefix_internal_srcset(&base, value))),
        _ => Some(Cow::Borrowed(value)),
    });
    builder.clean(html).to_string()
}

fn configure_markdown_sanitizer(builder: &mut Builder<'_>) {
    builder
        .add_tags([
            "br",
            "table",
            "thead",
            "tbody",
            "tr",
            "th",
            "td",
            "pre",
            "code",
            "button",
            "div",
            "span",
            "svg",
            "path",
            "polyline",
            "figure",
            "figcaption",
            "aside",
            "video",
            "source",
        ])
        .add_generic_attributes(["class", "id"])
        .add_tag_attributes("a", ["target", "rel", "aria-label"])
        .add_tag_attributes(
            "img",
            [
                "src", "srcset", "sizes", "alt", "width", "height", "loading", "decoding",
            ],
        )
        .add_tag_attributes("button", ["type", "data-anchor-link", "aria-label"])
        .add_tag_attributes(
            "video",
            [
                "src",
                "poster",
                "controls",
                "autoplay",
                "loop",
                "muted",
                "playsinline",
                "preload",
                "width",
                "height",
            ],
        )
        .add_tag_attributes("source", ["src", "type", "srcset", "sizes", "media"])
        .add_tag_attributes(
            "svg",
            [
                "xmlns",
                "viewBox",
                "viewbox",
                "fill",
                "stroke",
                "stroke-linecap",
                "stroke-linejoin",
                "stroke-width",
            ],
        )
        .add_tag_attributes("path", ["d"])
        .add_tag_attributes("polyline", ["points"])
        .link_rel(None);

    for tag in ["h1", "h2", "h3", "h4", "h5", "h6"] {
        builder.add_tag_attributes(tag, ["style"]);
    }
}

fn normalized_base_url(base_url: &str) -> String {
    base_url.trim_end_matches('/').to_owned()
}

fn prefix_internal_url(base_url: &str, url: &str) -> String {
    if base_url.is_empty()
        || !url.starts_with('/')
        || url.starts_with("//")
        || url == base_url
        || url.starts_with(&format!("{base_url}/"))
    {
        return url.to_owned();
    }
    format!("{base_url}{url}")
}

fn prefix_internal_srcset(base_url: &str, srcset: &str) -> String {
    srcset
        .split(',')
        .filter_map(|candidate| {
            let candidate = candidate.trim();
            if candidate.is_empty() {
                return None;
            }
            let mut parts = candidate.splitn(2, char::is_whitespace);
            let url = prefix_internal_url(base_url, parts.next().unwrap_or_default());
            match parts.next().map(str::trim).filter(|d| !d.is_empty()) {
                Some(descriptor) => Some(format!("{url} {descriptor}")),
                None => Some(url),
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_inline_code_as_strong<'a>(arena: &'a Arena<'a>, root: Node<'a>) {
    let code_nodes: Vec<Node<'a>> = root
        .descendants()
        .filter(|node| matches!(&node.data.borrow().value, NodeValue::Code(_)))
        .collect();

    for node in code_nodes {
        let literal = {
            let mut data = node.data.borrow_mut();
            let NodeValue::Code(code) = std::mem::replace(&mut data.value, NodeValue::Strong)
            else {
                continue;
            };
            code.literal
        };
        let text_node = arena.alloc(NodeValue::Text(literal.into()).into());
        node.append(text_node);
    }
}

fn create_slug(text: &str) -> String {
    let lower = text.to_lowercase();
    let mut out = String::with_capacity(lower.len());
    let mut last_dash = false;
    for ch in lower.chars() {
        if ch.is_alphanumeric() {
            out.push(ch);
            last_dash = false;
        } else if !last_dash {
            out.push('-');
            last_dash = true;
        }
    }
    out.trim_matches('-').to_owned()
}

#[derive(Debug)]
struct FluxerHeadingAdapter {
    copy_link_label: String,
    state: Mutex<HeadingRenderState>,
}

impl FluxerHeadingAdapter {
    fn new(copy_link_label: &str) -> Self {
        Self {
            copy_link_label: copy_link_label.to_owned(),
            state: Mutex::new(HeadingRenderState::default()),
        }
    }

    fn headings(&self) -> Vec<HeadingEntry> {
        self.state
            .lock()
            .expect("heading render state should not be poisoned")
            .headings
            .clone()
    }
}

impl HeadingAdapter for FluxerHeadingAdapter {
    fn enter(
        &self,
        output: &mut dyn fmt::Write,
        heading: &HeadingMeta,
        _sourcepos: Option<Sourcepos>,
    ) -> fmt::Result {
        let id = self
            .state
            .lock()
            .map_err(|_| fmt::Error)?
            .push_heading(heading);
        write!(
            output,
            r#"<h{} id="{}" class="heading-anchor-container" style="scroll-margin-top: var(--anchor-offset, 200px)">"#,
            heading.level,
            escape_html_attr(&id),
        )
    }

    fn exit(&self, output: &mut dyn fmt::Write, heading: &HeadingMeta) -> fmt::Result {
        let id = self
            .state
            .lock()
            .map_err(|_| fmt::Error)?
            .active_ids
            .pop()
            .ok_or(fmt::Error)?;
        write!(
            output,
            "{}</h{}>",
            heading_anchor_button(&id, &self.copy_link_label),
            heading.level,
        )
    }
}

#[derive(Default, Debug)]
struct HeadingRenderState {
    slug_counts: HashMap<String, usize>,
    active_ids: Vec<String>,
    headings: Vec<HeadingEntry>,
}

impl HeadingRenderState {
    fn push_heading(&mut self, heading: &HeadingMeta) -> String {
        let (title, custom_id) = parse_heading_metadata(&heading.content);
        let base_slug = custom_id.unwrap_or_else(|| create_slug(&title));
        let count = self.slug_counts.entry(base_slug.clone()).or_insert(0);
        *count += 1;
        let id = if *count == 1 {
            base_slug
        } else {
            format!("{}-{}", base_slug, count)
        };

        self.active_ids.push(id.clone());
        self.headings.push(HeadingEntry {
            id: id.clone(),
            title,
            level: u32::from(heading.level),
        });

        id
    }
}

fn parse_heading_metadata(content: &str) -> (String, Option<String>) {
    let trimmed = content.trim();
    match (trimmed.rfind("{#"), trimmed.ends_with('}')) {
        (Some(start), true) => {
            let id_part = &trimmed[start + 2..trimmed.len() - 1];
            if id_part.is_empty() {
                (trimmed.to_owned(), None)
            } else {
                (trimmed[..start].trim().to_owned(), Some(id_part.to_owned()))
            }
        }
        _ => (trimmed.to_owned(), None),
    }
}

fn heading_anchor_button(slug: &str, copy_link_label: &str) -> String {
    format!(
        r#"<button type="button" class="heading-anchor-link" data-anchor-link="{}" aria-label="{}"><span class="link-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" fill="currentColor" class="w-4 h-4"><path d="M137.54,186.36a8,8,0,0,1,0,11.31l-9.94,10A56,56,0,0,1,48.38,128.4L72.5,104.28A56,56,0,0,1,149.31,102a8,8,0,1,1-10.64,12,40,40,0,0,0-54.85,1.63L59.7,139.72a40,40,0,0,0,56.58,56.58l9.94-9.94A8,8,0,0,1,137.54,186.36Zm70.08-138a56.06,56.06,0,0,0-79.22,0l-9.94,9.95a8,8,0,0,0,11.32,11.31l9.94-9.94a40,40,0,0,1,56.58,56.58L172.18,140.4A40,40,0,0,1,117.33,142,8,8,0,1,0,106.69,154a56,56,0,0,0,76.81-2.26l24.12-24.12A56.06,56.06,0,0,0,207.62,48.38Z"/></svg></span><span class="check-icon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="24" class="w-4 h-4"><polyline points="40,144 96,200 224,72"/></svg></span></button>"#,
        escape_html_attr(slug),
        escape_html_attr(copy_link_label),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rendered(markdown: &str) -> String {
        render_markdown(markdown, "/en").0
    }

    #[test]
    fn external_links_match_ts_without_forced_new_window() {
        let html = rendered("See [Site](https://example.com) please.");
        assert!(
            html.contains(r#"href="https://example.com""#),
            "href preserved: {html}"
        );
        assert!(
            !html.contains(r#"target="_blank""#),
            "no target added: {html}"
        );
        assert!(
            !html.contains(r#"rel="noopener noreferrer""#),
            "no rel added: {html}"
        );
    }

    #[test]
    fn internal_links_are_locale_prefixed_and_stay_in_window() {
        let html = rendered("See [Privacy](/privacy) for details.");
        assert!(html.contains(r#"href="/en/privacy""#), "rewritten: {html}");
        assert!(!html.contains(r#"target="_blank""#), "no target: {html}");
    }

    #[test]
    fn raw_html_embed_urls_get_base_path_prefixed() {
        let html = render_markdown(
            "<figure>\n\
             <video poster=\"/blog/assets/clip-poster.jpg\"><source src=\"/blog/assets/clip.webm\" /></video>\n\
             <img src=\"/blog/assets/still.jpg\" srcset=\"/blog/assets/still-640.jpg 640w, /blog/assets/still-960.jpg 960w\" />\n\
             <a href=\"/blog/roadmap-2026\">more</a>\n\
             </figure>\n",
            "/marketing",
        )
        .0;
        assert!(
            html.contains(r#"poster="/marketing/blog/assets/clip-poster.jpg""#),
            "poster prefixed: {html}"
        );
        assert!(
            html.contains(r#"src="/marketing/blog/assets/clip.webm""#),
            "source src prefixed: {html}"
        );
        assert!(
            html.contains(r#"src="/marketing/blog/assets/still.jpg""#),
            "img src prefixed: {html}"
        );
        assert!(
            html.contains("/marketing/blog/assets/still-640.jpg 640w"),
            "srcset prefixed: {html}"
        );
        assert!(
            html.contains(r#"href="/marketing/blog/roadmap-2026""#),
            "href prefixed: {html}"
        );
        assert!(
            !html.contains(r#"poster="/blog/assets/clip-poster.jpg""#),
            "no unprefixed poster: {html}"
        );
    }

    #[test]
    fn base_path_prefix_does_not_double_apply_to_markdown_images() {
        let html = render_markdown("![cover](/blog/assets/cover.jpg)", "/marketing").0;
        assert!(
            html.contains(r#"src="/marketing/blog/assets/cover.jpg""#),
            "prefixed once: {html}"
        );
        assert!(
            !html.contains("/marketing/marketing"),
            "not double-prefixed: {html}"
        );
    }

    #[test]
    fn empty_base_path_leaves_raw_html_urls_unchanged() {
        let html = render_markdown(
            "<figure>\n<video poster=\"/blog/assets/clip-poster.jpg\"></video>\n</figure>\n",
            "",
        )
        .0;
        assert!(
            html.contains(r#"poster="/blog/assets/clip-poster.jpg""#),
            "unchanged: {html}"
        );
        assert!(!html.contains("/marketing"), "no spurious prefix: {html}");
    }

    #[test]
    fn autolinked_emails_become_mailto_anchors() {
        let html = rendered("Email <support@fluxer.app> today.");
        assert!(
            html.contains(r#"href="mailto:support@fluxer.app""#),
            "mailto: {html}"
        );
        assert!(
            !html.contains(r#"target="_blank""#),
            "mailto stays in-window: {html}"
        );
    }

    #[test]
    fn paragraph_soft_breaks_render_as_visual_line_breaks() {
        let html = rendered("**Support:** <support@fluxer.app>\n**Privacy:** <privacy@fluxer.app>");
        assert!(
            html.contains(
                r#"<strong>Support:</strong> <a href="mailto:support@fluxer.app">support@fluxer.app</a><br>"#
            ),
            "support line break: {html}"
        );
        assert!(
            html.contains(
                r#"<strong>Privacy:</strong> <a href="mailto:privacy@fluxer.app">privacy@fluxer.app</a>"#
            ),
            "privacy line: {html}"
        );
    }

    #[test]
    fn tables_render_with_thead_tbody() {
        let html = rendered("| a | b |\n|---|---|\n| 1 | 2 |\n");
        assert!(html.contains("<table>"), "table: {html}");
        assert!(html.contains("<thead>"), "thead: {html}");
        assert!(html.contains("<tbody>"), "tbody: {html}");
        assert!(html.contains("<th>a</th>"), "th: {html}");
        assert!(html.contains("<td>1</td>"), "td: {html}");
    }

    #[test]
    fn inline_code_renders_as_strong_like_ts_renderer() {
        let html = rendered("Use `locale` here.");
        assert!(
            html.contains("<strong>locale</strong>"),
            "inline strong: {html}"
        );
        assert!(
            !html.contains("<code>locale</code>"),
            "no inline code: {html}"
        );
    }

    #[test]
    fn headings_get_slug_ids() {
        let (markup, headings) =
            render_markdown_with_headings_and_copy_label("## My Section\n", "/en", "Copy section");
        let html = markup.0;
        assert_eq!(headings.len(), 1);
        assert_eq!(headings[0].id, "my-section");
        assert_eq!(headings[0].level, 2);
        assert!(
            html.contains(r#"<h2 id="my-section""#),
            "h2 id injected: {html}"
        );
        assert!(
            html.contains(r#"class="heading-anchor-container""#),
            "heading anchor container: {html}"
        );
        assert!(
            html.contains(r#"data-anchor-link="my-section""#),
            "copy link slug: {html}"
        );
        assert!(
            html.contains(r#"aria-label="Copy section""#),
            "localized copy label: {html}"
        );
    }
}
