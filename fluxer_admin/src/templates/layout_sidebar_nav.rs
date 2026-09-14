// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::acl;

pub struct NavItem {
    pub title: &'static str,
    pub path: &'static str,
    pub active_key: &'static str,
    pub required_acls: &'static [&'static str],
    pub hosted_only: bool,
}

pub struct NavSection {
    pub title: &'static str,
    pub items: &'static [NavItem],
}

macro_rules! item {
    ($t:expr, $p:expr, $k:expr, [ $($a:expr),+ $(,)? ]) => {
        NavItem { title: $t, path: $p, active_key: $k, required_acls: &[$($a),+], hosted_only: false }
    };
    ($t:expr, $p:expr, $k:expr, [ $($a:expr),+ $(,)? ], hosted) => {
        NavItem { title: $t, path: $p, active_key: $k, required_acls: &[$($a),+], hosted_only: true }
    };
}

pub const NAV_SECTIONS: &[NavSection] = &[
    NavSection {
        title: "Lookup",
        items: &[
            item!("Users", "/users", "users", [acl::USER_LOOKUP]),
            item!("Guilds", "/guilds", "guilds", [acl::GUILD_LOOKUP]),
            item!(
                "Applications",
                "/applications",
                "applications",
                [acl::APPLICATION_LOOKUP, acl::APPLICATION_LIST_BY_OWNER]
            ),
        ],
    },
    NavSection {
        title: "Moderation",
        items: &[
            item!("Reports", "/reports", "reports", [acl::REPORT_VIEW]),
            item!(
                "Discovery Review",
                "/discovery",
                "discovery",
                [acl::DISCOVERY_REVIEW]
            ),
            item!(
                "Bulk Actions",
                "/bulk-actions",
                "bulk-actions",
                [
                    acl::BULK_UPDATE_USER_FLAGS,
                    acl::BULK_UPDATE_GUILD_FEATURES,
                    acl::BULK_ADD_GUILD_MEMBERS,
                    acl::BULK_DELETE_USERS,
                    acl::BULK_DELETE_USER_MESSAGES,
                ]
            ),
        ],
    },
    NavSection {
        title: "Content Tools",
        items: &[
            item!(
                "Message Tools",
                "/messages",
                "message-tools",
                [
                    acl::MESSAGE_LOOKUP,
                    acl::MESSAGE_DELETE,
                    acl::MESSAGE_SHRED,
                    acl::MESSAGE_DELETE_ALL,
                ]
            ),
            item!(
                "System DMs",
                "/system-dms",
                "system-dms",
                [acl::SYSTEM_DM_SEND]
            ),
            item!(
                "Archives",
                "/archives",
                "archives",
                [
                    acl::ARCHIVE_VIEW_ALL,
                    acl::ARCHIVE_TRIGGER_USER,
                    acl::ARCHIVE_TRIGGER_GUILD,
                ]
            ),
        ],
    },
    NavSection {
        title: "User Bans",
        items: &[
            item!(
                "IP Bans",
                "/ip-bans",
                "ip-bans",
                [acl::BAN_IP_CHECK, acl::BAN_IP_ADD, acl::BAN_IP_REMOVE]
            ),
            item!(
                "Email Bans",
                "/email-bans",
                "email-bans",
                [
                    acl::BAN_EMAIL_CHECK,
                    acl::BAN_EMAIL_ADD,
                    acl::BAN_EMAIL_REMOVE
                ]
            ),
            item!(
                "Suspicious Email Domains",
                "/suspicious-email-domains",
                "suspicious-email-domains",
                [
                    acl::SUSPICIOUS_EMAIL_DOMAIN_CHECK,
                    acl::SUSPICIOUS_EMAIL_DOMAIN_ADD,
                    acl::SUSPICIOUS_EMAIL_DOMAIN_REMOVE,
                ]
            ),
            item!(
                "Phrase Bans",
                "/phrase-bans",
                "phrase-bans",
                [
                    acl::BAN_PHRASE_CHECK,
                    acl::BAN_PHRASE_ADD,
                    acl::BAN_PHRASE_REMOVE
                ]
            ),
        ],
    },
    NavSection {
        title: "Content Blocklists",
        items: &[
            item!(
                "URL Blocklist",
                "/url-bans",
                "url-bans",
                [acl::BAN_URL_CHECK, acl::BAN_URL_ADD, acl::BAN_URL_REMOVE]
            ),
            item!(
                "URL Domain Blocklist",
                "/url-domain-bans",
                "url-domain-bans",
                [
                    acl::BAN_URL_DOMAIN_CHECK,
                    acl::BAN_URL_DOMAIN_ADD,
                    acl::BAN_URL_DOMAIN_REMOVE
                ]
            ),
            item!(
                "File SHA Blocklist",
                "/file-sha-bans",
                "file-sha-bans",
                [
                    acl::BAN_FILE_SHA_CHECK,
                    acl::BAN_FILE_SHA_ADD,
                    acl::BAN_FILE_SHA_REMOVE
                ]
            ),
            item!(
                "Avatar Hash Blocklist",
                "/avatar-hash-bans",
                "avatar-hash-bans",
                [
                    acl::BAN_AVATAR_HASH_CHECK,
                    acl::BAN_AVATAR_HASH_ADD,
                    acl::BAN_AVATAR_HASH_REMOVE
                ]
            ),
            item!(
                "Profile Substring Blocklist",
                "/profile-substring-bans",
                "profile-substring-bans",
                [
                    acl::BAN_PROFILE_SUBSTRING_CHECK,
                    acl::BAN_PROFILE_SUBSTRING_ADD,
                    acl::BAN_PROFILE_SUBSTRING_REMOVE
                ]
            ),
        ],
    },
    NavSection {
        title: "Observability",
        items: &[
            item!(
                "Gateway",
                "/gateway",
                "gateway",
                [acl::GATEWAY_MEMORY_STATS, acl::GATEWAY_RELOAD_ALL]
            ),
            item!(
                "Audit Logs",
                "/audit-logs",
                "audit-logs",
                [acl::AUDIT_LOG_VIEW]
            ),
            item!("Jobs", "/jobs", "jobs", [acl::JOBS_VIEW]),
        ],
    },
    NavSection {
        title: "Platform",
        items: &[
            item!(
                "Search Index",
                "/search-index",
                "search-index",
                [acl::GUILD_LOOKUP]
            ),
            item!(
                "Voice Regions",
                "/voice-regions",
                "voice-regions",
                [acl::VOICE_REGION_LIST]
            ),
            item!(
                "Voice Servers",
                "/voice-servers",
                "voice-servers",
                [acl::VOICE_SERVER_LIST]
            ),
        ],
    },
    NavSection {
        title: "Configuration",
        items: &[
            item!(
                "Instance Config",
                "/instance-config",
                "instance-config",
                [acl::INSTANCE_CONFIG_VIEW, acl::INSTANCE_CONFIG_UPDATE]
            ),
            item!(
                "Limit Config",
                "/limit-config",
                "limit-config",
                [
                    acl::INSTANCE_LIMIT_CONFIG_VIEW,
                    acl::INSTANCE_LIMIT_CONFIG_UPDATE
                ]
            ),
            item!(
                "Admin API Keys",
                "/admin-api-keys",
                "admin-api-keys",
                [acl::ADMIN_API_KEY_MANAGE]
            ),
        ],
    },
    NavSection {
        title: "Hosted Features",
        items: &[item!(
            "Gift Codes",
            "/gift-codes",
            "gift-codes",
            [acl::GIFT_CODES_GENERATE],
            hosted
        )],
    },
];
