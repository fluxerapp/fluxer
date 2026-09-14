// SPDX-License-Identifier: AGPL-3.0-or-later

pub const WILDCARD: &str = "*";
pub const ACL_SET_USER: &str = "acl:set:user";
pub const ADMIN_API_KEY_MANAGE: &str = "admin_api_key:manage";
pub const APPLICATION_LOOKUP: &str = "application:lookup";
pub const APPLICATION_LIST_BY_OWNER: &str = "application:list:by_owner";
pub const APPLICATION_TRANSFER_OWNERSHIP: &str = "application:transfer_ownership";
pub const ARCHIVE_TRIGGER_GUILD: &str = "archive:trigger:guild";
pub const ARCHIVE_TRIGGER_USER: &str = "archive:trigger:user";
pub const ARCHIVE_VIEW_ALL: &str = "archive:view_all";
pub const ASSET_PURGE: &str = "asset:purge";
pub const AUDIT_LOG_VIEW: &str = "audit_log:view";
pub const AUTHENTICATE: &str = "admin:authenticate";
pub const JOBS_VIEW: &str = "jobs:view";
pub const JOBS_CANCEL: &str = "jobs:cancel";
pub const BAN_EMAIL_ADD: &str = "ban:email:add";
pub const BAN_EMAIL_CHECK: &str = "ban:email:check";
pub const BAN_EMAIL_REMOVE: &str = "ban:email:remove";
pub const SUSPICIOUS_EMAIL_DOMAIN_ADD: &str = "suspicious_email_domain:add";
pub const SUSPICIOUS_EMAIL_DOMAIN_CHECK: &str = "suspicious_email_domain:check";
pub const SUSPICIOUS_EMAIL_DOMAIN_REMOVE: &str = "suspicious_email_domain:remove";
pub const BAN_PHRASE_ADD: &str = "ban:phrase:add";
pub const BAN_PHRASE_CHECK: &str = "ban:phrase:check";
pub const BAN_PHRASE_REMOVE: &str = "ban:phrase:remove";
pub const BAN_IP_ADD: &str = "ban:ip:add";
pub const BAN_IP_CHECK: &str = "ban:ip:check";
pub const BAN_IP_REMOVE: &str = "ban:ip:remove";
pub const BAN_URL_ADD: &str = "ban:url:add";
pub const BAN_URL_CHECK: &str = "ban:url:check";
pub const BAN_URL_REMOVE: &str = "ban:url:remove";
pub const BAN_URL_DOMAIN_ADD: &str = "ban:url_domain:add";
pub const BAN_URL_DOMAIN_CHECK: &str = "ban:url_domain:check";
pub const BAN_URL_DOMAIN_REMOVE: &str = "ban:url_domain:remove";
pub const BAN_FILE_SHA_ADD: &str = "ban:file_sha:add";
pub const BAN_FILE_SHA_CHECK: &str = "ban:file_sha:check";
pub const BAN_FILE_SHA_REMOVE: &str = "ban:file_sha:remove";
pub const BAN_AVATAR_HASH_ADD: &str = "ban:avatar_hash:add";
pub const BAN_AVATAR_HASH_CHECK: &str = "ban:avatar_hash:check";
pub const BAN_AVATAR_HASH_REMOVE: &str = "ban:avatar_hash:remove";
pub const BAN_PROFILE_SUBSTRING_ADD: &str = "ban:profile_substring:add";
pub const BAN_PROFILE_SUBSTRING_CHECK: &str = "ban:profile_substring:check";
pub const BAN_PROFILE_SUBSTRING_REMOVE: &str = "ban:profile_substring:remove";
pub const BULK_ADD_GUILD_MEMBERS: &str = "bulk:add:guild_members";
pub const BULK_DELETE_USERS: &str = "bulk:delete:users";
pub const BULK_DELETE_USER_MESSAGES: &str = "bulk:delete:user_messages";
pub const BULK_UPDATE_GUILD_FEATURES: &str = "bulk:update:guild_features";
pub const BULK_UPDATE_SUSPICIOUS_ACTIVITY: &str = "bulk:update:suspicious_activity";
pub const BULK_UPDATE_USER_FLAGS: &str = "bulk:update:user_flags";
pub const CSAM_SUBMIT_NCMEC: &str = "csam:submit_ncmec";
pub const DISCOVERY_REMOVE: &str = "discovery:remove";
pub const DISCOVERY_REVIEW: &str = "discovery:review";
pub const GATEWAY_MEMORY_STATS: &str = "gateway:memory_stats";
pub const GATEWAY_RELOAD_ALL: &str = "gateway:reload_all";
pub const GIFT_CODES_GENERATE: &str = "gift_codes:generate";
pub const GUILD_AUDIT_LOG_VIEW: &str = "guild:audit_log:view";
pub const GUILD_BAN_MEMBER: &str = "guild:ban_member";
pub const GUILD_DELETE: &str = "guild:delete";
pub const GUILD_FORCE_ADD_MEMBER: &str = "guild:force_add_member";
pub const GUILD_KICK_MEMBER: &str = "guild:kick_member";
pub const GUILD_LIST_MEMBERS: &str = "guild:list:members";
pub const GUILD_LOOKUP: &str = "guild:lookup";
pub const GUILD_RELOAD: &str = "guild:reload";
pub const GUILD_SHUTDOWN: &str = "guild:shutdown";
pub const GUILD_TRANSFER_OWNERSHIP: &str = "guild:transfer_ownership";
pub const GUILD_UPDATE_FEATURES: &str = "guild:update:features";
pub const GUILD_UPDATE_NAME: &str = "guild:update:name";
pub const GUILD_UPDATE_SETTINGS: &str = "guild:update:settings";
pub const GUILD_UPDATE_VANITY: &str = "guild:update:vanity";
pub const INSTANCE_CONFIG_UPDATE: &str = "instance:config:update";
pub const INSTANCE_CONFIG_VIEW: &str = "instance:config:view";
pub const INSTANCE_LIMIT_CONFIG_UPDATE: &str = "instance:limit_config:update";
pub const INSTANCE_LIMIT_CONFIG_VIEW: &str = "instance:limit_config:view";
pub const MESSAGE_DELETE_ALL: &str = "message:delete_all";
pub const MESSAGE_DELETE: &str = "message:delete";
pub const MESSAGE_LOOKUP: &str = "message:lookup";
pub const MESSAGE_SHRED: &str = "message:shred";
pub const REPORT_RESOLVE: &str = "report:resolve";
pub const REPORT_VIEW: &str = "report:view";
pub const REPORT_VIEW_REPORTER_PII: &str = "report:view:reporter_pii";
pub const SYSTEM_DM_SEND: &str = "system_dm:send";
pub const SYSTEM_HEAP_SNAPSHOT: &str = "system:heap_snapshot";
pub const USER_CANCEL_BULK_MESSAGE_DELETION: &str = "user:cancel:bulk_message_deletion";
pub const USER_DELETE: &str = "user:delete";
pub const USER_DISABLE_SUSPICIOUS: &str = "user:disable:suspicious";
pub const USER_LIST_DM_CHANNELS: &str = "user:list:dm_channels";
pub const USER_LIST_GUILDS: &str = "user:list:guilds";
pub const USER_LIST_RELATIONSHIPS: &str = "user:list:relationships";
pub const USER_LIST_SESSIONS: &str = "user:list:sessions";
pub const USER_LOOKUP: &str = "user:lookup";
pub const USER_REMOVE_RELATIONSHIP: &str = "user:remove:relationship";
pub const USER_VIEW_CONTACT_LOG: &str = "user:view:contact_log";
pub const USER_VIEW_DOB: &str = "user:view:dob";
pub const USER_VIEW_EMAIL: &str = "user:view:email";
pub const USER_VIEW_IP: &str = "user:view:ip";
pub const USER_TEMP_BAN: &str = "user:temp_ban";
pub const USER_UPDATE_BOT_STATUS: &str = "user:update:bot_status";
pub const USER_UPDATE_DOB: &str = "user:update:dob";
pub const USER_UPDATE_EMAIL: &str = "user:update:email";
pub const USER_UPDATE_FLAGS: &str = "user:update:flags";
pub const USER_UPDATE_MFA: &str = "user:update:mfa";
pub const USER_UPDATE_PHONE: &str = "user:update:phone";
pub const USER_UPDATE_PROFILE: &str = "user:update:profile";
pub const USER_UPDATE_SUSPICIOUS_ACTIVITY: &str = "user:update:suspicious_activity";
pub const USER_UPDATE_TRAITS: &str = "user:update:traits";
pub const USER_UPDATE_USERNAME: &str = "user:update:username";
pub const VOICE_REGION_CREATE: &str = "voice:region:create";
pub const VOICE_REGION_DELETE: &str = "voice:region:delete";
pub const VOICE_REGION_LIST: &str = "voice:region:list";
pub const VOICE_REGION_UPDATE: &str = "voice:region:update";
pub const VOICE_SERVER_CREATE: &str = "voice:server:create";
pub const VOICE_SERVER_DELETE: &str = "voice:server:delete";
pub const VOICE_SERVER_LIST: &str = "voice:server:list";
pub const VOICE_SERVER_UPDATE: &str = "voice:server:update";
pub const ALL_ACLS: &[&str] = &[
    WILDCARD,
    ACL_SET_USER,
    ADMIN_API_KEY_MANAGE,
    APPLICATION_LOOKUP,
    APPLICATION_LIST_BY_OWNER,
    APPLICATION_TRANSFER_OWNERSHIP,
    ARCHIVE_TRIGGER_GUILD,
    ARCHIVE_TRIGGER_USER,
    ARCHIVE_VIEW_ALL,
    ASSET_PURGE,
    AUDIT_LOG_VIEW,
    AUTHENTICATE,
    JOBS_VIEW,
    JOBS_CANCEL,
    BAN_EMAIL_ADD,
    BAN_EMAIL_CHECK,
    BAN_EMAIL_REMOVE,
    SUSPICIOUS_EMAIL_DOMAIN_ADD,
    SUSPICIOUS_EMAIL_DOMAIN_CHECK,
    SUSPICIOUS_EMAIL_DOMAIN_REMOVE,
    BAN_PHRASE_ADD,
    BAN_PHRASE_CHECK,
    BAN_PHRASE_REMOVE,
    BAN_IP_ADD,
    BAN_IP_CHECK,
    BAN_IP_REMOVE,
    BAN_URL_ADD,
    BAN_URL_CHECK,
    BAN_URL_REMOVE,
    BAN_URL_DOMAIN_ADD,
    BAN_URL_DOMAIN_CHECK,
    BAN_URL_DOMAIN_REMOVE,
    BAN_FILE_SHA_ADD,
    BAN_FILE_SHA_CHECK,
    BAN_FILE_SHA_REMOVE,
    BAN_AVATAR_HASH_ADD,
    BAN_AVATAR_HASH_CHECK,
    BAN_AVATAR_HASH_REMOVE,
    BAN_PROFILE_SUBSTRING_ADD,
    BAN_PROFILE_SUBSTRING_CHECK,
    BAN_PROFILE_SUBSTRING_REMOVE,
    BULK_ADD_GUILD_MEMBERS,
    BULK_DELETE_USERS,
    BULK_DELETE_USER_MESSAGES,
    BULK_UPDATE_GUILD_FEATURES,
    BULK_UPDATE_SUSPICIOUS_ACTIVITY,
    BULK_UPDATE_USER_FLAGS,
    CSAM_SUBMIT_NCMEC,
    DISCOVERY_REMOVE,
    DISCOVERY_REVIEW,
    GATEWAY_MEMORY_STATS,
    GATEWAY_RELOAD_ALL,
    GIFT_CODES_GENERATE,
    GUILD_AUDIT_LOG_VIEW,
    GUILD_BAN_MEMBER,
    GUILD_DELETE,
    GUILD_FORCE_ADD_MEMBER,
    GUILD_KICK_MEMBER,
    GUILD_LIST_MEMBERS,
    GUILD_LOOKUP,
    GUILD_RELOAD,
    GUILD_SHUTDOWN,
    GUILD_TRANSFER_OWNERSHIP,
    GUILD_UPDATE_FEATURES,
    GUILD_UPDATE_NAME,
    GUILD_UPDATE_SETTINGS,
    GUILD_UPDATE_VANITY,
    INSTANCE_CONFIG_UPDATE,
    INSTANCE_CONFIG_VIEW,
    INSTANCE_LIMIT_CONFIG_UPDATE,
    INSTANCE_LIMIT_CONFIG_VIEW,
    MESSAGE_DELETE_ALL,
    MESSAGE_DELETE,
    MESSAGE_LOOKUP,
    MESSAGE_SHRED,
    REPORT_RESOLVE,
    REPORT_VIEW,
    REPORT_VIEW_REPORTER_PII,
    SYSTEM_DM_SEND,
    SYSTEM_HEAP_SNAPSHOT,
    USER_CANCEL_BULK_MESSAGE_DELETION,
    USER_DELETE,
    USER_DISABLE_SUSPICIOUS,
    USER_LIST_DM_CHANNELS,
    USER_LIST_GUILDS,
    USER_LIST_RELATIONSHIPS,
    USER_LIST_SESSIONS,
    USER_LOOKUP,
    USER_REMOVE_RELATIONSHIP,
    USER_VIEW_CONTACT_LOG,
    USER_VIEW_DOB,
    USER_VIEW_EMAIL,
    USER_VIEW_IP,
    USER_TEMP_BAN,
    USER_UPDATE_BOT_STATUS,
    USER_UPDATE_DOB,
    USER_UPDATE_EMAIL,
    USER_UPDATE_FLAGS,
    USER_UPDATE_MFA,
    USER_UPDATE_PHONE,
    USER_UPDATE_PROFILE,
    USER_UPDATE_SUSPICIOUS_ACTIVITY,
    USER_UPDATE_TRAITS,
    USER_UPDATE_USERNAME,
    VOICE_REGION_CREATE,
    VOICE_REGION_DELETE,
    VOICE_REGION_LIST,
    VOICE_REGION_UPDATE,
    VOICE_SERVER_CREATE,
    VOICE_SERVER_DELETE,
    VOICE_SERVER_LIST,
    VOICE_SERVER_UPDATE,
];

pub fn has_permission(admin_acls: &[String], required: &str) -> bool {
    let required_alias = acl_key_alias(required);
    admin_acls.iter().any(|acl| {
        acl == required || acl == WILDCARD || acl == &required_alias || acl == "WILDCARD"
    })
}

pub fn has_any_permission(admin_acls: &[String], required: &[&str]) -> bool {
    if required.is_empty() {
        return true;
    }
    required.iter().any(|acl| has_permission(admin_acls, acl))
}

fn acl_key_alias(acl: &str) -> String {
    acl.replace(':', "_").to_ascii_uppercase()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn acls(items: &[&str]) -> Vec<String> {
        items.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn has_permission_exact_match() {
        assert!(has_permission(&acls(&[USER_LOOKUP]), USER_LOOKUP));
    }

    #[test]
    fn has_permission_wildcard() {
        assert!(has_permission(&acls(&[WILDCARD]), USER_LOOKUP));
        assert!(has_permission(&acls(&[WILDCARD]), GUILD_DELETE));
    }

    #[test]
    fn has_permission_accepts_constant_key_aliases() {
        assert!(has_permission(
            &acls(&["USER_REMOVE_RELATIONSHIP"]),
            USER_REMOVE_RELATIONSHIP
        ));
        assert!(has_permission(&acls(&["WILDCARD"]), GUILD_DELETE));
    }

    #[test]
    fn has_permission_no_match() {
        assert!(!has_permission(&acls(&[USER_LOOKUP]), GUILD_LOOKUP));
        assert!(!has_permission(&acls(&[]), USER_LOOKUP));
    }

    #[test]
    fn has_any_permission_any_match() {
        let user_acls = acls(&[USER_LOOKUP, GUILD_LOOKUP]);
        assert!(has_any_permission(
            &user_acls,
            &[GUILD_DELETE, GUILD_LOOKUP]
        ));
    }

    #[test]
    fn has_any_permission_empty_required() {
        assert!(has_any_permission(&acls(&[USER_LOOKUP]), &[]));
        assert!(has_any_permission(&acls(&[]), &[]));
    }

    #[test]
    fn has_any_permission_none_match() {
        let user_acls = acls(&[USER_LOOKUP]);
        assert!(!has_any_permission(
            &user_acls,
            &[GUILD_DELETE, GUILD_LOOKUP]
        ));
    }
}
