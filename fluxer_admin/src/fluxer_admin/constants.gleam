//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

pub type Flag {
  Flag(name: String, value: Int)
}

pub const flag_staff = Flag("STAFF", 1)

pub const flag_ctp_member = Flag("CTP_MEMBER", 2)

pub const flag_partner = Flag("PARTNER", 4)

pub const flag_bug_hunter = Flag("BUG_HUNTER", 8)

pub const flag_high_global_rate_limit = Flag(
  "HIGH_GLOBAL_RATE_LIMIT",
  8_589_934_592,
)

pub const flag_premium_purchase_disabled = Flag(
  "PREMIUM_PURCHASE_DISABLED",
  35_184_372_088_832,
)

pub const flag_premium_enabled_override = Flag(
  "PREMIUM_ENABLED_OVERRIDE",
  70_368_744_177_664,
)

pub const flag_rate_limit_bypass = Flag(
  "RATE_LIMIT_BYPASS",
  140_737_488_355_328,
)

pub const flag_report_banned = Flag("REPORT_BANNED", 281_474_976_710_656)

pub const flag_verified_not_underage = Flag(
  "VERIFIED_NOT_UNDERAGE",
  562_949_953_421_312,
)

pub const flag_pending_manual_verification = Flag(
  "PENDING_MANUAL_VERIFICATION",
  1_125_899_906_842_624,
)

pub const flag_used_mobile_client = Flag(
  "USED_MOBILE_CLIENT",
  4_503_599_627_370_496,
)

pub const flag_app_store_reviewer = Flag(
  "APP_STORE_REVIEWER",
  9_007_199_254_740_992,
)

pub fn get_patchable_flags() -> List(Flag) {
  [
    flag_staff,
    flag_ctp_member,
    flag_partner,
    flag_bug_hunter,
    flag_high_global_rate_limit,
    flag_premium_purchase_disabled,
    flag_premium_enabled_override,
    flag_rate_limit_bypass,
    flag_report_banned,
    flag_verified_not_underage,
    flag_pending_manual_verification,
    flag_used_mobile_client,
    flag_app_store_reviewer,
  ]
}

pub const suspicious_flag_require_verified_email = Flag(
  "REQUIRE_VERIFIED_EMAIL",
  1,
)

pub const suspicious_flag_require_reverified_email = Flag(
  "REQUIRE_REVERIFIED_EMAIL",
  2,
)

pub const suspicious_flag_require_verified_phone = Flag(
  "REQUIRE_VERIFIED_PHONE",
  4,
)

pub const suspicious_flag_require_reverified_phone = Flag(
  "REQUIRE_REVERIFIED_PHONE",
  8,
)

pub const suspicious_flag_require_verified_email_or_verified_phone = Flag(
  "REQUIRE_VERIFIED_EMAIL_OR_VERIFIED_PHONE",
  16,
)

pub const suspicious_flag_require_reverified_email_or_verified_phone = Flag(
  "REQUIRE_REVERIFIED_EMAIL_OR_VERIFIED_PHONE",
  32,
)

pub const suspicious_flag_require_verified_email_or_reverified_phone = Flag(
  "REQUIRE_VERIFIED_EMAIL_OR_REVERIFIED_PHONE",
  64,
)

pub const suspicious_flag_require_reverified_email_or_reverified_phone = Flag(
  "REQUIRE_REVERIFIED_EMAIL_OR_REVERIFIED_PHONE",
  128,
)

pub fn get_suspicious_activity_flags() -> List(Flag) {
  [
    suspicious_flag_require_verified_email,
    suspicious_flag_require_reverified_email,
    suspicious_flag_require_verified_phone,
    suspicious_flag_require_reverified_phone,
    suspicious_flag_require_verified_email_or_verified_phone,
    suspicious_flag_require_reverified_email_or_verified_phone,
    suspicious_flag_require_verified_email_or_reverified_phone,
    suspicious_flag_require_reverified_email_or_reverified_phone,
  ]
}

pub const deletion_reason_user_requested = #(1, "User Requested")

pub const deletion_reason_other = #(2, "Other")

pub const deletion_reason_spam = #(3, "Spam")

pub const deletion_reason_hacks_cheats = #(4, "Hacks / Cheats")

pub const deletion_reason_raids = #(5, "Raids")

pub const deletion_reason_selfbot = #(6, "Selfbot")

pub const deletion_reason_nonconsensual_pornography = #(
  7,
  "Nonconsensual Pornography",
)

pub const deletion_reason_scam = #(8, "Scam")

pub const deletion_reason_lolicon = #(9, "Lolicon")

pub const deletion_reason_doxxing = #(10, "Doxxing")

pub const deletion_reason_harassment = #(11, "Harassment")

pub const deletion_reason_fraudulent_charge = #(12, "Fraudulent Charge")

pub const deletion_reason_coppa = #(13, "COPPA")

pub const deletion_reason_friendly_fraud = #(14, "Friendly Fraud")

pub const deletion_reason_unsolicited_nsfw = #(15, "Unsolicited NSFW")

pub const deletion_reason_gore = #(16, "Gore")

pub const deletion_reason_ban_evasion = #(17, "Ban Evasion")

pub const deletion_reason_token_solicitation = #(18, "Token Solicitation")

pub fn get_deletion_reasons() -> List(#(Int, String)) {
  [
    deletion_reason_user_requested,
    deletion_reason_other,
    deletion_reason_spam,
    deletion_reason_hacks_cheats,
    deletion_reason_raids,
    deletion_reason_selfbot,
    deletion_reason_nonconsensual_pornography,
    deletion_reason_scam,
    deletion_reason_lolicon,
    deletion_reason_doxxing,
    deletion_reason_harassment,
    deletion_reason_fraudulent_charge,
    deletion_reason_coppa,
    deletion_reason_friendly_fraud,
    deletion_reason_unsolicited_nsfw,
    deletion_reason_gore,
    deletion_reason_ban_evasion,
    deletion_reason_token_solicitation,
  ]
}

pub fn get_temp_ban_durations() -> List(#(Int, String)) {
  [
    #(1, "1 hour"),
    #(12, "12 hours"),
    #(24, "1 day"),
    #(72, "3 days"),
    #(120, "5 days"),
    #(168, "1 week"),
    #(336, "2 weeks"),
    #(720, "30 days"),
  ]
}

pub const acl_wildcard = "*"

pub const acl_authenticate = "admin:authenticate"

pub const acl_gateway_memory_stats = "gateway:memory_stats"

pub const acl_process_memory_stats = acl_gateway_memory_stats

pub const acl_gateway_reload_all = "gateway:reload_all"

pub const acl_user_lookup = "user:lookup"

pub const acl_user_list_sessions = "user:list:sessions"

pub const acl_user_list_guilds = "user:list:guilds"

pub const acl_user_terminate_sessions = "user:terminate:sessions"

pub const acl_user_update_mfa = "user:update:mfa"

pub const acl_user_update_avatar = "user:update:avatar"

pub const acl_user_update_banner = "user:update:banner"

pub const acl_user_update_profile = "user:update:profile"

pub const acl_user_update_bot_status = "user:update:bot_status"

pub const acl_user_update_email = "user:update:email"

pub const acl_user_update_phone = "user:update:phone"

pub const acl_user_update_dob = "user:update:dob"

pub const acl_user_update_username = "user:update:username"

pub const acl_user_update_flags = "user:update:flags"

pub const acl_user_update_suspicious_activity = "user:update:suspicious_activity"

pub const acl_user_temp_ban = "user:temp_ban"

pub const acl_user_disable_suspicious = "user:disable:suspicious"

pub const acl_user_delete = "user:delete"

pub const acl_user_cancel_bulk_message_deletion = "user:cancel:bulk_message_deletion"

pub const acl_pending_verification_view = "pending_verification:view"

pub const acl_pending_verification_review = "pending_verification:review"

pub const acl_beta_codes_generate = "beta_codes:generate"

pub const acl_gift_codes_generate = "gift_codes:generate"

pub const acl_guild_lookup = "guild:lookup"

pub const acl_guild_list_members = "guild:list:members"

pub const acl_guild_reload = "guild:reload"

pub const acl_guild_shutdown = "guild:shutdown"

pub const acl_guild_delete = "guild:delete"

pub const acl_guild_update_name = "guild:update:name"

pub const acl_guild_update_icon = "guild:update:icon"

pub const acl_guild_update_banner = "guild:update:banner"

pub const acl_guild_update_splash = "guild:update:splash"

pub const acl_guild_update_vanity = "guild:update:vanity"

pub const acl_guild_update_features = "guild:update:features"

pub const acl_guild_update_settings = "guild:update:settings"

pub const acl_guild_transfer_ownership = "guild:transfer_ownership"

pub const acl_guild_force_add_member = "guild:force_add_member"

pub const acl_asset_purge = "asset:purge"

pub const acl_message_lookup = "message:lookup"

pub const acl_message_delete = "message:delete"

pub const acl_message_shred = "message:shred"

pub const acl_message_delete_all = "message:delete_all"

pub const acl_ban_ip_check = "ban:ip:check"

pub const acl_ban_ip_add = "ban:ip:add"

pub const acl_ban_ip_remove = "ban:ip:remove"

pub const acl_ban_email_check = "ban:email:check"

pub const acl_ban_email_add = "ban:email:add"

pub const acl_ban_email_remove = "ban:email:remove"

pub const acl_ban_phone_check = "ban:phone:check"

pub const acl_ban_phone_add = "ban:phone:add"

pub const acl_ban_phone_remove = "ban:phone:remove"

pub const acl_bulk_update_user_flags = "bulk:update:user_flags"

pub const acl_bulk_update_guild_features = "bulk:update:guild_features"

pub const acl_bulk_add_guild_members = "bulk:add:guild_members"

pub const acl_archive_view_all = "archive:view_all"

pub const acl_archive_trigger_user = "archive:trigger:user"

pub const acl_archive_trigger_guild = "archive:trigger:guild"

pub const acl_bulk_delete_users = "bulk:delete:users"

pub const acl_audit_log_view = "audit_log:view"

pub const acl_report_view = "report:view"

pub const acl_report_resolve = "report:resolve"

pub const acl_voice_region_list = "voice:region:list"

pub const acl_voice_region_create = "voice:region:create"

pub const acl_voice_region_update = "voice:region:update"

pub const acl_voice_region_delete = "voice:region:delete"

pub const acl_voice_server_list = "voice:server:list"

pub const acl_voice_server_create = "voice:server:create"

pub const acl_voice_server_update = "voice:server:update"

pub const acl_voice_server_delete = "voice:server:delete"

pub const acl_acl_set_user = "acl:set:user"

pub const acl_metrics_view = "metrics:view"

pub const acl_feature_flag_view = "feature_flag:view"

pub const acl_feature_flag_manage = "feature_flag:manage"

pub const acl_instance_config_view = "instance:config:view"

pub const acl_instance_config_update = "instance:config:update"

pub const acl_instance_snowflake_reservation_view = "instance:snowflake_reservation:view"

pub const acl_instance_snowflake_reservation_manage = "instance:snowflake_reservation:manage"

pub type FeatureFlag {
  FeatureFlag(id: String, name: String, description: String)
}

pub const feature_flag_message_scheduling = FeatureFlag(
  "message_scheduling",
  "Message Scheduling",
  "Allows users to schedule messages to be sent at a later time",
)

pub const feature_flag_expression_packs = FeatureFlag(
  "expression_packs",
  "Expression Packs",
  "Allows users to create and use custom expression packs",
)

pub fn get_feature_flags() -> List(FeatureFlag) {
  [feature_flag_message_scheduling, feature_flag_expression_packs]
}

pub type GuildFeature {
  GuildFeature(value: String)
}

pub const feature_invite_splash = GuildFeature("INVITE_SPLASH")

pub const feature_vip_voice = GuildFeature("VIP_VOICE")

pub const feature_vanity_url = GuildFeature("VANITY_URL")

pub const feature_more_emoji = GuildFeature("MORE_EMOJI")

pub const feature_more_stickers = GuildFeature("MORE_STICKERS")

pub const feature_unlimited_emoji = GuildFeature("UNLIMITED_EMOJI")

pub const feature_unlimited_stickers = GuildFeature("UNLIMITED_STICKERS")

pub const feature_verified = GuildFeature("VERIFIED")

pub const feature_banner = GuildFeature("BANNER")

pub const feature_animated_banner = GuildFeature("ANIMATED_BANNER")

pub const feature_animated_icon = GuildFeature("ANIMATED_ICON")

pub const feature_invites_disabled = GuildFeature("INVITES_DISABLED")

pub const feature_text_channel_flexible_names = GuildFeature(
  "TEXT_CHANNEL_FLEXIBLE_NAMES",
)

pub const feature_unavailable_for_everyone = GuildFeature(
  "UNAVAILABLE_FOR_EVERYONE",
)

pub const feature_unavailable_for_everyone_but_staff = GuildFeature(
  "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF",
)

pub const feature_detached_banner = GuildFeature("DETACHED_BANNER")

pub const feature_expression_purge_allowed = GuildFeature(
  "EXPRESSION_PURGE_ALLOWED",
)

pub const feature_disallow_unclaimed_accounts = GuildFeature(
  "DISALLOW_UNCLAIMED_ACCOUNTS",
)

pub const feature_large_guild_override = GuildFeature("LARGE_GUILD_OVERRIDE")

pub fn get_guild_features() -> List(GuildFeature) {
  [
    feature_animated_icon,
    feature_animated_banner,
    feature_banner,
    feature_invite_splash,
    feature_invites_disabled,
    feature_more_emoji,
    feature_more_stickers,
    feature_unlimited_emoji,
    feature_unlimited_stickers,
    feature_text_channel_flexible_names,
    feature_unavailable_for_everyone,
    feature_unavailable_for_everyone_but_staff,
    feature_vanity_url,
    feature_verified,
    feature_vip_voice,
    feature_detached_banner,
    feature_expression_purge_allowed,
    feature_disallow_unclaimed_accounts,
    feature_large_guild_override,
  ]
}

pub const disabled_op_push_notifications = Flag("PUSH_NOTIFICATIONS", 1)

pub const disabled_op_everyone_mentions = Flag("EVERYONE_MENTIONS", 2)

pub const disabled_op_typing_events = Flag("TYPING_EVENTS", 4)

pub const disabled_op_instant_invites = Flag("INSTANT_INVITES", 8)

pub const disabled_op_send_message = Flag("SEND_MESSAGE", 16)

pub const disabled_op_reactions = Flag("REACTIONS", 32)

pub fn get_disabled_operations() -> List(Flag) {
  [
    disabled_op_push_notifications,
    disabled_op_everyone_mentions,
    disabled_op_typing_events,
    disabled_op_instant_invites,
    disabled_op_send_message,
    disabled_op_reactions,
  ]
}

pub fn get_all_acls() -> List(String) {
  [
    acl_wildcard,
    acl_authenticate,
    acl_gateway_memory_stats,
    acl_gateway_reload_all,
    acl_user_lookup,
    acl_user_list_sessions,
    acl_user_list_guilds,
    acl_user_terminate_sessions,
    acl_user_update_mfa,
    acl_user_update_avatar,
    acl_user_update_banner,
    acl_user_update_profile,
    acl_user_update_bot_status,
    acl_user_update_email,
    acl_user_update_phone,
    acl_user_update_dob,
    acl_user_update_username,
    acl_user_update_flags,
    acl_user_update_suspicious_activity,
    acl_user_temp_ban,
    acl_user_disable_suspicious,
    acl_user_delete,
    acl_user_cancel_bulk_message_deletion,
    acl_pending_verification_view,
    acl_pending_verification_review,
    acl_beta_codes_generate,
    acl_gift_codes_generate,
    acl_guild_lookup,
    acl_guild_list_members,
    acl_guild_reload,
    acl_guild_shutdown,
    acl_guild_delete,
    acl_guild_update_name,
    acl_guild_update_icon,
    acl_guild_update_banner,
    acl_guild_update_splash,
    acl_guild_update_vanity,
    acl_guild_update_features,
    acl_guild_update_settings,
    acl_guild_transfer_ownership,
    acl_guild_force_add_member,
    acl_asset_purge,
    acl_message_lookup,
    acl_message_delete,
    acl_message_shred,
    acl_message_delete_all,
    acl_ban_ip_check,
    acl_ban_ip_add,
    acl_ban_ip_remove,
    acl_ban_email_check,
    acl_ban_email_add,
    acl_ban_email_remove,
    acl_ban_phone_check,
    acl_ban_phone_add,
    acl_ban_phone_remove,
    acl_bulk_update_user_flags,
    acl_bulk_update_guild_features,
    acl_bulk_add_guild_members,
    acl_archive_view_all,
    acl_archive_trigger_user,
    acl_archive_trigger_guild,
    acl_bulk_delete_users,
    acl_audit_log_view,
    acl_report_view,
    acl_report_resolve,
    acl_voice_region_list,
    acl_voice_region_create,
    acl_voice_region_update,
    acl_voice_region_delete,
    acl_voice_server_list,
    acl_voice_server_create,
    acl_voice_server_update,
    acl_voice_server_delete,
    acl_acl_set_user,
    acl_metrics_view,
    acl_feature_flag_view,
    acl_feature_flag_manage,
    acl_instance_config_view,
    acl_instance_config_update,
  ]
}
