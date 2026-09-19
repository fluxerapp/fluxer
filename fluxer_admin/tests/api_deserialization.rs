// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_admin::api::generated::types as generated_types;
use fluxer_admin::api::types;

#[test]
fn deserialize_admin_users_me_response() {
    let json = r#"{
        "user": {
            "id": "1130650140672000000",
            "username": "Hampus",
            "discriminator": 0,
            "global_name": "Hampus",
            "bot": false,
            "system": false,
            "flags": "2308095358783193091",
            "premium_flags": 2,
            "avatar": "563de430",
            "banner": "bd221c57",
            "bio": "For support, please contact support@fluxer.app.",
            "pronouns": "he/him",
            "accent_color": 2631308,
            "email": "hampus@fluxer.com",
            "email_verified": true,
            "email_bounced": false,
            "has_verified_phone": true,
            "date_of_birth": "2003-02-25",
            "locale": "en-US",
            "premium_type": 2,
            "premium_since": "2024-01-01T00:00:00.000Z",
            "premium_until": null,
            "premium_grace_ends_at": null,
            "premium_lifetime_sequence": 1,
            "suspicious_activity_flags": 0,
            "temp_banned_until": null,
            "pending_deletion_at": null,
            "pending_bulk_message_deletion_at": null,
            "deletion_reason_code": null,
            "deletion_public_reason": null,
            "acls": ["super_admin"],
            "traits": ["beta_tester"],
            "has_totp": true,
            "authenticator_types": [1],
            "last_active_at": "2026-05-26T15:00:00.000Z",
            "last_active_ip": "1.2.3.4",
            "last_active_ip_reverse": "example.com",
            "last_active_location": "Stockholm, SE"
        }
    }"#;

    let resp: types::AdminUserMeResponse = serde_json::from_str(json).unwrap();
    let user = resp.user;

    assert_eq!(user.id, "1130650140672000000");
    assert_eq!(user.username, "Hampus");
    assert_eq!(user.discriminator, "0000");
    assert_eq!(user.flags, 2308095358783193091);
    assert_eq!(user.premium_flags, 2);
    assert!(!user.bot);
    assert!(!user.system);
    assert_eq!(user.acls, vec!["super_admin"]);
    assert_eq!(user.traits, vec!["beta_tester"]);
    assert_eq!(user.premium_type, Some(2));
    assert_eq!(user.suspicious_activity_flags, 0);
    assert!(user.has_totp);
    assert!(user.has_verified_phone);
    assert_eq!(user.last_active_ip.as_deref(), Some("1.2.3.4"));
}

#[test]
fn deserialize_flags_as_string_and_number() {
    let json_str = r#"{
        "user": {
            "id": "1", "username": "a", "discriminator": 1, "global_name": null,
            "bot": false, "system": false, "flags": "549755813888",
            "premium_flags": 0, "avatar": null, "banner": null, "bio": null,
            "pronouns": null, "accent_color": null, "email": null,
            "email_verified": false, "email_bounced": false,
            "has_verified_phone": false, "date_of_birth": null, "locale": null,
            "premium_type": null, "premium_since": null, "premium_until": null,
            "premium_grace_ends_at": null, "premium_lifetime_sequence": null,
            "suspicious_activity_flags": 0, "temp_banned_until": null,
            "pending_deletion_at": null, "pending_bulk_message_deletion_at": null,
            "deletion_reason_code": null, "deletion_public_reason": null,
            "acls": [], "traits": [], "has_totp": false, "authenticator_types": [],
            "last_active_at": null, "last_active_ip": null,
            "last_active_ip_reverse": null, "last_active_location": null
        }
    }"#;
    let resp: types::AdminUserMeResponse = serde_json::from_str(json_str).unwrap();
    assert_eq!(resp.user.flags, 549755813888);

    let json_zero = json_str.replace("\"549755813888\"", "\"0\"");
    let resp2: types::AdminUserMeResponse = serde_json::from_str(&json_zero).unwrap();
    assert_eq!(resp2.user.flags, 0);

    let json_num = json_str.replace("\"549755813888\"", "42");
    let resp3: types::AdminUserMeResponse = serde_json::from_str(&json_num).unwrap();
    assert_eq!(resp3.user.flags, 42);
}

#[test]
fn deserialize_discriminator_int_and_string() {
    let json_int = r#"{
        "user": {
            "id": "1", "username": "test", "discriminator": 4363, "global_name": null,
            "bot": true, "system": false, "flags": "0", "premium_flags": 0,
            "avatar": null, "banner": null, "bio": null, "pronouns": null,
            "accent_color": null, "email": null, "email_verified": false,
            "email_bounced": false, "has_verified_phone": false, "date_of_birth": null,
            "locale": null, "premium_type": null, "premium_since": null,
            "premium_until": null, "premium_grace_ends_at": null,
            "premium_lifetime_sequence": null, "suspicious_activity_flags": 0,
            "temp_banned_until": null, "pending_deletion_at": null,
            "pending_bulk_message_deletion_at": null, "deletion_reason_code": null,
            "deletion_public_reason": null, "acls": [], "traits": [],
            "has_totp": false, "authenticator_types": [],
            "last_active_at": null, "last_active_ip": null,
            "last_active_ip_reverse": null, "last_active_location": null
        }
    }"#;
    let resp: types::AdminUserMeResponse = serde_json::from_str(json_int).unwrap();
    assert_eq!(resp.user.discriminator, "4363");

    let json_zero = json_int.replace("4363", "0");
    let resp2: types::AdminUserMeResponse = serde_json::from_str(&json_zero).unwrap();
    assert_eq!(resp2.user.discriminator, "0000");

    let json_string = json_int.replace("4363", "\"7220\"");
    let resp3: types::AdminUserMeResponse = serde_json::from_str(&json_string).unwrap();
    assert_eq!(resp3.user.discriminator, "7220");

    let json_short = json_int.replace("4363", "\"42\"");
    let resp4: types::AdminUserMeResponse = serde_json::from_str(&json_short).unwrap();
    assert_eq!(resp4.user.discriminator, "0042");
}

#[test]
fn deserialize_search_users_response() {
    let json = r#"{
        "users": [
            {
                "id": "1508576042312688531",
                "username": "test",
                "discriminator": 4363,
                "global_name": null,
                "bot": true,
                "system": false,
                "flags": "0",
                "premium_flags": 0,
                "avatar": null,
                "banner": null,
                "bio": null,
                "pronouns": null,
                "accent_color": null,
                "email": null,
                "email_verified": false,
                "email_bounced": false,
                "has_verified_phone": false,
                "date_of_birth": null,
                "locale": null,
                "premium_type": null,
                "premium_since": null,
                "premium_until": null,
                "premium_grace_ends_at": null,
                "premium_lifetime_sequence": null,
                "suspicious_activity_flags": 0,
                "temp_banned_until": null,
                "pending_deletion_at": null,
                "pending_bulk_message_deletion_at": null,
                "deletion_reason_code": null,
                "deletion_public_reason": null,
                "acls": [],
                "traits": [],
                "has_totp": false,
                "authenticator_types": [],
                "last_active_at": null,
                "last_active_ip": null,
                "last_active_ip_reverse": null,
                "last_active_location": null
            }
        ],
        "total": 42
    }"#;

    let resp: types::SearchUsersResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.total, 42);
    assert_eq!(resp.users.len(), 1);
    assert_eq!(resp.users[0].username, "test");
    assert_eq!(resp.users[0].discriminator, "4363");
    assert!(resp.users[0].bot);
}

#[test]
fn deserialize_search_guilds_response() {
    let json = r#"{
        "guilds": [
            {
                "id": "1427764661718740994",
                "name": "Fluxer Testers",
                "features": ["ANIMATED_BANNER", "VERIFIED"],
                "owner_id": "1489329094902315533",
                "owner_username": null,
                "owner_global_name": null,
                "owner_discriminator": null,
                "icon": "de44253f",
                "banner": "048c048f",
                "member_count": 715,
                "nsfw_level": 0
            }
        ],
        "total": 6723
    }"#;

    let resp: types::SearchGuildsResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.total, 6723);
    assert_eq!(resp.guilds.len(), 1);
    assert_eq!(resp.guilds[0].name, "Fluxer Testers");
    assert_eq!(resp.guilds[0].member_count, 715);
    assert_eq!(resp.guilds[0].features.len(), 2);
}

#[test]
fn deserialize_audit_logs_response() {
    let json = r#"{
        "logs": [
            {
                "log_id": "1508822460457747580",
                "admin_user_id": "1130650140672000000",
                "target_type": "user",
                "target_id": "1130958221824557056",
                "action": "list_user_sessions",
                "access": "read",
                "audit_log_reason": null,
                "metadata": {"session_count": "3"},
                "created_at": "2026-05-26T13:21:47.138Z"
            }
        ],
        "total": 94623
    }"#;

    let resp: types::AuditLogsListResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.total, 94623);
    assert_eq!(resp.logs.len(), 1);
    assert_eq!(resp.logs[0].log_id, "1508822460457747580");
    assert_eq!(resp.logs[0].action, "list_user_sessions");
    assert_eq!(resp.logs[0].access.as_deref(), Some("read"));
    assert_eq!(resp.logs[0].target_type, "user");
    assert!(resp.logs[0].audit_log_reason.is_none());
    assert_eq!(resp.logs[0].metadata.get("session_count").unwrap(), "3");
}

#[test]
fn deserialize_guild_audit_logs_with_unknown_keys() {
    let json = r#"{
        "audit_log_entries": [
            {
                "id": "1508822460457747581",
                "action_type": 22,
                "user_id": "1130650140672000000",
                "target_id": "1130958221824557056",
                "reason": "spam",
                "options": {
                    "delete_message_seconds": 3600,
                    "future_option": {"nested": true}
                },
                "changes": [
                    {"key": "future_change", "future_field": 1}
                ],
                "future_entry_field": "ignored"
            },
            {
                "id": "1508822460457747580",
                "action_type": 31,
                "user_id": "1130650140672000000",
                "target_id": "1130958221824557057",
                "options": {
                    "id": "1130958221824557057",
                    "type": 0,
                    "channel_id": "1130958221824557058",
                    "role_name": "Moderators"
                },
                "changes": [
                    {"key": "permissions_diff", "new_value": {"added": ["SEND_MESSAGES"], "removed": []}}
                ]
            },
            {
                "id": "1508822460457747579",
                "action_type": 11,
                "user_id": "1130650140672000000",
                "target_id": "1130958221824557059",
                "options": {"type": 998},
                "changes": [
                    {"key": "name", "old_value": "old-name", "new_value": "new-name"},
                    {"key": "position", "old_value": 3, "new_value": 4},
                    {"key": "nsfw", "new_value": true},
                    {"key": "roles", "new_value": ["1130958221824557060", "1130958221824557061"]}
                ]
            }
        ],
        "users": [],
        "webhooks": []
    }"#;

    let resp: generated_types::ListGuildAuditLogsResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.audit_log_entries.len(), 3);

    let ban = &resp.audit_log_entries[0];
    assert_eq!(*ban.action_type, 22);
    assert_eq!(ban.reason.as_deref(), Some("spam"));
    assert!(ban.options.is_some());
    assert_eq!(ban.changes.len(), 1);
    assert_eq!(ban.changes[0].key, "future_change");
    assert!(ban.changes[0].new_value.is_none());

    let role_update = &resp.audit_log_entries[1];
    let options = role_update.options.as_ref().unwrap();
    assert_eq!(options.role_name.as_deref(), Some("Moderators"));
    assert_eq!(options.channel_id.as_deref(), Some("1130958221824557058"));
    assert_eq!(options.type_, Some(0.0));
    assert_eq!(role_update.changes[0].key, "permissions_diff");
    assert!(role_update.changes[0].new_value.is_some());

    let channel_update = &resp.audit_log_entries[2];
    assert_eq!(channel_update.changes.len(), 4);
    assert_eq!(
        channel_update.changes[0]
            .old_value
            .as_ref()
            .and_then(|value| value.as_str()),
        Some("old-name")
    );
    assert_eq!(
        channel_update.changes[0]
            .new_value
            .as_ref()
            .and_then(|value| value.as_str()),
        Some("new-name")
    );
    assert_eq!(
        channel_update.changes[1]
            .new_value
            .as_ref()
            .and_then(|value| value.as_i64()),
        Some(4)
    );
    assert_eq!(
        channel_update.changes[2]
            .new_value
            .as_ref()
            .and_then(|value| value.as_bool()),
        Some(true)
    );
    assert_eq!(
        channel_update.changes[3]
            .new_value
            .as_ref()
            .and_then(|value| value.as_array())
            .map(|value| value.len()),
        Some(2)
    );
}

#[test]
fn deserialize_instance_config_response_with_unknown_keys() {
    let json = r##"{
        "sso": {
            "enabled": true,
            "enforced": false,
            "display_name": "Fluxer SSO",
            "issuer": "https://id.example.com",
            "authorization_url": "https://id.example.com/authorize",
            "token_url": "https://id.example.com/token",
            "userinfo_url": "https://id.example.com/userinfo",
            "jwks_url": "https://id.example.com/jwks.json",
            "client_id": "fluxer-admin",
            "client_secret_set": true,
            "scope": "openid profile email",
            "allowed_domains": ["fluxer.com"],
            "auto_provision": true,
            "redirect_uri": "https://fluxer.com/sso/callback"
        },
        "gateway_rollout": {
            "session_rollout_percentage": 100,
            "session_rollout_mode": "modulo",
            "guild_rollout_percentage": 100,
            "rpc_request_timeout_ms": 10000,
            "max_concurrent_session_starts": 512,
            "max_concurrent_guild_starts": 256,
            "gateway_dispatch_relay_shards": 32,
            "gateway_dispatch_relay_max_queue": 50000,
            "voice_e2ee_scope": "guild_feature_only",
            "future_rollout_knob": 3
        },
        "voice_noise_suppression": {
            "enabled": true,
            "config_version": 4,
            "default_backend": "rnnoise",
            "enabled_backends": ["none", "standard", "rnnoise"],
            "allow_user_override": true,
            "rollout_basis_points": 10000,
            "rollout_salt": "voice-ns-v1",
            "included_user_ids": [],
            "excluded_user_ids": [],
            "guild_overrides": [],
            "suppression_strength": 80,
            "future_presentation_knob": "verbose",
            "future_knob": 7,
            "future_object_knob": {"nested": true},
            "future_list_knob": ["a", "b"]
        },
        "experiment_delivery": {"poll_interval_seconds": 300, "poll_jitter_percent": 15},
        "registration": {
            "mode": "open",
            "admin_registration_urls_enabled": false,
            "urls": [],
            "pending_registrations": []
        },
        "self_hosted": false,
        "app_public": {
            "branding": {
                "product_name": "Fluxer",
                "icon_url": "https://cdn.example.com/icon.png",
                "symbol_url": "https://cdn.example.com/symbol.svg",
                "logo_url": "https://cdn.example.com/logo.svg",
                "wordmark_url": "https://cdn.example.com/wordmark.svg",
                "favicon_url": "https://cdn.example.com/favicon.ico",
                "theme_color": "#5865f2",
                "future_asset_url": "https://cdn.example.com/future.png"
            },
            "setup": {"configured": true},
            "legal": {
                "terms_url": "https://fluxer.com/terms",
                "privacy_url": "https://fluxer.com/privacy"
            },
            "registration": {"collect_date_of_birth": true}
        },
        "policy": {
            "single_community_enabled": false,
            "single_community_guild_id": null,
            "direct_messages_disabled": false,
            "direct_messages_locked": false,
            "premium_mode": "mirror",
            "services": {
                "gif_enabled": true,
                "youtube_enabled": true,
                "bluesky_enabled": false,
                "future_service_enabled": true
            },
            "services_resolved": {
                "gif_enabled": true,
                "youtube_enabled": true,
                "bluesky_enabled": false
            },
            "services_available": {"gif": true, "youtube": true, "bluesky": false},
            "deferred_phone_gate": {
                "enabled": false,
                "window_hours": 24,
                "member_threshold": 100
            }
        },
        "integrations": {
            "gif": {"klipy_api_key_set": true, "effective_available": true},
            "youtube": {"api_key_set": true, "effective_available": true},
            "captcha": {
                "provider": "hcaptcha",
                "effective_provider": "hcaptcha",
                "hcaptcha_site_key": "site",
                "hcaptcha_secret_key_set": true,
                "turnstile_site_key": "",
                "turnstile_secret_key_set": false,
                "effective_enabled": true
            },
            "email": {
                "enabled": true,
                "effective_enabled": true,
                "provider": "smtp",
                "effective_provider": "smtp",
                "from_email": "noreply@fluxer.com",
                "from_name": "Fluxer",
                "smtp": {
                    "host": "smtp.example.com",
                    "port": 587,
                    "username": "fluxer",
                    "password_set": true,
                    "secure": true
                },
                "disable_new_ip_authorization": false,
                "effective_disable_new_ip_authorization": false
            },
            "bluesky": {
                "enabled": false,
                "effective_enabled": false,
                "client_name": "Fluxer",
                "client_uri": "https://fluxer.com",
                "logo_uri": "https://cdn.example.com/logo.svg",
                "tos_uri": "https://fluxer.com/terms",
                "policy_uri": "https://fluxer.com/privacy",
                "key_count": 0
            }
        },
        "media": {
            "attachment_decay": {
                "enabled": true,
                "min_size_mb": 10,
                "max_size_mb": 500,
                "max_eligible_size_mb": 500,
                "min_lifetime_days": 30,
                "max_lifetime_days": 365,
                "curve": 2,
                "renew_threshold_days": 7,
                "renew_window_days": 14,
                "effective": {
                    "enabled": true,
                    "min_size_mb": 10,
                    "max_size_mb": 500,
                    "max_eligible_size_mb": 500,
                    "min_lifetime_days": 30,
                    "max_lifetime_days": 365,
                    "curve": 2,
                    "renew_threshold_days": 7,
                    "renew_window_days": 14,
                    "future_curve": 1.5
                }
            }
        },
        "future_section": {"enabled": true, "rollout_basis_points": 10000},
        "future_flag": 3
    }"##;

    let resp: generated_types::InstanceConfigResponse = serde_json::from_str(json).expect(
        "the build.rs relaxation must keep unknown response keys from failing the whole response",
    );

    assert!(!resp.self_hosted);
    assert!(resp.voice_noise_suppression.enabled);
    assert_eq!(resp.voice_noise_suppression.config_version, 4);
    assert_eq!(resp.voice_noise_suppression.rollout_basis_points, 10000);
    assert_eq!(*resp.voice_noise_suppression.rollout_salt, "voice-ns-v1");
    assert_eq!(resp.voice_noise_suppression.enabled_backends.len(), 3);
    assert_eq!(resp.experiment_delivery.poll_interval_seconds, 300);
    assert!(resp.policy.single_community_guild_id.is_none());
    assert_eq!(resp.policy.services.gif_enabled, Some(true));
    assert_eq!(resp.app_public.branding.product_name, "Fluxer");
    assert!(resp.media.attachment_decay.effective.enabled);

    let without_unknown_keys = json
        .replace("\"future_rollout_knob\": 3,", "")
        .replace("\"future_presentation_knob\": \"verbose\",", "")
        .replace("\"future_knob\": 7,", "")
        .replace("\"future_object_knob\": {\"nested\": true},", "")
        .replace("\"future_list_knob\": [\"a\", \"b\"],", "")
        .replace(
            "\"future_asset_url\": \"https://cdn.example.com/future.png\",",
            "",
        )
        .replace("\"future_service_enabled\": true,", "")
        .replace("\"future_curve\": 1.5,", "")
        .replace(
            "\"future_section\": {\"enabled\": true, \"rollout_basis_points\": 10000},",
            "",
        )
        .replace("\"future_flag\": 3,", "");
    let baseline: generated_types::InstanceConfigResponse =
        serde_json::from_str(&without_unknown_keys).expect("known keys alone still deserialize");
    assert_eq!(
        serde_json::to_value(&baseline).unwrap(),
        serde_json::to_value(&resp).unwrap()
    );
}

#[test]
fn deserialize_search_reports_response() {
    let json = r#"{
        "reports": [
            {
                "report_id": "1508849882800552806",
                "reporter_id": "1474002886188635356",
                "reporter_tag": "user#2602",
                "reporter_username": "user",
                "reporter_global_name": null,
                "reporter_discriminator": "2602",
                "reporter_email": "user@example.com",
                "reporter_full_legal_name": null,
                "reporter_country_of_residence": null,
                "reported_at": "2026-05-26T15:10:45.134Z",
                "status": 0,
                "report_type": 0,
                "category": "nsfw_violation",
                "additional_info": null,
                "reported_user_id": "1461557793540882622",
                "reported_user_tag": "ReportedUser#4331",
                "reported_user_username": "ReportedUser",
                "reported_user_global_name": "ReportedUser",
                "reported_user_discriminator": "4331",
                "reported_user_avatar_hash": "75c675b2",
                "reported_guild_id": null,
                "reported_guild_name": null,
                "reported_guild_icon_hash": null,
                "reported_message_id": "1508564782082866178",
                "reported_channel_id": "1487475053377430086",
                "reported_channel_name": null,
                "reported_channel_nsfw": false,
                "reported_guild_invite_code": null,
                "reported_guild_nsfw_level": null,
                "reported_guild_nsfw": null,
                "reported_guild_content_warning_level": null,
                "reported_guild_content_warning_text": null,
                "reported_channel_nsfw_override": null,
                "reported_channel_content_warning_level": 0,
                "reported_channel_content_warning_text": null,
                "reported_channel_effective_nsfw": null,
                "reported_channel_effective_content_warning_level": null,
                "reported_channel_effective_content_warning_text": null,
                "resolved_at": null,
                "resolved_by_admin_id": null,
                "public_comment": null,
                "mutual_dm_channel_id": null,
                "message_context": []
            }
        ],
        "total": 1,
        "offset": 0,
        "limit": 25
    }"#;

    let resp: types::SearchReportsResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.total, 1);
    assert_eq!(resp.reports.len(), 1);
    assert_eq!(resp.reports[0].report_id, "1508849882800552806");
    assert_eq!(resp.reports[0].status, 0);
    assert_eq!(resp.reports[0].category.as_deref(), Some("nsfw_violation"));
    assert!(resp.reports[0].reported_guild_icon_hash.is_none());
}

#[test]
fn deserialize_lookup_guild_response() {
    let json = r#"{
        "guild": {
            "id": "123",
            "owner_id": "456",
            "owner_username": "admin",
            "owner_global_name": "Admin",
            "owner_discriminator": "0001",
            "name": "Test Guild",
            "vanity_url_code": null,
            "icon": null,
            "banner": null,
            "splash": null,
            "embed_splash": null,
            "features": [],
            "verification_level": 0,
            "mfa_level": 0,
            "nsfw_level": 0,
            "explicit_content_filter": 0,
            "default_message_notifications": 0,
            "afk_channel_id": null,
            "afk_timeout": 300,
            "system_channel_id": null,
            "system_channel_flags": 0,
            "rules_channel_id": null,
            "disabled_operations": 0,
            "member_count": 100,
            "channels": [],
            "roles": []
        }
    }"#;

    let resp: types::LookupGuildResponse = serde_json::from_str(json).unwrap();
    let guild = resp.guild.unwrap();
    assert_eq!(guild.id, "123");
    assert_eq!(guild.name, "Test Guild");
    assert_eq!(guild.member_count, 100);
}

#[test]
fn deserialize_lookup_guild_null() {
    let json = r#"{"guild": null}"#;
    let resp: types::LookupGuildResponse = serde_json::from_str(json).unwrap();
    assert!(resp.guild.is_none());
}

#[test]
fn deserialize_lookup_application_response() {
    let json = r#"{
        "application": {
            "id": "111",
            "name": "My Bot",
            "owner_user_id": "222",
            "owner_username": "dev",
            "owner_global_name": null,
            "owner_discriminator": "0001",
            "bot_user_id": "333",
            "bot_username": "MyBot",
            "bot_global_name": null,
            "bot_discriminator": "0001",
            "bot_is_public": true,
            "bot_require_code_grant": false,
            "oauth2_redirect_uris": ["https://example.com/cb"],
            "has_client_secret": true,
            "has_bot_token": true,
            "bot_token_preview": "...xyz",
            "bot_token_created_at": "2025-01-01T00:00:00.000Z",
            "client_secret_created_at": "2025-01-01T00:00:00.000Z",
            "version": 1
        }
    }"#;

    let resp: types::LookupApplicationResponse = serde_json::from_str(json).unwrap();
    let app = resp.application.unwrap();
    assert_eq!(app.id, "111");
    assert_eq!(app.name, "My Bot");
    assert!(app.bot_is_public);
}

#[test]
fn deserialize_refresh_search_index_response() {
    let json = r#"{"success": true, "job_id": "abc123"}"#;
    let resp: types::RefreshSearchIndexResponse = serde_json::from_str(json).unwrap();
    assert!(resp.success);
    assert_eq!(resp.job_id, "abc123");
}

#[test]
fn deserialize_ban_check_response() {
    let json = r#"{"banned": true}"#;
    let resp: types::BanCheckResult = serde_json::from_str(json).unwrap();
    assert!(resp.banned);
}

#[test]
fn deserialize_codes_response() {
    let json = r#"{"codes": ["ABC-DEF", "GHI-JKL"]}"#;
    let resp: types::CodesResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.codes.len(), 2);
}

#[test]
fn deserialize_user_mutation_response() {
    let json = r#"{
        "user": {
            "id": "1", "username": "updated", "discriminator": 42,
            "global_name": null, "bot": false, "system": false,
            "flags": "1", "premium_flags": 0, "avatar": null, "banner": null,
            "bio": null, "pronouns": null, "accent_color": null, "email": null,
            "email_verified": false, "email_bounced": false,
            "has_verified_phone": false, "date_of_birth": null, "locale": null,
            "premium_type": null, "premium_since": null, "premium_until": null,
            "premium_grace_ends_at": null, "premium_lifetime_sequence": null,
            "suspicious_activity_flags": 0, "temp_banned_until": null,
            "pending_deletion_at": null, "pending_bulk_message_deletion_at": null,
            "deletion_reason_code": null, "deletion_public_reason": null,
            "acls": [], "traits": [], "has_totp": false, "authenticator_types": [],
            "last_active_at": null, "last_active_ip": null,
            "last_active_ip_reverse": null, "last_active_location": null
        }
    }"#;
    let resp: types::UserMutationResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.user.username, "updated");
    assert_eq!(resp.user.discriminator, "0042");
    assert_eq!(resp.user.flags, 1);
}

#[test]
fn deserialize_guild_update_response() {
    let json = r#"{
        "guild": {
            "id": "123",
            "name": "Updated Guild",
            "features": ["VERIFIED"],
            "owner_id": "456",
            "owner_username": null,
            "owner_global_name": null,
            "owner_discriminator": null,
            "icon": null,
            "banner": null,
            "member_count": 50,
            "nsfw_level": 0
        }
    }"#;

    let resp: types::GuildUpdateResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.guild.name, "Updated Guild");
    assert_eq!(resp.guild.member_count, 50);
}

#[test]
fn deserialize_list_guild_members_response() {
    let json = r#"{
        "members": [
            {
                "user": {
                    "id": "111",
                    "username": "member1",
                    "discriminator": 1234,
                    "global_name": "Member One",
                    "avatar": null,
                    "bot": false
                },
                "nick": "Memb",
                "joined_at": "2025-01-01T00:00:00.000Z",
                "roles": ["222", "333"]
            }
        ],
        "total": 1,
        "limit": 50,
        "offset": 0
    }"#;

    let resp: types::ListGuildMembersResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.total, 1);
    assert_eq!(resp.members[0].user.username, "member1");
    assert_eq!(resp.members[0].user.discriminator, "1234");
    assert_eq!(resp.members[0].roles.len(), 2);
}

#[test]
fn deserialize_list_user_sessions_response() {
    let json = r#"{
        "sessions": [
            {
                "session_id_hash": "abc123",
                "created_at": "2025-01-01T00:00:00.000Z",
                "approx_last_used_at": "2025-06-01T00:00:00.000Z",
                "client_ip": "1.2.3.4",
                "client_ip_reverse": "host.example.com",
                "client_os": "Windows 11",
                "client_platform": "desktop",
                "client_location": "New York, US"
            }
        ]
    }"#;

    let resp: types::ListUserSessionsResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.sessions.len(), 1);
    assert_eq!(resp.sessions[0].client_ip, "1.2.3.4");
}

#[test]
fn deserialize_webauthn_credentials_response() {
    let json = r#"[
        {
            "id": "credential-a",
            "name": "YubiKey",
            "created_at": "2026-05-26T12:00:00.000Z",
            "last_used_at": null
        },
        {
            "id": "credential-b",
            "name": "Touch ID",
            "created_at": "2026-05-25T12:00:00.000Z",
            "last_used_at": "2026-05-26T13:00:00.000Z"
        }
    ]"#;

    let credentials: types::WebAuthnCredentialListResponse = serde_json::from_str(json).unwrap();
    assert_eq!(credentials.len(), 2);
    assert_eq!(credentials[0].name, "YubiKey");
    assert!(credentials[0].last_used_at.is_none());
    assert_eq!(
        credentials[1].last_used_at.as_deref(),
        Some("2026-05-26T13:00:00.000Z")
    );
}

#[test]
fn deserialize_list_user_relationships_response() {
    let json = r#"{
        "friends": [
            {
                "target_user_id": "111",
                "category": "friend",
                "nickname": null,
                "since": "2025-01-01T00:00:00.000Z",
                "target": {
                    "id": "111",
                    "username": "friend1",
                    "discriminator": "0042",
                    "global_name": "Friend One",
                    "avatar": null
                }
            }
        ],
        "incoming_requests": [],
        "outgoing_requests": [],
        "blocked": []
    }"#;

    let resp: types::ListUserRelationshipsResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.friends.len(), 1);
    assert_eq!(
        resp.friends[0].target.as_ref().unwrap().discriminator,
        "0042"
    );
}

#[test]
fn deserialize_create_admin_api_key_response() {
    let json = r#"{
        "key_id": "k_123",
        "key": "flx_secret_abc",
        "name": "My Key",
        "created_at": "2026-01-01T00:00:00.000Z",
        "expires_at": null,
        "acls": ["super_admin"]
    }"#;

    let resp: types::CreateAdminApiKeyResponse = serde_json::from_str(json).unwrap();
    assert_eq!(resp.key_id, "k_123");
    assert_eq!(resp.key, "flx_secret_abc");
    assert_eq!(resp.acls, vec!["super_admin"]);
}

#[test]
fn deserialize_list_admin_api_key_entry() {
    let json = r#"{
        "key_id": "k_123",
        "name": "My Key",
        "created_at": "2026-01-01T00:00:00.000Z",
        "last_used_at": "2026-05-01T00:00:00.000Z",
        "expires_at": null,
        "created_by_user_id": "1130650140672000000",
        "acls": ["super_admin", "reports"]
    }"#;

    let resp: types::ListAdminApiKeyEntry = serde_json::from_str(json).unwrap();
    assert_eq!(resp.key_id, "k_123");
    assert_eq!(resp.created_by_user_id, "1130650140672000000");
    assert_eq!(resp.acls.len(), 2);
}
