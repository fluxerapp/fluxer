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
            "date_of_birth": "2003-02-25",
            "locale": "en-US",
            "premium_type": 2,
            "premium_since": "2024-01-01T00:00:00.000Z",
            "premium_until": null,
            "premium_grace_ends_at": null,
            "premium_lifetime_sequence": 1,
            "temp_banned_until": null,
            "pending_deletion_at": null,
            "pending_bulk_message_deletion_at": null,
            "deletion_reason_code": null,
            "deletion_public_reason": null,
            "deletion_audit_log_reason": null,
            "deletion_scheduled_by": null,
            "deletion_scheduled_at": null,
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
    assert!(user.has_totp);
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
            "date_of_birth": null, "locale": null,
            "premium_type": null, "premium_since": null, "premium_until": null,
            "premium_grace_ends_at": null, "premium_lifetime_sequence": null,
            "temp_banned_until": null,
            "pending_deletion_at": null, "pending_bulk_message_deletion_at": null,
            "deletion_reason_code": null, "deletion_public_reason": null, "deletion_audit_log_reason": null,
            "deletion_scheduled_by": null, "deletion_scheduled_at": null,
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
            "email_bounced": false, "date_of_birth": null,
            "locale": null, "premium_type": null, "premium_since": null,
            "premium_until": null, "premium_grace_ends_at": null,
            "premium_lifetime_sequence": null,
            "temp_banned_until": null, "pending_deletion_at": null,
            "pending_bulk_message_deletion_at": null, "deletion_reason_code": null,
            "deletion_public_reason": null, "deletion_audit_log_reason": null,
            "deletion_scheduled_by": null, "deletion_scheduled_at": null, "acls": [], "traits": [],
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
                "date_of_birth": null,
                "locale": null,
                "premium_type": null,
                "premium_since": null,
                "premium_until": null,
                "premium_grace_ends_at": null,
                "premium_lifetime_sequence": null,
                "temp_banned_until": null,
                "pending_deletion_at": null,
                "pending_bulk_message_deletion_at": null,
                "deletion_reason_code": null,
                "deletion_public_reason": null,
                "deletion_audit_log_reason": null,
                "deletion_scheduled_by": null,
                "deletion_scheduled_at": null,
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
        "push_relay": {
            "relay_consent_accepted": true,
            "relay_consent_accepted_at": "2026-09-27T10:11:12.000Z",
            "relay_consent_accepted_by": "1130650140672000000"
        },
        "domain_migration": {
            "enabled": true,
            "config_version": 2,
            "rollout_basis_points": 2500,
            "rollout_salt": "domain-migration-v1",
            "included_user_ids": ["1500000000000000001"],
            "excluded_user_ids": [],
            "included_guild_ids": [],
            "include_premium_users": false,
            "future_migration_knob": 9,
            "future_presentation_knob": "verbose",
            "future_knob": 7,
            "future_object_knob": {"nested": true},
            "future_list_knob": ["a", "b"],
            "anonymous_rollout_basis_points": 100,
            "standalone_forwarding": true
        },
        "captcha": {
            "enabled": true,
            "cost": 5000,
            "max_counter": 1000,
            "future_captcha_knob": 1
        },
        "experiment_delivery": {"poll_interval_seconds": 300, "poll_jitter_percent": 15},
        "registration": {
            "mode": "open",
            "admin_registration_urls_enabled": false,
            "urls": [],
            "pending_registrations": []
        },
        "self_hosted": false,
        "account_identity": {"mode": "username", "locked": true, "tag_style": "none"},
        "app_public": {
            "branding": {
                "product_name": "Fluxer",
                "icon_url": "https://cdn.example.com/icon.png",
                "symbol_url": "https://cdn.example.com/symbol.svg",
                "logo_url": "https://cdn.example.com/logo.svg",
                "wordmark_url": "https://cdn.example.com/wordmark.svg",
                "favicon_url": "https://cdn.example.com/favicon.ico",
                "theme_color": "#5865f2",
                "future_asset_url": "https://cdn.example.com/future.png",
                "premium_product_name": "Gold",
                "premium_info_url": "https://example.com/gold"
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
            "guild_create_access": false,
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
            "services_available": {"gif": true, "youtube": true, "bluesky": false}
        },
        "integrations": {
            "gif": {"klipy_api_key_set": true, "effective_available": true},
            "youtube": {"api_key_set": true, "effective_available": true},
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
        "billing": {
            "enabled": true,
            "effective_enabled": true,
            "stripe_secret_key_set": true,
            "stripe_webhook_secret_set": false,
            "stripe_secret_key_stored": true,
            "stripe_webhook_secret_stored": false,
            "automatic_tax": null,
            "tax_id_collection": true,
            "terms_consent_required": false,
            "effective_automatic_tax": false,
            "effective_tax_id_collection": true,
            "effective_terms_consent_required": false,
            "default_currency": "GBP",
            "prices": {
                "GBP": {
                    "monthly": "price_1GbpM",
                    "yearly": "price_1GbpY",
                    "gift_1_month": null,
                    "gift_1_year": "price_1GbpG"
                }
            },
            "country_currencies": {"GB": "GBP"},
            "legacy_prices": {"monthly_GBP": ["price_1OldA"]},
            "billing_active": true,
            "stripe_serviceable": true,
            "catalog_mode": "operator",
            "webhook_url": "https://api.example.com/stripe/webhook",
            "future_billing_knob": 1
        },
        "future_section": {"enabled": true, "rollout_basis_points": 10000},
        "future_flag": 3
    }"##;

    let resp: generated_types::InstanceConfigResponse = serde_json::from_str(json).expect(
        "the build.rs relaxation must keep unknown response keys from failing the whole response",
    );

    assert!(!resp.self_hosted);
    assert!(resp.domain_migration.enabled);
    assert_eq!(resp.domain_migration.config_version, 2);
    assert_eq!(resp.domain_migration.rollout_basis_points, 2500);
    assert_eq!(*resp.domain_migration.rollout_salt, "domain-migration-v1");
    assert_eq!(resp.domain_migration.included_user_ids.len(), 1);
    assert_eq!(resp.domain_migration.anonymous_rollout_basis_points, 100);
    assert!(resp.domain_migration.standalone_forwarding);
    assert!(resp.push_relay.relay_consent_accepted);
    assert!(resp.captcha.enabled);
    assert_eq!(resp.captcha.max_counter, 1000);
    assert_eq!(resp.experiment_delivery.poll_interval_seconds, 300);
    assert!(resp.policy.single_community_guild_id.is_none());
    assert_eq!(resp.policy.services.gif_enabled, Some(true));
    assert_eq!(resp.app_public.branding.product_name, "Fluxer");
    assert_eq!(resp.app_public.branding.premium_product_name, "Gold");
    assert!(resp.billing.billing_active);
    assert!(resp.media.attachment_decay.effective.enabled);
    assert!(resp.account_identity.locked);

    let ours: types::InstanceConfigResponse =
        serde_json::from_str(json).expect("hand-written instance config");
    assert_eq!(
        ours.account_identity.mode,
        types::AccountIdentityMode::Username
    );
    assert_eq!(ours.account_identity.locked, Some(true));
    assert_eq!(ours.app_public.branding.premium_product_name, "Gold");
    assert!(ours.billing.stripe_secret_key_stored);
    assert_eq!(ours.billing.tax_id_collection, Some(true));
    assert!(ours.billing.effective_tax_id_collection);
    assert_eq!(
        ours.app_public.branding.premium_info_url.as_deref(),
        Some("https://example.com/gold")
    );
    assert!(ours.billing.billing_active);
    assert!(ours.billing.stripe_serviceable);
    assert!(!ours.billing.stripe_webhook_secret_set);
    assert_eq!(
        ours.billing.catalog_mode,
        types::BillingCatalogMode::Operator
    );
    assert_eq!(ours.billing.default_currency.as_deref(), Some("GBP"));
    let gbp = &ours.billing.prices.as_ref().expect("prices")["GBP"];
    assert_eq!(gbp.gift_1_year.as_deref(), Some("price_1GbpG"));
    assert_eq!(gbp.gift_1_month, None);
    assert_eq!(
        ours.billing.legacy_prices.as_ref().expect("legacy")["monthly_GBP"],
        vec!["price_1OldA".to_owned()]
    );

    let without_unknown_keys = json
        .replace("\"future_rollout_knob\": 3,", "")
        .replace("\"future_presentation_knob\": \"verbose\",", "")
        .replace("\"future_knob\": 7,", "")
        .replace("\"future_migration_knob\": 9,", "")
        .replace("\"future_object_knob\": {\"nested\": true},", "")
        .replace("\"future_list_knob\": [\"a\", \"b\"],", "")
        .replace(
            "\"future_asset_url\": \"https://cdn.example.com/future.png\",",
            "",
        )
        .replace("\"future_service_enabled\": true,", "")
        .replace("\"future_curve\": 1.5,", "")
        .replace(",\n            \"future_billing_knob\": 1", "")
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
fn deserialize_push_relay_config() {
    let accepted: types::PushRelayConfigResponse = serde_json::from_str(
        r#"{
        "relay_consent_accepted": true,
        "relay_consent_accepted_at": "2026-09-27T10:11:12.000Z",
        "relay_consent_accepted_by": "1130650140672000000"
    }"#,
    )
    .expect("an accepted relay consent must deserialize");

    assert!(accepted.relay_consent_accepted);
    assert_eq!(
        accepted.relay_consent_accepted_at.as_deref(),
        Some("2026-09-27T10:11:12.000Z")
    );
    assert_eq!(
        accepted.relay_consent_accepted_by.as_deref(),
        Some("1130650140672000000")
    );

    let empty: types::PushRelayConfigResponse =
        serde_json::from_str("{}").expect("an empty push relay config must deserialize");

    assert!(!empty.relay_consent_accepted);
    assert!(empty.relay_consent_accepted_at.is_none());
    assert!(empty.relay_consent_accepted_by.is_none());
}

#[test]
fn serialize_push_relay_update_omits_an_unset_consent() {
    assert_eq!(
        serde_json::to_value(types::PushRelayConfigUpdateRequest::default()).unwrap(),
        serde_json::json!({})
    );

    let with = types::PushRelayConfigUpdateRequest {
        relay_consent_accepted: Some(true),
    };
    assert_eq!(
        serde_json::to_value(&with).unwrap(),
        serde_json::json!({"relay_consent_accepted": true})
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

fn report_json(extra: serde_json::Value) -> serde_json::Value {
    let mut report = serde_json::json!({
        "report_id": "1556008115701286089",
        "reporter_id": "1556008115617399989",
        "reporter_tag": "reporter#0896",
        "reporter_username": "reporter",
        "reporter_global_name": "Avery Reporter",
        "reporter_discriminator": "0896",
        "reporter_email": null,
        "reporter_full_legal_name": null,
        "reporter_country_of_residence": "FR",
        "reported_at": "2026-10-03T18:30:33.848Z",
        "status": 0,
        "report_type": 0,
        "category": "child_safety",
        "additional_info": null,
        "reported_user_id": "1556008115625788599",
        "reported_user_tag": "target#6741",
        "reported_user_username": "target",
        "reported_user_global_name": "Jordan Target",
        "reported_user_discriminator": "6741",
        "reported_user_avatar_hash": null,
        "reported_guild_id": null,
        "reported_guild_name": null,
        "reported_guild_icon_hash": null,
        "reported_message_id": "1556008115697091784",
        "reported_channel_id": "1556008115692897479",
        "reported_channel_name": null,
        "reported_guild_invite_code": null,
        "resolved_at": null,
        "resolved_by_admin_id": null,
        "public_comment": null
    });
    for (key, value) in extra.as_object().expect("object").clone() {
        report[key] = value;
    }
    report
}

fn v2_dsa_report_json() -> serde_json::Value {
    report_json(serde_json::json!({
        "reason": "csam",
        "reason_label": "Child sexual abuse material",
        "reason_highest_priority": true,
        "reporter_good_faith_confirmed": true,
        "flow": {
            "revision_hash": "b7667e8b32c98c40",
            "surface": "dsa",
            "locale": "fr",
            "steps": [
                {"screen_id": "root_message", "screen_title": "Report message", "option_id": "abuse", "option_label": "Abusive or harmful content", "items": []},
                {"screen_id": "abuse", "screen_title": "What does it involve?", "option_id": "sexual", "option_label": "Sexual content", "items": []},
                {"screen_id": "private_info", "screen_title": "What private information is shared?", "option_id": null, "option_label": null, "items": [{"id": "email", "label": "Email address"}, {"id": "phone", "label": "Phone number"}]},
                {"screen_id": "profile_intro", "screen_title": "Report profile", "option_id": null, "option_label": null, "items": []}
            ]
        }
    }))
}

fn decode_report_both_ways(json: serde_json::Value) -> types::ReportEntry {
    let generated: generated_types::ReportAdminResponseSchema =
        serde_json::from_value(json.clone()).expect("generated report type");
    let via_generated: types::ReportEntry =
        serde_json::from_value(serde_json::to_value(generated).expect("serialize generated"))
            .expect("hand-written report type from generated");
    let direct: types::ReportEntry =
        serde_json::from_value(json).expect("hand-written report type");
    assert_eq!(
        serde_json::to_value(&via_generated).unwrap(),
        serde_json::to_value(&direct).unwrap()
    );
    direct
}

#[test]
fn deserialize_v2_report_with_reason_and_answers() {
    let report = decode_report_both_ways(v2_dsa_report_json());
    assert_eq!(report.reason.as_deref(), Some("csam"));
    assert_eq!(
        report.reason_label.as_deref(),
        Some("Child sexual abuse material")
    );
    assert_eq!(report.reason_highest_priority, Some(true));
    assert_eq!(report.reporter_good_faith_confirmed, Some(true));
    let flow = report.flow.expect("flow answers");
    assert_eq!(flow.revision_hash, "b7667e8b32c98c40");
    assert_eq!(flow.surface, "dsa");
    assert_eq!(flow.locale.as_deref(), Some("fr"));
    assert_eq!(flow.steps.len(), 4);
    assert_eq!(flow.steps[0].option_id.as_deref(), Some("abuse"));
    assert_eq!(flow.steps[2].option_id, None);
    assert_eq!(flow.steps[2].items[1].label, "Phone number");
    assert_eq!(flow.steps[3].option_label, None);
    assert!(flow.steps[3].items.is_empty());
}

#[test]
fn deserialize_legacy_report_without_reason_fields() {
    let nulls = report_json(serde_json::json!({
        "reason": null,
        "reason_label": null,
        "reason_highest_priority": null,
        "flow": null,
        "reporter_good_faith_confirmed": null
    }));
    for json in [report_json(serde_json::json!({})), nulls] {
        let report = decode_report_both_ways(json);
        assert!(report.reason.is_none());
        assert!(report.reason_label.is_none());
        assert!(report.reason_highest_priority.is_none());
        assert!(report.flow.is_none());
        assert!(report.reporter_good_faith_confirmed.is_none());
    }
}

#[test]
fn deserialize_report_with_unknown_reason_and_ids() {
    let report = decode_report_both_ways(report_json(serde_json::json!({
        "reason": "future_reason",
        "reason_label": "future_reason",
        "reason_highest_priority": null,
        "flow": {
            "revision_hash": "0000000000000000",
            "surface": "future_surface",
            "locale": null,
            "steps": [
                {"screen_id": "future_screen", "screen_title": "future_screen", "option_id": "future_option", "option_label": "future_option", "items": [{"id": "future_item", "label": "future_item"}]}
            ]
        }
    })));
    assert_eq!(report.reason.as_deref(), Some("future_reason"));
    assert_eq!(report.reason_label.as_deref(), Some("future_reason"));
    assert_eq!(report.reason_highest_priority, None);
    let flow = report.flow.expect("flow answers");
    assert_eq!(flow.surface, "future_surface");
    assert_eq!(flow.locale, None);
    assert_eq!(flow.steps[0].items[0].id, "future_item");
}

#[test]
fn deserialize_report_with_unknown_status_and_type() {
    let report = decode_report_both_ways(report_json(serde_json::json!({
        "status": 7,
        "report_type": 9
    })));
    assert_eq!(report.status, 7);
    assert_eq!(report.report_type, 9);
    let generated: generated_types::ReportAdminResponseSchema = serde_json::from_value(
        report_json(serde_json::json!({"status": 7, "report_type": 9})),
    )
    .expect("generated report type");
    assert_eq!(*generated.status, 7);
    assert_eq!(*generated.report_type, 9);
}

#[test]
fn deserialize_resolve_report_response_with_unknown_status() {
    let json = serde_json::json!({
        "report_id": "1556008115701286089",
        "status": 7,
        "resolved_at": "2026-10-03T18:40:00.000Z",
        "public_comment": null
    });
    let generated: generated_types::ResolveReportResponse =
        serde_json::from_value(json.clone()).expect("generated resolve response");
    assert_eq!(*generated.status, 7);
    let via_generated: types::ResolveReportResponse =
        serde_json::from_value(serde_json::to_value(generated).expect("serialize generated"))
            .expect("hand-written resolve response from generated");
    let direct: types::ResolveReportResponse =
        serde_json::from_value(json).expect("hand-written resolve response");
    assert_eq!(via_generated.status, 7);
    assert_eq!(direct.status, 7);
    assert_eq!(direct.report_id, "1556008115701286089");
}

#[test]
fn deserialize_webhook_message_report() {
    let json = report_json(serde_json::json!({
        "category": "spam",
        "reported_user_id": null,
        "reported_user_tag": null,
        "reported_user_username": null,
        "reported_user_global_name": null,
        "reported_user_discriminator": null,
        "reported_webhook_id": "1556008115710000001",
        "reported_webhook_name": "Harbor Bulletin",
        "reported_webhook_avatar_hash": "a_1f2e3d",
        "reported_guild_id": "1556008115710000002",
        "reported_guild_name": "Harbor",
        "message_context": [
            {
                "id": "1556008115697091784",
                "channel_id": "1556008115692897479",
                "channel_nsfw": false,
                "guild_id": "1556008115710000002",
                "guild_nsfw_level": 0,
                "content": "Free stuff here",
                "timestamp": "2026-10-04T10:00:00.000Z",
                "attachments": [],
                "author_id": "1556008115710000001",
                "author_username": "Harbor Bulletin",
                "author_global_name": null,
                "author_discriminator": "0000",
                "author_avatar": "a_1f2e3d",
                "webhook_id": "1556008115710000001",
                "user_prior_ncmec_report_ids": []
            },
            {
                "id": "1556008115697091780",
                "channel_id": "1556008115692897479",
                "channel_nsfw": false,
                "guild_id": "1556008115710000002",
                "guild_nsfw_level": 0,
                "content": "hello",
                "timestamp": "2026-10-04T09:59:00.000Z",
                "attachments": [],
                "author_id": "1556008115625788599",
                "author_username": "target",
                "author_global_name": null,
                "author_discriminator": "6741",
                "author_avatar": null,
                "webhook_id": null,
                "user_prior_ncmec_report_ids": []
            }
        ]
    }));
    let mut json = json;
    let webhook_details = serde_json::json!({
        "reported_webhook_default_name": "Harbor Hook",
        "reported_webhook_default_avatar_hash": "b_4c5d6e",
        "reported_webhook_type": 1,
        "reported_webhook_application_id": null,
        "reported_webhook_channel_id": "1556008115692897479",
        "reported_webhook_guild_id": "1556008115710000002",
        "reported_webhook_created_at": "2026-10-04T09:00:00.000Z",
        "reported_webhook_creator_id": "1556008115625788600",
        "reported_webhook_creator_tag": "keeper#0042",
        "reported_webhook_creator_username": "keeper",
        "reported_webhook_creator_global_name": "Harbor Keeper",
        "reported_webhook_creator_discriminator": "0042",
        "reported_webhook_creator_avatar_hash": "c_7f8a9b",
    });
    for (key, value) in webhook_details.as_object().expect("object") {
        json[key] = value.clone();
    }
    let generated: generated_types::ReportAdminResponseSchema =
        serde_json::from_value(json.clone()).expect("generated report type");
    let via_generated: types::ReportEntry =
        serde_json::from_value(serde_json::to_value(generated).expect("serialize generated"))
            .expect("hand-written report type from generated");
    let direct: types::ReportEntry =
        serde_json::from_value(json).expect("hand-written report type");
    for report in [&via_generated, &direct] {
        assert!(report.reported_user_id.is_none());
        assert_eq!(
            report.reported_webhook_id.as_deref(),
            Some("1556008115710000001")
        );
        assert_eq!(
            report.reported_webhook_name.as_deref(),
            Some("Harbor Bulletin")
        );
        assert_eq!(
            report.reported_webhook_avatar_hash.as_deref(),
            Some("a_1f2e3d")
        );
        assert_eq!(
            report.reported_webhook_default_name.as_deref(),
            Some("Harbor Hook")
        );
        assert_eq!(
            report.reported_webhook_creator_id.as_deref(),
            Some("1556008115625788600")
        );
        assert_eq!(
            report.reported_webhook_creator_tag.as_deref(),
            Some("keeper#0042")
        );
        assert_eq!(
            report.reported_webhook_creator_username.as_deref(),
            Some("keeper")
        );
        assert_eq!(
            report.reported_webhook_creator_global_name.as_deref(),
            Some("Harbor Keeper")
        );
        assert_eq!(
            report.reported_webhook_creator_discriminator.as_deref(),
            Some("0042")
        );
        assert_eq!(
            report.reported_webhook_creator_avatar_hash.as_deref(),
            Some("c_7f8a9b")
        );
        assert_eq!(
            report.reported_webhook_default_avatar_hash.as_deref(),
            Some("b_4c5d6e")
        );
        assert_eq!(report.reported_webhook_type, Some(1));
        assert!(report.reported_webhook_application_id.is_none());
        assert_eq!(
            report.reported_webhook_channel_id.as_deref(),
            Some("1556008115692897479")
        );
        assert_eq!(
            report.reported_webhook_guild_id.as_deref(),
            Some("1556008115710000002")
        );
        assert_eq!(
            report.reported_webhook_created_at.as_deref(),
            Some("2026-10-04T09:00:00.000Z")
        );
        let context = report.message_context.as_ref().expect("message context");
        assert_eq!(context[0]["webhook_id"], "1556008115710000001");
        assert_eq!(context[0]["author_id"], "1556008115710000001");
        assert!(context[1]["webhook_id"].is_null());
    }
}

#[test]
fn deserialize_report_without_webhook_fields() {
    let nulls = report_json(serde_json::json!({
        "reported_webhook_id": null,
        "reported_webhook_name": null,
        "reported_webhook_avatar_hash": null,
        "reported_webhook_default_name": null,
        "reported_webhook_default_avatar_hash": null,
        "reported_webhook_type": null,
        "reported_webhook_application_id": null,
        "reported_webhook_channel_id": null,
        "reported_webhook_guild_id": null,
        "reported_webhook_created_at": null,
        "reported_webhook_creator_id": null,
        "reported_webhook_creator_tag": null
    }));
    for json in [report_json(serde_json::json!({})), nulls] {
        let report = decode_report_both_ways(json);
        assert_eq!(
            report.reported_user_id.as_deref(),
            Some("1556008115625788599")
        );
        assert!(report.reported_webhook_id.is_none());
        assert!(report.reported_webhook_name.is_none());
        assert!(report.reported_webhook_avatar_hash.is_none());
        assert!(report.reported_webhook_default_name.is_none());
        assert!(report.reported_webhook_creator_id.is_none());
        assert!(report.reported_webhook_creator_tag.is_none());
        assert!(report.reported_webhook_default_avatar_hash.is_none());
        assert!(report.reported_webhook_type.is_none());
        assert!(report.reported_webhook_application_id.is_none());
        assert!(report.reported_webhook_channel_id.is_none());
        assert!(report.reported_webhook_guild_id.is_none());
        assert!(report.reported_webhook_created_at.is_none());
    }
}

fn snapshot_asset(hash: &str, url: Option<&str>) -> serde_json::Value {
    serde_json::json!({"hash": hash, "url": url})
}

fn evidence_report_json() -> serde_json::Value {
    report_json(serde_json::json!({
        "reported_user_bot": true,
        "reported_profile_snapshot": {
            "captured_at": "2026-10-04T10:00:00.000Z",
            "user": {
                "id": "1556008115625788599",
                "username": "target",
                "discriminator": "6741",
                "global_name": "Jordan Target",
                "bio": "Original bio",
                "pronouns": null,
                "avatar": snapshot_asset("avatar1", Some("https://reports.example.test/a?sig=1")),
                "banner": snapshot_asset("banner1", None)
            },
            "member": {
                "guild_id": "1556008115710000002",
                "nick": "Harbor Nick",
                "bio": null,
                "pronouns": null,
                "joined_at": "2026-10-01T08:00:00.000Z",
                "avatar": null,
                "banner": null
            },
            "guild": null
        },
        "message_context": [
            {
                "id": "1556008115697091784",
                "channel_id": "1556008115692897479",
                "channel_nsfw": false,
                "guild_id": null,
                "guild_nsfw_level": null,
                "content": "",
                "timestamp": "2026-10-04T10:00:00.000Z",
                "attachments": [],
                "author_id": "1556008115625788599",
                "author_username": "target",
                "author_global_name": null,
                "author_discriminator": "6741",
                "author_avatar": null,
                "webhook_id": null,
                "author_bot": true,
                "missing_attachments": [{
                    "id": "1556008115697091790",
                    "filename": "evidence.png",
                    "nsfw": null,
                    "content_type": "image/png",
                    "width": 640,
                    "height": 480,
                    "size": 4096
                }],
                "user_prior_ncmec_report_ids": []
            },
            {
                "id": "1556008115697091780",
                "channel_id": "1556008115692897479",
                "channel_nsfw": false,
                "guild_id": null,
                "guild_nsfw_level": null,
                "content": "hello",
                "timestamp": "2026-10-04T09:59:00.000Z",
                "attachments": [],
                "author_id": "1556008115617399989",
                "author_username": "reporter",
                "author_global_name": null,
                "author_discriminator": "0896",
                "author_avatar": null,
                "webhook_id": null,
                "author_bot": false,
                "missing_attachments": [],
                "user_prior_ncmec_report_ids": []
            }
        ]
    }))
}

#[test]
fn deserialize_report_with_bot_flags_profile_snapshot_and_missing_attachments() {
    let json = evidence_report_json();
    let generated: generated_types::ReportAdminResponseSchema =
        serde_json::from_value(json.clone()).expect("generated report type");
    let via_generated: types::ReportEntry =
        serde_json::from_value(serde_json::to_value(generated).expect("serialize generated"))
            .expect("hand-written report type from generated");
    let direct: types::ReportEntry =
        serde_json::from_value(json).expect("hand-written report type");
    for report in [&via_generated, &direct] {
        assert_eq!(report.reported_user_bot, Some(true));
        let snapshot = report
            .reported_profile_snapshot
            .as_ref()
            .expect("profile snapshot");
        assert_eq!(
            snapshot.captured_at.as_deref(),
            Some("2026-10-04T10:00:00.000Z")
        );
        let user = snapshot.user.as_ref().expect("user snapshot");
        assert_eq!(user.id, "1556008115625788599");
        assert_eq!(user.bio.as_deref(), Some("Original bio"));
        let avatar = user.avatar.as_ref().expect("avatar");
        assert_eq!(avatar.hash, "avatar1");
        assert_eq!(
            avatar.url.as_deref(),
            Some("https://reports.example.test/a?sig=1")
        );
        let banner = user.banner.as_ref().expect("banner");
        assert_eq!(banner.hash, "banner1");
        assert!(banner.url.is_none());
        let member = snapshot.member.as_ref().expect("member snapshot");
        assert_eq!(member.nick.as_deref(), Some("Harbor Nick"));
        assert!(snapshot.guild.is_none());
        let context = report.message_context.as_ref().expect("message context");
        assert_eq!(context[0]["author_bot"], true);
        assert_eq!(context[1]["author_bot"], false);
        let missing = context[0]["missing_attachments"]
            .as_array()
            .expect("missing attachments");
        assert_eq!(missing.len(), 1);
        assert_eq!(missing[0]["filename"], "evidence.png");
        assert_eq!(missing[0]["size"], 4096);
        assert!(
            context[1]["missing_attachments"]
                .as_array()
                .is_none_or(Vec::is_empty)
        );
    }
}

#[test]
fn deserialize_report_without_bot_flags_snapshot_or_hold() {
    let nulls = report_json(serde_json::json!({
        "reported_user_bot": null,
        "reported_profile_snapshot": null,
        "legal_hold_until": null,
        "legal_hold_reason": null
    }));
    for json in [report_json(serde_json::json!({})), nulls] {
        let report = decode_report_both_ways(json);
        assert!(report.reported_user_bot.is_none());
        assert!(report.reported_profile_snapshot.is_none());
        assert!(report.legal_hold_until.is_none());
        assert!(report.legal_hold_reason.is_none());
    }
}

#[test]
fn deserialize_report_legal_hold_fields_and_hold_response() {
    let report = decode_report_both_ways(report_json(serde_json::json!({
        "legal_hold_until": "2027-01-31T23:59:59.999Z",
        "legal_hold_reason": "Court order 42"
    })));
    assert_eq!(
        report.legal_hold_until.as_deref(),
        Some("2027-01-31T23:59:59.999Z")
    );
    assert_eq!(report.legal_hold_reason.as_deref(), Some("Court order 42"));

    for json in [
        serde_json::json!({
            "report_id": "1556008115701286089",
            "legal_hold_until": "2027-01-31T23:59:59.999Z",
            "legal_hold_reason": "Court order 42"
        }),
        serde_json::json!({
            "report_id": "1556008115701286089",
            "legal_hold_until": null,
            "legal_hold_reason": null
        }),
    ] {
        let generated: generated_types::ReportLegalHoldResponse =
            serde_json::from_value(json.clone()).expect("generated hold response");
        let via_generated: types::ReportLegalHoldResponse =
            serde_json::from_value(serde_json::to_value(generated).expect("serialize generated"))
                .expect("hand-written hold response from generated");
        let direct: types::ReportLegalHoldResponse =
            serde_json::from_value(json.clone()).expect("hand-written hold response");
        assert_eq!(
            serde_json::to_value(&via_generated).unwrap(),
            serde_json::to_value(&direct).unwrap()
        );
        assert_eq!(direct.report_id, "1556008115701286089");
        assert_eq!(
            direct.legal_hold_until.as_deref(),
            json["legal_hold_until"].as_str()
        );
    }
}

#[test]
fn deserialize_webhook_report_with_a_bot_creator_and_a_deleted_creator() {
    let bot = decode_report_both_ways(report_json(serde_json::json!({
        "reported_user_id": null,
        "reported_webhook_id": "1556008115710000001",
        "reported_webhook_name": "Relay",
        "reported_webhook_type": 2,
        "reported_webhook_application_id": "1556008115625788601",
        "reported_webhook_created_at": "2026-10-04T09:00:00.000Z",
        "reported_webhook_creator_id": "1556008115625788601",
        "reported_webhook_creator_tag": "relaybot#0000",
        "reported_webhook_creator_username": "relaybot",
        "reported_webhook_creator_discriminator": "0000"
    })));
    assert_eq!(bot.reported_webhook_type, Some(2));
    assert_eq!(
        bot.reported_webhook_application_id.as_deref(),
        Some("1556008115625788601")
    );
    assert_eq!(
        bot.reported_webhook_application_id,
        bot.reported_webhook_creator_id
    );

    let gone = decode_report_both_ways(report_json(serde_json::json!({
        "reported_user_id": null,
        "reported_webhook_id": "1556008115710000001",
        "reported_webhook_name": "Night Relay",
        "reported_webhook_type": 1,
        "reported_webhook_created_at": "2026-10-04T09:00:00.000Z",
        "reported_webhook_creator_id": "1556008115625788602",
        "reported_webhook_creator_tag": null,
        "reported_webhook_creator_username": null,
        "reported_webhook_creator_global_name": null,
        "reported_webhook_creator_discriminator": null,
        "reported_webhook_creator_avatar_hash": null
    })));
    assert_eq!(
        gone.reported_webhook_creator_id.as_deref(),
        Some("1556008115625788602")
    );
    assert!(gone.reported_webhook_creator_tag.is_none());
    assert!(gone.reported_webhook_creator_username.is_none());
    assert!(gone.reported_webhook_application_id.is_none());
}

#[test]
fn deserialize_report_reason_list() {
    let json = r#"{
        "reasons": [
            {"key": "csam", "label": "Child sexual abuse material", "highest_priority": true, "legacy_category_message": "child_safety", "legacy_category_user": "child_safety", "legacy_category_guild": "child_safety"},
            {"key": "raid", "label": "Raid or brigading", "highest_priority": false, "legacy_category_message": "harassment", "legacy_category_user": "harassment", "legacy_category_guild": "raid_coordination"}
        ]
    }"#;
    let generated: generated_types::AdminReportReasonsResponse =
        serde_json::from_str(json).expect("generated reason list type");
    assert_eq!(generated.reasons.len(), 2);
    let response: types::ReportReasonListResponse =
        serde_json::from_str(json).expect("hand-written reason list type");
    assert_eq!(response.reasons[0].key, "csam");
    assert!(response.reasons[0].highest_priority);
    assert_eq!(
        response.reasons[1].legacy_category_guild.as_deref(),
        Some("raid_coordination")
    );

    let lenient: types::ReportReasonListResponse = serde_json::from_str(
        r#"{"reasons": [{"key": "future_reason", "label": "Future reason", "future_field": 1}], "future_field": []}"#,
    )
    .expect("the reason list tolerates missing and unknown fields");
    assert_eq!(lenient.reasons[0].key, "future_reason");
    assert!(!lenient.reasons[0].highest_priority);
    assert!(lenient.reasons[0].legacy_category_message.is_none());
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
fn deserialize_ban_check_response_with_expiry() {
    let json = r#"{"banned": true, "expires_at": "2026-10-04T12:00:00.000Z"}"#;
    let resp: types::BanCheckResult = serde_json::from_str(json).unwrap();
    assert!(resp.banned);
    assert_eq!(resp.expires_at.as_deref(), Some("2026-10-04T12:00:00.000Z"));
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
            "date_of_birth": null, "locale": null,
            "premium_type": null, "premium_since": null, "premium_until": null,
            "premium_grace_ends_at": null, "premium_lifetime_sequence": null,
            "temp_banned_until": null,
            "pending_deletion_at": null, "pending_bulk_message_deletion_at": null,
            "deletion_reason_code": null, "deletion_public_reason": null, "deletion_audit_log_reason": null,
            "deletion_scheduled_by": null, "deletion_scheduled_at": null,
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
            "last_used_at": null,
            "rp_id": "fluxer.com"
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

#[test]
fn account_identity_lock_is_unknown_when_the_api_omits_it() {
    let identity: types::AccountIdentityConfigResponse =
        serde_json::from_str("{}").expect("empty account identity");
    assert_eq!(identity.mode, types::AccountIdentityMode::Email);
    assert_eq!(identity.locked, None);
}

#[test]
fn deserialize_guild_threads_response() {
    let json = r#"{
        "threads": [
            {
                "id": "1600000000000000010",
                "type": 12,
                "guild_id": "1600000000000000001",
                "parent_id": "1600000000000000002",
                "owner_id": "1500000000000000001",
                "name": "secret plans",
                "last_message_id": null,
                "last_pin_timestamp": null,
                "rate_limit_per_user": 0,
                "flags": 0,
                "thread_metadata": {
                    "archived": true,
                    "auto_archive_duration": 4320,
                    "archive_timestamp": "2026-09-27T12:00:00.000Z",
                    "locked": false,
                    "invitable": false,
                    "create_timestamp": "2026-09-26T12:00:00.000Z"
                },
                "message_count": 3,
                "total_message_sent": 4,
                "member_count": 2
            }
        ]
    }"#;
    let generated: generated_types::ListGuildThreadsResponse =
        serde_json::from_str(json).expect("the generated client must accept the thread list");
    assert_eq!(generated.threads.len(), 1);
    let resp: types::ListGuildThreadsResponse = serde_json::from_str(json).unwrap();
    let thread = &resp.threads[0];
    assert_eq!(thread.channel_type, 12);
    assert_eq!(thread.name.as_deref(), Some("secret plans"));
    assert_eq!(thread.member_count, Some(2));
    assert!(
        thread
            .thread_metadata
            .as_ref()
            .is_some_and(|m| m.archived && !m.locked)
    );
}
