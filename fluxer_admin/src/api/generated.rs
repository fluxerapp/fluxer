// SPDX-License-Identifier: AGPL-3.0-or-later

#[allow(
    clippy::all,
    unused_imports,
    unreachable_code,
    unused_variables,
    dead_code,
    renamed_and_removed_lints
)]
mod inner {
    include!(concat!(env!("OUT_DIR"), "/admin_api_generated.rs"));
}

pub use inner::types;

pub use inner::Client as GeneratedClient;

pub(crate) fn number_to_u64(value: f64, field: &str) -> Result<u64, String> {
    const MAX_SAFE_INTEGER: f64 = 9_007_199_254_740_991.0;
    if !value.is_finite() || value < 0.0 || value.fract() != 0.0 || value > MAX_SAFE_INTEGER {
        return Err(format!("{field} is not a safe unsigned integer: {value}"));
    }
    Ok(value as u64)
}

pub(crate) fn i64_to_u64(value: i64, field: &str) -> Result<u64, String> {
    u64::try_from(value).map_err(|_| format!("{field} is negative: {value}"))
}

pub(crate) fn nonzero_u32(value: u32, field: &str) -> Result<std::num::NonZeroU32, String> {
    std::num::NonZeroU32::new(value).ok_or_else(|| format!("{field} must be greater than zero"))
}

pub(crate) fn nonzero_u64(value: u64, field: &str) -> Result<std::num::NonZeroU64, String> {
    std::num::NonZeroU64::new(value).ok_or_else(|| format!("{field} must be greater than zero"))
}

#[cfg(test)]
mod tests {
    use super::{number_to_u64, types::*};

    #[test]
    fn deserialize_admin_users_me() {
        let json = serde_json::json!({
            "user": {
                "id": "123456789012345678",
                "username": "testadmin",
                "discriminator": 1,
                "global_name": "Test Admin",
                "bot": false,
                "system": false,
                "flags": "131141",
                "premium_flags": 0,
                "avatar": "abc123def456",
                "banner": null,
                "bio": "Hello world",
                "pronouns": null,
                "accent_color": null,
                "email": "admin@example.com",
                "email_verified": true,
                "email_bounced": false,
                "has_verified_phone": false,
                "date_of_birth": "2000-01-15",
                "locale": "en-US",
                "premium_type": 2,
                "premium_since": "2024-01-01T00:00:00.000Z",
                "premium_until": null,
                "premium_grace_ends_at": null,
                "premium_lifetime_sequence": null,
                "suspicious_activity_flags": 0,
                "phone_verification_deferred": false,
                "temp_banned_until": null,
                "pending_deletion_at": null,
                "pending_bulk_message_deletion_at": null,
                "deletion_reason_code": null,
                "deletion_public_reason": null,
                "acls": ["MANAGE_USERS", "VIEW_AUDIT_LOG"],
                "traits": ["STAFF"],
                "has_totp": true,
                "authenticator_types": [0],
                "last_active_at": "2026-05-26T12:00:00.000Z",
                "last_active_ip": "203.0.113.1",
                "last_active_ip_reverse": "1.113.0.203.in-addr.arpa",
                "last_active_location": "New York, US"
            }
        });

        let response: AdminUsersMeResponse =
            serde_json::from_value(json).expect("failed to deserialize AdminUsersMeResponse");

        assert_eq!(response.user.id.to_string(), "123456789012345678");
        assert_eq!(response.user.username, "testadmin");
        assert!(!response.user.bot);
        assert!(response.user.email_verified);
        assert_eq!(response.user.acls.len(), 2);
        assert_eq!(response.user.global_name, Some("Test Admin".to_string()));
        assert!(response.user.banner.is_none());
        assert!(response.user.has_totp);
    }

    #[test]
    fn deserialize_search_guilds_response() {
        let json = serde_json::json!({
            "guilds": [
                {
                    "id": "987654321098765432",
                    "name": "Test Guild",
                    "icon": "def456abc123",
                    "banner": null,
                    "owner_id": "123456789012345678",
                    "owner_username": "guildowner",
                    "owner_global_name": "Guild Owner",
                    "owner_discriminator": "0001",
                    "member_count": 42,
                    "features": ["COMMUNITY", "NEWS"]
                }
            ],
            "total": 1
        });

        let response: SearchGuildsResponse =
            serde_json::from_value(json).expect("failed to deserialize SearchGuildsResponse");

        assert_eq!(response.total as i64, 1);
        assert_eq!(response.guilds.len(), 1);
        assert_eq!(response.guilds[0].name, "Test Guild");
        assert_eq!(response.guilds[0].member_count, 42);
    }

    #[test]
    fn reject_lossy_generated_number_count() {
        assert_eq!(number_to_u64(42.0, "total").unwrap(), 42);
        assert!(number_to_u64(42.5, "total").is_err());
        assert!(number_to_u64(9_007_199_254_740_992.0, "total").is_err());
    }
}
