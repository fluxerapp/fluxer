// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Deserializer, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "op")]
pub enum UserRequest {
    GetById {
        user_id: i64,
    },
    GetPartialById {
        user_id: i64,
    },
    GetPartialsByIds {
        user_ids: Vec<i64>,
    },
    GetApiPartialById {
        user_id: String,
    },
    GetApiPartialsByIds {
        user_ids: Vec<String>,
    },
    Invalidate {
        #[serde(deserialize_with = "deserialize_i64_from_number_or_string")]
        user_id: i64,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum UserResponse {
    Found(User),
    FoundPartial(UserPartial),
    FoundPartials(Vec<UserPartial>),
    FoundApiPartial(ApiUserPartial),
    FoundApiPartials(Vec<ApiUserPartial>),
    NotFound,
    Invalidated,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub user_id: i64,
    pub username: String,
    pub discriminator: i32,
    pub bot: Option<bool>,
    pub system: Option<bool>,
    pub email: Option<String>,
    pub email_verified: Option<bool>,
    pub email_bounced: Option<bool>,
    pub authenticator_types: Vec<i32>,
    pub avatar_hash: Option<String>,
    pub avatar_color: Option<i32>,
    pub banner_hash: Option<String>,
    pub banner_color: Option<i32>,
    pub bio: Option<String>,
    pub accent_color: Option<i32>,
    pub date_of_birth: Option<String>,
    pub locale: Option<String>,
    pub flags: Option<i64>,
    pub premium_flags: Option<i32>,
    pub global_name: Option<String>,
    pub pronouns: Option<String>,
    pub traits: Vec<String>,
    pub premium_type: Option<i32>,
    pub premium_since: Option<i64>,
    pub premium_until: Option<i64>,
    pub premium_gift_extension_ends_at: Option<i64>,
    pub premium_lifetime_sequence: Option<i32>,
    pub premium_billing_cycle: Option<String>,
    pub premium_will_cancel: Option<bool>,
    pub premium_onboarding_dismissed_at: Option<i64>,
    pub has_ever_purchased: Option<bool>,
    pub stripe_subscription_id: Option<String>,
    pub stripe_customer_id: Option<String>,
    pub gift_inventory_server_seq: Option<i32>,
    pub gift_inventory_client_seq: Option<i32>,
    pub suspicious_activity_flags: Option<i32>,
    pub terms_agreed_at: Option<i64>,
    pub privacy_agreed_at: Option<i64>,
    pub last_active_at: Option<i64>,
    pub last_active_ip: Option<String>,
    pub temp_banned_until: Option<i64>,
    pub pending_deletion_at: Option<i64>,
    pub pending_bulk_message_deletion_at: Option<i64>,
    pub pending_bulk_message_deletion_channel_count: Option<i32>,
    pub pending_bulk_message_deletion_message_count: Option<i32>,
    pub password_last_changed_at: Option<i64>,
    pub acls: Vec<String>,
    pub deletion_reason_code: Option<i32>,
    pub deletion_public_reason: Option<String>,
    pub deletion_audit_log_reason: Option<String>,
    pub first_refund_at: Option<i64>,
    pub version: i32,
    pub has_verified_phone: Option<bool>,
    pub premium_grace_ends_at: Option<i64>,
    pub mention_flags: Option<i32>,
    pub last_voice_activity_sharing_change_at: Option<i64>,
    pub timezone: Option<String>,
    pub timezone_privacy_flags: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserPartial {
    pub user_id: i64,
    pub username: String,
    pub discriminator: i32,
    pub global_name: Option<String>,
    pub avatar_hash: Option<String>,
    pub bot: Option<bool>,
    pub system: Option<bool>,
    pub flags: Option<i64>,
    pub banner_hash: Option<String>,
    pub banner_color: Option<i32>,
    pub accent_color: Option<i32>,
    pub avatar_color: Option<i32>,
    pub mention_flags: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiUserPartial {
    pub id: String,
    pub username: String,
    pub discriminator: String,
    pub global_name: Option<String>,
    pub avatar: Option<String>,
    pub avatar_color: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bot: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system: Option<bool>,
    pub flags: i32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mention_flags: Option<i32>,
}

const USER_FLAG_STAFF: i64 = 1 << 0;
const USER_FLAG_CTP_MEMBER: i64 = 1 << 1;
const USER_FLAG_PARTNER: i64 = 1 << 2;
const USER_FLAG_BUG_HUNTER: i64 = 1 << 3;
const USER_FLAG_FRIENDLY_BOT: i64 = 1 << 4;
const USER_FLAG_FRIENDLY_BOT_MANUAL_APPROVAL: i64 = 1 << 5;
const USER_FLAG_SPAMMER: i64 = 1 << 6;
const USER_FLAG_STAFF_HIDDEN: i64 = 1 << 57;
const PUBLIC_USER_FLAGS: i64 = USER_FLAG_STAFF
    | USER_FLAG_CTP_MEMBER
    | USER_FLAG_PARTNER
    | USER_FLAG_BUG_HUNTER
    | USER_FLAG_FRIENDLY_BOT
    | USER_FLAG_FRIENDLY_BOT_MANUAL_APPROVAL
    | USER_FLAG_SPAMMER;
const PUBLIC_USER_FLAGS_WITHOUT_STAFF: i64 = PUBLIC_USER_FLAGS & !USER_FLAG_STAFF;
const FLUXER_SYSTEM_USER_ID: i64 = 0;
const FLUXER_SYSTEM_USERNAME: &str = "Fluxer";
const FLUXER_SYSTEM_DISCRIMINATOR: &str = "0000";

impl User {
    pub fn to_partial(&self) -> UserPartial {
        UserPartial {
            user_id: self.user_id,
            username: self.username.clone(),
            discriminator: self.discriminator,
            global_name: self.global_name.clone(),
            avatar_hash: self.avatar_hash.clone(),
            bot: self.bot,
            system: self.system,
            flags: self.flags,
            banner_hash: self.banner_hash.clone(),
            banner_color: self.banner_color,
            accent_color: self.accent_color,
            avatar_color: self.avatar_color,
            mention_flags: self.mention_flags,
        }
    }

    pub fn to_api_partial(&self) -> ApiUserPartial {
        if self.user_id == FLUXER_SYSTEM_USER_ID {
            return fluxer_system_user();
        }
        ApiUserPartial {
            id: self.user_id.to_string(),
            username: self.username.clone(),
            discriminator: format!("{:04}", self.discriminator),
            global_name: self.global_name.clone(),
            avatar: self.avatar_hash.clone(),
            avatar_color: self.avatar_color,
            bot: self.bot.filter(|bot| *bot),
            system: self.system.filter(|system| *system),
            flags: visible_user_flags(self.flags.unwrap_or_default()),
            mention_flags: self.mention_flags.filter(|flags| *flags != 0),
        }
    }
}

impl UserPartial {
    pub fn to_api_partial(&self) -> ApiUserPartial {
        if self.user_id == FLUXER_SYSTEM_USER_ID {
            return fluxer_system_user();
        }
        ApiUserPartial {
            id: self.user_id.to_string(),
            username: self.username.clone(),
            discriminator: format!("{:04}", self.discriminator),
            global_name: self.global_name.clone(),
            avatar: self.avatar_hash.clone(),
            avatar_color: self.avatar_color,
            bot: self.bot.filter(|bot| *bot),
            system: self.system.filter(|system| *system),
            flags: visible_user_flags(self.flags.unwrap_or_default()),
            mention_flags: self.mention_flags.filter(|flags| *flags != 0),
        }
    }
}

fn visible_user_flags(flags: i64) -> i32 {
    let visible_flags = if (flags & USER_FLAG_STAFF_HIDDEN) != 0 {
        PUBLIC_USER_FLAGS_WITHOUT_STAFF
    } else {
        PUBLIC_USER_FLAGS
    };
    (flags & visible_flags) as i32
}

fn fluxer_system_user() -> ApiUserPartial {
    ApiUserPartial {
        id: FLUXER_SYSTEM_USER_ID.to_string(),
        username: FLUXER_SYSTEM_USERNAME.to_owned(),
        discriminator: FLUXER_SYSTEM_DISCRIMINATOR.to_owned(),
        global_name: None,
        avatar: None,
        avatar_color: None,
        bot: Some(true),
        system: Some(true),
        flags: USER_FLAG_STAFF as i32,
        mention_flags: None,
    }
}

fn deserialize_i64_from_number_or_string<'de, D>(deserializer: D) -> Result<i64, D::Error>
where
    D: Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum I64OrString {
        Number(i64),
        String(String),
    }

    match I64OrString::deserialize(deserializer)? {
        I64OrString::Number(value) => Ok(value),
        I64OrString::String(value) => value.parse::<i64>().map_err(serde::de::Error::custom),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{Value, json};

    const USER_FLAG_DELETED: i64 = 1 << 34;

    fn partial_with_flags(user_id: i64, flags: i64) -> UserPartial {
        UserPartial {
            user_id,
            username: "Ada".to_owned(),
            discriminator: 7,
            global_name: Some("Ada Lovelace".to_owned()),
            avatar_hash: Some("avatar_hash".to_owned()),
            bot: Some(false),
            system: Some(false),
            flags: Some(flags),
            banner_hash: None,
            banner_color: None,
            accent_color: None,
            avatar_color: Some(0x336699),
            mention_flags: Some(0),
        }
    }

    fn user_with_flags(user_id: i64, flags: i64) -> User {
        User {
            user_id,
            username: "Ada".to_owned(),
            discriminator: 7,
            bot: Some(false),
            system: Some(false),
            email: Some("ada@example.com".to_owned()),
            email_verified: Some(true),
            email_bounced: Some(false),
            authenticator_types: vec![1],
            avatar_hash: Some("avatar_hash".to_owned()),
            avatar_color: Some(0x336699),
            banner_hash: Some("banner_hash".to_owned()),
            banner_color: Some(0x112233),
            bio: Some("analytical engine enjoyer".to_owned()),
            accent_color: Some(0x445566),
            date_of_birth: Some("1815-12-10".to_owned()),
            locale: Some("en-US".to_owned()),
            flags: Some(flags),
            premium_flags: Some(1),
            global_name: Some("Ada Lovelace".to_owned()),
            pronouns: Some("she/her".to_owned()),
            traits: vec!["founder".to_owned()],
            premium_type: Some(2),
            premium_since: Some(1_781_526_896_789),
            premium_until: Some(1_781_526_896_789),
            premium_gift_extension_ends_at: None,
            premium_lifetime_sequence: None,
            premium_billing_cycle: Some("monthly".to_owned()),
            premium_will_cancel: Some(false),
            premium_onboarding_dismissed_at: None,
            has_ever_purchased: Some(true),
            stripe_subscription_id: Some("sub_123".to_owned()),
            stripe_customer_id: Some("cus_123".to_owned()),
            gift_inventory_server_seq: Some(3),
            gift_inventory_client_seq: Some(3),
            suspicious_activity_flags: Some(0),
            terms_agreed_at: Some(1_781_526_896_789),
            privacy_agreed_at: Some(1_781_526_896_789),
            last_active_at: Some(1_781_526_896_789),
            last_active_ip: Some("203.0.113.7".to_owned()),
            temp_banned_until: None,
            pending_deletion_at: None,
            pending_bulk_message_deletion_at: None,
            pending_bulk_message_deletion_channel_count: None,
            pending_bulk_message_deletion_message_count: None,
            password_last_changed_at: Some(1_781_526_896_789),
            acls: vec!["admin".to_owned()],
            deletion_reason_code: None,
            deletion_public_reason: None,
            deletion_audit_log_reason: None,
            first_refund_at: None,
            version: 3,
            has_verified_phone: Some(true),
            premium_grace_ends_at: None,
            mention_flags: Some(2),
            last_voice_activity_sharing_change_at: None,
            timezone: Some("Europe/London".to_owned()),
            timezone_privacy_flags: Some(1),
        }
    }

    #[test]
    fn direct_api_partial_matches_the_two_step_conversion() {
        for user_id in [123, FLUXER_SYSTEM_USER_ID] {
            for flags in [
                0,
                USER_FLAG_STAFF,
                USER_FLAG_STAFF | USER_FLAG_STAFF_HIDDEN | USER_FLAG_PARTNER,
                USER_FLAG_DELETED,
            ] {
                let user = user_with_flags(user_id, flags);

                assert_eq!(
                    serde_json::to_value(user.to_api_partial()).unwrap(),
                    serde_json::to_value(user.to_partial().to_api_partial()).unwrap(),
                    "user_id {user_id} flags {flags}"
                );
            }
        }
    }

    #[test]
    fn direct_api_partial_ignores_fields_outside_the_partial() {
        let mut user = user_with_flags(123, USER_FLAG_STAFF);
        user.mention_flags = Some(0);
        let api_partial = user.to_api_partial();

        assert_eq!(api_partial.id, "123");
        assert_eq!(api_partial.username, "Ada");
        assert_eq!(api_partial.discriminator, "0007");
        assert_eq!(api_partial.global_name, Some("Ada Lovelace".to_owned()));
        assert_eq!(api_partial.avatar, Some("avatar_hash".to_owned()));
        assert_eq!(api_partial.avatar_color, Some(0x336699));
        assert_eq!(api_partial.bot, None);
        assert_eq!(api_partial.system, None);
        assert_eq!(api_partial.flags, USER_FLAG_STAFF as i32);
        assert_eq!(api_partial.mention_flags, None);
    }

    #[test]
    fn api_partial_requests_use_string_snowflakes() {
        let request: UserRequest = serde_json::from_value(json!({
            "op": "GetApiPartialsByIds",
            "user_ids": ["9007199254740993", "9223372036854775807"]
        }))
        .unwrap();

        match request {
            UserRequest::GetApiPartialsByIds { user_ids } => {
                assert_eq!(user_ids, vec!["9007199254740993", "9223372036854775807"]);
            }
            other => panic!("unexpected request: {other:?}"),
        }
    }

    #[test]
    fn invalidate_accepts_number_or_string_snowflakes() {
        let string_request: UserRequest =
            serde_json::from_value(json!({"op": "Invalidate", "user_id": "9007199254740993"}))
                .unwrap();
        let numeric_request: UserRequest =
            serde_json::from_value(json!({"op": "Invalidate", "user_id": 42})).unwrap();

        assert!(matches!(
            string_request,
            UserRequest::Invalidate {
                user_id: 9007199254740993
            }
        ));
        assert!(matches!(
            numeric_request,
            UserRequest::Invalidate { user_id: 42 }
        ));
    }

    #[test]
    fn api_partial_response_serializes_for_the_api_client() {
        let response = UserResponse::FoundApiPartials(vec![
            partial_with_flags(123, USER_FLAG_STAFF).to_api_partial(),
        ]);

        let serialized = serde_json::to_value(response).unwrap();

        assert_eq!(
            serialized,
            json!({
                "FoundApiPartials": [{
                    "id": "123",
                    "username": "Ada",
                    "discriminator": "0007",
                    "global_name": "Ada Lovelace",
                    "avatar": "avatar_hash",
                    "avatar_color": 0x336699,
                    "flags": 1
                }]
            })
        );
    }

    #[test]
    fn api_partial_hides_staff_flag_when_staff_hidden_is_set() {
        let partial = partial_with_flags(
            123,
            USER_FLAG_STAFF | USER_FLAG_STAFF_HIDDEN | USER_FLAG_PARTNER,
        );

        let api_partial = partial.to_api_partial();

        assert_eq!(api_partial.flags, USER_FLAG_PARTNER as i32);
        assert_eq!(api_partial.bot, None);
        assert_eq!(api_partial.system, None);
        assert_eq!(api_partial.mention_flags, None);
    }

    #[test]
    fn api_partial_preserves_stored_fields_when_deleted_flag_is_set() {
        let deleted = partial_with_flags(123, USER_FLAG_DELETED).to_api_partial();

        assert_eq!(deleted.username, "Ada");
        assert_eq!(deleted.global_name, Some("Ada Lovelace".to_owned()));
        assert_eq!(deleted.avatar, Some("avatar_hash".to_owned()));
        assert_eq!(deleted.flags, 0);
    }

    #[test]
    fn api_partial_maps_system_user_like_the_api_mapper() {
        let system = partial_with_flags(FLUXER_SYSTEM_USER_ID, USER_FLAG_STAFF).to_api_partial();

        assert_eq!(system.username, FLUXER_SYSTEM_USERNAME);
        assert_eq!(system.bot, Some(true));
        assert_eq!(system.system, Some(true));
        assert_eq!(
            serde_json::to_value(system).unwrap()["system"],
            Value::Bool(true)
        );
    }
}
