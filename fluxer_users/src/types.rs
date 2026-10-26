// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::pseudonym::pseudonym;
use fluxer_common::user_flags::AccountStanding;
#[cfg(test)]
use fluxer_common::user_flags::USER_FLAG_PARTNER;
#[cfg(test)]
use fluxer_common::user_flags::USER_FLAG_STAFF;
pub use fluxer_common::user_partial::ApiUserPartial;
use fluxer_common::user_partial::{
    FLUXER_SYSTEM_USER_ID, UserPartialFields, default_instance_name,
};
use serde::{Deserialize, Deserializer, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "op")]
pub enum UserRequest {
    GetPartialsByIds {
        user_ids: Vec<i64>,
        #[serde(default = "default_instance_name")]
        instance_name: String,
    },
    GetApiPartialsByIds {
        user_ids: Vec<String>,
        #[serde(default = "default_instance_name")]
        instance_name: String,
    },
    Invalidate {
        #[serde(deserialize_with = "deserialize_i64_from_number_or_string")]
        user_id: i64,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum UserResponse {
    FoundPartials(Vec<UserPartial>),
    FoundApiPartials(Vec<ApiUserPartial>),
    Invalidated,
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
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub content_hidden_since: Option<i64>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub hidden_profile_badges: Vec<String>,
}

impl UserPartial {
    pub fn visible_to_others(self, standing: &AccountStanding, now_ms: i64) -> UserPartial {
        if self.user_id == FLUXER_SYSTEM_USER_ID || !standing.profile_hidden(now_ms) {
            return self;
        }
        let pseudonym = pseudonym(self.user_id);
        UserPartial {
            username: pseudonym.username,
            discriminator: pseudonym.discriminator,
            global_name: None,
            avatar_hash: None,
            banner_hash: None,
            banner_color: None,
            accent_color: None,
            avatar_color: None,
            ..self
        }
    }

    pub fn to_api_partial(&self) -> ApiUserPartial {
        ApiUserPartial::from(UserPartialFields {
            user_id: self.user_id,
            username: self.username.clone(),
            discriminator: self.discriminator,
            global_name: self.global_name.clone(),
            avatar_hash: self.avatar_hash.clone(),
            avatar_color: self.avatar_color,
            bot: self.bot,
            system: self.system,
            flags: self.flags,
            hidden_profile_badges: self.hidden_profile_badges.clone(),
            mention_flags: self.mention_flags,
        })
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
    use serde_json::json;

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
            content_hidden_since: None,
            hidden_profile_badges: Vec::new(),
        }
    }

    #[test]
    fn api_partial_requests_use_string_snowflakes() {
        let request: UserRequest = serde_json::from_value(json!({
            "op": "GetApiPartialsByIds",
            "user_ids": ["9007199254740993", "9223372036854775807"],
            "instance_name": "Example Chat"
        }))
        .unwrap();

        match request {
            UserRequest::GetApiPartialsByIds {
                user_ids,
                instance_name,
            } => {
                assert_eq!(user_ids, vec!["9007199254740993", "9223372036854775807"]);
                assert_eq!(instance_name, "Example Chat");
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
    fn api_partial_drops_the_flags_of_hidden_badges() {
        let mut partial = partial_with_flags(123, USER_FLAG_STAFF | USER_FLAG_PARTNER);
        partial.hidden_profile_badges = vec!["staff".to_owned()];

        let api_partial = partial.to_api_partial();

        assert_eq!(api_partial.flags, USER_FLAG_PARTNER as i32);
        assert_eq!(api_partial.bot, None);
        assert_eq!(api_partial.system, None);
        assert_eq!(api_partial.mention_flags, None);
    }
}
