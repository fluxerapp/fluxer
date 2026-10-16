// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::user_flags::{USER_FLAG_STAFF, visible_user_flags};
use serde::{Deserialize, Serialize};

pub const FLUXER_SYSTEM_USER_ID: i64 = 0;
pub const FLUXER_SYSTEM_USERNAME: &str = "Fluxer";
pub const FLUXER_SYSTEM_DISCRIMINATOR: i32 = 0;

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

pub struct UserPartialFields {
    pub user_id: i64,
    pub username: String,
    pub discriminator: i32,
    pub global_name: Option<String>,
    pub avatar_hash: Option<String>,
    pub avatar_color: Option<i32>,
    pub bot: Option<bool>,
    pub system: Option<bool>,
    pub flags: Option<i64>,
    pub mention_flags: Option<i32>,
}

impl From<UserPartialFields> for ApiUserPartial {
    fn from(partial: UserPartialFields) -> Self {
        if partial.user_id == FLUXER_SYSTEM_USER_ID {
            return fluxer_system_user();
        }
        Self {
            id: partial.user_id.to_string(),
            username: partial.username,
            discriminator: format!("{:04}", partial.discriminator),
            global_name: partial.global_name,
            avatar: partial.avatar_hash,
            avatar_color: partial.avatar_color,
            bot: partial.bot.filter(|bot| *bot),
            system: partial.system.filter(|system| *system),
            flags: visible_user_flags(partial.flags.unwrap_or_default()),
            mention_flags: partial.mention_flags.filter(|flags| *flags != 0),
        }
    }
}

pub fn fluxer_system_user() -> ApiUserPartial {
    ApiUserPartial {
        id: FLUXER_SYSTEM_USER_ID.to_string(),
        username: FLUXER_SYSTEM_USERNAME.to_owned(),
        discriminator: format!("{:04}", FLUXER_SYSTEM_DISCRIMINATOR),
        global_name: None,
        avatar: None,
        avatar_color: None,
        bot: Some(true),
        system: Some(true),
        flags: USER_FLAG_STAFF as i32,
        mention_flags: None,
    }
}
