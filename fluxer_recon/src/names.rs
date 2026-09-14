// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::ids::{ChannelId, ConnectionId, GuildId, IdError, RoomKey, UserId};

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum NameError {
    #[error("room name has {0} underscore-separated parts")]
    RoomPartCount(usize),
    #[error("room name has an unrecognised shape")]
    RoomShape,
    #[error("room name has a field that is not a plain decimal u64")]
    RoomNumber,
    #[error("participant identity has {0} underscore-separated parts")]
    IdentityPartCount(usize),
    #[error("participant identity has an unrecognised shape")]
    IdentityShape,
    #[error("participant identity has a field that is not a plain decimal u64")]
    IdentityNumber,
    #[error("participant identity carries an invalid connection id: {0}")]
    IdentityConnection(IdError),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParticipantIdentity {
    pub user_id: UserId,
    pub connection_id: ConnectionId,
}

fn parse_u64_strict(value: &str) -> Option<u64> {
    if value.is_empty() || !value.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    value.parse::<u64>().ok()
}

pub fn format_room_name(room: RoomKey) -> String {
    match room {
        RoomKey::Guild {
            guild_id,
            channel_id,
        } => format!("guild_{guild_id}_channel_{channel_id}"),
        RoomKey::Dm { channel_id } => format!("dm_channel_{channel_id}"),
    }
}

pub fn parse_room_name(name: &str) -> Result<RoomKey, NameError> {
    let parts: Vec<&str> = name.split('_').collect();
    match parts.as_slice() {
        ["guild", guild, "channel", channel] => Ok(RoomKey::Guild {
            guild_id: GuildId::new(parse_u64_strict(guild).ok_or(NameError::RoomNumber)?),
            channel_id: ChannelId::new(parse_u64_strict(channel).ok_or(NameError::RoomNumber)?),
        }),
        ["dm", "channel", channel] => Ok(RoomKey::Dm {
            channel_id: ChannelId::new(parse_u64_strict(channel).ok_or(NameError::RoomNumber)?),
        }),
        [_, _, _] | [_, _, _, _] => Err(NameError::RoomShape),
        other => Err(NameError::RoomPartCount(other.len())),
    }
}

pub fn format_participant_identity(identity: &ParticipantIdentity) -> String {
    let ParticipantIdentity {
        user_id,
        connection_id,
    } = identity;
    format!("user_{user_id}_{connection_id}")
}

pub fn parse_participant_identity(identity: &str) -> Result<ParticipantIdentity, NameError> {
    let parts: Vec<&str> = identity.split('_').collect();
    match parts.as_slice() {
        ["user", user, connection] => Ok(ParticipantIdentity {
            user_id: UserId::new(parse_u64_strict(user).ok_or(NameError::IdentityNumber)?),
            connection_id: ConnectionId::new(connection).map_err(NameError::IdentityConnection)?,
        }),
        [_, _, _] => Err(NameError::IdentityShape),
        other => Err(NameError::IdentityPartCount(other.len())),
    }
}
