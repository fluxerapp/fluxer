// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::Serialize;
use serde_json::Value;

use fluxer_svc::transport::Transport;

use crate::evidence::{GatewayVoiceState, PendingJoin};
use crate::gateway::codes::GatewayError;
use crate::gateway::{
    ActiveVoiceRoom, ActiveVoiceRooms, ConfirmOutcome, DisconnectOutcome, GatewayApi, GatewayFault,
    GatewayMethod, Nonce, RepairOutcome,
};
use crate::ids::{
    ChannelId, ConnectionId, GuildId, Location, RegionId, RoomKey, ServerId, UserId, WallMillis,
};

#[derive(Debug, Serialize)]
struct NoParams {}

#[derive(Debug, Serialize)]
struct ChannelReadParams {
    channel_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    guild_id: Option<String>,
}

#[derive(Debug, Serialize)]
struct ConfirmParams {
    channel_id: String,
    connection_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    guild_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    token_nonce: Option<String>,
}

#[derive(Debug, Serialize)]
struct RepairParams {
    channel_id: String,
    user_id: String,
    connection_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    guild_id: Option<String>,
}

#[derive(Debug, Serialize)]
struct DisconnectParams {
    channel_id: String,
    user_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    guild_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    connection_id: Option<String>,
}

fn guild_param(room: RoomKey) -> Option<String> {
    room.guild_id().map(|guild_id| guild_id.to_string())
}

fn channel_read_params(room: RoomKey) -> ChannelReadParams {
    ChannelReadParams {
        channel_id: room.channel_id().to_string(),
        guild_id: guild_param(room),
    }
}

fn confirm_params(room: RoomKey, connection: &ConnectionId, nonce: &Nonce) -> ConfirmParams {
    ConfirmParams {
        channel_id: room.channel_id().to_string(),
        connection_id: connection.as_str().to_owned(),
        guild_id: guild_param(room),
        token_nonce: nonce.as_str().map(str::to_owned),
    }
}

fn repair_params(room: RoomKey, user_id: UserId, connection: &ConnectionId) -> RepairParams {
    RepairParams {
        channel_id: room.channel_id().to_string(),
        user_id: user_id.to_string(),
        connection_id: connection.as_str().to_owned(),
        guild_id: guild_param(room),
    }
}

fn disconnect_params(
    room: RoomKey,
    user_id: UserId,
    connection: Option<&ConnectionId>,
) -> DisconnectParams {
    DisconnectParams {
        channel_id: room.channel_id().to_string(),
        user_id: user_id.to_string(),
        guild_id: guild_param(room),
        connection_id: connection.map(|id| id.as_str().to_owned()),
    }
}

pub fn classify_transport_error(error: &anyhow::Error) -> GatewayFault {
    if error.chain().any(|cause| {
        cause
            .to_string()
            .to_ascii_lowercase()
            .contains("no responders")
    }) {
        return GatewayFault::NoResponders;
    }
    if error
        .chain()
        .any(|cause| cause.is::<tokio::time::error::Elapsed>())
    {
        return GatewayFault::Timeout;
    }
    if error.chain().any(|cause| {
        let text = cause.to_string().to_ascii_lowercase();
        text.contains("timed out") || text.contains("timeout") || text.contains("deadline")
    }) {
        return GatewayFault::Timeout;
    }
    GatewayFault::TransportFailed
}

pub fn decode_envelope(bytes: &[u8]) -> Result<Value, GatewayFault> {
    let Ok(body) = serde_json::from_slice::<Value>(bytes) else {
        return Err(GatewayFault::DecodeFailed);
    };
    let Some(object) = body.as_object() else {
        return Err(GatewayFault::DecodeFailed);
    };
    let Some(ok) = object.get("ok").and_then(Value::as_bool) else {
        return Err(GatewayFault::DecodeFailed);
    };
    if ok {
        return Ok(object.get("result").cloned().unwrap_or(Value::Null));
    }
    let error = object.get("error").and_then(Value::as_str);
    Err(GatewayFault::NotOk(GatewayError::from_wire(error)))
}

fn required<'a>(result: &'a Value, key: &str) -> Result<&'a Value, GatewayFault> {
    result.get(key).ok_or(GatewayFault::DecodeFailed)
}

fn optional<'a>(result: &'a Value, key: &str) -> Option<&'a Value> {
    match result.get(key) {
        None | Some(Value::Null) => None,
        Some(value) => Some(value),
    }
}

fn entries(result: &Value, key: &str) -> Result<Vec<Value>, GatewayFault> {
    required(result, key)?
        .as_array()
        .cloned()
        .ok_or(GatewayFault::DecodeFailed)
}

fn decimal_u64(value: &str) -> Option<u64> {
    if value.is_empty() || !value.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    value.parse::<u64>().ok()
}

fn snowflake(value: &Value) -> Result<u64, GatewayFault> {
    match value {
        Value::String(raw) => decimal_u64(raw).ok_or(GatewayFault::DecodeFailed),
        Value::Number(number) => number.as_u64().ok_or(GatewayFault::DecodeFailed),
        Value::Null | Value::Bool(_) | Value::Array(_) | Value::Object(_) => {
            Err(GatewayFault::DecodeFailed)
        }
    }
}

fn text(value: &Value) -> Result<&str, GatewayFault> {
    value.as_str().ok_or(GatewayFault::DecodeFailed)
}

fn count(value: &Value) -> Result<u32, GatewayFault> {
    let number = value.as_u64().ok_or(GatewayFault::DecodeFailed)?;
    u32::try_from(number).map_err(|_| GatewayFault::DecodeFailed)
}

fn flag(result: &Value, key: &str) -> Result<bool, GatewayFault> {
    match optional(result, key) {
        None => Ok(false),
        Some(value) => value.as_bool().ok_or(GatewayFault::DecodeFailed),
    }
}

fn reason(result: &Value) -> Result<Option<Box<str>>, GatewayFault> {
    match optional(result, "reason") {
        None => Ok(None),
        Some(value) => Ok(Some(Box::from(text(value)?))),
    }
}

fn error_of(result: &Value) -> GatewayError {
    GatewayError::from_wire(optional(result, "error").and_then(Value::as_str))
}

fn connection_of(value: &Value) -> Result<Option<ConnectionId>, GatewayFault> {
    let raw = text(value)?;
    if raw.is_empty() {
        return Ok(None);
    }
    ConnectionId::new(raw)
        .map(Some)
        .map_err(|_| GatewayFault::DecodeFailed)
}

fn location_hint(entry: &Value) -> Result<Option<Location>, GatewayFault> {
    let region = match optional(entry, "region_id") {
        None => None,
        Some(value) => Some(RegionId::new(text(value)?).map_err(|_| GatewayFault::DecodeFailed)?),
    };
    let server = match optional(entry, "server_id") {
        None => None,
        Some(value) => Some(ServerId::new(text(value)?).map_err(|_| GatewayFault::DecodeFailed)?),
    };
    match (region, server) {
        (Some(region), Some(server)) => Ok(Some(Location::new(region, server))),
        (Some(_), None) | (None, Some(_)) | (None, None) => Ok(None),
    }
}

fn room_of(entry: &Value) -> Result<RoomKey, GatewayFault> {
    let channel_id = ChannelId::new(snowflake(required(entry, "channel_id")?)?);
    match optional(entry, "guild_id") {
        None => Ok(RoomKey::Dm { channel_id }),
        Some(value) => Ok(RoomKey::Guild {
            guild_id: GuildId::new(snowflake(value)?),
            channel_id,
        }),
    }
}

fn decode_active_voice_room(entry: &Value) -> Result<ActiveVoiceRoom, GatewayFault> {
    Ok(ActiveVoiceRoom {
        room: room_of(entry)?,
        voice_state_count: count(required(entry, "voice_state_count")?)?,
    })
}

pub fn decode_active_voice_rooms(result: &Value) -> Result<ActiveVoiceRooms, GatewayFault> {
    let node_count = count(required(result, "node_count")?)?;
    let mut rooms = Vec::new();
    let mut unparseable_rooms = 0u32;

    for entry in entries(result, "rooms")? {
        if let Ok(room) = decode_active_voice_room(&entry) {
            rooms.push(room);
        } else {
            unparseable_rooms = unparseable_rooms.saturating_add(1);
        }
    }

    Ok(ActiveVoiceRooms {
        node_count,
        rooms,
        unparseable_rooms,
    })
}

fn decode_voice_state(entry: &Value) -> Result<GatewayVoiceState, GatewayFault> {
    let connection = match optional(entry, "connection_id") {
        None => None,
        Some(value) => connection_of(value)?,
    };
    let guild_id = match optional(entry, "guild_id") {
        None => None,
        Some(value) => Some(GuildId::new(snowflake(value)?)),
    };

    Ok(GatewayVoiceState {
        connection,
        user_id: UserId::new(snowflake(required(entry, "user_id")?)?),
        channel_id: ChannelId::new(snowflake(required(entry, "channel_id")?)?),
        guild_id,
        hint: location_hint(entry)?,
    })
}

pub fn decode_voice_states(result: &Value) -> Result<Vec<GatewayVoiceState>, GatewayFault> {
    entries(result, "voice_states")?
        .iter()
        .map(decode_voice_state)
        .collect()
}

fn decode_pending_join(entry: &Value) -> Result<PendingJoin, GatewayFault> {
    let connection = ConnectionId::new(text(required(entry, "connection_id")?)?)
        .map_err(|_| GatewayFault::DecodeFailed)?;
    let expires_at = required(entry, "expires_at")?
        .as_i64()
        .ok_or(GatewayFault::DecodeFailed)?;
    let nonce = match optional(entry, "token_nonce") {
        None => Nonce::Empty,
        Some(value) => Nonce::from_wire(Some(text(value)?)),
    };

    Ok(PendingJoin {
        connection,
        user_id: UserId::new(snowflake(required(entry, "user_id")?)?),
        expires_at: WallMillis::new(u64::try_from(expires_at).unwrap_or(0)),
        nonce,
    })
}

pub fn decode_pending_joins(result: &Value) -> Result<Vec<PendingJoin>, GatewayFault> {
    entries(result, "pending_joins")?
        .iter()
        .map(decode_pending_join)
        .collect()
}

pub fn decode_confirm(result: &Value) -> Result<ConfirmOutcome, GatewayFault> {
    if !flag(result, "success")? {
        return Ok(ConfirmOutcome::Failed(error_of(result)));
    }
    if flag(result, "call_not_found")? {
        return Ok(ConfirmOutcome::CallNotFound);
    }
    if flag(result, "already_confirmed")? {
        return Ok(ConfirmOutcome::AlreadyConfirmed);
    }
    Ok(ConfirmOutcome::Confirmed)
}

pub fn decode_repair(result: &Value) -> Result<RepairOutcome, GatewayFault> {
    if !flag(result, "success")? {
        return Ok(RepairOutcome::Failed(error_of(result)));
    }
    match optional(result, "repaired") {
        None => Ok(RepairOutcome::NoChange),
        Some(value) => match value.as_bool().ok_or(GatewayFault::DecodeFailed)? {
            true => Ok(RepairOutcome::Repaired),
            false => Ok(RepairOutcome::NoChange),
        },
    }
}

pub fn decode_disconnect(result: &Value) -> Result<DisconnectOutcome, GatewayFault> {
    if !flag(result, "success")? {
        return Ok(DisconnectOutcome::Failed(error_of(result)));
    }
    if flag(result, "ignored")? {
        return Ok(DisconnectOutcome::Ignored {
            reason: reason(result)?,
        });
    }
    if flag(result, "call_not_found")? {
        return Ok(DisconnectOutcome::CallNotFound);
    }
    Ok(DisconnectOutcome::Removed)
}

pub struct NatsGateway<T> {
    transport: T,
}

impl<T> NatsGateway<T> {
    pub const fn new(transport: T) -> Self {
        Self { transport }
    }

    pub const fn transport(&self) -> &T {
        &self.transport
    }
}

impl<T: Transport> NatsGateway<T> {
    async fn call<P: Serialize>(
        &self,
        method: GatewayMethod,
        params: &P,
    ) -> Result<Value, GatewayFault> {
        let Ok(payload) = serde_json::to_vec(params) else {
            return Err(GatewayFault::TransportFailed);
        };
        let bytes = self
            .transport
            .request(&method.subject(), &payload, method.deadline())
            .await
            .map_err(|error| classify_transport_error(&error))?;
        decode_envelope(&bytes)
    }
}

impl<T: Transport> GatewayApi for NatsGateway<T> {
    async fn active_voice_rooms(&self) -> Result<ActiveVoiceRooms, GatewayFault> {
        let result = self
            .call(GatewayMethod::ActiveVoiceRooms, &NoParams {})
            .await?;
        decode_active_voice_rooms(&result)
    }

    async fn voice_states_for_channel(
        &self,
        room: RoomKey,
    ) -> Result<Vec<GatewayVoiceState>, GatewayFault> {
        let result = self
            .call(
                GatewayMethod::VoiceStatesForChannel,
                &channel_read_params(room),
            )
            .await?;
        decode_voice_states(&result)
    }

    async fn pending_joins_for_channel(
        &self,
        room: RoomKey,
    ) -> Result<Vec<PendingJoin>, GatewayFault> {
        let result = self
            .call(
                GatewayMethod::PendingJoinsForChannel,
                &channel_read_params(room),
            )
            .await?;
        decode_pending_joins(&result)
    }

    async fn confirm_connection(
        &self,
        room: RoomKey,
        connection: &ConnectionId,
        nonce: &Nonce,
    ) -> Result<ConfirmOutcome, GatewayFault> {
        let result = self
            .call(
                GatewayMethod::ConfirmConnection,
                &confirm_params(room, connection, nonce),
            )
            .await?;
        decode_confirm(&result)
    }

    async fn repair_state_from_cache(
        &self,
        room: RoomKey,
        user_id: UserId,
        connection: &ConnectionId,
    ) -> Result<RepairOutcome, GatewayFault> {
        let result = self
            .call(
                GatewayMethod::RepairStateFromCache,
                &repair_params(room, user_id, connection),
            )
            .await?;
        decode_repair(&result)
    }

    async fn disconnect_user_if_in_channel(
        &self,
        room: RoomKey,
        user_id: UserId,
        connection: Option<&ConnectionId>,
    ) -> Result<DisconnectOutcome, GatewayFault> {
        let result = self
            .call(
                GatewayMethod::DisconnectUserIfInChannel,
                &disconnect_params(room, user_id, connection),
            )
            .await?;
        decode_disconnect(&result)
    }
}
