// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;
use std::fmt;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use serde::de::{self, Visitor};
use serde::{Deserialize, Deserializer, Serialize};

use crate::ids::{ApiSecret, Location, RoomKey};
use crate::livekit::auth::{VideoGrant, mint_admin_token};
use crate::livekit::{
    LiveKitApi, LiveKitFault, ParticipantRecord, ParticipantState, ReadResult, RemoveOutcome,
    ServerCredentials,
};
use crate::names::{ParticipantIdentity, format_participant_identity, format_room_name};

pub const TWIRP_PREFIX: &str = "/twirp/livekit.RoomService/";
pub const METHOD_LIST_ROOMS: &str = "ListRooms";
pub const METHOD_LIST_PARTICIPANTS: &str = "ListParticipants";
pub const METHOD_REMOVE_PARTICIPANT: &str = "RemoveParticipant";

const MAX_IDLE_CONNECTIONS_PER_HOST: usize = 1;
const POOL_IDLE_TIMEOUT: Duration = Duration::from_secs(90);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TwirpTimeouts {
    pub connect: Duration,
    pub request: Duration,
}

impl TwirpTimeouts {
    pub const DEFAULT: Self = Self {
        connect: Duration::from_secs(3),
        request: Duration::from_secs(5),
    };
}

impl Default for TwirpTimeouts {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum EndpointError {
    #[error("endpoint scheme is not ws, wss, http or https")]
    UnsupportedScheme,
    #[error("endpoint names no host")]
    MissingHost,
    #[error("endpoint carries a query string or fragment")]
    QueryOrFragment,
    #[error("endpoint contains whitespace")]
    ContainsWhitespace,
}

pub fn twirp_base(endpoint: &str) -> Result<String, EndpointError> {
    if endpoint.contains(char::is_whitespace) {
        return Err(EndpointError::ContainsWhitespace);
    }
    if endpoint.contains('?') || endpoint.contains('#') {
        return Err(EndpointError::QueryOrFragment);
    }

    let rewritten = if let Some(rest) = endpoint.strip_prefix("wss://") {
        format!("https://{rest}")
    } else if let Some(rest) = endpoint.strip_prefix("ws://") {
        format!("http://{rest}")
    } else if endpoint.starts_with("https://") || endpoint.starts_with("http://") {
        endpoint.to_owned()
    } else {
        return Err(EndpointError::UnsupportedScheme);
    };

    let authority_and_path = rewritten
        .split_once("://")
        .expect("a rewritten endpoint always carries a scheme separator")
        .1;
    let host = authority_and_path.split('/').next().unwrap_or_default();
    if host.is_empty() {
        return Err(EndpointError::MissingHost);
    }

    Ok(rewritten.trim_end_matches('/').to_owned())
}

pub fn twirp_url(endpoint: &str, method: &str) -> Result<String, EndpointError> {
    twirp_base(endpoint).map(|base| format!("{base}{TWIRP_PREFIX}{method}"))
}

pub const fn classify_status(status: u16) -> LiveKitFault {
    match status {
        401 | 403 => LiveKitFault::AuthFailed,
        404 => LiveKitFault::NotFound,
        500..=599 => LiveKitFault::Retryable { status },
        other => LiveKitFault::Other { status: other },
    }
}

#[derive(Serialize)]
struct ListRoomsRequest {
    names: [&'static str; 0],
}

#[derive(Serialize)]
struct RoomRequest<'a> {
    room: &'a str,
}

#[derive(Serialize)]
struct RemoveParticipantRequest<'a> {
    room: &'a str,
    identity: &'a str,
}

#[derive(Debug, Deserialize)]
struct ListRoomsResponse {
    #[serde(default)]
    rooms: Vec<RoomWire>,
}

#[derive(Debug, Deserialize)]
struct RoomWire {
    #[serde(default)]
    name: String,
}

#[derive(Debug, Deserialize)]
struct ListParticipantsResponse {
    #[serde(default)]
    participants: Vec<ParticipantWire>,
}

#[derive(Debug, Deserialize)]
struct ParticipantWire {
    #[serde(default)]
    identity: String,
    #[serde(
        rename = "joinedAt",
        alias = "joined_at",
        default,
        deserialize_with = "protojson_int64"
    )]
    joined_at: u64,
    #[serde(default)]
    state: ParticipantState,
    #[serde(rename = "isPublisher", alias = "is_publisher", default)]
    is_publisher: bool,
}

fn protojson_int64<'de, D: Deserializer<'de>>(deserializer: D) -> Result<u64, D::Error> {
    struct Int64Visitor;

    impl Visitor<'_> for Int64Visitor {
        type Value = u64;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("a protojson int64 as a json number or a decimal string")
        }

        fn visit_u64<E: de::Error>(self, value: u64) -> Result<Self::Value, E> {
            Ok(value)
        }

        fn visit_i64<E: de::Error>(self, value: i64) -> Result<Self::Value, E> {
            Ok(u64::try_from(value).unwrap_or_default())
        }

        fn visit_f64<E: de::Error>(self, value: f64) -> Result<Self::Value, E> {
            Err(E::custom(format!("int64 encoded as a float: {value}")))
        }

        fn visit_str<E: de::Error>(self, value: &str) -> Result<Self::Value, E> {
            value
                .parse::<i64>()
                .map(|seconds| u64::try_from(seconds).unwrap_or_default())
                .map_err(|_| E::custom(format!("int64 string is not decimal: {value}")))
        }
    }

    deserializer.deserialize_any(Int64Visitor)
}

impl<'de> Deserialize<'de> for ParticipantState {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct StateVisitor;

        impl Visitor<'_> for StateVisitor {
            type Value = ParticipantState;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a protobuf enum as its name or its number")
            }

            fn visit_u64<E: de::Error>(self, value: u64) -> Result<Self::Value, E> {
                Ok(match value {
                    0 => ParticipantState::Joining,
                    1 => ParticipantState::Joined,
                    2 => ParticipantState::Active,
                    3 => ParticipantState::Disconnected,
                    _ => ParticipantState::Unrecognised,
                })
            }

            fn visit_i64<E: de::Error>(self, value: i64) -> Result<Self::Value, E> {
                match u64::try_from(value) {
                    Ok(value) => self.visit_u64(value),
                    Err(_) => Ok(ParticipantState::Unrecognised),
                }
            }

            fn visit_str<E: de::Error>(self, value: &str) -> Result<Self::Value, E> {
                Ok(match value {
                    "JOINING" => ParticipantState::Joining,
                    "JOINED" => ParticipantState::Joined,
                    "ACTIVE" => ParticipantState::Active,
                    "DISCONNECTED" => ParticipantState::Disconnected,
                    _ => ParticipantState::Unrecognised,
                })
            }
        }

        deserializer.deserialize_any(StateVisitor)
    }
}

fn decode_rooms(body: &str) -> Result<Vec<Box<str>>, serde_json::Error> {
    let response: ListRoomsResponse = serde_json::from_str(body)?;
    Ok(response
        .rooms
        .into_iter()
        .filter(|room| !room.name.is_empty())
        .map(|room| Box::from(room.name.as_str()))
        .collect())
}

fn decode_participants(body: &str) -> Result<Vec<ParticipantRecord>, serde_json::Error> {
    let response: ListParticipantsResponse = serde_json::from_str(body)?;
    Ok(response
        .participants
        .into_iter()
        .map(|participant| ParticipantRecord {
            identity: Box::from(participant.identity.as_str()),
            joined_at_unix_seconds: participant.joined_at,
            state: participant.state,
            is_publisher: participant.is_publisher,
        })
        .collect())
}

#[derive(Debug)]
enum HttpOutcome {
    Success { status: u16, body: String },
    Status { status: u16 },
    Transport { fault: LiveKitFault },
}

fn transport_fault(error: &reqwest::Error) -> LiveKitFault {
    if error.is_connect() {
        return LiveKitFault::Transport { timeout: false };
    }
    if error.is_timeout() {
        return LiveKitFault::Transport { timeout: true };
    }
    LiveKitFault::Transport { timeout: false }
}

fn current_unix_seconds() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |elapsed| elapsed.as_secs())
}

#[derive(Debug)]
struct ServerClient {
    base: String,
    api_key: Box<str>,
    api_secret: ApiSecret,
    http: reqwest::Client,
}

impl ServerClient {
    fn url(&self, method: &str) -> String {
        format!("{}{TWIRP_PREFIX}{method}", self.base)
    }

    async fn post(&self, method: &str, grant: &VideoGrant, body: &impl Serialize) -> HttpOutcome {
        let token = mint_admin_token(
            &self.api_key,
            &self.api_secret,
            grant,
            current_unix_seconds(),
        );
        let response = match self
            .http
            .post(self.url(method))
            .bearer_auth(token)
            .json(body)
            .send()
            .await
        {
            Ok(response) => response,
            Err(error) => {
                return HttpOutcome::Transport {
                    fault: transport_fault(&error),
                };
            }
        };

        let status = response.status();
        if !status.is_success() {
            return HttpOutcome::Status {
                status: status.as_u16(),
            };
        }
        match response.text().await {
            Ok(body) => HttpOutcome::Success {
                status: status.as_u16(),
                body,
            },
            Err(error) => HttpOutcome::Transport {
                fault: transport_fault(&error),
            },
        }
    }
}

#[derive(Debug)]
enum ServerEntry {
    Ready(Box<ServerClient>),
    Unusable(LiveKitFault),
}

#[derive(Debug)]
pub struct TwirpLiveKit {
    servers: HashMap<Location, ServerEntry>,
    timeouts: TwirpTimeouts,
}

impl TwirpLiveKit {
    pub fn new(servers: Vec<ServerCredentials>, timeouts: TwirpTimeouts) -> anyhow::Result<Self> {
        let mut adapter = Self {
            servers: HashMap::new(),
            timeouts,
        };
        adapter.replace_servers(servers)?;
        Ok(adapter)
    }

    pub fn replace_servers(&mut self, servers: Vec<ServerCredentials>) -> anyhow::Result<()> {
        let mut replacement: HashMap<Location, ServerEntry> = HashMap::with_capacity(servers.len());
        for credentials in servers {
            let entry = match twirp_base(&credentials.endpoint) {
                Ok(base) => ServerEntry::Ready(Box::new(ServerClient {
                    base,
                    api_key: credentials.api_key,
                    api_secret: credentials.api_secret,
                    http: self.build_http_client()?,
                })),
                Err(error) => {
                    tracing::warn!(
                        location = %credentials.location,
                        %error,
                        "livekit endpoint is unusable"
                    );
                    ServerEntry::Unusable(LiveKitFault::MalformedEndpoint)
                }
            };
            replacement.insert(credentials.location, entry);
        }
        self.servers = replacement;
        Ok(())
    }

    fn build_http_client(&self) -> anyhow::Result<reqwest::Client> {
        Ok(reqwest::Client::builder()
            .connect_timeout(self.timeouts.connect)
            .timeout(self.timeouts.request)
            .pool_max_idle_per_host(MAX_IDLE_CONNECTIONS_PER_HOST)
            .pool_idle_timeout(POOL_IDLE_TIMEOUT)
            .build()?)
    }

    fn client(&self, location: &Location) -> Result<&ServerClient, LiveKitFault> {
        match self.servers.get(location) {
            None => Err(LiveKitFault::ServerMissing),
            Some(ServerEntry::Unusable(fault)) => Err(*fault),
            Some(ServerEntry::Ready(client)) => Ok(client),
        }
    }
}

impl LiveKitApi for TwirpLiveKit {
    async fn list_rooms(&self, location: &Location) -> ReadResult<Vec<Box<str>>> {
        let client = match self.client(location) {
            Ok(client) => client,
            Err(fault) => return ReadResult::Unreadable(fault),
        };

        let outcome = client
            .post(
                METHOD_LIST_ROOMS,
                &VideoGrant::RoomList,
                &ListRoomsRequest { names: [] },
            )
            .await;

        match outcome {
            HttpOutcome::Transport { fault } => ReadResult::Unreadable(fault),
            HttpOutcome::Status { status } => ReadResult::Unreadable(classify_status(status)),
            HttpOutcome::Success { status, body } => match decode_rooms(&body) {
                Ok(rooms) => ReadResult::Read(rooms),
                Err(_) => ReadResult::Unreadable(LiveKitFault::Other { status }),
            },
        }
    }

    async fn list_participants(
        &self,
        location: &Location,
        room: RoomKey,
    ) -> ReadResult<Vec<ParticipantRecord>> {
        let client = match self.client(location) {
            Ok(client) => client,
            Err(fault) => return ReadResult::Unreadable(fault),
        };

        let room_name = format_room_name(room);
        let outcome = client
            .post(
                METHOD_LIST_PARTICIPANTS,
                &VideoGrant::RoomAdmin {
                    room: room_name.clone(),
                },
                &RoomRequest { room: &room_name },
            )
            .await;

        match outcome {
            HttpOutcome::Transport { fault } => ReadResult::Unreadable(fault),
            HttpOutcome::Status { status } => ReadResult::Unreadable(classify_status(status)),
            HttpOutcome::Success { status, body } => match decode_participants(&body) {
                Ok(participants) => ReadResult::Read(participants),
                Err(_) => ReadResult::Unreadable(LiveKitFault::Other { status }),
            },
        }
    }

    async fn remove_participant(
        &self,
        location: &Location,
        room: RoomKey,
        identity: &ParticipantIdentity,
    ) -> RemoveOutcome {
        let client = match self.client(location) {
            Ok(client) => client,
            Err(fault) => return RemoveOutcome::Failed(fault),
        };

        let room_name = format_room_name(room);
        let participant_identity = format_participant_identity(identity);
        let outcome = client
            .post(
                METHOD_REMOVE_PARTICIPANT,
                &VideoGrant::RoomAdmin {
                    room: room_name.clone(),
                },
                &RemoveParticipantRequest {
                    room: &room_name,
                    identity: &participant_identity,
                },
            )
            .await;

        match outcome {
            HttpOutcome::Transport { fault } => RemoveOutcome::Failed(fault),
            HttpOutcome::Success { .. } => RemoveOutcome::Removed,
            HttpOutcome::Status { status: 404 } => RemoveOutcome::AlreadyGone,
            HttpOutcome::Status { status } => RemoveOutcome::Failed(classify_status(status)),
        }
    }
}
