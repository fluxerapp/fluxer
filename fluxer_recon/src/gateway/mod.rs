// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod codes;
pub mod nats;

use std::future::Future;
use std::time::Duration;

use crate::evidence::{GatewayVoiceState, PendingJoin};
use crate::gateway::codes::{Confidence, GatewayError};
use crate::ids::{ConnectionId, RoomKey, UserId};
use crate::ledger::RepairVerdict;

pub const SUBJECT_PREFIX: &str = "rpc.gateway.";

pub const MUTATION_DEADLINE: Duration = Duration::from_secs(4);
pub const CHANNEL_READ_DEADLINE: Duration = Duration::from_secs(5);
pub const CENSUS_DEADLINE: Duration = Duration::from_secs(12);

pub const GATEWAY_GUILD_CALL_BUDGET: Duration = Duration::from_secs(4);
pub const GATEWAY_CENSUS_NODE_RPC_BUDGET: Duration = Duration::from_secs(10);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MethodClass {
    Read,
    Constructive,
    Destructive,
}

impl MethodClass {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Read => "read",
            Self::Constructive => "constructive",
            Self::Destructive => "destructive",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum GatewayMethod {
    ActiveVoiceRooms,
    VoiceStatesForChannel,
    PendingJoinsForChannel,
    ConfirmConnection,
    RepairStateFromCache,
    DisconnectUserIfInChannel,
}

impl GatewayMethod {
    pub const ALL: [Self; 6] = [
        Self::ActiveVoiceRooms,
        Self::VoiceStatesForChannel,
        Self::PendingJoinsForChannel,
        Self::ConfirmConnection,
        Self::RepairStateFromCache,
        Self::DisconnectUserIfInChannel,
    ];

    pub const fn name(self) -> &'static str {
        match self {
            Self::ActiveVoiceRooms => "process.active_voice_rooms",
            Self::VoiceStatesForChannel => "voice.get_voice_states_for_channel",
            Self::PendingJoinsForChannel => "voice.get_pending_joins_for_channel",
            Self::ConfirmConnection => "voice.confirm_connection",
            Self::RepairStateFromCache => "voice.repair_state_from_cache",
            Self::DisconnectUserIfInChannel => "voice.disconnect_user_if_in_channel",
        }
    }

    pub const fn deadline(self) -> Duration {
        match self {
            Self::ActiveVoiceRooms => CENSUS_DEADLINE,
            Self::VoiceStatesForChannel | Self::PendingJoinsForChannel => CHANNEL_READ_DEADLINE,
            Self::ConfirmConnection
            | Self::RepairStateFromCache
            | Self::DisconnectUserIfInChannel => MUTATION_DEADLINE,
        }
    }

    pub const fn class(self) -> MethodClass {
        match self {
            Self::ActiveVoiceRooms | Self::VoiceStatesForChannel | Self::PendingJoinsForChannel => {
                MethodClass::Read
            }
            Self::ConfirmConnection | Self::RepairStateFromCache => MethodClass::Constructive,
            Self::DisconnectUserIfInChannel => MethodClass::Destructive,
        }
    }

    pub fn subject(self) -> String {
        let mut subject = String::with_capacity(SUBJECT_PREFIX.len() + self.name().len());
        subject.push_str(SUBJECT_PREFIX);
        subject.push_str(self.name());
        subject
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FaultOrigin {
    CouldNotAsk,
    Unintelligible,
    GatewaySaidNo,
}

impl FaultOrigin {
    pub const fn label(self) -> &'static str {
        match self {
            Self::CouldNotAsk => "could_not_ask",
            Self::Unintelligible => "unintelligible",
            Self::GatewaySaidNo => "gateway_said_no",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GatewayFault {
    Timeout,
    NoResponders,
    TransportFailed,
    DecodeFailed,
    NotOk(GatewayError),
}

impl GatewayFault {
    pub const fn label(&self) -> &'static str {
        match self {
            Self::Timeout => "timeout",
            Self::NoResponders => "no_responders",
            Self::TransportFailed => "transport_failed",
            Self::DecodeFailed => "decode_failed",
            Self::NotOk(_) => "not_ok",
        }
    }

    pub const fn origin(&self) -> FaultOrigin {
        match self {
            Self::Timeout | Self::NoResponders | Self::TransportFailed => FaultOrigin::CouldNotAsk,
            Self::DecodeFailed => FaultOrigin::Unintelligible,
            Self::NotOk(_) => FaultOrigin::GatewaySaidNo,
        }
    }

    pub const fn is_evidence(&self) -> bool {
        matches!(self.origin(), FaultOrigin::GatewaySaidNo)
    }

    pub const fn confidence(&self) -> Confidence {
        match self {
            Self::Timeout | Self::NoResponders | Self::TransportFailed => Confidence::Transient,
            Self::DecodeFailed => Confidence::Unknown,
            Self::NotOk(error) => error.confidence(),
        }
    }

    pub const fn error(&self) -> Option<&GatewayError> {
        match self {
            Self::Timeout | Self::NoResponders | Self::TransportFailed | Self::DecodeFailed => None,
            Self::NotOk(error) => Some(error),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub enum Nonce {
    #[default]
    Empty,
    Value(Box<str>),
}

impl Nonce {
    pub fn from_wire(value: Option<&str>) -> Self {
        match value {
            None => Self::Empty,
            Some("") => Self::Empty,
            Some(raw) => Self::Value(Box::from(raw)),
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::Empty => None,
            Self::Value(value) => Some(value),
        }
    }

    pub const fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ActiveVoiceRoom {
    pub room: RoomKey,
    pub voice_state_count: u32,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ActiveVoiceRooms {
    pub node_count: u32,
    pub rooms: Vec<ActiveVoiceRoom>,
    pub unparseable_rooms: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConfirmOutcome {
    Confirmed,
    AlreadyConfirmed,
    CallNotFound,
    Failed(GatewayError),
}

impl ConfirmOutcome {
    pub const fn succeeded(&self) -> bool {
        matches!(self, Self::Confirmed | Self::AlreadyConfirmed)
    }

    pub const fn label(&self) -> &'static str {
        match self {
            Self::Confirmed => "confirmed",
            Self::AlreadyConfirmed => "already_confirmed",
            Self::CallNotFound => "call_not_found",
            Self::Failed(_) => "failed",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RepairOutcome {
    Repaired,
    NoChange,
    Failed(GatewayError),
}

impl RepairOutcome {
    pub const fn verdict(&self) -> RepairVerdict {
        match self {
            Self::Repaired => RepairVerdict::Repaired,
            Self::NoChange => RepairVerdict::NoChange,
            Self::Failed(error) => match error.confidence() {
                Confidence::Definitive => RepairVerdict::NotRepairable,
                Confidence::Transient | Confidence::Refused | Confidence::Unknown => {
                    RepairVerdict::NoChange
                }
            },
        }
    }

    pub const fn label(&self) -> &'static str {
        match self {
            Self::Repaired => "repaired",
            Self::NoChange => "no_change",
            Self::Failed(_) => "failed",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DisconnectOutcome {
    Removed,
    Ignored { reason: Option<Box<str>> },
    CallNotFound,
    Failed(GatewayError),
}

impl DisconnectOutcome {
    pub const fn removed_state(&self) -> bool {
        matches!(self, Self::Removed)
    }

    pub const fn label(&self) -> &'static str {
        match self {
            Self::Removed => "removed",
            Self::Ignored { .. } => "ignored",
            Self::CallNotFound => "call_not_found",
            Self::Failed(_) => "failed",
        }
    }
}

pub trait GatewayApi {
    fn active_voice_rooms(
        &self,
    ) -> impl Future<Output = Result<ActiveVoiceRooms, GatewayFault>> + Send;

    fn voice_states_for_channel(
        &self,
        room: RoomKey,
    ) -> impl Future<Output = Result<Vec<GatewayVoiceState>, GatewayFault>> + Send;

    fn pending_joins_for_channel(
        &self,
        room: RoomKey,
    ) -> impl Future<Output = Result<Vec<PendingJoin>, GatewayFault>> + Send;

    fn confirm_connection(
        &self,
        room: RoomKey,
        connection: &ConnectionId,
        nonce: &Nonce,
    ) -> impl Future<Output = Result<ConfirmOutcome, GatewayFault>> + Send;

    fn repair_state_from_cache(
        &self,
        room: RoomKey,
        user_id: UserId,
        connection: &ConnectionId,
    ) -> impl Future<Output = Result<RepairOutcome, GatewayFault>> + Send;

    fn disconnect_user_if_in_channel(
        &self,
        room: RoomKey,
        user_id: UserId,
        connection: Option<&ConnectionId>,
    ) -> impl Future<Output = Result<DisconnectOutcome, GatewayFault>> + Send;
}
