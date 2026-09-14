// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Confidence {
    Transient,
    Definitive,
    Refused,
    Unknown,
}

impl Confidence {
    pub const ALL: [Self; 4] = [
        Self::Transient,
        Self::Definitive,
        Self::Refused,
        Self::Unknown,
    ];

    pub const fn label(self) -> &'static str {
        match self {
            Self::Transient => "transient",
            Self::Definitive => "definitive",
            Self::Refused => "refused",
            Self::Unknown => "unknown",
        }
    }

    pub const fn is_definitive(self) -> bool {
        matches!(self, Self::Definitive)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum GatewayErrorCode {
    Timeout,
    NoResponders,
    Overloaded,
    GuildNotFound,
    VoiceStatesError,
    PendingJoinsError,
    CallLookupError,
    CallStateError,
    CallPendingJoinsError,
    DisconnectUserError,
    ConfirmConnectionError,
    ConnectionNotFound,
    VoiceStateMismatch,
    VoiceInvalidState,
    VoiceNotSupported,
    EventMutationsPaused,
    InvalidParams,
}

impl GatewayErrorCode {
    pub const ALL: [Self; 17] = [
        Self::Timeout,
        Self::NoResponders,
        Self::Overloaded,
        Self::GuildNotFound,
        Self::VoiceStatesError,
        Self::PendingJoinsError,
        Self::CallLookupError,
        Self::CallStateError,
        Self::CallPendingJoinsError,
        Self::DisconnectUserError,
        Self::ConfirmConnectionError,
        Self::ConnectionNotFound,
        Self::VoiceStateMismatch,
        Self::VoiceInvalidState,
        Self::VoiceNotSupported,
        Self::EventMutationsPaused,
        Self::InvalidParams,
    ];

    pub const fn wire(self) -> &'static str {
        match self {
            Self::Timeout => "timeout",
            Self::NoResponders => "no_responders",
            Self::Overloaded => "overloaded",
            Self::GuildNotFound => "guild_not_found",
            Self::VoiceStatesError => "voice_states_error",
            Self::PendingJoinsError => "pending_joins_error",
            Self::CallLookupError => "call_lookup_error",
            Self::CallStateError => "call_state_error",
            Self::CallPendingJoinsError => "call_pending_joins_error",
            Self::DisconnectUserError => "disconnect_user_error",
            Self::ConfirmConnectionError => "confirm_connection_error",
            Self::ConnectionNotFound => "connection_not_found",
            Self::VoiceStateMismatch => "voice_state_mismatch",
            Self::VoiceInvalidState => "voice_invalid_state",
            Self::VoiceNotSupported => "voice_not_supported",
            Self::EventMutationsPaused => "event_mutations_paused",
            Self::InvalidParams => "invalid_params",
        }
    }

    pub const fn confidence(self) -> Confidence {
        match self {
            Self::Timeout
            | Self::NoResponders
            | Self::Overloaded
            | Self::GuildNotFound
            | Self::VoiceStatesError
            | Self::PendingJoinsError
            | Self::CallLookupError
            | Self::CallStateError
            | Self::CallPendingJoinsError
            | Self::DisconnectUserError
            | Self::ConfirmConnectionError => Confidence::Transient,
            Self::ConnectionNotFound
            | Self::VoiceStateMismatch
            | Self::VoiceInvalidState
            | Self::VoiceNotSupported => Confidence::Definitive,
            Self::EventMutationsPaused | Self::InvalidParams => Confidence::Refused,
        }
    }
}

pub const MISSING_ERROR_LABEL: &str = "missing_error";

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GatewayError {
    Known(GatewayErrorCode),
    Unrecognised(Box<str>),
    Missing,
}

impl GatewayError {
    pub fn from_wire(value: Option<&str>) -> Self {
        match value {
            None => Self::Missing,
            Some(raw) => Self::from_code(raw),
        }
    }

    fn from_code(raw: &str) -> Self {
        GatewayErrorCode::ALL
            .into_iter()
            .find(|code| code.wire() == raw)
            .map_or_else(|| Self::Unrecognised(Box::from(raw)), Self::Known)
    }

    pub const fn confidence(&self) -> Confidence {
        match self {
            Self::Known(code) => code.confidence(),
            Self::Unrecognised(_) | Self::Missing => Confidence::Unknown,
        }
    }

    pub fn label(&self) -> &str {
        match self {
            Self::Known(code) => code.wire(),
            Self::Unrecognised(raw) => raw,
            Self::Missing => MISSING_ERROR_LABEL,
        }
    }

    pub const fn code(&self) -> Option<GatewayErrorCode> {
        match self {
            Self::Known(code) => Some(*code),
            Self::Unrecognised(_) | Self::Missing => None,
        }
    }
}
