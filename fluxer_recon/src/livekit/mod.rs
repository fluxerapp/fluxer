// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod auth;
pub mod twirp;

use std::future::Future;

use crate::ids::{ApiSecret, Location, RoomKey};
use crate::names::ParticipantIdentity;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum LiveKitFault {
    Retryable { status: u16 },
    NotFound,
    AuthFailed,
    Other { status: u16 },
    Transport { timeout: bool },
    ServerMissing,
    MalformedEndpoint,
}

impl LiveKitFault {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Retryable { .. } => "retryable",
            Self::NotFound => "not_found",
            Self::AuthFailed => "auth_failed",
            Self::Other { .. } => "other",
            Self::Transport { timeout: true } => "transport_timeout",
            Self::Transport { timeout: false } => "transport_refused",
            Self::ServerMissing => "server_missing",
            Self::MalformedEndpoint => "malformed_endpoint",
        }
    }

    pub const fn is_retryable(self) -> bool {
        match self {
            Self::Retryable { .. } | Self::Transport { .. } => true,
            Self::NotFound
            | Self::AuthFailed
            | Self::Other { .. }
            | Self::ServerMissing
            | Self::MalformedEndpoint => false,
        }
    }

    pub const fn is_auth_failure(self) -> bool {
        matches!(self, Self::AuthFailed)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReadResult<T> {
    Read(T),
    Unreadable(LiveKitFault),
}

impl<T> ReadResult<T> {
    pub fn is_readable(&self) -> bool {
        matches!(self, Self::Read(_))
    }

    pub fn as_read(&self) -> Option<&T> {
        match self {
            Self::Read(value) => Some(value),
            Self::Unreadable(_) => None,
        }
    }

    pub fn into_read(self) -> Option<T> {
        match self {
            Self::Read(value) => Some(value),
            Self::Unreadable(_) => None,
        }
    }

    pub fn fault(&self) -> Option<LiveKitFault> {
        match self {
            Self::Read(_) => None,
            Self::Unreadable(fault) => Some(*fault),
        }
    }

    pub fn map<U>(self, transform: impl FnOnce(T) -> U) -> ReadResult<U> {
        match self {
            Self::Read(value) => ReadResult::Read(transform(value)),
            Self::Unreadable(fault) => ReadResult::Unreadable(fault),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RemoveOutcome {
    Removed,
    AlreadyGone,
    Failed(LiveKitFault),
}

impl RemoveOutcome {
    pub const fn is_success(self) -> bool {
        match self {
            Self::Removed | Self::AlreadyGone => true,
            Self::Failed(_) => false,
        }
    }

    pub const fn fault(self) -> Option<LiveKitFault> {
        match self {
            Self::Removed | Self::AlreadyGone => None,
            Self::Failed(fault) => Some(fault),
        }
    }

    pub const fn label(self) -> &'static str {
        match self {
            Self::Removed => "removed",
            Self::AlreadyGone => "already_gone",
            Self::Failed(_) => "failed",
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum ParticipantState {
    #[default]
    Joining,
    Joined,
    Active,
    Disconnected,
    Unrecognised,
}

impl ParticipantState {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Joining => "joining",
            Self::Joined => "joined",
            Self::Active => "active",
            Self::Disconnected => "disconnected",
            Self::Unrecognised => "unrecognised",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParticipantRecord {
    pub identity: Box<str>,
    pub joined_at_unix_seconds: u64,
    pub state: ParticipantState,
    pub is_publisher: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ServerCredentials {
    pub location: Location,
    pub endpoint: Box<str>,
    pub api_key: Box<str>,
    pub api_secret: ApiSecret,
}

pub trait LiveKitApi {
    fn list_rooms(
        &self,
        location: &Location,
    ) -> impl Future<Output = ReadResult<Vec<Box<str>>>> + Send;

    fn list_participants(
        &self,
        location: &Location,
        room: RoomKey,
    ) -> impl Future<Output = ReadResult<Vec<ParticipantRecord>>> + Send;

    fn remove_participant(
        &self,
        location: &Location,
        room: RoomKey,
        identity: &ParticipantIdentity,
    ) -> impl Future<Output = RemoveOutcome> + Send;
}
