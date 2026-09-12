// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt;

const REDACTED: &str = "***";

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum IdError {
    #[error("identifier is empty")]
    Empty,
    #[error("identifier contains whitespace")]
    ContainsWhitespace,
    #[error("identifier contains an underscore")]
    ContainsUnderscore,
}

macro_rules! u64_newtype {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(u64);

        impl $name {
            pub const fn new(value: u64) -> Self {
                Self(value)
            }

            pub const fn get(self) -> u64 {
                self.0
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Display::fmt(&self.0, f)
            }
        }
    };
}

u64_newtype!(GuildId);
u64_newtype!(ChannelId);
u64_newtype!(UserId);
u64_newtype!(TurnId);
u64_newtype!(Epoch);
u64_newtype!(Millis);
u64_newtype!(WallMillis);

impl TurnId {
    pub const FIRST: Self = Self(0);

    pub const fn next(self) -> Self {
        Self(self.0.saturating_add(1))
    }
}

impl Epoch {
    pub const FIRST: Self = Self(0);

    pub const fn next(self) -> Self {
        Self(self.0.saturating_add(1))
    }
}

impl Millis {
    pub const ZERO: Self = Self(0);

    pub const fn saturating_since(self, earlier: Self) -> u64 {
        self.0.saturating_sub(earlier.0)
    }

    pub const fn saturating_add_millis(self, delta: u64) -> Self {
        Self(self.0.saturating_add(delta))
    }
}

impl WallMillis {
    pub const ZERO: Self = Self(0);

    pub const fn saturating_since(self, earlier: Self) -> u64 {
        self.0.saturating_sub(earlier.0)
    }

    pub const fn saturating_add_millis(self, delta: u64) -> Self {
        Self(self.0.saturating_add(delta))
    }
}

fn checked_text(value: &str) -> Result<Box<str>, IdError> {
    if value.is_empty() {
        return Err(IdError::Empty);
    }
    if value.contains(char::is_whitespace) {
        return Err(IdError::ContainsWhitespace);
    }
    Ok(Box::from(value))
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConnectionId(Box<str>);

impl ConnectionId {
    pub fn new(value: &str) -> Result<Self, IdError> {
        let text = checked_text(value)?;
        if text.contains('_') {
            return Err(IdError::ContainsUnderscore);
        }
        Ok(Self(text))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ConnectionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegionId(Box<str>);

impl RegionId {
    pub fn new(value: &str) -> Result<Self, IdError> {
        checked_text(value).map(Self)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for RegionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ServerId(Box<str>);

impl ServerId {
    pub fn new(value: &str) -> Result<Self, IdError> {
        checked_text(value).map(Self)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ServerId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct ApiSecret(Box<str>);

impl ApiSecret {
    pub fn new(value: &str) -> Self {
        Self(Box::from(value))
    }

    pub fn expose(&self) -> &str {
        &self.0
    }
}

impl fmt::Debug for ApiSecret {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(REDACTED)
    }
}

impl fmt::Display for ApiSecret {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(REDACTED)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Scope {
    Guild,
    Dm,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RoomKey {
    Guild {
        guild_id: GuildId,
        channel_id: ChannelId,
    },
    Dm {
        channel_id: ChannelId,
    },
}

impl RoomKey {
    pub const fn scope(self) -> Scope {
        match self {
            Self::Guild { .. } => Scope::Guild,
            Self::Dm { .. } => Scope::Dm,
        }
    }

    pub const fn channel_id(self) -> ChannelId {
        match self {
            Self::Guild { channel_id, .. } => channel_id,
            Self::Dm { channel_id } => channel_id,
        }
    }

    pub const fn guild_id(self) -> Option<GuildId> {
        match self {
            Self::Guild { guild_id, .. } => Some(guild_id),
            Self::Dm { .. } => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub region: RegionId,
    pub server: ServerId,
}

impl Location {
    pub const fn new(region: RegionId, server: ServerId) -> Self {
        Self { region, server }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.region, self.server)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConnectionKey {
    pub room: RoomKey,
    pub connection: ConnectionId,
}

impl ConnectionKey {
    pub const fn new(room: RoomKey, connection: ConnectionId) -> Self {
        Self { room, connection }
    }
}
