// SPDX-License-Identifier: AGPL-3.0-or-later

macro_rules! snowflake {
    ($name:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        pub struct $name(u64);

        impl $name {
            pub const fn new(value: u64) -> Self {
                Self(value)
            }

            pub const fn get(self) -> u64 {
                self.0
            }
        }
    };
}

macro_rules! dense {
    ($name:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        pub struct $name(u32);

        impl $name {
            pub const fn new(value: u32) -> Self {
                Self(value)
            }

            pub const fn get(self) -> u32 {
                self.0
            }

            pub const fn index(self) -> usize {
                self.0 as usize
            }

            pub fn from_index(index: usize) -> Option<Self> {
                u32::try_from(index).ok().map(Self)
            }
        }
    };
}

snowflake!(GuildId);
snowflake!(UserId);
snowflake!(RoleId);
snowflake!(ChannelId);

dense!(RoleIdx);
dense!(RoleSetId);
dense!(ChannelIdx);
dense!(SigId);
dense!(ClassId);

impl RoleSetId {
    pub const EVERYONE: RoleSetId = RoleSetId(0);
}

impl RoleIdx {
    pub const EVERYONE: RoleIdx = RoleIdx(0);
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct Epoch(u64);

impl Epoch {
    pub const ZERO: Epoch = Epoch(0);

    pub const fn new(value: u64) -> Self {
        Self(value)
    }

    pub const fn get(self) -> u64 {
        self.0
    }

    pub const fn next(self) -> Self {
        Self(self.0.wrapping_add(1))
    }
}
