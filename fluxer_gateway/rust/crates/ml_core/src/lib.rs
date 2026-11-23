// SPDX-License-Identifier: AGPL-3.0-or-later

#![forbid(unsafe_code)]
#![deny(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects
)]

pub mod blocked;
pub mod budget;
pub mod casefold;
pub mod casefold_table;
pub mod classes;
pub mod core;
pub mod digits;
pub mod event;
pub mod flags;
pub mod fragments;
pub mod id_order;
pub mod ids;
pub mod masks;
pub mod members;
pub mod names;
pub mod payloads;
pub mod pending;
pub mod ranges;
#[cfg(feature = "reference")]
pub mod reference;
pub mod render;
pub mod sections;
pub mod sessions;

pub use crate::core::{
    Accounting, Config, Core, CoreError, Dispatch, Effects, Group, MAX_EVENTS, Mode, PageQuery,
    Row, Stats, SyncBody, SyncOp, Target,
};
pub use crate::event::{Change, Connection, Event, Snapshot, SyncScope, Timing, Updates};
pub use crate::flags::Status;
pub use crate::ids::{ListKey, SessionKey, Slot};
pub use crate::masks::{MaskReply, MaskUpdate};
pub use crate::payloads::{Guarded, Payload};
pub use crate::pending::{Mark, TimerAction};
pub use crate::ranges::{Range, Ranges};
pub use crate::render::{Done, EncodedItem, Page, Rendered, Sent, Step};
pub use crate::sections::Label;
pub use perm_core::{ChannelId, ClassId, Delta, GuildId, RoleId, RoleIdx, RoleSetId, UserId};
