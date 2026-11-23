// SPDX-License-Identifier: AGPL-3.0-or-later

#![forbid(unsafe_code)]
#![deny(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects
)]

pub mod audience;
pub mod budget;
pub mod channels;
pub mod classes;
pub mod counts;
pub mod diff;
pub mod event;
pub mod everyone;
pub mod exceptions;
pub mod flags;
pub mod ids;
pub mod json;
pub mod members;
pub mod metrics;
pub mod model;
pub mod prune;
pub mod ranks;
pub mod roles;
pub mod rolesets;
pub mod shared;
pub mod signatures;
pub mod stage;
pub mod threads;
pub mod views;
pub mod viewsets;

#[cfg(feature = "reference")]
pub mod audit;
#[cfg(feature = "reference")]
pub mod reference;

mod apply;
mod bits;
mod eval;
mod hashing;
mod sharded;

pub use apply::load_estimate;
pub use audience::{Audience, Need};
pub use budget::{Budget, Meter, Units};
pub use channels::{
    Channel, ChannelKind, ChannelSpec, ChannelTable, Nsfw, OverwriteSpec, OverwriteTarget, Rules,
};
pub use classes::{Class, Classes};
pub use counts::{MutualOnline, PublicOnline};
pub use diff::{ClassChange, Delta, Group, Regroup, Target, VisibilityChange, diff, diff_units};
pub use event::{ChannelCause, Cutoff, Event, RoleList, Snapshot};
pub use exceptions::{Exceptions, VirtualView};
pub use flags::{ChannelFlags, MemberFlags, Permissions, Visible};
pub use ids::{
    ChannelId, ChannelIdx, ClassId, Epoch, GuildId, RoleId, RoleIdx, RoleSetId, SigId, UserId,
};
pub use members::{MemberEntry, MemberIndex};
pub use metrics::MetricsSink;
pub use model::{GuildFlags, Model};
pub use prune::{Pruned, PushPrune};
pub use roles::{Hoist, RankPos, Role, RoleSpec, RoleTable};
pub use rolesets::{Base, RoleSet, RoleSets};
pub use shared::{
    Config, Effects, MAX_EVENTS, PermsError, PermsShared, Retired, Stats, Status, View,
};
pub use signatures::{Planes, Signature, Signatures};
pub use stage::{Grant, Stage};
pub use threads::ThreadAccess;
pub use views::{Read, Scope};
pub use viewsets::{MAX_CANDIDATES, PAIR_MEMO_LIMIT, Subject, ViewKey, ViewSetId, ViewSetsAudit};
