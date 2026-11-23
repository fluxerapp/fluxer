// SPDX-License-Identifier: AGPL-3.0-or-later

#![cfg(feature = "nif")]
#![deny(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects
)]

pub mod decode;
pub mod encode;
pub mod resource;

use rustler::Env;

use resource::{Counts, MemberList, PayloadRes};

pub mod atoms {
    pub use rustler::types::atom::{error, false_, nil, ok, true_};

    rustler::atoms! {
        none,
        some,
        changed,
        unchanged,
        immediate,
        deferred,
        online,
        idle,
        dnd,
        invisible,
        offline,
        other_status,
        connected,
        disconnected,
        updates_on,
        updates_off,
        flush,
        member_upsert,
        member_remove,
        presence,
        connection,
        subscribe,
        drop_session,
        view_gained,
        permissions,
        all_lists,
        channel_list,
        set_updates,
        config,
        ml_config,
        snapshot,
        ml_snapshot,
        encoded_item,
        subscriptions,
        subscription,
        ranges_reply,
        counts_reply,
        ids_reply,
        subscriptions_reply,
        session_ranges,
        channel_counts,
        page_ids,
        page,
        packed_ids,
        needs_dirty,
        broken,
        gone,
        bad_input,
        keep_timer,
        arm_timer,
        cancel_timer,
        to_sessions,
        to_session,
        dispatch,
        effects,
        done,
        page_done,
        partial,
        need_items,
        mask_update,
        mask_query,
        supersets,
        masks,
        stale,
    }
}

pub fn link(env: Env<'_>) -> bool {
    env.register::<MemberList>().is_ok()
        && env.register::<Counts>().is_ok()
        && env.register::<PayloadRes>().is_ok()
}

pub mod nifs;
