// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::budget::Meter;
use crate::flags::Permissions;
use crate::ids::{ChannelId, UserId};
use crate::shared::{PermsError, View};
use crate::views::Scope;

fn push_decimal(out: &mut Vec<u8>, value: u64) {
    let mut digits = [0u8; 20];
    let mut rest = value;
    let mut start = digits.len();
    loop {
        start = start.saturating_sub(1);
        if let Some(slot) = digits.get_mut(start) {
            *slot = b'0'.saturating_add(u8::try_from(rest % 10).unwrap_or(0));
        }
        rest /= 10;
        if rest == 0 || start == 0 {
            break;
        }
    }
    out.extend_from_slice(digits.get(start..).unwrap_or_default());
}

pub fn permissions_reply(bits: Permissions) -> Vec<u8> {
    let mut out = Vec::with_capacity(40);
    out.extend_from_slice(b"{\"permissions\":\"");
    push_decimal(&mut out, bits.bits());
    out.extend_from_slice(b"\"}");
    out
}

pub fn check_reply(granted: bool) -> Vec<u8> {
    if granted {
        b"{\"has_permission\":true}".to_vec()
    } else {
        b"{\"has_permission\":false}".to_vec()
    }
}

pub fn channel_ids_reply(ids: &[ChannelId]) -> Vec<u8> {
    let mut out = Vec::with_capacity(ids.len().saturating_mul(23).saturating_add(18));
    out.extend_from_slice(b"{\"channel_ids\":[");
    for (i, id) in ids.iter().enumerate() {
        if i > 0 {
            out.push(b',');
        }
        out.push(b'"');
        push_decimal(&mut out, id.get());
        out.push(b'"');
    }
    out.extend_from_slice(b"]}");
    out
}

impl View<'_> {
    pub fn user_permissions_reply(
        &self,
        user: UserId,
        scope: Scope,
    ) -> Result<Vec<u8>, PermsError> {
        self.permissions(user, scope).map(permissions_reply)
    }

    pub fn check_permission_reply(
        &self,
        user: UserId,
        scope: Scope,
        bits: Permissions,
    ) -> Result<Vec<u8>, PermsError> {
        self.check(user, scope, bits).map(check_reply)
    }

    pub fn viewable_channels_reply(&self, user: UserId, meter: &mut Meter) -> Vec<u8> {
        channel_ids_reply(&self.viewable(user, meter))
    }
}
