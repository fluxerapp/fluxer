// SPDX-License-Identifier: AGPL-3.0-or-later

pub use perm_core::{ChannelId, ClassId, GuildId, RoleId, RoleIdx, RoleSetId, UserId};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Slot(u32);

impl Slot {
    pub const fn new(value: u32) -> Slot {
        Slot(value)
    }

    pub const fn get(self) -> u32 {
        self.0
    }

    pub const fn index(self) -> usize {
        self.0 as usize
    }

    pub fn from_index(index: usize) -> Option<Slot> {
        u32::try_from(index).ok().map(Slot)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SessionKey(Box<[u8]>);

impl SessionKey {
    pub fn new(bytes: &[u8]) -> SessionKey {
        SessionKey(bytes.into())
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ListKey {
    channel: ChannelId,
}

impl ListKey {
    pub const fn new(channel: ChannelId) -> ListKey {
        ListKey { channel }
    }

    pub const fn channel(self) -> ChannelId {
        self.channel
    }

    fn digits(self) -> ([u8; 20], usize) {
        let mut buf = [0u8; 20];
        let mut value = self.channel.get();
        let mut at = buf.len();
        while let Some(next) = at.checked_sub(1) {
            at = next;
            if let Some(cell) = buf.get_mut(at) {
                *cell = b'0'.wrapping_add((value % 10) as u8);
            }
            value /= 10;
            if value == 0 {
                break;
            }
        }
        (buf, at)
    }

    pub fn decimal(self) -> Vec<u8> {
        let (buf, at) = self.digits();
        buf.get(at..).map(<[u8]>::to_vec).unwrap_or_default()
    }
}

const DECIMAL_WIDTH: u32 = 20;

fn decimal_width(value: u64) -> u32 {
    value
        .checked_ilog10()
        .map_or(1, |log| log.saturating_add(1))
}

fn left_aligned(value: u64, width: u32) -> u128 {
    10u128
        .checked_pow(DECIMAL_WIDTH.saturating_sub(width))
        .map_or(0, |scale| u128::from(value).saturating_mul(scale))
}

impl Ord for ListKey {
    fn cmp(&self, other: &ListKey) -> core::cmp::Ordering {
        let (a, b) = (self.channel.get(), other.channel.get());
        let (wa, wb) = (decimal_width(a), decimal_width(b));
        left_aligned(a, wa)
            .cmp(&left_aligned(b, wb))
            .then(wa.cmp(&wb))
    }
}

impl PartialOrd for ListKey {
    fn partial_cmp(&self, other: &ListKey) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::{ChannelId, ListKey};

    #[test]
    fn list_keys_order_like_their_decimal_strings() {
        let values = [
            0u64,
            1,
            9,
            10,
            11,
            19,
            2,
            20,
            99,
            100,
            1_000,
            999,
            u64::MAX,
            u64::MAX - 1,
            18_446_744_073_709_551_610,
            1_844_674_407_370_955_161,
            1_420_000_000_000_000_001,
            142_000_000_000_000_000,
            1_420_000_000_000_000_000,
            7,
            70,
            700_000_000_000_000_000,
        ];
        for a in values {
            for b in values {
                let (ka, kb) = (
                    ListKey::new(ChannelId::new(a)),
                    ListKey::new(ChannelId::new(b)),
                );
                assert_eq!(
                    ka.cmp(&kb),
                    ka.decimal().cmp(&kb.decimal()),
                    "{a} against {b}"
                );
            }
        }
    }
}
