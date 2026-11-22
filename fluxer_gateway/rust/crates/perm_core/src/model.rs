// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock};

use crate::bits;
use crate::budget::Meter;
use crate::channels::{Channel, ChannelTable};
use crate::classes::{Class, Classes};
use crate::event::Cutoff;
use crate::exceptions::{Exceptions, VirtualView};
use crate::flags::Visible;
use crate::ids::{ChannelId, ChannelIdx, ClassId, Epoch, GuildId, RoleSetId, UserId};
use crate::roles::RoleTable;
use crate::rolesets::RoleSets;
use crate::signatures::Signatures;
use crate::viewsets::ViewSets;

pub const AGE_RESTRICTED_LEVEL: u8 = 3;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct GuildFlags {
    pub nsfw: Option<bool>,
    pub nsfw_level: u8,
    pub cutoff: Cutoff,
}

impl GuildFlags {
    pub fn guild_nsfw(&self) -> bool {
        self.nsfw.unwrap_or(self.nsfw_level == AGE_RESTRICTED_LEVEL)
    }

    pub fn history_open(&self, message_ms: i64) -> bool {
        match self.cutoff {
            Cutoff::AtMillis(cutoff) => message_ms >= cutoff,
            Cutoff::Missing | Cutoff::Malformed => false,
        }
    }
}

impl Default for GuildFlags {
    fn default() -> GuildFlags {
        GuildFlags {
            nsfw: None,
            nsfw_level: 0,
            cutoff: Cutoff::Missing,
        }
    }
}

#[derive(Debug)]
pub(crate) struct ViewableCache {
    lists: Vec<OnceLock<Arc<[ChannelIdx]>>>,
    bytes: AtomicUsize,
    limit: usize,
}

impl ViewableCache {
    pub(crate) fn new(sets: usize, limit: usize) -> ViewableCache {
        ViewableCache {
            lists: (0..sets).map(|_| OnceLock::new()).collect(),
            bytes: AtomicUsize::new(0),
            limit,
        }
    }

    pub(crate) fn bytes(&self) -> usize {
        self.bytes.load(Ordering::Relaxed)
    }
}

#[derive(Debug)]
pub struct Model {
    pub(crate) epoch: Epoch,
    pub(crate) guild: GuildId,
    pub(crate) owner: Option<UserId>,
    pub(crate) roles: Arc<RoleTable>,
    pub(crate) rolesets: Arc<RoleSets>,
    pub(crate) channels: Arc<ChannelTable>,
    pub(crate) signatures: Arc<Signatures>,
    pub(crate) classes: Arc<Classes>,
    pub(crate) exceptions: Arc<Exceptions>,
    pub(crate) virtual_view: Arc<VirtualView>,
    pub(crate) flags: GuildFlags,
    pub(crate) viewable: ViewableCache,
    pub(crate) viewsets: ViewSets,
}

impl Model {
    pub fn empty(guild: GuildId) -> Model {
        let roles = RoleTable::new(guild);
        let rolesets = RoleSets::new(&roles);
        let sets = rolesets.len();
        Model {
            epoch: Epoch::ZERO,
            guild,
            owner: None,
            roles: Arc::new(roles),
            rolesets: Arc::new(rolesets),
            channels: Arc::new(ChannelTable::default()),
            signatures: Arc::new(Signatures::default()),
            classes: Arc::new(Classes::default()),
            exceptions: Arc::new(Exceptions::default()),
            virtual_view: Arc::new(VirtualView::default()),
            flags: GuildFlags::default(),
            viewable: ViewableCache::new(sets, 0),
            viewsets: ViewSets::new(0),
        }
    }

    pub fn epoch(&self) -> Epoch {
        self.epoch
    }

    pub fn guild(&self) -> GuildId {
        self.guild
    }

    pub fn owner(&self) -> Option<UserId> {
        self.owner
    }

    pub fn roles(&self) -> &RoleTable {
        &self.roles
    }

    pub fn rolesets(&self) -> &RoleSets {
        &self.rolesets
    }

    pub fn channels(&self) -> &ChannelTable {
        &self.channels
    }

    pub fn signatures(&self) -> &Signatures {
        &self.signatures
    }

    pub fn classes(&self) -> &Classes {
        &self.classes
    }

    pub fn exceptions(&self) -> &Exceptions {
        &self.exceptions
    }

    pub fn virtual_view(&self) -> &VirtualView {
        &self.virtual_view
    }

    pub fn flags(&self) -> GuildFlags {
        self.flags
    }

    pub fn viewable_cache_bytes(&self) -> usize {
        self.viewable.bytes()
    }

    pub fn is_exception(&self, user: UserId) -> bool {
        self.exceptions.contains(user)
    }

    pub fn channel(&self, channel: ChannelIdx) -> Option<&Channel> {
        self.channels.get(channel)
    }

    pub fn channel_idx(&self, channel: ChannelId) -> Option<ChannelIdx> {
        self.channels.resolve(channel)
    }

    pub fn class_of(&self, channel: ChannelIdx) -> Option<ClassId> {
        self.classes.of_channel(channel)
    }

    pub fn class_of_id(&self, channel: ChannelId) -> Option<ClassId> {
        self.channel_idx(channel).and_then(|idx| self.class_of(idx))
    }

    pub fn class(&self, class: ClassId) -> Option<&Class> {
        self.classes.get(class)
    }

    pub fn visible(&self, class: ClassId, set: RoleSetId, user: UserId) -> Visible {
        self.classes
            .get(class)
            .map_or(Visible::empty(), |c| c.visible(set, user))
    }

    pub(crate) fn viewable_by(
        &self,
        meter: &mut Meter,
        view: impl Fn(&Class) -> bool,
    ) -> Vec<ChannelIdx> {
        let slots = self.channels.slots();
        meter.charge(u64::try_from(self.channels.len().saturating_mul(2)).unwrap_or(u64::MAX));
        let mut included = vec![0u64; bits::words_for(slots)];
        let mut out = Vec::new();
        for idx in self.channels.order() {
            if self.classes.class_of_channel(*idx).is_some_and(&view) {
                bits::set(&mut included, idx.index());
                out.push(*idx);
            }
        }
        let mut needed = vec![0u64; bits::words_for(slots)];
        let mut any = false;
        for idx in &out {
            if let Some(parent) = self.channels.get(*idx).and_then(Channel::parent)
                && !bits::test(&included, parent.index())
            {
                bits::set(&mut needed, parent.index());
                any = true;
            }
        }
        if any {
            for idx in self.channels.order() {
                if bits::test(&needed, idx.index()) {
                    out.push(*idx);
                }
            }
        }
        out
    }

    pub(crate) fn set_viewable(&self, set: RoleSetId, meter: &mut Meter) -> Arc<[ChannelIdx]> {
        let compute = |meter: &mut Meter| -> Arc<[ChannelIdx]> {
            self.viewable_by(meter, |class| {
                class.planes().get(set).contains(Visible::VIEW)
            })
            .into()
        };
        let Some(slot) = self.viewable.lists.get(set.index()) else {
            return compute(meter);
        };
        if let Some(list) = slot.get() {
            return list.clone();
        }
        let list = compute(meter);
        let size = list.len().saturating_mul(4).saturating_add(16);
        let used = self.viewable.bytes.load(Ordering::Relaxed);
        if used.saturating_add(size) > self.viewable.limit {
            return list;
        }
        if slot.set(list.clone()).is_ok() {
            self.viewable.bytes.fetch_add(size, Ordering::Relaxed);
        }
        slot.get().cloned().unwrap_or(list)
    }

    pub(crate) fn ids(&self, list: &[ChannelIdx]) -> Vec<ChannelId> {
        list.iter()
            .filter_map(|idx| self.channels.get(*idx).map(Channel::id))
            .collect()
    }
}
