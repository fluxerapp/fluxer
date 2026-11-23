// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;

use crate::ids::{ChannelId, ListKey, SessionKey, UserId};
use crate::ranges::Ranges;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Session<P> {
    pid: P,
    user: UserId,
    list: Option<ChannelId>,
    ranges: Ranges,
}

impl<P: Copy> Session<P> {
    pub fn pid(&self) -> P {
        self.pid
    }

    pub fn user(&self) -> UserId {
        self.user
    }

    pub fn list(&self) -> Option<ChannelId> {
        self.list
    }

    pub fn ranges(&self) -> &Ranges {
        &self.ranges
    }
}

#[derive(Clone, Debug)]
pub struct Subscription<P> {
    pub session: SessionKey,
    pub pid: P,
    pub user: UserId,
    pub channel: ChannelId,
    pub ranges: Ranges,
}

#[derive(Clone, Debug)]
pub struct Sessions<P> {
    by_key: BTreeMap<SessionKey, ChannelId>,
    by_list: BTreeMap<ListKey, BTreeMap<SessionKey, Session<P>>>,
}

impl<P> Default for Sessions<P> {
    fn default() -> Sessions<P> {
        Sessions {
            by_key: BTreeMap::new(),
            by_list: BTreeMap::new(),
        }
    }
}

impl<P: Copy> Sessions<P> {
    pub fn new() -> Sessions<P> {
        Sessions::default()
    }

    pub fn len(&self) -> usize {
        self.by_key.len()
    }

    pub fn is_empty(&self) -> bool {
        self.by_key.is_empty()
    }

    pub fn get(&self, key: &SessionKey) -> Option<&Session<P>> {
        let channel = self.by_key.get(key)?;
        self.by_list.get(&ListKey::new(*channel))?.get(key)
    }

    pub fn lists(&self) -> impl Iterator<Item = ListKey> + '_ {
        self.by_list.keys().copied()
    }

    pub fn subscribers(&self, list: ListKey) -> impl Iterator<Item = (&SessionKey, &Session<P>)> {
        self.by_list.get(&list).into_iter().flatten()
    }

    pub fn has_subscribers(&self, list: ListKey) -> bool {
        self.by_list.get(&list).is_some_and(|s| !s.is_empty())
    }

    pub fn subscribe(
        &mut self,
        key: &SessionKey,
        pid: P,
        user: UserId,
        channel: ChannelId,
        ranges: Ranges,
    ) -> Option<ChannelId> {
        let list = ListKey::new(channel);
        let previous = self.by_key.get(key).copied();
        if let Some(old) = previous
            && old != channel
        {
            self.unlink(key, ListKey::new(old));
        }
        let session = Session {
            pid,
            user,
            list: Some(channel),
            ranges,
        };
        let members = self.by_list.entry(list).or_default();
        match members.get_mut(key) {
            Some(slot) => *slot = session,
            None => {
                members.insert(key.clone(), session);
            }
        }
        match self.by_key.get_mut(key) {
            Some(slot) => *slot = channel,
            None => {
                self.by_key.insert(key.clone(), channel);
            }
        }
        previous.filter(|old| *old != channel)
    }

    fn unlink(&mut self, key: &SessionKey, list: ListKey) {
        if let Some(set) = self.by_list.get_mut(&list) {
            set.remove(key);
            if set.is_empty() {
                self.by_list.remove(&list);
            }
        }
    }

    pub fn unsubscribe(&mut self, key: &SessionKey, channel: ChannelId) -> bool {
        let subscribed = self.by_key.get(key) == Some(&channel);
        if subscribed {
            self.unlink(key, ListKey::new(channel));
            self.by_key.remove(key);
        }
        subscribed
    }

    pub fn drop_session(&mut self, key: &SessionKey) -> Option<ChannelId> {
        let channel = self.by_key.remove(key)?;
        self.unlink(key, ListKey::new(channel));
        Some(channel)
    }

    pub fn readers(&self) -> impl Iterator<Item = (ChannelId, usize)> + '_ {
        self.by_list.iter().map(|(k, s)| (k.channel(), s.len()))
    }

    pub fn subscriptions(&self) -> Vec<Subscription<P>> {
        self.by_key
            .keys()
            .filter_map(|k| {
                let s = self.get(k)?;
                s.list.map(|channel| Subscription {
                    session: k.clone(),
                    pid: s.pid,
                    user: s.user,
                    channel,
                    ranges: s.ranges.clone(),
                })
            })
            .collect()
    }

    pub fn bytes(&self) -> usize {
        self.by_key
            .keys()
            .map(|k| k.as_bytes().len().saturating_mul(2).saturating_add(160))
            .fold(0usize, usize::saturating_add)
    }
}
