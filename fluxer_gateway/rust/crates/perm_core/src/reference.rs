// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, BTreeSet};

use crate::channels::{ChannelKind, ChannelSpec, Nsfw, OverwriteTarget};
use crate::counts::{MutualOnline, PublicOnline};
use crate::event::{Cutoff, Event, RoleList, Snapshot};
use crate::flags::{Permissions, Visible};
use crate::ids::{ChannelId, ClassId, GuildId, RoleId, RoleSetId, UserId};
use crate::model::Model;
use crate::roles::{RankPos, RoleSpec};
use crate::threads::ThreadAccess;
use crate::views::Scope;
use crate::viewsets::Subject;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RefNeed {
    View,
    ViewMembers,
    MessageAccess(i64),
    History(i64),
    ChannelBits(Permissions),
    GuildBits(Permissions),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Malformed;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum RefGroup {
    Roles(Vec<RoleId>),
    User(UserId),
}

#[derive(Clone, Debug)]
pub struct Reference {
    pub guild: GuildId,
    pub owner: Option<UserId>,
    pub roles: Vec<RoleSpec>,
    pub channels: Vec<ChannelSpec>,
    pub members: BTreeMap<UserId, RoleList>,
    pub virtual_access: BTreeMap<UserId, Vec<ChannelId>>,
    pub cutoff: Cutoff,
    pub nsfw: Option<bool>,
    pub nsfw_level: u8,
}

enum Who {
    Owner,
    Admin,
    Member(Permissions, Vec<RoleId>),
    Quarantined,
    Stranger,
}

impl Reference {
    pub fn new(snapshot: &Snapshot) -> Reference {
        let mut r = Reference {
            guild: snapshot.guild,
            owner: snapshot.owner,
            roles: snapshot.roles.clone(),
            channels: Vec::new(),
            members: BTreeMap::new(),
            virtual_access: BTreeMap::new(),
            cutoff: snapshot.cutoff,
            nsfw: snapshot.nsfw,
            nsfw_level: snapshot.nsfw_level,
        };
        for c in &snapshot.channels {
            r.apply(&Event::ChannelUpsert(
                c.clone(),
                crate::event::ChannelCause::Created,
            ));
        }
        for (u, l) in &snapshot.members {
            r.members.insert(*u, l.clone());
        }
        for (u, l) in &snapshot.virtual_access {
            r.apply(&Event::VirtualAccess(*u, l.clone()));
        }
        r
    }

    pub fn apply(&mut self, event: &Event) {
        match event {
            Event::Roles(specs) => self.roles = specs.clone(),
            Event::Owner(o) => self.owner = *o,
            Event::ChannelUpsert(spec, _) => {
                match self.channels.iter_mut().find(|c| c.id == spec.id) {
                    Some(c) => *c = spec.clone(),
                    None => self.channels.push(spec.clone()),
                }
            }
            Event::ChannelDelete(id) => self.channels.retain(|c| c.id != *id),
            Event::ChannelOrder(ids) => {
                let mut next: Vec<ChannelSpec> = Vec::new();
                for id in ids {
                    if next.iter().all(|c| c.id != *id)
                        && let Some(c) = self.channels.iter().find(|c| c.id == *id)
                    {
                        next.push(c.clone());
                    }
                }
                for c in &self.channels {
                    if next.iter().all(|n| n.id != c.id) {
                        next.push(c.clone());
                    }
                }
                self.channels = next;
            }
            Event::MemberUpsert(u, l) => {
                self.members.insert(*u, l.clone());
            }
            Event::MemberRemove(u) => {
                self.members.remove(u);
            }
            Event::VirtualAccess(u, l) => {
                let mut list = l.clone();
                list.sort();
                list.dedup();
                if list.is_empty() {
                    self.virtual_access.remove(u);
                } else {
                    self.virtual_access.insert(*u, list);
                }
            }
            Event::HistoryCutoff(c) => self.cutoff = *c,
            Event::GuildNsfw(n, l) => {
                self.nsfw = *n;
                self.nsfw_level = *l;
            }
        }
    }

    fn everyone_id(&self) -> RoleId {
        RoleId::new(self.guild.get())
    }

    fn role(&self, id: RoleId) -> Option<&RoleSpec> {
        self.roles.iter().rev().find(|r| r.id == id)
    }

    fn role_bits(&self, id: RoleId) -> Permissions {
        self.role(id)
            .map_or(Permissions::empty(), |r| r.permissions)
    }

    fn who(&self, user: UserId) -> Who {
        if self.owner == Some(user) {
            return Who::Owner;
        }
        match self.members.get(&user) {
            None => Who::Stranger,
            Some(RoleList::Malformed) => Who::Quarantined,
            Some(RoleList::Roles(ids)) => self.who_roles(ids),
        }
    }

    fn who_roles(&self, ids: &[RoleId]) -> Who {
        let mut bits = self.role_bits(self.everyone_id());
        for id in ids {
            bits |= self.role_bits(*id);
        }
        if bits.contains(Permissions::ADMINISTRATOR) {
            Who::Admin
        } else {
            Who::Member(bits, ids.to_vec())
        }
    }

    pub fn channel(&self, id: ChannelId) -> Option<&ChannelSpec> {
        self.channels.iter().find(|c| c.id == id)
    }

    fn overwrites(
        &self,
        mut bits: Permissions,
        roles: &[RoleId],
        user: Option<UserId>,
        spec: &ChannelSpec,
    ) -> Permissions {
        for o in &spec.overwrites {
            if o.target == OverwriteTarget::Role(self.everyone_id()) {
                bits = (bits & !o.deny) | o.allow;
            }
        }
        let mut allow = Permissions::empty();
        let mut deny = Permissions::empty();
        for o in &spec.overwrites {
            if let OverwriteTarget::Role(id) = o.target
                && roles.contains(&id)
            {
                allow |= o.allow;
                deny |= o.deny;
            }
        }
        bits = (bits & !deny) | allow;
        for o in &spec.overwrites {
            if let Some(u) = user
                && o.target == OverwriteTarget::Member(u)
            {
                bits = (bits & !o.deny) | o.allow;
            }
        }
        bits
    }

    fn who_channel(&self, who: &Who, user: Option<UserId>, id: ChannelId) -> Permissions {
        match who {
            Who::Owner | Who::Admin => Permissions::all(),
            Who::Quarantined | Who::Stranger => Permissions::empty(),
            Who::Member(bits, roles) => match self.channel(id) {
                Some(spec) => self.overwrites(*bits, roles, user, spec),
                None => Permissions::empty(),
            },
        }
    }

    pub fn permissions(&self, user: UserId, scope: Scope) -> Result<Permissions, Malformed> {
        let who = self.who(user);
        match (&who, scope) {
            (Who::Quarantined, _) => Err(Malformed),
            (Who::Owner | Who::Admin, _) => Ok(Permissions::all()),
            (Who::Stranger, _) => Ok(Permissions::empty()),
            (Who::Member(bits, _), Scope::Guild) => Ok(*bits),
            (_, Scope::Channel(id)) => Ok(self.who_channel(&who, Some(user), id)),
        }
    }

    fn has_virtual(&self, user: UserId, id: ChannelId) -> bool {
        self.virtual_access
            .get(&user)
            .is_some_and(|l| l.contains(&id))
    }

    fn rule3(&self, who: &Who, user: Option<UserId>, id: ChannelId) -> bool {
        self.channel(id)
            .is_some_and(|c| c.kind == ChannelKind::Category)
            && self.channels.iter().any(|child| {
                child.parent == Some(id)
                    && self
                        .who_channel(who, user, child.id)
                        .contains(Permissions::VIEW_CHANNEL)
            })
    }

    fn member_visible(&self, who: &Who, user: Option<UserId>, id: ChannelId) -> Visible {
        let mut v = Visible::from_permissions(self.who_channel(who, user, id));
        if user.is_some_and(|u| self.has_virtual(u, id)) {
            v |= Visible::VIEW;
        }
        if !v.contains(Visible::VIEW) && self.rule3(who, user, id) {
            v |= Visible::VIEW;
        }
        v
    }

    pub fn can_view(&self, user: UserId, id: ChannelId) -> Visible {
        match self.who(user) {
            Who::Quarantined => Visible::empty(),
            Who::Owner => Visible::all(),
            Who::Stranger => {
                if self.has_virtual(user, id) {
                    Visible::VIEW
                } else {
                    Visible::empty()
                }
            }
            who => self.member_visible(&who, Some(user), id),
        }
    }

    pub fn session_visible(&self, user: UserId, id: ChannelId) -> Visible {
        let member = self.members.contains_key(&user);
        match self.who(user) {
            Who::Owner if member => Visible::all(),
            Who::Owner | Who::Stranger => {
                if self.has_virtual(user, id) {
                    Visible::VIEW
                } else {
                    Visible::empty()
                }
            }
            Who::Quarantined => Visible::empty(),
            who => self.member_visible(&who, Some(user), id),
        }
    }

    pub fn is_exception(&self, user: UserId) -> bool {
        self.owner == Some(user)
            || self.virtual_access.contains_key(&user)
            || matches!(self.members.get(&user), Some(RoleList::Malformed))
            || self.channels.iter().any(|c| {
                c.overwrites
                    .iter()
                    .any(|o| o.target == OverwriteTarget::Member(user))
            })
    }

    pub fn role_key(&self, user: UserId) -> Option<Vec<RoleId>> {
        match self.members.get(&user)? {
            RoleList::Roles(ids) => {
                let mut list = ids.clone();
                list.sort();
                list.dedup();
                Some(list)
            }
            RoleList::Malformed => Some(Vec::new()),
        }
    }

    pub fn group(&self, user: UserId) -> Option<RefGroup> {
        if self.members.contains_key(&user) {
            if self.is_exception(user) {
                Some(RefGroup::User(user))
            } else {
                self.role_key(user).map(RefGroup::Roles)
            }
        } else if self.virtual_access.contains_key(&user) {
            Some(RefGroup::User(user))
        } else {
            None
        }
    }

    fn list_with_parents(&self, view: impl Fn(&ChannelSpec) -> bool) -> Vec<ChannelId> {
        let included: Vec<ChannelId> = self
            .channels
            .iter()
            .filter(|c| view(c))
            .map(|c| c.id)
            .collect();
        let mut needed: BTreeSet<ChannelId> = BTreeSet::new();
        for id in &included {
            if let Some(p) = self.channel(*id).and_then(|c| c.parent)
                && !included.contains(&p)
            {
                needed.insert(p);
            }
        }
        let mut out = included;
        for c in &self.channels {
            if needed.contains(&c.id) {
                out.push(c.id);
            }
        }
        out
    }

    pub fn viewable(&self, user: UserId) -> Vec<ChannelId> {
        if !self.members.contains_key(&user) {
            return Vec::new();
        }
        if matches!(self.who(user), Who::Quarantined) {
            return Vec::new();
        }
        self.list_with_parents(|c| self.session_visible(user, c.id).contains(Visible::VIEW))
    }

    pub fn roles_view(&self, roles: &[RoleId]) -> Vec<ChannelId> {
        let who = self.who_roles(roles);
        let mut list = self.list_with_parents(|c| {
            self.member_visible(&who, None, c.id)
                .contains(Visible::VIEW)
        });
        list.sort();
        list
    }

    pub fn session_view(&self, user: UserId) -> Vec<ChannelId> {
        let mut list = if self.members.contains_key(&user) {
            self.viewable(user)
        } else {
            self.virtual_access
                .get(&user)
                .map(|l| {
                    l.iter()
                        .copied()
                        .filter(|id| self.channel(*id).is_some())
                        .collect()
                })
                .unwrap_or_default()
        };
        list.sort();
        list
    }

    pub fn group_view(&self, group: Option<&RefGroup>) -> Vec<ChannelId> {
        match group {
            None => Vec::new(),
            Some(RefGroup::Roles(roles)) => self.roles_view(roles),
            Some(RefGroup::User(u)) => self.session_view(*u),
        }
    }

    fn history_open(&self, ms: i64) -> bool {
        match self.cutoff {
            Cutoff::AtMillis(c) => ms >= c,
            Cutoff::Missing | Cutoff::Malformed => false,
        }
    }

    pub fn message_access(&self, user: UserId, id: ChannelId, ms: i64) -> bool {
        match self.permissions(user, Scope::Channel(id)) {
            Ok(p) => p.contains(Permissions::READ_MESSAGE_HISTORY) || self.history_open(ms),
            Err(Malformed) => false,
        }
    }

    pub fn in_audience(&self, user: UserId, id: ChannelId, need: RefNeed) -> bool {
        let meets = |v: Visible| match need {
            RefNeed::View => v.contains(Visible::VIEW),
            RefNeed::ViewMembers => v.contains(Visible::VIEW) && v.contains(Visible::VIEW_MEMBERS),
            RefNeed::MessageAccess(ms) => {
                v.contains(Visible::VIEW) && (v.contains(Visible::HISTORY) || self.history_open(ms))
            }
            RefNeed::History(_) | RefNeed::ChannelBits(_) | RefNeed::GuildBits(_) => false,
        };
        match need {
            RefNeed::History(ms) => self.message_access(user, id, ms),
            RefNeed::ChannelBits(p) => self
                .permissions(user, Scope::Channel(id))
                .is_ok_and(|b| b.contains(p)),
            RefNeed::GuildBits(p) => self
                .permissions(user, Scope::Guild)
                .is_ok_and(|b| b.contains(p)),
            _ => meets(self.session_visible(user, id)),
        }
    }

    pub fn roles_visible(&self, roles: &[RoleId], id: ChannelId) -> Visible {
        self.member_visible(&self.who_roles(roles), None, id)
    }

    pub fn roles_in_audience(&self, roles: &[RoleId], id: ChannelId, need: RefNeed) -> bool {
        let who = self.who_roles(roles);
        let bits = self.who_channel(&who, None, id);
        match need {
            RefNeed::ChannelBits(p) => bits.contains(p),
            RefNeed::GuildBits(p) => match who {
                Who::Admin => true,
                Who::Member(b, _) => b.contains(p),
                _ => false,
            },
            RefNeed::View => self.member_visible(&who, None, id).contains(Visible::VIEW),
            RefNeed::ViewMembers => {
                self.member_visible(&who, None, id).contains(Visible::VIEW)
                    && bits.contains(Permissions::VIEW_CHANNEL_MEMBERS)
            }
            RefNeed::MessageAccess(ms) => {
                self.member_visible(&who, None, id).contains(Visible::VIEW)
                    && (bits.contains(Permissions::READ_MESSAGE_HISTORY) || self.history_open(ms))
            }
            RefNeed::History(ms) => {
                bits.contains(Permissions::READ_MESSAGE_HISTORY) || self.history_open(ms)
            }
        }
    }

    pub fn viewers(&self, id: ChannelId) -> BTreeSet<UserId> {
        self.members
            .keys()
            .copied()
            .filter(|u| self.can_view(*u, id).contains(Visible::VIEW))
            .collect()
    }

    pub fn members_with_role(&self, role: RoleId) -> Vec<UserId> {
        self.members
            .iter()
            .filter(|(_, l)| matches!(l, RoleList::Roles(ids) if ids.contains(&role)))
            .map(|(u, _)| *u)
            .collect()
    }

    fn everyone_view(&self, spec: &ChannelSpec) -> bool {
        let everyone = self.role_bits(self.everyone_id());
        if everyone.contains(Permissions::ADMINISTRATOR) {
            return true;
        }
        let mut bits = everyone;
        for o in &spec.overwrites {
            if o.target == OverwriteTarget::Role(self.everyone_id()) {
                bits = (bits & !o.deny) | o.allow;
            }
        }
        bits.contains(Permissions::VIEW_CHANNEL)
    }

    fn nsfw_flag(n: Nsfw) -> Option<bool> {
        match n {
            Nsfw::Inherit => None,
            Nsfw::Safe => Some(false),
            Nsfw::Adult => Some(true),
        }
    }

    fn visitor_bits(&self, c: &ChannelSpec) -> Permissions {
        let mut bits = self.role_bits(self.everyone_id()) & !Permissions::ADMINISTRATOR;
        for o in &c.overwrites {
            if o.target == OverwriteTarget::Role(self.everyone_id()) {
                bits = (bits & !o.deny) | o.allow;
            }
        }
        bits & (Permissions::VIEW_CHANNEL | Permissions::READ_MESSAGE_HISTORY)
    }

    pub fn visitor_permissions(&self, id: ChannelId) -> Permissions {
        self.channel(id)
            .map_or(Permissions::empty(), |c| self.visitor_bits(c))
    }

    pub fn shares_view(&self, subscriber: UserId, target: UserId) -> bool {
        self.members.contains_key(&target)
            && self.viewable(subscriber).iter().any(|id| {
                self.channel(*id)
                    .is_some_and(|c| c.kind != ChannelKind::Category)
                    && self.can_view(target, *id).contains(Visible::VIEW)
            })
    }

    pub fn view_ids(&self, subject: &Subject) -> Vec<ChannelId> {
        match subject {
            Subject::Member(user) => self.viewable(*user),
            Subject::NonMember(_) => Vec::new(),
            Subject::Visitor => self
                .visitor_channels(false)
                .into_iter()
                .map(|(id, _)| id)
                .collect(),
        }
    }

    pub fn view_set(&self, subject: &Subject) -> Vec<u64> {
        let ids: BTreeSet<ChannelId> = self.view_ids(subject).into_iter().collect();
        let mut words = vec![0u64; self.channels.len().div_ceil(64)];
        for (i, c) in self.channels.iter().enumerate() {
            if !ids.contains(&c.id) {
                continue;
            }
            let bit = u32::try_from(i.rem_euclid(64)).unwrap_or(0);
            if let Some(word) = words.get_mut(i.div_euclid(64)) {
                *word |= 1u64.checked_shl(bit).unwrap_or(0);
            }
        }
        words
    }

    pub fn filter_sharing(&self, target: &Subject, candidates: &[Subject]) -> Vec<bool> {
        let user = match target {
            Subject::Member(user) if self.members.contains_key(user) => *user,
            _ => return vec![false; candidates.len()],
        };
        let theirs: BTreeSet<ChannelId> = self.view_ids(target).into_iter().collect();
        candidates
            .iter()
            .map(|candidate| {
                candidate.user() != Some(user)
                    && self
                        .view_ids(candidate)
                        .iter()
                        .any(|id| theirs.contains(id))
            })
            .collect()
    }

    pub fn visitor_channels(&self, adult: bool) -> Vec<(ChannelId, Permissions)> {
        let guild_nsfw = self.nsfw.unwrap_or(self.nsfw_level == 3);
        let mut out = Vec::new();
        for c in &self.channels {
            let bits = self.visitor_bits(c);
            if !bits.contains(Permissions::VIEW_CHANNEL) {
                continue;
            }
            let parent = c.parent.and_then(|p| self.channel(p));
            let nsfw = match Self::nsfw_flag(c.nsfw) {
                Some(v) => v,
                None => match parent.filter(|_| c.kind != ChannelKind::Category) {
                    Some(p) => Self::nsfw_flag(p.nsfw).unwrap_or(guild_nsfw),
                    None => guild_nsfw,
                },
            };
            if !adult && nsfw {
                continue;
            }
            out.push((c.id, bits));
        }
        out
    }

    pub fn everyone_mentions(&self, ids: &[ChannelId]) -> Vec<ChannelId> {
        let mut out: Vec<ChannelId> = Vec::new();
        for id in ids {
            if out.contains(id) {
                continue;
            }
            if let Some(c) = self.channel(*id)
                && !matches!(c.kind, ChannelKind::Forum | ChannelKind::Media)
                && c.named
                && self.everyone_view(c)
            {
                out.push(*id);
            }
        }
        out
    }

    pub fn first_everyone_viewable(&self, ids: &[ChannelId]) -> Option<ChannelId> {
        ids.iter().copied().find(|id| {
            self.channel(*id).is_some_and(|c| {
                matches!(
                    c.kind,
                    ChannelKind::Text | ChannelKind::Voice | ChannelKind::Announcement
                ) && self.everyone_view(c)
            })
        })
    }

    pub fn public_online(&self, online: &[UserId]) -> PublicOnline {
        let mut open = false;
        let mut restricted: BTreeSet<ChannelId> = BTreeSet::new();
        for c in &self.channels {
            if matches!(c.kind, ChannelKind::Forum | ChannelKind::Media) || !self.everyone_view(c) {
                continue;
            }
            if c.overwrites
                .iter()
                .any(|o| o.deny.contains(Permissions::VIEW_CHANNEL))
            {
                restricted.insert(c.id);
            } else {
                open = true;
            }
        }
        if open {
            return PublicOnline::Open;
        }
        if restricted.is_empty() {
            return PublicOnline::Empty;
        }
        let users: BTreeSet<UserId> = online.iter().copied().collect();
        let n = users
            .iter()
            .filter(|u| self.viewable(**u).iter().any(|c| restricted.contains(c)))
            .count();
        PublicOnline::Count(u64::try_from(n).unwrap_or(u64::MAX))
    }

    pub fn mutual_online(&self, requester: UserId, online: &[UserId]) -> MutualOnline {
        if self
            .permissions(requester, Scope::Guild)
            .is_ok_and(|p| p.contains(Permissions::ADMINISTRATOR))
        {
            return MutualOnline::All;
        }
        let users: BTreeSet<UserId> = online.iter().copied().collect();
        let own = self.viewable(requester);
        if own.is_empty() {
            return MutualOnline::Count(u64::from(users.contains(&requester)));
        }
        let shared: BTreeSet<ChannelId> = own
            .into_iter()
            .filter(|id| {
                self.channel(*id).is_some_and(|c| {
                    c.kind != ChannelKind::Category
                        && !matches!(c.kind, ChannelKind::Forum | ChannelKind::Media)
                })
            })
            .collect();
        let n = users
            .iter()
            .filter(|u| **u == requester || self.viewable(**u).iter().any(|c| shared.contains(c)))
            .count();
        MutualOnline::Count(u64::try_from(n).unwrap_or(u64::MAX))
    }

    fn held_roles(&self, user: UserId) -> Vec<&RoleSpec> {
        match self.members.get(&user) {
            Some(RoleList::Roles(ids)) => ids.iter().filter_map(|id| self.role(*id)).collect(),
            _ => Vec::new(),
        }
    }

    fn max_position(&self, user: UserId) -> i64 {
        self.held_roles(user)
            .iter()
            .map(|r| match r.position {
                RankPos::Int(p) => p,
                RankPos::Undefined => 0,
            })
            .max()
            .unwrap_or(-1)
    }

    fn highest(&self, user: UserId) -> Option<&RoleSpec> {
        let mut best: Option<&RoleSpec> = None;
        for r in self.held_roles(user) {
            best = match best {
                None => Some(r),
                Some(b) if r.position > b.position || (r.position == b.position && r.id < b.id) => {
                    Some(r)
                }
                Some(b) => Some(b),
            };
        }
        best
    }

    fn rank_ok(&self, user: UserId, role: &RoleSpec) -> bool {
        let max = RankPos::Int(self.max_position(user));
        max > role.position
            || (max == role.position && self.highest(user).is_some_and(|h| h.id < role.id))
    }

    pub fn can_manage_roles(&self, user: UserId, role: RoleId) -> Result<bool, Malformed> {
        if self.owner == Some(user) {
            return Ok(true);
        }
        let bits = self.permissions(user, Scope::Guild)?;
        if !bits.contains(Permissions::MANAGE_ROLES) {
            return Ok(false);
        }
        Ok(self.role(role).is_some_and(|r| self.rank_ok(user, r)))
    }

    pub fn can_manage_role(&self, user: UserId, role: RoleId) -> bool {
        self.role(role).is_some_and(|r| self.rank_ok(user, r))
    }

    pub fn can_manage_member(&self, actor: UserId, target: UserId) -> bool {
        if self.owner == Some(actor) {
            return true;
        }
        if self.owner == Some(target) {
            return false;
        }
        let rank = |u: UserId| match self.highest(u) {
            Some(r) => (r.position, 0i128.saturating_sub(i128::from(r.id.get()))),
            None => (RankPos::Int(-1), 0),
        };
        rank(actor) > rank(target)
    }

    pub fn thread(
        &self,
        user: UserId,
        parent: ChannelId,
        access: ThreadAccess,
    ) -> Result<Permissions, Malformed> {
        if self.channel(parent).is_none() {
            return Ok(Permissions::empty());
        }
        let p = self.permissions(user, Scope::Channel(parent))?;
        let all = Permissions::all();
        let aliased = if p == all {
            p
        } else if p.contains(Permissions::SEND_MESSAGES_IN_THREADS) {
            p | Permissions::SEND_MESSAGES
        } else {
            p & !Permissions::SEND_MESSAGES
        };
        let moderator = aliased == all
            || aliased.contains(Permissions::ADMINISTRATOR)
            || (aliased.contains(Permissions::MANAGE_THREADS) && !access.timed_out);
        if aliased.contains(Permissions::VIEW_CHANNEL)
            && (!access.private || moderator || access.member)
        {
            Ok(aliased)
        } else {
            Ok(aliased & !Permissions::VIEW_CHANNEL)
        }
    }

    pub fn permissions_reply(&self, user: UserId, scope: Scope) -> Option<String> {
        self.permissions(user, scope)
            .ok()
            .map(|p| format!("{{\"permissions\":\"{}\"}}", p.bits()))
    }

    pub fn check_reply(&self, user: UserId, scope: Scope, bits: Permissions) -> Option<String> {
        if bits.is_empty() {
            return None;
        }
        self.permissions(user, scope)
            .ok()
            .map(|p| format!("{{\"has_permission\":{}}}", p.contains(bits)))
    }

    pub fn viewable_reply(&self, user: UserId) -> String {
        let ids: Vec<String> = self
            .viewable(user)
            .iter()
            .map(|c| format!("\"{}\"", c.get()))
            .collect();
        format!("{{\"channel_ids\":[{}]}}", ids.join(","))
    }
}

pub fn row_diff(old: &Model, new: &Model, class: ClassId) -> Vec<(RoleSetId, Visible, Visible)> {
    let rows = new.rolesets().len();
    let mut origin: Vec<Option<RoleSetId>> = (0..rows).map(RoleSetId::from_index).collect();
    if old.rolesets().generation() != new.rolesets().generation()
        && let Some(map) = new.rolesets().remap()
    {
        origin = vec![None; rows];
        for (from, to) in map.iter().enumerate() {
            if let Some(slot) = usize::try_from(*to).ok().and_then(|t| origin.get_mut(t)) {
                *slot = RoleSetId::from_index(from);
            }
        }
    }
    let get = |model: &Model, set: RoleSetId| {
        model
            .class(class)
            .map_or(Visible::empty(), |c| c.planes().get(set))
    };
    origin
        .iter()
        .enumerate()
        .filter_map(|(i, from)| {
            let set = RoleSetId::from_index(i)?;
            let before = from.map_or(Visible::empty(), |f| get(old, f));
            let after = get(new, set);
            (before != after).then_some((set, before, after))
        })
        .collect()
}
