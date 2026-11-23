// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;
use std::sync::Arc;

use crate::bits::{self, BitIter};
use crate::budget::{Meter, Units};
use crate::classes::Class;
use crate::flags::Visible;
use crate::ids::{ChannelId, ChannelIdx, ClassId, Epoch, RoleSetId, UserId};
use crate::members::MemberEntry;
use crate::model::Model;
use crate::rolesets::DEAD;
use crate::signatures::Planes;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ClassChange {
    pub class: ClassId,
    pub toggled: Visible,
    pub flipped: Vec<(RoleSetId, Visible, Visible)>,
    pub overrides: Vec<(UserId, Option<Visible>, Option<Visible>)>,
}

impl ClassChange {
    pub fn expanded(&self, after: &Model) -> Vec<(RoleSetId, Visible, Visible)> {
        let changed = |(_, a, b): &&(RoleSetId, Visible, Visible)| a != b;
        if self.toggled.is_empty() {
            return self.flipped.iter().filter(changed).copied().collect();
        }
        let planes = after.class(self.class).map(Class::planes);
        let mut listed = self.flipped.iter().peekable();
        let mut out = Vec::with_capacity(after.rolesets.len());
        for row in 0..after.rolesets.len() {
            let Some(set) = RoleSetId::from_index(row) else {
                break;
            };
            if let Some(entry) = listed.next_if(|(s, _, _)| *s == set) {
                out.extend(Some(entry).filter(changed).copied());
                continue;
            }
            let now = planes.map_or(Visible::empty(), |p| p.get(set));
            out.push((set, now ^ self.toggled, now));
        }
        out.extend(listed.filter(changed).copied());
        out
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Delta {
    pub from: Epoch,
    pub to: Epoch,
    pub classes: Vec<ClassChange>,
    pub moves: Vec<(ChannelIdx, Option<ClassId>, Option<ClassId>)>,
    pub dropped: Vec<(ChannelIdx, ClassId)>,
    pub created: Vec<ClassId>,
    pub freed: Vec<ClassId>,
    pub remap: Option<Arc<[u32]>>,
    pub channel_remap: Option<Arc<[u32]>>,
    pub roles_changed: bool,
    pub resync: bool,
    pub viewsets_changed: bool,
}

impl Delta {
    pub fn is_empty(&self) -> bool {
        self.classes.is_empty()
            && self.moves.is_empty()
            && self.dropped.is_empty()
            && self.created.is_empty()
            && self.freed.is_empty()
            && self.remap.is_none()
            && self.channel_remap.is_none()
            && !self.roles_changed
            && !self.resync
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Group {
    RoleSet(RoleSetId),
    User(UserId),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Regroup {
    pub user: UserId,
    pub old: Option<Group>,
    pub new: Option<Group>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Target {
    Group(Group),
    OneUser(UserId),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VisibilityChange {
    pub target: Target,
    pub lost: Vec<ChannelId>,
    pub gained: Vec<ChannelId>,
}

const DIFF_UNIT_COST: u64 = 8;

pub fn diff_units(old: &Model, new: &Model) -> Units {
    let slots = count(old.channels.slots().max(new.channels.slots())).saturating_mul(4);
    let classes = if Arc::ptr_eq(&old.classes, &new.classes) {
        0
    } else {
        let words = count(old.rolesets.words().max(new.rolesets.words()));
        let rows = count(old.rolesets.len().max(new.rolesets.len()));
        let per_class = words
            .saturating_mul(3)
            .saturating_add(rows)
            .saturating_add(4);
        let overrides = [old, new]
            .into_iter()
            .flat_map(|model| model.classes.iter())
            .map(|(_, class)| count(class.overrides().len()))
            .fold(0u64, u64::saturating_add);
        count(old.classes.capacity().max(new.classes.capacity()))
            .saturating_mul(per_class)
            .saturating_add(overrides)
    };
    Units::new(slots.saturating_add(classes).saturating_mul(DIFF_UNIT_COST))
}

fn count(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

pub fn diff(old: &Model, new: &Model) -> Delta {
    let mut delta = changes(old, new);
    delta.viewsets_changed = viewsets_changed(old, new, &delta);
    delta
}

fn viewsets_changed(old: &Model, new: &Model, delta: &Delta) -> bool {
    if delta.from == delta.to {
        return false;
    }
    let channels_moved = !Arc::ptr_eq(&old.channels, &new.channels)
        && (old.channels.len() != new.channels.len()
            || old
                .channels
                .in_order()
                .zip(new.channels.in_order())
                .any(|((_, a), (_, b))| a != b));
    delta.resync
        || !delta.classes.is_empty()
        || !delta.moves.is_empty()
        || !delta.dropped.is_empty()
        || !delta.created.is_empty()
        || !delta.freed.is_empty()
        || delta.remap.is_some()
        || delta.channel_remap.is_some()
        || delta.roles_changed
        || channels_moved
        || old.owner != new.owner
        || old.exceptions != new.exceptions
        || old.virtual_view != new.virtual_view
        || old.flags.guild_nsfw() != new.flags.guild_nsfw()
}

fn changes(old: &Model, new: &Model) -> Delta {
    let roles_changed = !Arc::ptr_eq(&old.roles, &new.roles) && old.roles != new.roles;
    let (from, to) = (old.rolesets.generation(), new.rolesets.generation());
    let remap = if from == to {
        None
    } else {
        new.rolesets.remap().cloned()
    };
    let (slots_from, slots_to) = (old.channels.generation(), new.channels.generation());
    let channel_remap = if slots_from == slots_to {
        None
    } else {
        new.channels.remap().cloned()
    };
    let mut delta = Delta {
        from: old.epoch,
        to: new.epoch,
        classes: Vec::new(),
        moves: Vec::new(),
        dropped: Vec::new(),
        created: Vec::new(),
        freed: Vec::new(),
        remap: remap.clone(),
        channel_remap: channel_remap.clone(),
        roles_changed,
        resync: false,
        viewsets_changed: false,
    };
    if (from != to && to != from.wrapping_add(1))
        || (slots_from != slots_to && slots_to != slots_from.wrapping_add(1))
    {
        delta.resync = true;
        return delta;
    }
    if let Some(map) = &channel_remap {
        delta.dropped = (0..old.channels.slots())
            .filter_map(|i| {
                let idx = ChannelIdx::from_index(i)?;
                let class = old.classes.of_channel(idx)?;
                let target = map.get(i).copied().unwrap_or(DEAD);
                (target == DEAD).then_some((idx, class))
            })
            .collect();
    }
    if Arc::ptr_eq(&old.classes, &new.classes) {
        return delta;
    }
    let words = new.rolesets.words();
    let rows = new.rolesets.len();
    let mut scratch = Vec::new();
    let capacity = old.classes.capacity().max(new.classes.capacity());
    for slot in 0..capacity {
        let Some(id) = ClassId::from_index(slot) else {
            continue;
        };
        match (old.classes.get(id), new.classes.get(id)) {
            (Some(a), Some(b)) => {
                let change = match &remap {
                    Some(map) => {
                        let old = a.planes().remapped(map, words);
                        class_change(id, &old, (a, b), rows, &mut scratch)
                    }
                    None if a.same_content(b) => None,
                    None => class_change(id, a.planes(), (a, b), rows, &mut scratch),
                };
                delta.classes.extend(change);
            }
            (None, Some(_)) => delta.created.push(id),
            (Some(_), None) => delta.freed.push(id),
            (None, None) => {}
        }
    }
    let slots = old.channels.slots().max(new.channels.slots());
    let mut origin: Vec<Option<ChannelIdx>> = (0..slots).map(ChannelIdx::from_index).collect();
    if let Some(map) = &channel_remap {
        origin = vec![None; new.channels.slots()];
        for (i, target) in map.iter().enumerate() {
            if let Some(entry) = usize::try_from(*target)
                .ok()
                .and_then(|t| origin.get_mut(t))
            {
                *entry = ChannelIdx::from_index(i);
            }
        }
    }
    delta.moves = origin
        .into_iter()
        .enumerate()
        .filter_map(|(i, from)| {
            let idx = ChannelIdx::from_index(i)?;
            let a = from.and_then(|f| old.classes.of_channel(f));
            let b = new.classes.of_channel(idx);
            (a != b).then_some((idx, a, b))
        })
        .collect();
    delta
}

fn plane_words(planes: &Planes, word: usize) -> (u64, u64, u64) {
    let get = |plane: &[u64]| plane.get(word).copied().unwrap_or(0);
    (
        get(planes.view()),
        get(planes.view_members()),
        get(planes.history()),
    )
}

fn visible_at((view, members, history): (u64, u64, u64), mask: u64) -> Visible {
    let mut visible = Visible::empty();
    visible.set(Visible::VIEW, view & mask != 0);
    visible.set(Visible::VIEW_MEMBERS, members & mask != 0);
    visible.set(Visible::HISTORY, history & mask != 0);
    visible
}

const PLANES: [Visible; 3] = [Visible::VIEW, Visible::VIEW_MEMBERS, Visible::HISTORY];

fn valid_bits(rows: usize) -> impl Fn(usize) -> u64 {
    let (full, mask) = bits::locate(rows);
    move |word| match word.cmp(&full) {
        core::cmp::Ordering::Less => u64::MAX,
        core::cmp::Ordering::Equal => mask.wrapping_sub(1),
        core::cmp::Ordering::Greater => 0,
    }
}

fn ones_below(x: &[u64], y: &[u64], rows: usize) -> usize {
    if core::ptr::eq(x, y) {
        return 0;
    }
    let (full, mask) = bits::locate(rows);
    let (short, long) = if x.len() <= y.len() { (x, y) } else { (y, x) };
    let get = |w: usize| x.get(w).copied().unwrap_or(0) ^ y.get(w).copied().unwrap_or(0);
    let common = x
        .iter()
        .zip(y)
        .take(full)
        .map(|(p, q)| (p ^ q).count_ones())
        .fold(0u32, u32::saturating_add);
    let tail = long
        .iter()
        .take(full)
        .skip(short.len())
        .map(|p| p.count_ones())
        .fold(common, u32::saturating_add);
    let partial = (get(full) & mask.wrapping_sub(1)).count_ones();
    usize::try_from(tail.saturating_add(partial)).unwrap_or(usize::MAX)
}

fn changed_words(pairs: [(&[u64], &[u64]); 3], rows: usize, toggled: Visible, out: &mut Vec<u64>) {
    let words = pairs
        .iter()
        .map(|(x, y)| x.len().max(y.len()))
        .fold(bits::words_for(rows), usize::max);
    out.clear();
    out.resize(words, 0);
    for ((x, y), plane) in pairs.into_iter().zip(PLANES) {
        if toggled.contains(plane) {
            let (full, _) = bits::locate(rows);
            for ((slot, p), q) in out.iter_mut().zip(x).zip(y).take(full) {
                *slot |= !(p ^ q);
            }
            let done = full.min(x.len()).min(y.len());
            let valid = valid_bits(rows);
            for (word, slot) in out.iter_mut().enumerate().skip(done) {
                let get = |p: &[u64]| p.get(word).copied().unwrap_or(0);
                *slot |= get(x) ^ get(y) ^ valid(word);
            }
            continue;
        }
        if core::ptr::eq(x, y) {
            continue;
        }
        for ((slot, p), q) in out.iter_mut().zip(x).zip(y) {
            *slot |= p ^ q;
        }
        let (short, long) = if x.len() <= y.len() { (x, y) } else { (y, x) };
        for (slot, p) in out.iter_mut().zip(long).skip(short.len()) {
            *slot |= p;
        }
    }
}

fn class_change(
    id: ClassId,
    pa: &Planes,
    (a, b): (&Class, &Class),
    rows: usize,
    scratch: &mut Vec<u64>,
) -> Option<ClassChange> {
    let pb = b.planes();
    let pairs = [
        (pa.view(), pb.view()),
        (pa.view_members(), pb.view_members()),
        (pa.history(), pb.history()),
    ];
    let mut toggled = Visible::empty();
    let mut capacity = 0usize;
    for ((x, y), plane) in pairs.into_iter().zip(PLANES) {
        let count = ones_below(x, y, rows);
        let on = count.saturating_mul(2) > rows;
        toggled.set(plane, on);
        capacity = capacity.saturating_add(if on {
            rows.saturating_sub(count)
        } else {
            count
        });
    }
    changed_words(pairs, rows, toggled, scratch);
    let mut flipped = Vec::with_capacity(capacity.min(rows));
    for (word, changed) in scratch.iter().copied().enumerate() {
        if changed == 0 {
            continue;
        }
        let (before, after) = (plane_words(pa, word), plane_words(pb, word));
        for bit in BitIter(changed) {
            let index = word.checked_mul(64).and_then(|w| w.checked_add(bit));
            let mask = 1u64.wrapping_shl(u32::try_from(bit).unwrap_or(0));
            if let Some(set) = index.and_then(RoleSetId::from_index) {
                flipped.push((set, visible_at(before, mask), visible_at(after, mask)));
            }
        }
    }
    let mut overrides = Vec::new();
    let (oa, ob) = (a.overrides(), b.overrides());
    let (mut i, mut j) = (0usize, 0usize);
    loop {
        match (oa.get(i), ob.get(j)) {
            (Some((ua, va)), Some((ub, vb))) => match ua.cmp(ub) {
                core::cmp::Ordering::Less => {
                    overrides.push((*ua, Some(*va), None));
                    i = i.saturating_add(1);
                }
                core::cmp::Ordering::Greater => {
                    overrides.push((*ub, None, Some(*vb)));
                    j = j.saturating_add(1);
                }
                core::cmp::Ordering::Equal => {
                    if va != vb {
                        overrides.push((*ua, Some(*va), Some(*vb)));
                    }
                    i = i.saturating_add(1);
                    j = j.saturating_add(1);
                }
            },
            (Some((ua, va)), None) => {
                overrides.push((*ua, Some(*va), None));
                i = i.saturating_add(1);
            }
            (None, Some((ub, vb))) => {
                overrides.push((*ub, None, Some(*vb)));
                j = j.saturating_add(1);
            }
            (None, None) => break,
        }
    }
    if toggled.is_empty() && flipped.is_empty() && overrides.is_empty() {
        return None;
    }
    Some(ClassChange {
        class: id,
        toggled,
        flipped,
        overrides,
    })
}

pub(crate) fn group_of(model: &Model, user: UserId, entry: Option<&MemberEntry>) -> Option<Group> {
    match entry {
        Some(_) if model.exceptions.contains(user) => Some(Group::User(user)),
        Some(e) => Some(Group::RoleSet(e.roleset())),
        None if !model.virtual_view.get(user).is_empty() => Some(Group::User(user)),
        None => None,
    }
}

pub(crate) fn group_view(
    model: &Model,
    group: Option<Group>,
    entry: Option<&MemberEntry>,
    meter: &mut Meter,
) -> Vec<ChannelId> {
    let mut ids = match (group, entry) {
        (None, _) => Vec::new(),
        (Some(Group::RoleSet(set)), _) => model.ids(&model.set_viewable(set, meter)),
        (Some(Group::User(user)), Some(e)) => {
            let set = e.roleset();
            let list = model.viewable_by(meter, |class| {
                class.visible(set, user).contains(Visible::VIEW)
            });
            model.ids(&list)
        }
        (Some(Group::User(user)), None) => {
            let list = model.virtual_view.get(user);
            meter.charge(u64::try_from(list.len()).unwrap_or(u64::MAX));
            list.iter()
                .copied()
                .filter(|id| model.channels.resolve(*id).is_some())
                .collect()
        }
    };
    ids.sort_unstable();
    ids
}

fn split(old: &[ChannelId], new: &[ChannelId]) -> (Vec<ChannelId>, Vec<ChannelId>) {
    let lost = old
        .iter()
        .filter(|c| new.binary_search(c).is_err())
        .copied()
        .collect();
    let gained = new
        .iter()
        .filter(|c| old.binary_search(c).is_err())
        .copied()
        .collect();
    (lost, gained)
}

pub(crate) struct Side<'a> {
    pub(crate) model: &'a Model,
    pub(crate) entry: &'a dyn Fn(UserId) -> Option<MemberEntry>,
}

pub(crate) fn visibility_changes(
    old: &Side<'_>,
    new: &Side<'_>,
    old_refcount: &dyn Fn(RoleSetId) -> u32,
    regroups: &[Regroup],
    meter: &mut Meter,
) -> Vec<VisibilityChange> {
    let mut out = Vec::new();
    if !core::ptr::eq(old.model, new.model) {
        role_set_changes(old.model, new.model, old_refcount, meter, &mut out);
        let regrouped: Vec<UserId> = regroups.iter().map(|r| r.user).collect();
        for user in old.model.exceptions.users() {
            if regrouped.binary_search(user).is_ok() {
                continue;
            }
            let old_entry = (old.entry)(*user);
            let new_entry = (new.entry)(*user);
            let before = group_of(old.model, *user, old_entry.as_ref());
            let after = group_of(new.model, *user, new_entry.as_ref());
            if before != Some(Group::User(*user)) || after != before {
                continue;
            }
            let a = group_view(old.model, before, old_entry.as_ref(), meter);
            let b = group_view(new.model, after, new_entry.as_ref(), meter);
            let (lost, gained) = split(&a, &b);
            if !lost.is_empty() || !gained.is_empty() {
                out.push(VisibilityChange {
                    target: Target::Group(Group::User(*user)),
                    lost,
                    gained,
                });
            }
        }
    }
    for regroup in regroups {
        let old_entry = (old.entry)(regroup.user);
        let new_entry = (new.entry)(regroup.user);
        let a = group_view(old.model, regroup.old, old_entry.as_ref(), meter);
        let b = group_view(new.model, regroup.new, new_entry.as_ref(), meter);
        let (lost, gained) = split(&a, &b);
        if !lost.is_empty() || !gained.is_empty() {
            out.push(VisibilityChange {
                target: Target::OneUser(regroup.user),
                lost,
                gained,
            });
        }
    }
    out.sort_by_key(|a| a.target);
    out
}

fn role_set_changes(
    old: &Model,
    new: &Model,
    old_refcount: &dyn Fn(RoleSetId) -> u32,
    meter: &mut Meter,
    out: &mut Vec<VisibilityChange>,
) {
    let mut changes: BTreeMap<RoleSetId, (Vec<ChannelId>, Vec<ChannelId>)> = BTreeMap::new();
    let regular = !old.channels.has_irregular_parents() && !new.channels.has_irregular_parents();
    if regular {
        if Arc::ptr_eq(&old.classes, &new.classes) {
            return;
        }
        let slots = old.channels.slots().max(new.channels.slots());
        let words = old.rolesets.words().max(new.rolesets.words());
        for slot in 0..slots {
            let Some(idx) = ChannelIdx::from_index(slot) else {
                continue;
            };
            let (id, before, after) = match old.channels.get(idx) {
                Some(channel) => {
                    let id = channel.id();
                    let after = if new.channels.get(idx).is_some_and(|c| c.id() == id) {
                        Some(idx)
                    } else {
                        meter.charge(1);
                        new.channels.resolve(id)
                    };
                    (id, Some(idx), after)
                }
                None => {
                    let Some(channel) = new.channels.get(idx) else {
                        continue;
                    };
                    meter.charge(1);
                    if old.channels.resolve(channel.id()).is_some() {
                        continue;
                    }
                    (channel.id(), None, Some(idx))
                }
            };
            let a = before.and_then(|i| old.classes.class_of_channel(i));
            let b = after.and_then(|i| new.classes.class_of_channel(i));
            let va = a.map_or(&[][..], |c| c.planes().view());
            let vb = b.map_or(&[][..], |c| c.planes().view());
            if core::ptr::eq(va, vb) {
                continue;
            }
            meter.charge(u64::try_from(words).unwrap_or(u64::MAX));
            for word in 0..words {
                let x = va.get(word).copied().unwrap_or(0);
                let y = vb.get(word).copied().unwrap_or(0);
                for bit in BitIter(x ^ y) {
                    let Some(set) = word
                        .checked_mul(64)
                        .and_then(|w| w.checked_add(bit))
                        .and_then(RoleSetId::from_index)
                    else {
                        continue;
                    };
                    if old_refcount(set) == 0 {
                        continue;
                    }
                    meter.charge(1);
                    let entry = changes.entry(set).or_default();
                    if bits::test(va, set.index()) {
                        entry.0.push(id);
                    } else {
                        entry.1.push(id);
                    }
                }
            }
        }
    } else {
        for (set, _) in old.rolesets.iter() {
            if old_refcount(set) == 0 {
                continue;
            }
            let mut a = old.ids(&old.set_viewable(set, meter));
            let mut b = new.ids(&new.set_viewable(set, meter));
            a.sort_unstable();
            b.sort_unstable();
            let (lost, gained) = split(&a, &b);
            if !lost.is_empty() || !gained.is_empty() {
                changes.insert(set, (lost, gained));
            }
        }
    }
    for (set, (mut lost, mut gained)) in changes {
        lost.sort_unstable();
        gained.sort_unstable();
        out.push(VisibilityChange {
            target: Target::Group(Group::RoleSet(set)),
            lost,
            gained,
        });
    }
}
