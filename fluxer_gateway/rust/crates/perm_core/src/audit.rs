// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, BTreeSet};

use crate::bits;
use crate::budget::Meter;
use crate::classes::{Classes, Content, Overrides};
use crate::eval::{self, Who};
use crate::exceptions::Exceptions;
use crate::flags::Visible;
use crate::ids::{ChannelIdx, ClassId, RoleSetId, SigId, UserId};
use crate::members::MemberIndex;
use crate::model::Model;
use crate::signatures::Planes;

fn fail<T>(message: String) -> Result<T, String> {
    Err(message)
}

fn fresh_sig_planes(model: &Model, idx: ChannelIdx) -> Option<Planes> {
    let channel = model.channels.get(idx)?;
    let words = model.rolesets.words();
    let mut view = vec![0u64; words];
    let mut view_members = vec![0u64; words];
    let mut history = vec![0u64; words];
    for (id, set) in model.rolesets.iter() {
        let v = eval::role_set_visible(
            set.base(),
            set.roles(),
            channel.rules().everyone(),
            channel.rules().roles(),
        );
        bits::assign(&mut view, id.index(), v.contains(Visible::VIEW));
        bits::assign(
            &mut view_members,
            id.index(),
            v.contains(Visible::VIEW_MEMBERS),
        );
        bits::assign(&mut history, id.index(), v.contains(Visible::HISTORY));
    }
    Some(Planes::from_vecs(view, view_members, history))
}

fn fresh_content(model: &Model, members: &MemberIndex, idx: ChannelIdx) -> Option<Content> {
    let channel = model.channels.get(idx)?;
    let own = fresh_sig_planes(model, idx)?;
    let planes = if channel.kind().is_category() && !model.channels.children(idx).is_empty() {
        let mut view = own.view().to_vec();
        for child in model.channels.children(idx) {
            if let Some(p) = fresh_sig_planes(model, *child) {
                for (w, c) in view.iter_mut().zip(p.view()) {
                    *w |= *c;
                }
            }
        }
        own.with_view(view)
    } else {
        own
    };
    let mut overrides = Overrides::new();
    let mut meter = Meter::new();
    for user in model.exceptions.users() {
        let entry = members.get(*user);
        let who = Who::of(model.owner, &model.rolesets, *user, entry);
        let va = model.virtual_view.get(*user);
        if !who.is_member() && va.is_empty() {
            continue;
        }
        let bits = eval::session_bits(who, *user, va, &model.channels, idx, &mut meter);
        let reference = entry.map_or(Visible::empty(), |e| planes.get(e.roleset()));
        if bits != reference {
            overrides.push((*user, bits));
        }
    }
    Some(Content { planes, overrides })
}

pub fn audit(model: &Model, members: &MemberIndex) -> Result<(), String> {
    let words = model.rolesets.words();
    let sets = model.rolesets.len();

    let mut users: BTreeMap<SigId, u32> = BTreeMap::new();
    for (idx, channel) in model.channels.live() {
        let Some(sig) = model.signatures.of_channel(idx) else {
            return fail(format!("channel {idx:?} has no signature"));
        };
        let Some(entry) = model.signatures.get(sig) else {
            return fail(format!("channel {idx:?} names dead signature {sig:?}"));
        };
        if entry.everyone() != channel.rules().everyone()
            || entry.roles() != channel.rules().roles()
        {
            return fail(format!("channel {idx:?} signature rules differ"));
        }
        let fresh = fresh_sig_planes(model, idx).unwrap_or_else(|| Planes::empty(words));
        if !entry.planes().same_as(&fresh) {
            return fail(format!(
                "signature {sig:?} planes differ from a fresh evaluation"
            ));
        }
        let count = users.entry(sig).or_default();
        *count = count.saturating_add(1);
    }
    let mut grants = 0u64;
    for (id, sig) in model.signatures.iter() {
        if users.get(&id).copied().unwrap_or(0) != sig.refcount() {
            return fail(format!("signature {id:?} refcount {}", sig.refcount()));
        }
        grants = grants.saturating_add(u64::try_from(sig.roles().len()).unwrap_or(0));
    }
    if grants != model.signatures.grant_total() {
        return fail("signature grant total".to_owned());
    }

    let mut seen: BTreeSet<ChannelIdx> = BTreeSet::new();
    let live: Vec<(ClassId, &crate::classes::Class)> = model.classes.iter().collect();
    for (i, (id, class)) in live.iter().enumerate() {
        if class.channels().is_empty() {
            return fail(format!("class {id:?} has no channels"));
        }
        if class.planes().words() != words {
            return fail(format!(
                "class {id:?} has {} words, want {words}",
                class.planes().words()
            ));
        }
        for plane in [
            class.planes().view(),
            class.planes().view_members(),
            class.planes().history(),
        ] {
            for (w, word) in plane.iter().enumerate() {
                if word & !bits::tail_mask(sets, w) != 0 {
                    return fail(format!("class {id:?} has bits beyond the last role set"));
                }
            }
        }
        let mut sorted = class.channels().to_vec();
        sorted.sort_unstable();
        if sorted != class.channels() {
            return fail(format!("class {id:?} channels unsorted"));
        }
        for channel in class.channels() {
            if model.classes.of_channel(*channel) != Some(*id) {
                return fail(format!("channel {channel:?} maps away from class {id:?}"));
            }
            if model.channels.get(*channel).is_none() {
                return fail(format!("class {id:?} holds dead channel {channel:?}"));
            }
            let Some(fresh) = fresh_content(model, members, *channel) else {
                return fail(format!("channel {channel:?} vanished"));
            };
            if !class.planes().same_as(&fresh.planes) {
                return fail(format!(
                    "channel {channel:?} planes differ from a fresh evaluation"
                ));
            }
            if class.overrides() != fresh.overrides.as_slice() {
                return fail(format!(
                    "channel {channel:?} overrides {:?} want {:?}",
                    class.overrides(),
                    fresh.overrides
                ));
            }
            seen.insert(*channel);
        }
        for (_, other) in live.iter().skip(i.saturating_add(1)) {
            if class.same_content(other) {
                return fail(format!("class {id:?} duplicates another class"));
            }
        }
    }
    for (idx, _) in model.channels.live() {
        if !seen.contains(&idx) {
            return fail(format!("channel {idx:?} has no class"));
        }
    }

    let quarantined: Vec<UserId> = members
        .iter()
        .filter(|e| e.is_quarantined())
        .map(|e| e.user())
        .collect();
    let rebuilt = Exceptions::build(
        model.owner,
        &model.channels,
        &model.virtual_view,
        quarantined.iter().copied(),
    );
    if rebuilt != *model.exceptions {
        return fail("exceptions differ from a rebuild".to_owned());
    }
    let mut counts = vec![0u32; sets];
    for entry in members.iter() {
        if entry.roleset().index() >= sets {
            return fail(format!("member {:?} names unknown role set", entry.user()));
        }
        if let Some(c) = counts.get_mut(entry.roleset().index()) {
            *c = c.saturating_add(1);
        }
        if entry.is_exception() != model.exceptions.contains(entry.user()) {
            return fail(format!("member {:?} exception flag", entry.user()));
        }
        if members.get(entry.user()) != Some(entry) {
            return fail(format!("member {:?} lookup", entry.user()));
        }
    }
    for (i, c) in counts.iter().enumerate() {
        let set = RoleSetId::from_index(i).unwrap_or(RoleSetId::EVERYONE);
        if members.refcount(set) != *c {
            return fail(format!(
                "role set {set:?} refcount {} want {c}",
                members.refcount(set)
            ));
        }
    }
    let live_sets = u32::try_from(counts.iter().filter(|c| **c > 0).count()).unwrap_or(0);
    if members.live_sets() != live_sets {
        return fail("live role set count".to_owned());
    }
    if members.quarantined_len() != quarantined.len() {
        return fail("quarantined count".to_owned());
    }
    if members.generation() != model.rolesets.generation() {
        return fail("index generation".to_owned());
    }

    let mut meter = Meter::new();
    for (set, _) in model.rolesets.iter() {
        let cached = model.set_viewable(set, &mut meter);
        let fresh = model.viewable_by(&mut meter, |class| {
            class.planes().get(set).contains(Visible::VIEW)
        });
        if cached.as_ref() != fresh.as_slice() {
            return fail(format!("viewable cache of {set:?} differs"));
        }
    }
    Ok(())
}

pub fn audit_class_ids(prev: &Model, next: &Model) -> Result<(), String> {
    if prev.rolesets.generation() != next.rolesets.generation() {
        return Ok(());
    }
    let contents: Vec<(ChannelIdx, Content)> = next
        .channels
        .live()
        .filter_map(|(idx, _)| next.classes.content_of(idx).map(|c| (idx, c)))
        .collect();
    let full = Classes::assign(
        &prev.classes,
        contents,
        next.channels.slots(),
        &mut Meter::new(),
    );
    if full != *next.classes {
        let mine: Vec<_> = next
            .channels
            .live()
            .map(|(i, _)| (i, next.classes.of_channel(i)))
            .collect();
        let want: Vec<_> = next
            .channels
            .live()
            .map(|(i, _)| (i, full.of_channel(i)))
            .collect();
        return fail(format!(
            "incremental class ids {mine:?} differ from the full procedure {want:?}"
        ));
    }
    Ok(())
}
