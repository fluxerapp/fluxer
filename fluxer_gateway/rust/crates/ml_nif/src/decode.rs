// SPDX-License-Identifier: AGPL-3.0-or-later

use ml_core::masks::MAX_UPDATES;
use ml_core::ranges::MAX_SYNC_RANGES;
use ml_core::render::MAX_FILL;
use ml_core::{
    Change, ChannelId, Config, Connection, EncodedItem, Event, GuildId, MAX_EVENTS, MaskUpdate,
    Ranges, SessionKey, Snapshot, Status, SyncScope, Timing, Updates, UserId,
};
use rustler::types::tuple::get_tuple;
use rustler::{Atom, Binary, LocalPid, Term, TermType};

use crate::atoms;
use crate::resource::Subscriber;

pub type Decoded<T> = Result<T, String>;

pub const MAX_CHANNELS: usize = 10_000;
pub const MAX_SNAPSHOT: usize = 4_000_000;
pub const MAX_SESSION_KEY: usize = 256;
pub const MAX_NAME: usize = 1 << 16;
pub const MAX_ENCODED: usize = 1 << 22;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Query {
    SessionRanges(SessionKey),
    ChannelCounts(Vec<ChannelId>),
    PageIds { offset: u32, limit: u32 },
    Subscriptions,
}

fn fail<T>(path: &str, what: &str) -> Decoded<T> {
    Err(format!("{path}: {what}"))
}

fn atom(term: Term<'_>) -> Option<Atom> {
    if term.get_type() == TermType::Atom {
        term.decode::<Atom>().ok()
    } else {
        None
    }
}

fn is(term: Term<'_>, expected: Atom) -> bool {
    atom(term) == Some(expected)
}

fn tuple<'a>(term: Term<'a>, arity: usize, path: &str) -> Decoded<Vec<Term<'a>>> {
    match get_tuple(term) {
        Ok(items) if items.len() == arity => Ok(items),
        Ok(_) => fail(path, "wrong arity"),
        Err(_) => fail(path, "expected a tuple"),
    }
}

fn tagged<'a>(term: Term<'a>, tags: &[Atom], arity: usize, path: &str) -> Decoded<Vec<Term<'a>>> {
    let items = tuple(term, arity, path)?;
    match items.first().and_then(|first| atom(*first)) {
        Some(found) if tags.contains(&found) => Ok(items),
        _ => fail(path, "wrong tag"),
    }
}

fn at<'a>(items: &[Term<'a>], index: usize, path: &str) -> Decoded<Term<'a>> {
    match items.get(index) {
        Some(term) => Ok(*term),
        None => fail(path, "missing field"),
    }
}

fn tag_of(term: Term<'_>) -> Option<Atom> {
    match atom(term) {
        Some(found) => Some(found),
        None => get_tuple(term)
            .ok()
            .and_then(|items| items.first().copied())
            .and_then(atom),
    }
}

pub fn list<'a>(term: Term<'a>, max: usize, path: &str) -> Decoded<Vec<Term<'a>>> {
    let mut out = Vec::new();
    let mut rest = term;
    loop {
        if rest.is_empty_list() {
            return Ok(out);
        }
        let Ok((head, tail)) = rest.list_get_cell() else {
            return fail(path, "expected a proper list");
        };
        if out.len() >= max {
            return fail(path, "too many items");
        }
        out.push(head);
        rest = tail;
    }
}

fn int(term: Term<'_>, path: &str) -> Decoded<u64> {
    if term.get_type() != TermType::Integer {
        return fail(path, "expected an integer");
    }
    match term.decode::<u64>() {
        Ok(value) => Ok(value),
        Err(_) => fail(path, "integer out of range"),
    }
}

fn small(term: Term<'_>, path: &str) -> Decoded<u32> {
    let value = int(term, path)?;
    match u32::try_from(value) {
        Ok(value) => Ok(value),
        Err(_) => fail(path, "integer out of range"),
    }
}

fn boolean(term: Term<'_>, path: &str) -> Decoded<bool> {
    match atom(term) {
        Some(found) if found == atoms::true_() => Ok(true),
        Some(found) if found == atoms::false_() => Ok(false),
        _ => fail(path, "expected a boolean"),
    }
}

fn snowflake(term: Term<'_>, path: &str) -> Decoded<u64> {
    let value = int(term, path)?;
    if value == 0 || value > i64::MAX as u64 {
        return fail(path, "expected a positive id");
    }
    Ok(value)
}

pub fn guild(term: Term<'_>, path: &str) -> Decoded<GuildId> {
    snowflake(term, path).map(GuildId::new)
}

pub fn user(term: Term<'_>, path: &str) -> Decoded<UserId> {
    snowflake(term, path).map(UserId::new)
}

pub fn channel(term: Term<'_>, path: &str) -> Decoded<ChannelId> {
    snowflake(term, path).map(ChannelId::new)
}

fn bytes(term: Term<'_>, max: usize, path: &str) -> Decoded<Box<[u8]>> {
    if term.get_type() != TermType::Binary {
        return fail(path, "expected a binary");
    }
    match term.decode::<Binary<'_>>() {
        Ok(binary) if binary.len() <= max => Ok(binary.as_slice().into()),
        Ok(_) => fail(path, "binary too large"),
        Err(_) => fail(path, "expected a binary"),
    }
}

pub fn session(term: Term<'_>, path: &str) -> Decoded<SessionKey> {
    bytes(term, MAX_SESSION_KEY, path).map(|key| SessionKey::new(&key))
}

pub fn owner(term: Term<'_>, path: &str) -> Decoded<LocalPid> {
    if term.get_type() != TermType::Pid {
        return fail(path, "expected a local pid");
    }
    match term.decode::<LocalPid>() {
        Ok(found) => Ok(found),
        Err(_) => fail(path, "expected a local pid"),
    }
}

pub fn subscriber(term: Term<'_>, path: &str) -> Decoded<Subscriber> {
    int(term, path).map(Subscriber)
}

fn change(term: Term<'_>, path: &str) -> Decoded<Change> {
    match atom(term) {
        Some(found) if found == atoms::changed() => Ok(Change::Changed),
        Some(found) if found == atoms::unchanged() => Ok(Change::Unchanged),
        _ => fail(path, "expected changed or unchanged"),
    }
}

fn timing(term: Term<'_>, path: &str) -> Decoded<Timing> {
    match atom(term) {
        Some(found) if found == atoms::immediate() => Ok(Timing::Immediate),
        Some(found) if found == atoms::deferred() => Ok(Timing::Deferred),
        _ => fail(path, "expected immediate or deferred"),
    }
}

pub fn status(term: Term<'_>, path: &str) -> Decoded<Status> {
    if let Some(found) = atom(term) {
        return match found {
            f if f == atoms::online() => Ok(Status::Online),
            f if f == atoms::idle() => Ok(Status::Idle),
            f if f == atoms::dnd() => Ok(Status::Dnd),
            f if f == atoms::invisible() => Ok(Status::Invisible),
            f if f == atoms::offline() => Ok(Status::Offline),
            _ => fail(path, "unknown status"),
        };
    }
    let items = tagged(term, &[atoms::other_status()], 2, path)?;
    bytes(at(&items, 1, path)?, MAX_NAME, path)?;
    Ok(Status::Other)
}

fn connection(term: Term<'_>, path: &str) -> Decoded<Connection> {
    if is(term, atoms::disconnected()) {
        return Ok(Connection::Disconnected);
    }
    let items = tagged(term, &[atoms::connected()], 2, path)?;
    Ok(Connection::Connected {
        presence: change(at(&items, 1, path)?, path)?,
    })
}

fn updates(term: Term<'_>, path: &str) -> Decoded<Updates> {
    match atom(term) {
        Some(found) if found == atoms::updates_on() || found == atoms::true_() => {
            Ok(Updates::UpdatesOn)
        }
        Some(found) if found == atoms::updates_off() || found == atoms::false_() => {
            Ok(Updates::UpdatesOff)
        }
        _ => fail(path, "expected updates_on or updates_off"),
    }
}

fn sync_scope(term: Term<'_>, path: &str) -> Decoded<SyncScope> {
    if is(term, atoms::all_lists()) {
        return Ok(SyncScope::AllLists);
    }
    let items = tagged(term, &[atoms::channel_list()], 2, path)?;
    Ok(SyncScope::Channel(channel(at(&items, 1, path)?, path)?))
}

fn pairs(term: Term<'_>, path: &str) -> Decoded<Vec<(u32, u32)>> {
    let mut out = Vec::new();
    for item in list(term, MAX_SYNC_RANGES, path)? {
        let pair = tuple(item, 2, path)?;
        out.push((
            small(at(&pair, 0, path)?, path)?,
            small(at(&pair, 1, path)?, path)?,
        ));
    }
    Ok(out)
}

pub fn ranges(term: Term<'_>, path: &str) -> Decoded<Ranges> {
    let items = pairs(term, path)?;
    Ranges::normalized(&items).map_err(|e| format!("{path}: {e:?}"))
}

fn sync_ranges(term: Term<'_>, path: &str) -> Decoded<Ranges> {
    let items = pairs(term, path)?;
    Ranges::sync(&items).map_err(|e| format!("{path}: {e:?}"))
}

fn one_user(term: Term<'_>, tag: Atom, path: &str) -> Decoded<UserId> {
    let f = tagged(term, &[tag], 2, path)?;
    user(at(&f, 1, path)?, path)
}

pub fn event(term: Term<'_>) -> Decoded<Event<Subscriber>> {
    let Some(tag) = tag_of(term) else {
        return fail("event", "expected a tagged event");
    };
    if tag == atoms::flush() && atom(term).is_some() {
        return Ok(Event::Flush);
    }
    if tag == atoms::member_upsert() {
        let p = "event.member_upsert";
        let f = tagged(term, &[tag], 4, p)?;
        return Ok(Event::MemberUpsert {
            user: user(at(&f, 1, p)?, "event.member_upsert.user")?,
            display_name: bytes(at(&f, 2, p)?, MAX_NAME, "event.member_upsert.display_name")?,
            change: change(at(&f, 3, p)?, "event.member_upsert.change")?,
        });
    }
    if tag == atoms::member_remove() {
        return Ok(Event::MemberRemove(one_user(
            term,
            tag,
            "event.member_remove",
        )?));
    }
    if tag == atoms::presence() {
        let p = "event.presence";
        let f = tagged(term, &[tag], 5, p)?;
        return Ok(Event::Presence {
            user: user(at(&f, 1, p)?, "event.presence.user")?,
            status: status(at(&f, 2, p)?, "event.presence.status")?,
            change: change(at(&f, 3, p)?, "event.presence.change")?,
            timing: timing(at(&f, 4, p)?, "event.presence.timing")?,
        });
    }
    if tag == atoms::connection() {
        let p = "event.connection";
        let f = tagged(term, &[tag], 3, p)?;
        return Ok(Event::Connection(
            user(at(&f, 1, p)?, "event.connection.user")?,
            connection(at(&f, 2, p)?, "event.connection.state")?,
        ));
    }
    if tag == atoms::subscribe() {
        let p = "event.subscribe";
        let f = tagged(term, &[tag], 7, p)?;
        return Ok(Event::Subscribe {
            session: session(at(&f, 1, p)?, "event.subscribe.session")?,
            pid: subscriber(at(&f, 2, p)?, "event.subscribe.subscriber")?,
            user: user(at(&f, 3, p)?, "event.subscribe.user")?,
            channel: channel(at(&f, 4, p)?, "event.subscribe.channel")?,
            ranges: ranges(at(&f, 5, p)?, "event.subscribe.ranges")?,
            sync: sync_ranges(at(&f, 6, p)?, "event.subscribe.sync")?,
        });
    }
    if tag == atoms::drop_session() {
        let p = "event.drop_session";
        let f = tagged(term, &[tag], 2, p)?;
        return Ok(Event::DropSession(session(
            at(&f, 1, p)?,
            "event.drop_session.session",
        )?));
    }
    if tag == atoms::view_gained() {
        let p = "event.view_gained";
        let f = tagged(term, &[tag], 3, p)?;
        return Ok(Event::ViewGained {
            session: session(at(&f, 1, p)?, "event.view_gained.session")?,
            channel: channel(at(&f, 2, p)?, "event.view_gained.channel")?,
        });
    }
    if tag == atoms::permissions() {
        let p = "event.permissions";
        let f = tagged(term, &[tag], 2, p)?;
        return Ok(Event::Permissions(sync_scope(
            at(&f, 1, p)?,
            "event.permissions.scope",
        )?));
    }
    if tag == atoms::set_updates() {
        let p = "event.set_updates";
        let f = tagged(term, &[tag], 2, p)?;
        return Ok(Event::Updates(updates(
            at(&f, 1, p)?,
            "event.set_updates.updates",
        )?));
    }
    fail("event", "unknown event")
}

pub fn events(term: Term<'_>) -> Decoded<Vec<Event<Subscriber>>> {
    list(term, MAX_EVENTS, "events")?
        .into_iter()
        .map(event)
        .collect()
}

pub fn config(term: Term<'_>) -> Decoded<Config> {
    let p = "config";
    let f = tagged(term, &[atoms::ml_config(), atoms::config()], 7, p)?;
    let config = Config {
        offline_threshold: small(at(&f, 1, p)?, "config.offline_threshold")?,
        offline_drop: small(at(&f, 2, p)?, "config.offline_drop")?,
        work_budget_units: int(at(&f, 3, p)?, "config.work_budget_units")?,
        fragment_budget_bytes: int(at(&f, 4, p)?, "config.fragment_budget_bytes")?,
        payload_budget_bytes: int(at(&f, 5, p)?, "config.payload_budget_bytes")?,
        seed: int(at(&f, 6, p)?, "config.seed")?,
    };
    if config.work_budget_units == 0 {
        return fail("config.work_budget_units", "must be positive");
    }
    if config.offline_drop < config.offline_threshold {
        return fail("config.offline_drop", "below the offline threshold");
    }
    Ok(config)
}

pub fn snapshot(term: Term<'_>) -> Decoded<Snapshot> {
    let p = "snapshot";
    let f = tagged(term, &[atoms::ml_snapshot(), atoms::snapshot()], 5, p)?;
    let mut members = Vec::new();
    for item in list(at(&f, 1, p)?, MAX_SNAPSHOT, "snapshot.members")? {
        let pair = tuple(item, 2, "snapshot.members")?;
        members.push((
            user(at(&pair, 0, p)?, "snapshot.members.user")?,
            bytes(at(&pair, 1, p)?, MAX_NAME, "snapshot.members.display_name")?,
        ));
    }
    let mut statuses = Vec::new();
    for item in list(at(&f, 2, p)?, MAX_SNAPSHOT, "snapshot.statuses")? {
        let pair = tuple(item, 2, "snapshot.statuses")?;
        statuses.push((
            user(at(&pair, 0, p)?, "snapshot.statuses.user")?,
            status(at(&pair, 1, p)?, "snapshot.statuses.status")?,
        ));
    }
    let mut connected = Vec::new();
    for item in list(at(&f, 3, p)?, MAX_SNAPSHOT, "snapshot.connected")? {
        connected.push(user(item, "snapshot.connected")?);
    }
    let updates_off = updates(at(&f, 4, p)?, "snapshot.updates")? == Updates::UpdatesOff;
    Ok(Snapshot {
        members,
        statuses,
        connected,
        updates_off,
    })
}

pub fn snapshot_rows(term: Term<'_>, max: usize) -> usize {
    let Ok(fields) = tagged(
        term,
        &[atoms::ml_snapshot(), atoms::snapshot()],
        5,
        "snapshot",
    ) else {
        return 0;
    };
    fields.iter().skip(1).take(3).fold(0, |seen: usize, rows| {
        seen.saturating_add(length(*rows, max.saturating_sub(seen)))
    })
}

fn length(term: Term<'_>, max: usize) -> usize {
    let mut seen = 0usize;
    let mut rest = term;
    while seen <= max {
        let Ok((_, tail)) = rest.list_get_cell() else {
            break;
        };
        seen = seen.saturating_add(1);
        rest = tail;
    }
    seen
}

pub fn option<'a>(term: Term<'a>, path: &str) -> Decoded<Option<Term<'a>>> {
    if is(term, atoms::none()) {
        return Ok(None);
    }
    let items = tagged(term, &[atoms::some()], 2, path)?;
    at(&items, 1, path).map(Some)
}

pub fn items(term: Term<'_>) -> Decoded<Vec<EncodedItem>> {
    let entries = list(term, MAX_FILL, "items")?;
    let mut out = Vec::with_capacity(entries.len());
    for entry in entries {
        let p = "items.encoded_item";
        let e = tagged(entry, &[atoms::encoded_item()], 4, p)?;
        out.push(EncodedItem {
            user: user(at(&e, 1, p)?, "items.encoded_item.user")?,
            list: bytes(at(&e, 2, p)?, MAX_ENCODED, "items.encoded_item.list_bytes")?,
            page: bytes(at(&e, 3, p)?, MAX_ENCODED, "items.encoded_item.page_bytes")?,
        });
    }
    Ok(out)
}

pub fn query(term: Term<'_>) -> Decoded<Query> {
    let Some(tag) = tag_of(term) else {
        return fail("query", "expected a tagged query");
    };
    if tag == atoms::subscriptions() && atom(term).is_some() {
        return Ok(Query::Subscriptions);
    }
    if tag == atoms::session_ranges() {
        let p = "query.session_ranges";
        let f = tagged(term, &[tag], 2, p)?;
        return Ok(Query::SessionRanges(session(
            at(&f, 1, p)?,
            "query.session_ranges.session",
        )?));
    }
    if tag == atoms::channel_counts() {
        let p = "query.channel_counts";
        let f = tagged(term, &[tag], 2, p)?;
        let mut out = Vec::new();
        for item in list(at(&f, 1, p)?, MAX_CHANNELS, "query.channel_counts.channels")? {
            out.push(channel(item, "query.channel_counts.channels")?);
        }
        return Ok(Query::ChannelCounts(out));
    }
    if tag == atoms::page_ids() {
        let p = "query.page_ids";
        let f = tagged(term, &[tag], 3, p)?;
        return Ok(Query::PageIds {
            offset: small(at(&f, 1, p)?, "query.page_ids.offset")?,
            limit: small(at(&f, 2, p)?, "query.page_ids.limit")?,
        });
    }
    fail("query", "unknown query")
}

pub fn mask_updates(term: Term<'_>) -> Decoded<Vec<MaskUpdate>> {
    let mut out = Vec::new();
    for item in list(term, MAX_UPDATES, "mask_updates")? {
        let p = "mask_updates.mask_update";
        let f = tagged(item, &[atoms::mask_update()], 6, p)?;
        out.push(MaskUpdate {
            user: user(at(&f, 1, p)?, "mask_updates.mask_update.user")?,
            all_messages: boolean(at(&f, 2, p)?, "mask_updates.mask_update.all_messages")?,
            uncertain: boolean(at(&f, 3, p)?, "mask_updates.mask_update.uncertain")?,
            muted: boolean(at(&f, 4, p)?, "mask_updates.mask_update.muted")?,
            active: boolean(at(&f, 5, p)?, "mask_updates.mask_update.active")?,
        });
    }
    Ok(out)
}

pub fn mask_query(term: Term<'_>) -> Decoded<u64> {
    let p = "mask_query";
    let f = tagged(term, &[atoms::mask_query(), atoms::supersets()], 2, p)?;
    int(at(&f, 1, p)?, "mask_query.epoch")
}
