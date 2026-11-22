// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashMap;
use std::hash::Hash;

use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Config, Cutoff, Event, GuildId, Hoist, Need,
    Nsfw, OverwriteSpec, OverwriteTarget, Permissions, RankPos, RoleId, RoleList, RoleSpec, Scope,
    Snapshot, UserId,
};

pub const MAX_TERMS: usize = 1 << 22;
pub const MAX_IDS: usize = 1 << 20;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Shape {
    Atom,
    Integer,
    Tuple,
    List,
    Nil,
    Binary,
    Other,
}

macro_rules! names {
    ($($variant:ident = $text:literal,)*) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
        pub enum Name {
            $($variant,)*
        }

        impl Name {
            pub const ALL: &'static [Name] = &[$(Name::$variant,)*];

            pub const fn text(self) -> &'static str {
                match self {
                    $(Name::$variant => $text,)*
                }
            }
        }
    };
}

names! {
    True = "true",
    False = "false",
    Some = "some",
    None = "none",
    GuildScope = "guild_scope",
    ChannelScope = "channel_scope",
    View = "view",
    ViewMembers = "view_members",
    MessageAccess = "message_access",
    History = "history",
    ChannelBits = "channel_bits",
    GuildBits = "guild_bits",
    Position = "position",
    UndefinedPosition = "undefined_position",
    NotHoisted = "not_hoisted",
    Hoisted = "hoisted",
    RoleTarget = "role_target",
    MemberTarget = "member_target",
    Role = "role",
    Member = "member",
    InheritNsfw = "inherit_nsfw",
    SafeForWork = "safe_for_work",
    AdultOnly = "adult_only",
    Text = "text",
    Dm = "dm",
    Voice = "voice",
    GroupDm = "group_dm",
    Category = "category",
    Announcement = "announcement",
    AnnouncementThread = "announcement_thread",
    PublicThread = "public_thread",
    PrivateThread = "private_thread",
    Forum = "forum",
    Media = "media",
    ThreadKind = "thread_kind",
    OtherKind = "other_kind",
    NoCutoff = "no_cutoff",
    CutoffAt = "cutoff_at",
    MalformedCutoff = "malformed_cutoff",
    Missing = "missing",
    At = "at",
    Malformed = "malformed",
    MalformedRoles = "malformed_roles",
    Roles = "roles",
    RoleIds = "role_ids",
    Owner = "owner",
    ChannelUpsert = "channel_upsert",
    ChannelDelete = "channel_delete",
    ChannelOrder = "channel_order",
    MemberUpsert = "member_upsert",
    MemberRemove = "member_remove",
    VirtualAccess = "virtual_access",
    HistoryCutoff = "history_cutoff",
    GuildNsfw = "guild_nsfw",
    Created = "created",
    Updated = "updated",
    PackedIds = "packed_ids",
    VisitorChannels = "visitor_channels",
    PublicOnline = "public_online",
    MutualOnline = "mutual_online",
    PushPrune = "push_prune",
    GroupOf = "group_of",
    UserPermissions = "user_permissions",
    CheckPermission = "check_permission",
    ViewableChannels = "viewable_channels",
}

pub trait Source: Copy {
    fn shape(self) -> Shape;
    fn is(self, name: Name) -> bool;
    fn uint(self) -> Option<u64>;
    fn int(self) -> Option<i64>;
    fn elements(self) -> Option<Vec<Self>>;
    fn cell(self) -> Option<(Self, Self)>;
    fn words(self) -> Option<Vec<u64>>;
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct BadInput(pub String);

impl BadInput {
    pub fn at(what: &str, problem: &str) -> BadInput {
        BadInput(format!("{what}: {problem}"))
    }
}

pub type Decoded<T> = Result<T, BadInput>;

fn bad<T>(what: &str, problem: &str) -> Decoded<T> {
    Err(BadInput::at(what, problem))
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Query {
    VisitorChannels(bool),
    PublicOnline(Vec<UserId>),
    MutualOnline(UserId, Vec<UserId>),
    PushPrune(PushPruneQuery),
    GroupOf(UserId),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PushPruneQuery {
    pub channel: ChannelId,
    pub message_ms: i64,
    pub mentioned: Vec<UserId>,
    pub roles: Vec<RoleId>,
    pub everyone: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ReplyQuery {
    UserPermissions(UserId, Scope),
    CheckPermission(UserId, Scope, Permissions),
    ViewableChannels(UserId),
}

pub fn uint<S: Source>(term: S, what: &str) -> Decoded<u64> {
    match term.shape() {
        Shape::Integer => term
            .uint()
            .map_or_else(|| bad(what, "integer out of range"), Ok),
        Shape::Tuple => match term.elements().as_deref() {
            Some([tag, value]) if tag.shape() == Shape::Atom && value.shape() == Shape::Integer => {
                value
                    .uint()
                    .map_or_else(|| bad(what, "integer out of range"), Ok)
            }
            _ => bad(what, "expected an integer"),
        },
        _ => bad(what, "expected an integer"),
    }
}

pub fn int<S: Source>(term: S, what: &str) -> Decoded<i64> {
    match term.shape() {
        Shape::Integer => term
            .int()
            .map_or_else(|| bad(what, "integer out of range"), Ok),
        _ => bad(what, "expected an integer"),
    }
}

pub fn id<S: Source>(term: S, what: &str) -> Decoded<u64> {
    match uint(term, what)? {
        0 => bad(what, "id is not positive"),
        value => Ok(value),
    }
}

pub fn user<S: Source>(term: S, what: &str) -> Decoded<UserId> {
    id(term, what).map(UserId::new)
}

pub fn channel<S: Source>(term: S, what: &str) -> Decoded<ChannelId> {
    id(term, what).map(ChannelId::new)
}

pub fn role<S: Source>(term: S, what: &str) -> Decoded<RoleId> {
    id(term, what).map(RoleId::new)
}

pub fn boolean<S: Source>(term: S, what: &str) -> Decoded<bool> {
    if term.is(Name::True) {
        Ok(true)
    } else if term.is(Name::False) {
        Ok(false)
    } else {
        bad(what, "expected a boolean")
    }
}

pub fn permissions<S: Source>(term: S, what: &str) -> Decoded<Permissions> {
    uint(term, what).map(Permissions::from_bits_retain)
}

fn tuple<S: Source>(term: S, what: &str) -> Decoded<Vec<S>> {
    if term.shape() != Shape::Tuple {
        return bad(what, "expected a tuple");
    }
    term.elements()
        .map_or_else(|| bad(what, "expected a tuple"), Ok)
}

fn tagged<S: Source>(term: S, what: &str) -> Decoded<(S, Vec<S>)> {
    let mut items = tuple(term, what)?;
    if items.is_empty() {
        return bad(what, "wrong tag");
    }
    let tag = items.remove(0);
    if tag.shape() != Shape::Atom {
        return bad(what, "wrong tag");
    }
    Ok((tag, items))
}

fn record<S: Source>(term: S, what: &str) -> Decoded<Vec<S>> {
    let items = tuple(term, what)?;
    match items.first() {
        Some(tag) if tag.shape() == Shape::Atom && items.len() > 1 => {
            Ok(items.into_iter().skip(1).collect())
        }
        _ => Ok(items),
    }
}

pub fn option<S: Source>(term: S, what: &str) -> Decoded<Option<S>> {
    if term.is(Name::None) {
        return Ok(None);
    }
    let (tag, items) = tagged(term, what)?;
    match items.as_slice() {
        [value] if tag.is(Name::Some) => Ok(Some(*value)),
        _ => bad(what, "expected an option"),
    }
}

pub struct Decoder {
    terms: usize,
}

impl Default for Decoder {
    fn default() -> Decoder {
        Decoder::new()
    }
}

impl Decoder {
    pub const fn new() -> Decoder {
        Decoder { terms: 0 }
    }

    fn spend(&mut self, what: &str) -> Decoded<()> {
        if self.terms >= MAX_TERMS {
            return bad(what, "input too large");
        }
        self.terms = self.terms.saturating_add(1);
        Ok(())
    }

    pub fn list<S: Source>(&mut self, term: S, max: usize, what: &str) -> Decoded<Vec<S>> {
        let mut out = Vec::new();
        let mut rest = term;
        loop {
            match rest.shape() {
                Shape::Nil => return Ok(out),
                Shape::List => {
                    let Some((item, tail)) = rest.cell() else {
                        return bad(what, "expected a list");
                    };
                    if out.len() >= max {
                        return bad(what, "too many items");
                    }
                    self.spend(what)?;
                    out.push(item);
                    rest = tail;
                }
                _ if out.is_empty() => return bad(what, "expected a list"),
                _ => return bad(what, "improper list"),
            }
        }
    }

    pub fn ids<S: Source>(&mut self, term: S, max: usize, what: &str) -> Decoded<Vec<u64>> {
        if term.shape() == Shape::Tuple {
            let (tag, items) = tagged(term, what)?;
            let [bytes] = items.as_slice() else {
                return bad(what, "wrong arity");
            };
            if !tag.is(Name::PackedIds) {
                return bad(what, "wrong tag");
            }
            return packed(*bytes, max, what);
        }
        let items = self.list(term, max, what)?;
        items.into_iter().map(|item| id(item, what)).collect()
    }

    pub fn users<S: Source>(&mut self, term: S, max: usize, what: &str) -> Decoded<Vec<UserId>> {
        Ok(self
            .ids(term, max, what)?
            .into_iter()
            .map(UserId::new)
            .collect())
    }

    pub fn channels<S: Source>(
        &mut self,
        term: S,
        max: usize,
        what: &str,
    ) -> Decoded<Vec<ChannelId>> {
        Ok(self
            .ids(term, max, what)?
            .into_iter()
            .map(ChannelId::new)
            .collect())
    }

    pub fn roles<S: Source>(&mut self, term: S, max: usize, what: &str) -> Decoded<Vec<RoleId>> {
        Ok(self
            .ids(term, max, what)?
            .into_iter()
            .map(RoleId::new)
            .collect())
    }

    pub fn config<S: Source>(&mut self, term: S) -> Decoded<Config> {
        const WHAT: &str = "config";
        if term.shape() == Shape::Atom {
            return Ok(Config::default());
        }
        let fields = record(term, WHAT)?;
        let [budget, cache, floor, chunk] = fields.as_slice() else {
            return bad(WHAT, "wrong arity");
        };
        let to_u32 = |value: u64| u32::try_from(value).map_err(|_| BadInput::at(WHAT, "too large"));
        let config = Config {
            work_budget_units: uint(*budget, WHAT)?.max(1),
            viewable_cache_bytes: uint(*cache, WHAT)?,
            dead_role_set_floor: to_u32(uint(*floor, WHAT)?)?,
            scan_chunk: to_u32(uint(*chunk, WHAT)?)?.max(1),
        };
        Ok(config)
    }

    pub fn scope<S: Source>(&mut self, term: S) -> Decoded<Scope> {
        const WHAT: &str = "scope";
        if term.is(Name::GuildScope) {
            return Ok(Scope::Guild);
        }
        let (tag, items) = tagged(term, WHAT)?;
        match items.as_slice() {
            [channel_id] if tag.is(Name::ChannelScope) => {
                Ok(Scope::Channel(channel(*channel_id, WHAT)?))
            }
            _ => bad(WHAT, "wrong tag"),
        }
    }

    pub fn need<S: Source>(&mut self, term: S) -> Decoded<Need> {
        const WHAT: &str = "need";
        if term.is(Name::View) {
            return Ok(Need::View);
        }
        if term.is(Name::ViewMembers) {
            return Ok(Need::ViewMembers);
        }
        let (tag, items) = tagged(term, WHAT)?;
        let [value] = items.as_slice() else {
            return bad(WHAT, "wrong arity");
        };
        if tag.is(Name::MessageAccess) {
            Ok(Need::MessageAccess(int(*value, WHAT)?))
        } else if tag.is(Name::History) {
            Ok(Need::History(int(*value, WHAT)?))
        } else if tag.is(Name::ChannelBits) {
            Ok(Need::ChannelBits(permissions(*value, WHAT)?))
        } else if tag.is(Name::GuildBits) {
            Ok(Need::GuildBits(permissions(*value, WHAT)?))
        } else {
            bad(WHAT, "wrong tag")
        }
    }

    pub fn snapshot<S: Source>(&mut self, term: S) -> Decoded<Snapshot> {
        const WHAT: &str = "snapshot";
        let fields = record(term, WHAT)?;
        match fields.as_slice() {
            [guild, events] => self.event_snapshot(*guild, *events),
            [
                guild,
                owner,
                roles,
                channels,
                members,
                virtual_access,
                cutoff,
                nsfw,
                level,
            ] => {
                let guild = GuildId::new(id(*guild, "snapshot guild")?);
                let mut snapshot = Snapshot::new(guild, self.owner(*owner)?);
                snapshot.roles = self.role_specs(*roles)?;
                let terms = self.list(*channels, MAX_TERMS, "snapshot channels")?;
                snapshot.channels = terms
                    .into_iter()
                    .map(|c| self.channel_spec(c))
                    .collect::<Decoded<_>>()?;
                let terms = self.list(*members, MAX_TERMS, "snapshot members")?;
                snapshot.members = terms
                    .into_iter()
                    .map(|m| self.member(m))
                    .collect::<Decoded<_>>()?;
                let terms = self.list(*virtual_access, MAX_TERMS, "snapshot virtual access")?;
                snapshot.virtual_access = terms
                    .into_iter()
                    .map(|v| self.virtual_row(v))
                    .collect::<Decoded<_>>()?;
                snapshot.cutoff = cutoff_of(*cutoff)?;
                snapshot.nsfw = guild_nsfw(*nsfw)?;
                snapshot.nsfw_level = nsfw_level(*level)?;
                Ok(snapshot)
            }
            _ => bad(WHAT, "wrong arity"),
        }
    }

    fn event_snapshot<S: Source>(&mut self, guild: S, events: S) -> Decoded<Snapshot> {
        let guild = GuildId::new(id(guild, "snapshot guild")?);
        let mut snapshot = Snapshot::new(guild, None);
        let mut channels = Keyed::new();
        let mut members = Keyed::new();
        let mut virtual_access = Keyed::new();
        let terms = self.list(events, MAX_TERMS, "snapshot events")?;
        for (at, term) in terms.into_iter().enumerate() {
            let event = self
                .event(term)
                .map_err(|BadInput(problem)| BadInput(format!("event {at}: {problem}")))?;
            match event {
                Event::Roles(roles) => snapshot.roles = roles,
                Event::Owner(owner) => snapshot.owner = owner,
                Event::ChannelUpsert(spec, _) => channels.upsert(spec.id, spec),
                Event::ChannelDelete(id) => channels.remove(id),
                Event::ChannelOrder(order) => channels.reorder(&order),
                Event::MemberUpsert(user, roles) => members.upsert(user, roles),
                Event::MemberRemove(user) => members.remove(user),
                Event::VirtualAccess(user, view) if view.is_empty() => virtual_access.remove(user),
                Event::VirtualAccess(user, view) => virtual_access.upsert(user, view),
                Event::HistoryCutoff(cutoff) => snapshot.cutoff = cutoff,
                Event::GuildNsfw(nsfw, level) => {
                    snapshot.nsfw = nsfw;
                    snapshot.nsfw_level = level;
                }
            }
        }
        snapshot.channels = channels.into_values();
        snapshot.members = members.into_entries();
        snapshot.virtual_access = virtual_access.into_entries();
        Ok(snapshot)
    }

    fn owner<S: Source>(&mut self, term: S) -> Decoded<Option<UserId>> {
        option(term, "owner")?
            .map(|value| user(value, "owner"))
            .transpose()
    }

    pub fn events<S: Source>(&mut self, term: S, max: usize) -> Decoded<Vec<Event>> {
        let terms = self.list(term, max, "events")?;
        let mut out = Vec::with_capacity(terms.len());
        for (at, term) in terms.into_iter().enumerate() {
            let event = self
                .event(term)
                .map_err(|BadInput(problem)| BadInput(format!("event {at}: {problem}")))?;
            out.push(event);
        }
        Ok(out)
    }

    pub fn event<S: Source>(&mut self, term: S) -> Decoded<Event> {
        const WHAT: &str = "event";
        self.spend(WHAT)?;
        let (tag, items) = tagged(term, WHAT)?;
        match items.as_slice() {
            [roles] if tag.is(Name::Roles) => Ok(Event::Roles(self.role_specs(*roles)?)),
            [owner] if tag.is(Name::Owner) => Ok(Event::Owner(self.owner(*owner)?)),
            [spec, cause] if tag.is(Name::ChannelUpsert) => {
                let spec = self.channel_spec(*spec)?;
                let cause = if cause.is(Name::Created) {
                    ChannelCause::Created
                } else if cause.is(Name::Updated) {
                    ChannelCause::Updated
                } else {
                    return bad("channel cause", "expected created or updated");
                };
                Ok(Event::ChannelUpsert(spec, cause))
            }
            [spec] if tag.is(Name::ChannelUpsert) => Ok(Event::ChannelUpsert(
                self.channel_spec(*spec)?,
                ChannelCause::Updated,
            )),
            [channel_id] if tag.is(Name::ChannelDelete) => Ok(Event::ChannelDelete(channel(
                *channel_id,
                "channel_delete",
            )?)),
            [order] if tag.is(Name::ChannelOrder) => Ok(Event::ChannelOrder(self.channels(
                *order,
                MAX_TERMS,
                "channel_order",
            )?)),
            [user_id, roles] if tag.is(Name::MemberUpsert) => Ok(Event::MemberUpsert(
                user(*user_id, "member_upsert user")?,
                self.role_list(*roles)?,
            )),
            [user_id] if tag.is(Name::MemberRemove) => {
                Ok(Event::MemberRemove(user(*user_id, "member_remove")?))
            }
            [user_id, view] if tag.is(Name::VirtualAccess) => Ok(Event::VirtualAccess(
                user(*user_id, "virtual_access user")?,
                self.channels(*view, MAX_TERMS, "virtual_access")?,
            )),
            [cutoff] if tag.is(Name::HistoryCutoff) => {
                Ok(Event::HistoryCutoff(cutoff_of(*cutoff)?))
            }
            [nsfw, level] if tag.is(Name::GuildNsfw) => {
                Ok(Event::GuildNsfw(guild_nsfw(*nsfw)?, nsfw_level(*level)?))
            }
            _ => bad(WHAT, "wrong tag or arity"),
        }
    }

    fn role_specs<S: Source>(&mut self, term: S) -> Decoded<Vec<RoleSpec>> {
        let terms = self.list(term, MAX_TERMS, "roles")?;
        let mut out = Vec::with_capacity(terms.len());
        for term in terms {
            out.push(self.role_spec(term)?);
        }
        Ok(out)
    }

    fn role_spec<S: Source>(&mut self, term: S) -> Decoded<RoleSpec> {
        const WHAT: &str = "role";
        self.spend(WHAT)?;
        let fields = record(term, WHAT)?;
        let mut rest = fields.into_iter().filter(|f| f.shape() != Shape::Binary);
        let Some(role_id) = rest.next() else {
            return bad(WHAT, "wrong arity");
        };
        let Some(bits) = rest.next() else {
            return bad(WHAT, "wrong arity");
        };
        let Some(position) = rest.next() else {
            return bad(WHAT, "wrong arity");
        };
        let Some(hoist) = rest.next() else {
            return bad(WHAT, "wrong arity");
        };
        Ok(RoleSpec {
            id: role(role_id, WHAT)?,
            permissions: permissions(bits, "role permissions")?,
            position: rank(position)?,
            hoist: hoist_of(hoist)?,
        })
    }

    fn channel_spec<S: Source>(&mut self, term: S) -> Decoded<ChannelSpec> {
        const WHAT: &str = "channel";
        self.spend(WHAT)?;
        let fields = record(term, WHAT)?;
        let [channel_id, kind, parent, rest @ ..] = fields.as_slice() else {
            return bad(WHAT, "wrong arity");
        };
        let parent = option(*parent, "channel parent")?
            .map(|p| channel(p, "channel parent"))
            .transpose()?;
        let mut overwrites = None;
        let mut nsfw = None;
        let mut flags = Vec::new();
        for field in rest {
            match field.shape() {
                Shape::List | Shape::Nil if overwrites.is_none() => {
                    let terms = self.list(*field, MAX_TERMS, "overwrites")?;
                    let mut list = Vec::with_capacity(terms.len());
                    for overwrite in terms {
                        list.push(self.overwrite(overwrite)?);
                    }
                    overwrites = Some(list);
                }
                Shape::Atom if field.is(Name::True) || field.is(Name::False) => {
                    flags.push(boolean(*field, "channel flag")?);
                }
                Shape::Atom if nsfw.is_none() => nsfw = Some(nsfw_of(*field)?),
                _ => {}
            }
        }
        let (legacy, named) = match flags.as_slice() {
            [] => (false, true),
            [named] => (false, *named),
            [legacy, named, ..] => (*legacy, *named),
        };
        let nsfw = match nsfw.unwrap_or(Nsfw::Inherit) {
            Nsfw::Inherit if legacy => Nsfw::Adult,
            other => other,
        };
        Ok(ChannelSpec {
            id: channel(*channel_id, WHAT)?,
            kind: kind_of(*kind)?,
            parent,
            overwrites: overwrites.unwrap_or_default(),
            nsfw,
            named,
        })
    }

    fn overwrite<S: Source>(&mut self, term: S) -> Decoded<OverwriteSpec> {
        const WHAT: &str = "overwrite";
        self.spend(WHAT)?;
        let fields = record(term, WHAT)?;
        let [target, allow, deny] = fields.as_slice() else {
            return bad(WHAT, "wrong arity");
        };
        let (tag, parts) = tagged(*target, "overwrite target")?;
        let [raw] = parts.as_slice() else {
            return bad("overwrite target", "wrong arity");
        };
        let raw = id(*raw, "overwrite target")?;
        let target = if tag.is(Name::RoleTarget) || tag.is(Name::Role) {
            OverwriteTarget::Role(RoleId::new(raw))
        } else if tag.is(Name::MemberTarget) || tag.is(Name::Member) {
            OverwriteTarget::Member(UserId::new(raw))
        } else {
            return bad("overwrite target", "wrong tag");
        };
        Ok(OverwriteSpec {
            target,
            allow: permissions(*allow, "overwrite allow")?,
            deny: permissions(*deny, "overwrite deny")?,
        })
    }

    fn member<S: Source>(&mut self, term: S) -> Decoded<(UserId, RoleList)> {
        const WHAT: &str = "member";
        self.spend(WHAT)?;
        let fields = record(term, WHAT)?;
        let [user_id, roles, ..] = fields.as_slice() else {
            return bad(WHAT, "wrong arity");
        };
        Ok((user(*user_id, WHAT)?, self.role_list(*roles)?))
    }

    fn role_list<S: Source>(&mut self, term: S) -> Decoded<RoleList> {
        const WHAT: &str = "role list";
        if term.is(Name::Malformed) || term.is(Name::MalformedRoles) || term.is(Name::None) {
            return Ok(RoleList::Malformed);
        }
        if term.shape() == Shape::Tuple {
            let (tag, items) = tagged(term, WHAT)?;
            let [ids] = items.as_slice() else {
                return bad(WHAT, "wrong arity");
            };
            if !(tag.is(Name::Roles) || tag.is(Name::RoleIds) || tag.is(Name::Some)) {
                return bad(WHAT, "wrong tag");
            }
            return Ok(RoleList::Roles(self.roles(*ids, MAX_TERMS, WHAT)?));
        }
        Ok(RoleList::Roles(self.roles(term, MAX_TERMS, WHAT)?))
    }

    fn virtual_row<S: Source>(&mut self, term: S) -> Decoded<(UserId, Vec<ChannelId>)> {
        const WHAT: &str = "virtual access";
        self.spend(WHAT)?;
        let fields = record(term, WHAT)?;
        let [user_id, view] = fields.as_slice() else {
            return bad(WHAT, "wrong arity");
        };
        Ok((
            user(*user_id, WHAT)?,
            self.channels(*view, MAX_TERMS, WHAT)?,
        ))
    }

    pub fn query<S: Source>(&mut self, term: S) -> Decoded<Query> {
        const WHAT: &str = "query";
        let (tag, items) = tagged(term, WHAT)?;
        match items.as_slice() {
            [adult] if tag.is(Name::VisitorChannels) => {
                Ok(Query::VisitorChannels(boolean(*adult, WHAT)?))
            }
            [us] if tag.is(Name::PublicOnline) => {
                Ok(Query::PublicOnline(self.users(*us, MAX_IDS, WHAT)?))
            }
            [r, us] if tag.is(Name::MutualOnline) => Ok(Query::MutualOnline(
                user(*r, WHAT)?,
                self.users(*us, MAX_IDS, WHAT)?,
            )),
            [c, ms, mentioned, roles, everyone] if tag.is(Name::PushPrune) => {
                Ok(Query::PushPrune(PushPruneQuery {
                    channel: channel(*c, WHAT)?,
                    message_ms: int(*ms, WHAT)?,
                    mentioned: self.users(*mentioned, MAX_IDS, WHAT)?,
                    roles: self.roles(*roles, MAX_IDS, WHAT)?,
                    everyone: boolean(*everyone, WHAT)?,
                }))
            }
            [u] if tag.is(Name::GroupOf) => Ok(Query::GroupOf(user(*u, WHAT)?)),
            _ => bad(WHAT, "wrong tag or arity"),
        }
    }

    pub fn reply_query<S: Source>(&mut self, term: S) -> Decoded<ReplyQuery> {
        const WHAT: &str = "reply query";
        let (tag, items) = tagged(term, WHAT)?;
        match items.as_slice() {
            [u, scope] if tag.is(Name::UserPermissions) => Ok(ReplyQuery::UserPermissions(
                user(*u, WHAT)?,
                self.scope(*scope)?,
            )),
            [u, scope, bits] if tag.is(Name::CheckPermission) => Ok(ReplyQuery::CheckPermission(
                user(*u, WHAT)?,
                self.scope(*scope)?,
                permissions(*bits, WHAT)?,
            )),
            [u] if tag.is(Name::ViewableChannels) => {
                Ok(ReplyQuery::ViewableChannels(user(*u, WHAT)?))
            }
            _ => bad(WHAT, "wrong tag or arity"),
        }
    }
}

struct Keyed<K, V> {
    slots: Vec<Option<(K, V)>>,
    at: HashMap<K, usize>,
}

impl<K: Copy + Eq + Hash, V> Keyed<K, V> {
    fn new() -> Keyed<K, V> {
        Keyed {
            slots: Vec::new(),
            at: HashMap::new(),
        }
    }

    fn push(&mut self, key: K, value: V) {
        self.at.insert(key, self.slots.len());
        self.slots.push(Some((key, value)));
    }

    fn upsert(&mut self, key: K, value: V) {
        match self.at.get(&key).and_then(|at| self.slots.get_mut(*at)) {
            Some(slot) => *slot = Some((key, value)),
            None => self.push(key, value),
        }
    }

    fn remove(&mut self, key: K) {
        if let Some(slot) = self.at.remove(&key).and_then(|at| self.slots.get_mut(at)) {
            *slot = None;
        }
    }

    fn reorder(&mut self, keys: &[K]) {
        let mut slots = std::mem::take(&mut self.slots);
        let at = std::mem::take(&mut self.at);
        for key in keys {
            if let Some((key, value)) = at
                .get(key)
                .and_then(|at| slots.get_mut(*at))
                .and_then(Option::take)
            {
                self.push(key, value);
            }
        }
        for (key, value) in slots.into_iter().flatten() {
            self.push(key, value);
        }
    }

    fn into_entries(self) -> Vec<(K, V)> {
        self.slots.into_iter().flatten().collect()
    }

    fn into_values(self) -> Vec<V> {
        self.slots
            .into_iter()
            .flatten()
            .map(|(_, value)| value)
            .collect()
    }
}

pub fn snapshot_items<S: Source>(term: S, max: usize) -> usize {
    let Ok(fields) = record(term, "snapshot") else {
        return 0;
    };
    let lists = match fields.as_slice() {
        [_, events] => vec![*events],
        [_, _, roles, channels, members, virtual_access, ..] => {
            vec![*roles, *channels, *members, *virtual_access]
        }
        _ => return 0,
    };
    lists.into_iter().fold(0, |seen, list| {
        if seen > max {
            seen
        } else {
            seen.saturating_add(length(list, max.saturating_sub(seen)))
        }
    })
}

fn length<S: Source>(term: S, max: usize) -> usize {
    let mut seen = 0;
    let mut rest = term;
    while seen <= max {
        let Some((_, tail)) = rest.cell() else {
            break;
        };
        seen = seen.saturating_add(1);
        rest = tail;
    }
    seen
}

fn packed<S: Source>(term: S, max: usize, what: &str) -> Decoded<Vec<u64>> {
    let Some(words) = term.words() else {
        return bad(what, "expected a binary of 64 bit ids");
    };
    if words.len() > max {
        return bad(what, "too many items");
    }
    if words.contains(&0) {
        return bad(what, "id is not positive");
    }
    Ok(words)
}

fn rank<S: Source>(term: S) -> Decoded<RankPos> {
    const WHAT: &str = "role position";
    if term.is(Name::UndefinedPosition) {
        return Ok(RankPos::Undefined);
    }
    if term.shape() == Shape::Integer {
        return Ok(RankPos::Int(int(term, WHAT)?));
    }
    let (tag, items) = tagged(term, WHAT)?;
    match items.as_slice() {
        [value] if tag.is(Name::Position) => Ok(RankPos::Int(int(*value, WHAT)?)),
        _ => bad(WHAT, "wrong tag"),
    }
}

fn hoist_of<S: Source>(term: S) -> Decoded<Hoist> {
    const WHAT: &str = "role hoist";
    if term.is(Name::NotHoisted) {
        return Ok(Hoist::NotHoisted);
    }
    let (tag, items) = tagged(term, WHAT)?;
    match items.as_slice() {
        [value] if tag.is(Name::Hoisted) => Ok(Hoist::Hoisted(
            option(*value, WHAT)?.map(|p| int(p, WHAT)).transpose()?,
        )),
        _ => bad(WHAT, "wrong tag"),
    }
}

fn nsfw_of<S: Source>(term: S) -> Decoded<Nsfw> {
    if term.is(Name::InheritNsfw) {
        Ok(Nsfw::Inherit)
    } else if term.is(Name::SafeForWork) {
        Ok(Nsfw::Safe)
    } else if term.is(Name::AdultOnly) {
        Ok(Nsfw::Adult)
    } else {
        bad("channel nsfw", "wrong tag")
    }
}

fn guild_nsfw<S: Source>(term: S) -> Decoded<Option<bool>> {
    if term.is(Name::True) || term.is(Name::False) {
        return boolean(term, "guild nsfw").map(Some);
    }
    option(term, "guild nsfw")?
        .map(|value| boolean(value, "guild nsfw"))
        .transpose()
}

fn nsfw_level<S: Source>(term: S) -> Decoded<u8> {
    u8::try_from(uint(term, "nsfw level")?)
        .map_err(|_| BadInput::at("nsfw level", "integer out of range"))
}

fn cutoff_of<S: Source>(term: S) -> Decoded<Cutoff> {
    const WHAT: &str = "history cutoff";
    if term.is(Name::NoCutoff) || term.is(Name::Missing) || term.is(Name::None) {
        return Ok(Cutoff::Missing);
    }
    if term.is(Name::MalformedCutoff) || term.is(Name::Malformed) {
        return Ok(Cutoff::Malformed);
    }
    let (tag, items) = tagged(term, WHAT)?;
    match items.as_slice() {
        [ms] if tag.is(Name::CutoffAt) || tag.is(Name::At) || tag.is(Name::Some) => {
            Ok(Cutoff::AtMillis(int(*ms, WHAT)?))
        }
        _ => bad(WHAT, "wrong tag"),
    }
}

fn kind_of<S: Source>(term: S) -> Decoded<ChannelKind> {
    const WHAT: &str = "channel kind";
    if term.shape() == Shape::Integer {
        let code = uint(term, WHAT)?;
        return u32::try_from(code)
            .map(ChannelKind::from_code)
            .map_err(|_| BadInput::at(WHAT, "integer out of range"));
    }
    let plain = [
        (Name::Text, 0),
        (Name::Dm, 1),
        (Name::Voice, 2),
        (Name::GroupDm, 3),
        (Name::Category, 4),
        (Name::Announcement, 5),
        (Name::AnnouncementThread, 10),
        (Name::PublicThread, 11),
        (Name::PrivateThread, 12),
        (Name::Forum, 15),
        (Name::Media, 16),
    ];
    if let Some((_, code)) = plain.iter().find(|(name, _)| term.is(*name)) {
        return Ok(ChannelKind::from_code(*code));
    }
    let (tag, items) = tagged(term, WHAT)?;
    let [code] = items.as_slice() else {
        return bad(WHAT, "wrong arity");
    };
    if tag.is(Name::ThreadKind) || tag.is(Name::OtherKind) {
        u32::try_from(uint(*code, WHAT)?)
            .map(ChannelKind::from_code)
            .map_err(|_| BadInput::at(WHAT, "integer out of range"))
    } else {
        bad(WHAT, "wrong tag")
    }
}
