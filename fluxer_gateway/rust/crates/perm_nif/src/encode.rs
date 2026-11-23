// SPDX-License-Identifier: AGPL-3.0-or-later

use perm_core::{
    Audience, Group, MutualOnline, Permissions, PermsError, PublicOnline, Regroup, Target, UserId,
    VisibilityChange,
};
use rustler::{Atom, Binary, Encoder, Env, NewBinary, Term};

use crate::decode::{Name, Shape, Source};

pub const MESSAGE_LIMIT: usize = 256;

pub mod atoms {
    rustler::atoms! {
        ok,
        error,
        nil,
        some,
        none,
        needs_dirty,
        broken,
        gone,
        bad_input,
        effects,
        regroup,
        visibility_change,
        role_set_group,
        user_group,
        group_target,
        one_user,
        audience,
        packed_ids,
        pruned,
        public_open,
        public_count,
        public_empty,
        mutual_all,
        mutual_count,
        channel_grants,
        group_reply,
    }
}

impl Source for Term<'_> {
    fn shape(self) -> Shape {
        match self.get_type() {
            rustler::TermType::Atom => Shape::Atom,
            rustler::TermType::Integer => Shape::Integer,
            rustler::TermType::Tuple => Shape::Tuple,
            rustler::TermType::List if self.is_empty_list() => Shape::Nil,
            rustler::TermType::List => Shape::List,
            rustler::TermType::Binary => Shape::Binary,
            _ => Shape::Other,
        }
    }

    fn is(self, name: Name) -> bool {
        let Ok(atom) = self.decode::<Atom>() else {
            return false;
        };
        Atom::from_str(self.get_env(), name.text()).is_ok_and(|wanted| wanted == atom)
    }

    fn uint(self) -> Option<u64> {
        self.decode::<u64>().ok()
    }

    fn int(self) -> Option<i64> {
        self.decode::<i64>().ok()
    }

    fn elements(self) -> Option<Vec<Self>> {
        rustler::types::tuple::get_tuple(self).ok()
    }

    fn cell(self) -> Option<(Self, Self)> {
        self.list_get_cell().ok()
    }

    fn words(self) -> Option<Vec<u64>> {
        let binary = self.decode::<Binary>().ok()?;
        let (chunks, rest) = binary.as_slice().as_chunks::<8>();
        if !rest.is_empty() {
            return None;
        }
        Some(
            chunks
                .iter()
                .map(|chunk| u64::from_be_bytes(*chunk))
                .collect(),
        )
    }
}

pub fn message(problem: &str) -> &[u8] {
    let bytes = problem.as_bytes();
    let mut end = bytes.len().min(MESSAGE_LIMIT);
    while end > 0 && !problem.is_char_boundary(end) {
        end = end.saturating_sub(1);
    }
    bytes.get(..end).unwrap_or_default()
}

pub fn binary<'a>(env: Env<'a>, bytes: &[u8]) -> Term<'a> {
    let mut out = NewBinary::new(env, bytes.len());
    out.as_mut_slice().copy_from_slice(bytes);
    Binary::from(out).to_term(env)
}

pub fn ok<'a>(env: Env<'a>, value: Term<'a>) -> Term<'a> {
    (atoms::ok(), value).encode(env)
}

pub fn reason<'a>(env: Env<'a>, error: &PermsError) -> Term<'a> {
    let bad = |text: &str| (atoms::bad_input(), binary(env, message(text))).encode(env);
    match error {
        PermsError::NeedsDirty => atoms::needs_dirty().encode(env),
        PermsError::Broken => atoms::broken().encode(env),
        PermsError::Gone => atoms::gone().encode(env),
        PermsError::NotOwner => bad("not_owner"),
        PermsError::MalformedMember(_) => bad("malformed_member"),
        PermsError::EmptyRequirement => bad("empty_requirement"),
        PermsError::BadInput(problem) if problem == "model generation" => atoms::gone().encode(env),
        PermsError::BadInput(problem) => bad(problem),
    }
}

pub fn failure<'a>(env: Env<'a>, error: &PermsError) -> Term<'a> {
    (atoms::error(), reason(env, error)).encode(env)
}

pub fn result<'a>(env: Env<'a>, value: Result<Term<'a>, PermsError>) -> Term<'a> {
    match value {
        Ok(term) => ok(env, term),
        Err(error) => failure(env, &error),
    }
}

pub fn option<'a>(env: Env<'a>, value: Option<Term<'a>>) -> Term<'a> {
    match value {
        Some(term) => (atoms::some(), term).encode(env),
        None => atoms::none().encode(env),
    }
}

pub fn ids_binary<'a>(env: Env<'a>, ids: impl ExactSizeIterator<Item = u64>) -> Term<'a> {
    let len = ids.len().saturating_mul(8);
    let mut out = NewBinary::new(env, len);
    let slice = out.as_mut_slice();
    for (chunk, id) in slice.as_chunks_mut::<8>().0.iter_mut().zip(ids) {
        *chunk = id.to_be_bytes();
    }
    Binary::from(out).to_term(env)
}

pub fn packed<'a>(env: Env<'a>, ids: impl ExactSizeIterator<Item = u64>) -> Term<'a> {
    (atoms::packed_ids(), ids_binary(env, ids)).encode(env)
}

pub fn packed_users<'a>(env: Env<'a>, users: &[UserId]) -> Term<'a> {
    packed(env, users.iter().map(|u| u.get()))
}

pub fn bits<'a>(env: Env<'a>, words: &[u64]) -> Term<'a> {
    let mut out = NewBinary::new(env, words.len().saturating_mul(8));
    for (chunk, word) in out
        .as_mut_slice()
        .as_chunks_mut::<8>()
        .0
        .iter_mut()
        .zip(words)
    {
        for (byte, slot) in chunk.iter_mut().enumerate() {
            let shift = u32::try_from(byte.saturating_mul(8)).unwrap_or(0);
            let raw = word.checked_shr(shift).unwrap_or(0) & 0xFF;
            *slot = u8::try_from(raw).unwrap_or(0).reverse_bits();
        }
    }
    Binary::from(out).to_term(env)
}

pub fn group<'a>(env: Env<'a>, group: Group) -> Term<'a> {
    match group {
        Group::RoleSet(set) => (atoms::role_set_group(), set.get()).encode(env),
        Group::User(user) => (atoms::user_group(), user.get()).encode(env),
    }
}

pub fn regroup<'a>(env: Env<'a>, change: &Regroup) -> Term<'a> {
    (
        atoms::regroup(),
        change.user.get(),
        option(env, change.old.map(|g| group(env, g))),
        option(env, change.new.map(|g| group(env, g))),
    )
        .encode(env)
}

pub fn visibility_change<'a>(env: Env<'a>, change: &VisibilityChange) -> Term<'a> {
    let target = match change.target {
        Target::Group(g) => (atoms::group_target(), group(env, g)).encode(env),
        Target::OneUser(user) => (atoms::one_user(), user.get()).encode(env),
    };
    let lost: Vec<u64> = change.lost.iter().map(|c| c.get()).collect();
    let gained: Vec<u64> = change.gained.iter().map(|c| c.get()).collect();
    (atoms::visibility_change(), target, lost, gained).encode(env)
}

pub fn audience<'a>(env: Env<'a>, audience: &Audience) -> Term<'a> {
    (
        atoms::audience(),
        audience.epoch().get(),
        bits(env, audience.rolesets()),
        packed_users(env, audience.users()),
    )
        .encode(env)
}

pub fn public_online<'a>(env: Env<'a>, value: PublicOnline) -> Term<'a> {
    match value {
        PublicOnline::Open => atoms::public_open().encode(env),
        PublicOnline::Count(n) => (atoms::public_count(), n).encode(env),
        PublicOnline::Empty => atoms::public_empty().encode(env),
    }
}

pub fn mutual_online<'a>(env: Env<'a>, value: MutualOnline) -> Term<'a> {
    match value {
        MutualOnline::All => atoms::mutual_all().encode(env),
        MutualOnline::Count(n) => (atoms::mutual_count(), n).encode(env),
    }
}

pub fn permissions<'a>(env: Env<'a>, bits: Permissions) -> Term<'a> {
    bits.bits().encode(env)
}
