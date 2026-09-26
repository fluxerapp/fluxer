// SPDX-License-Identifier: AGPL-3.0-or-later

use std::cmp::Ordering;
use std::mem;
use std::sync::Mutex;

use rustler::types::binary::{Binary, OwnedBinary};
use rustler::{Decoder, Encoder, Env, Error, NifResult, Resource, ResourceArc, Term};

const MAX_RANGE_COUNT: usize = 65_536;
const MAX_SORT_KEY_BYTES: usize = 512;
const MAX_FROM_SORTED_KEYS: usize = 250_000;

mod atoms {
    rustler::atoms! {
        ok,
        none,
        not_found
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct MemberKey {
    section_idx: u64,
    sort_key: Vec<u8>,
    user_id: i64,
}

impl Ord for MemberKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.section_idx
            .cmp(&other.section_idx)
            .then_with(|| self.sort_key.cmp(&other.sort_key))
            .then_with(|| self.user_id.cmp(&other.user_id))
    }
}

impl PartialOrd for MemberKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Decoder<'a> for MemberKey {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let (section_idx, sort_key, user_id): (u64, Binary<'a>, i64) = term.decode()?;
        if sort_key.as_slice().len() > MAX_SORT_KEY_BYTES {
            return Err(Error::BadArg);
        }
        Ok(Self {
            section_idx,
            sort_key: sort_key.as_slice().to_vec(),
            user_id,
        })
    }
}

impl Encoder for MemberKey {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        let mut sort_key = OwnedBinary::new(self.sort_key.len()).expect("binary allocation failed");
        sort_key.as_mut_slice().copy_from_slice(&self.sort_key);
        let sort_key_term = sort_key.release(env).to_term(env);
        let terms = [
            self.section_idx.encode(env),
            sort_key_term,
            self.user_id.encode(env),
        ];
        rustler::types::tuple::make_tuple(env, &terms)
    }
}

struct OSetResource {
    inner: Mutex<MemberOSet>,
}

#[rustler::resource_impl]
impl Resource for OSetResource {}

#[derive(Default)]
struct MemberOSet {
    root: Link,
}

type Link = Option<Box<Node>>;

struct Node {
    key: MemberKey,
    priority: u64,
    left: Link,
    right: Link,
    size: usize,
}

impl Node {
    fn new(key: MemberKey) -> Self {
        let priority = priority_for_key(&key);
        Self {
            key,
            priority,
            left: None,
            right: None,
            size: 1,
        }
    }

    fn refresh_size(&mut self) {
        self.size = 1 + link_size(&self.left) + link_size(&self.right);
    }
}

impl MemberOSet {
    fn replace_from_keys(&mut self, mut keys: Vec<MemberKey>) {
        keys.sort_unstable();
        keys.dedup();
        self.root = build_balanced(&keys);
    }

    fn clear(&mut self) {
        self.root = None;
    }

    fn size(&self) -> usize {
        link_size(&self.root)
    }

    fn memory_bytes(&self) -> usize {
        memory_bytes_link(&self.root)
    }

    fn insert(&mut self, key: MemberKey) -> usize {
        match rank_or_insert_pos(&self.root, &key, 0) {
            RankLookup::Found(rank) => rank,
            RankLookup::InsertAt(rank) => {
                self.root = insert_link(self.root.take(), key);
                rank
            }
        }
    }

    fn delete(&mut self, key: &MemberKey) -> Option<usize> {
        match rank_or_insert_pos(&self.root, key, 0) {
            RankLookup::Found(rank) => {
                self.root = delete_link(self.root.take(), key);
                Some(rank)
            }
            RankLookup::InsertAt(_) => None,
        }
    }

    fn rank(&self, key: &MemberKey) -> Option<usize> {
        match rank_or_insert_pos(&self.root, key, 0) {
            RankLookup::Found(rank) => Some(rank),
            RankLookup::InsertAt(_) => None,
        }
    }

    fn at(&self, index: usize) -> Option<MemberKey> {
        select_key(&self.root, index).cloned()
    }

    fn range(&self, start: usize, count: usize) -> Vec<MemberKey> {
        if count == 0 || start >= self.size() {
            return Vec::new();
        }
        let end = start.saturating_add(count).min(self.size());
        let mut out = Vec::with_capacity(end - start);
        collect_range(&self.root, start, end, 0, &mut out);
        out
    }
}

enum RankLookup {
    Found(usize),
    InsertAt(usize),
}

#[rustler::nif]
fn new() -> ResourceArc<OSetResource> {
    ResourceArc::new(OSetResource {
        inner: Mutex::new(MemberOSet::default()),
    })
}

#[rustler::nif(schedule = "DirtyCpu")]
fn from_sorted<'a>(
    env: Env<'a>,
    resource: ResourceArc<OSetResource>,
    keys: Term<'a>,
) -> NifResult<Term<'a>> {
    let keys = decode_key_list(keys)?;
    if keys.len() > MAX_FROM_SORTED_KEYS {
        return Err(Error::BadArg);
    }
    let mut set = resource.inner.lock().map_err(|_| Error::BadArg)?;
    set.replace_from_keys(keys);
    Ok(atoms::ok().encode(env))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn destroy<'a>(env: Env<'a>, resource: ResourceArc<OSetResource>) -> NifResult<Term<'a>> {
    let mut set = resource.inner.lock().map_err(|_| Error::BadArg)?;
    set.clear();
    Ok(atoms::ok().encode(env))
}

#[rustler::nif]
fn size(resource: ResourceArc<OSetResource>) -> NifResult<usize> {
    let set = resource.inner.lock().map_err(|_| Error::BadArg)?;
    Ok(set.size())
}

#[rustler::nif(schedule = "DirtyCpu")]
fn memory_bytes(resource: ResourceArc<OSetResource>) -> NifResult<usize> {
    let set = resource.inner.lock().map_err(|_| Error::BadArg)?;
    Ok(set.memory_bytes())
}

#[rustler::nif]
fn insert(resource: ResourceArc<OSetResource>, key: MemberKey) -> NifResult<usize> {
    let mut set = resource.inner.lock().map_err(|_| Error::BadArg)?;
    Ok(set.insert(key))
}

#[rustler::nif]
fn delete<'a>(
    env: Env<'a>,
    resource: ResourceArc<OSetResource>,
    key: MemberKey,
) -> NifResult<Term<'a>> {
    let mut set = resource.inner.lock().map_err(|_| Error::BadArg)?;
    match set.delete(&key) {
        Some(rank) => Ok(rank.encode(env)),
        None => Ok(atoms::not_found().encode(env)),
    }
}

#[rustler::nif]
fn rank<'a>(
    env: Env<'a>,
    resource: ResourceArc<OSetResource>,
    key: MemberKey,
) -> NifResult<Term<'a>> {
    let set = resource.inner.lock().map_err(|_| Error::BadArg)?;
    match set.rank(&key) {
        Some(rank) => Ok(rank.encode(env)),
        None => Ok(atoms::not_found().encode(env)),
    }
}

#[rustler::nif]
fn at<'a>(env: Env<'a>, resource: ResourceArc<OSetResource>, index: i64) -> NifResult<Term<'a>> {
    if index < 0 {
        return Ok(atoms::none().encode(env));
    }
    let set = resource.inner.lock().map_err(|_| Error::BadArg)?;
    match set.at(index as usize) {
        Some(key) => Ok(key.encode(env)),
        None => Ok(atoms::none().encode(env)),
    }
}

#[rustler::nif(schedule = "DirtyCpu")]
fn range(
    resource: ResourceArc<OSetResource>,
    start: usize,
    count: usize,
) -> NifResult<Vec<MemberKey>> {
    if count > MAX_RANGE_COUNT {
        return Err(Error::BadArg);
    }
    let set = resource.inner.lock().map_err(|_| Error::BadArg)?;
    Ok(set.range(start, count))
}

#[rustler::nif(schedule = "DirtyCpu")]
fn to_list(resource: ResourceArc<OSetResource>) -> NifResult<Vec<MemberKey>> {
    let set = resource.inner.lock().map_err(|_| Error::BadArg)?;
    Ok(set.range(0, set.size()))
}

fn decode_key_list<'a>(term: Term<'a>) -> NifResult<Vec<MemberKey>> {
    term.decode::<Vec<MemberKey>>()
}

fn build_balanced(keys: &[MemberKey]) -> Link {
    if keys.is_empty() {
        return None;
    }
    let mid = keys.len() / 2;
    let mut node = Box::new(Node::new(keys[mid].clone()));
    node.left = build_balanced(&keys[..mid]);
    node.right = build_balanced(&keys[mid + 1..]);
    node.refresh_size();
    Some(node)
}

fn link_size(link: &Link) -> usize {
    link.as_ref().map_or(0, |node| node.size)
}

fn insert_link(link: Link, key: MemberKey) -> Link {
    match link {
        None => Some(Box::new(Node::new(key))),
        Some(mut node) => {
            if key < node.key {
                node.left = insert_link(node.left.take(), key);
                if child_priority(&node.left) < node.priority {
                    return Some(rotate_right(node));
                }
            } else {
                node.right = insert_link(node.right.take(), key);
                if child_priority(&node.right) < node.priority {
                    return Some(rotate_left(node));
                }
            }
            node.refresh_size();
            Some(node)
        }
    }
}

fn delete_link(link: Link, key: &MemberKey) -> Link {
    match link {
        None => None,
        Some(mut node) => match key.cmp(&node.key) {
            Ordering::Less => {
                node.left = delete_link(node.left.take(), key);
                node.refresh_size();
                Some(node)
            }
            Ordering::Greater => {
                node.right = delete_link(node.right.take(), key);
                node.refresh_size();
                Some(node)
            }
            Ordering::Equal => join_links(node.left.take(), node.right.take()),
        },
    }
}

fn join_links(left: Link, right: Link) -> Link {
    match (left, right) {
        (None, right) => right,
        (left, None) => left,
        (Some(mut left), Some(mut right)) => {
            if left.priority <= right.priority {
                left.right = join_links(left.right.take(), Some(right));
                left.refresh_size();
                Some(left)
            } else {
                right.left = join_links(Some(left), right.left.take());
                right.refresh_size();
                Some(right)
            }
        }
    }
}

fn rotate_right(mut node: Box<Node>) -> Box<Node> {
    let mut left = node
        .left
        .take()
        .expect("left child missing for rotate_right");
    node.left = left.right.take();
    node.refresh_size();
    left.right = Some(node);
    left.refresh_size();
    left
}

fn rotate_left(mut node: Box<Node>) -> Box<Node> {
    let mut right = node
        .right
        .take()
        .expect("right child missing for rotate_left");
    node.right = right.left.take();
    node.refresh_size();
    right.left = Some(node);
    right.refresh_size();
    right
}

fn child_priority(link: &Link) -> u64 {
    link.as_ref().map_or(u64::MAX, |node| node.priority)
}

fn rank_or_insert_pos(link: &Link, key: &MemberKey, base: usize) -> RankLookup {
    match link {
        None => RankLookup::InsertAt(base),
        Some(node) => match key.cmp(&node.key) {
            Ordering::Less => rank_or_insert_pos(&node.left, key, base),
            Ordering::Greater => {
                rank_or_insert_pos(&node.right, key, base + link_size(&node.left) + 1)
            }
            Ordering::Equal => RankLookup::Found(base + link_size(&node.left)),
        },
    }
}

fn select_key(link: &Link, index: usize) -> Option<&MemberKey> {
    let node = link.as_ref()?;
    let left_size = link_size(&node.left);
    match index.cmp(&left_size) {
        Ordering::Less => select_key(&node.left, index),
        Ordering::Equal => Some(&node.key),
        Ordering::Greater => select_key(&node.right, index - left_size - 1),
    }
}

fn collect_range(link: &Link, start: usize, end: usize, base: usize, out: &mut Vec<MemberKey>) {
    let Some(node) = link.as_ref() else {
        return;
    };
    let left_size = link_size(&node.left);
    let rank = base + left_size;
    if start < rank {
        collect_range(&node.left, start, end, base, out);
    }
    if start <= rank && rank < end {
        out.push(node.key.clone());
    }
    if rank + 1 < end {
        collect_range(&node.right, start, end, rank + 1, out);
    }
}

fn memory_bytes_link(link: &Link) -> usize {
    match link {
        None => 0,
        Some(node) => {
            mem::size_of::<Node>()
                + node.key.sort_key.len()
                + memory_bytes_link(&node.left)
                + memory_bytes_link(&node.right)
        }
    }
}

fn priority_for_key(key: &MemberKey) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    hash_u64(&mut hash, key.section_idx);
    hash_bytes(&mut hash, &key.sort_key);
    hash_u64(&mut hash, key.user_id as u64);
    splitmix64(hash)
}

fn hash_u64(hash: &mut u64, value: u64) {
    for byte in value.to_le_bytes() {
        hash_byte(hash, byte);
    }
}

fn hash_bytes(hash: &mut u64, bytes: &[u8]) {
    hash_u64(hash, bytes.len() as u64);
    for byte in bytes {
        hash_byte(hash, *byte);
    }
}

fn hash_byte(hash: &mut u64, byte: u8) {
    *hash ^= byte as u64;
    *hash = hash.wrapping_mul(0x100000001b3);
}

fn splitmix64(mut value: u64) -> u64 {
    value = value.wrapping_add(0x9e3779b97f4a7c15);
    value = (value ^ (value >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    value = (value ^ (value >> 27)).wrapping_mul(0x94d049bb133111eb);
    value ^ (value >> 31)
}

rustler::init!("guild_member_list_oset_nif");
