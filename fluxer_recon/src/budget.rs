// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;

use crate::guards::AbortReason;
use crate::health::{ModeClamp, ModeClamps, ReconMode, WarmupGate, effective_mode};
use crate::ids::{ConnectionKey, GuildId, Location, Millis, RoomKey, TurnId, UserId};
use crate::ledger::ActionKind;

pub const SLIDING_WINDOW_MS: u64 = 60_000;
pub const BREAKER_HYSTERESIS_MS: u64 = 60_000;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TokenBucket {
    capacity: f64,
    tokens: f64,
    refill_per_ms: f64,
    last_refill: Millis,
}

impl TokenBucket {
    pub fn new(capacity: f64, refill_per_ms: f64, now: Millis) -> Self {
        Self {
            capacity,
            tokens: capacity,
            refill_per_ms,
            last_refill: now,
        }
    }

    pub fn per_second(rate: u32, now: Millis) -> Self {
        Self::new(f64::from(rate), f64::from(rate) / 1_000.0, now)
    }

    fn refill(&mut self, now: Millis) {
        let elapsed = now.saturating_since(self.last_refill);
        if elapsed == 0 {
            return;
        }
        self.last_refill = now;
        self.tokens = (self.tokens + (elapsed as f64) * self.refill_per_ms).min(self.capacity);
    }

    pub fn tokens(&mut self, now: Millis) -> f64 {
        self.refill(now);
        self.tokens
    }

    pub fn has_capacity(&mut self, now: Millis) -> bool {
        self.tokens(now) >= 1.0
    }

    pub fn try_acquire(&mut self, now: Millis) -> bool {
        self.try_acquire_many(1, now)
    }

    pub fn try_acquire_many(&mut self, count: u32, now: Millis) -> bool {
        self.refill(now);
        let wanted = f64::from(count);
        if self.tokens < wanted {
            return false;
        }
        self.tokens -= wanted;
        true
    }

    pub fn refund(&mut self, count: u32) {
        self.tokens = (self.tokens + f64::from(count)).min(self.capacity);
    }

    pub const fn capacity(&self) -> f64 {
        self.capacity
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SlidingLimiter {
    limit: u32,
    window: SlidingMinute,
}

impl SlidingLimiter {
    pub fn new(limit: u32) -> Self {
        Self {
            limit,
            window: SlidingMinute::default(),
        }
    }

    pub fn count(&self, now: Millis) -> u32 {
        self.window.count(now)
    }

    pub const fn limit(&self) -> u32 {
        self.limit
    }

    pub fn remaining(&self, now: Millis) -> u32 {
        self.limit.saturating_sub(self.count(now))
    }

    pub fn has_capacity(&self, now: Millis) -> bool {
        self.count(now) < self.limit
    }

    pub fn try_acquire(&mut self, now: Millis) -> bool {
        if !self.has_capacity(now) {
            return false;
        }
        self.window.record(now);
        true
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BudgetClass {
    GatewayRead,
    GatewayMutate,
    LiveKitRead,
    LiveKitMutate,
}

impl BudgetClass {
    pub const ALL: [Self; 4] = [
        Self::GatewayRead,
        Self::GatewayMutate,
        Self::LiveKitRead,
        Self::LiveKitMutate,
    ];

    pub const fn label(self) -> &'static str {
        match self {
            Self::GatewayRead => "gateway_read",
            Self::GatewayMutate => "gateway_mutate",
            Self::LiveKitRead => "livekit_read",
            Self::LiveKitMutate => "livekit_mutate",
        }
    }

    pub const fn for_action(action: ActionKind) -> Self {
        match action {
            ActionKind::RemoveGatewayState => Self::GatewayMutate,
            ActionKind::RemoveParticipant => Self::LiveKitMutate,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InflightSemaphore {
    limit: usize,
    held: usize,
}

impl InflightSemaphore {
    pub const fn new(limit: usize) -> Self {
        Self { limit, held: 0 }
    }

    pub const fn try_acquire(&mut self) -> bool {
        if self.held >= self.limit {
            return false;
        }
        self.held += 1;
        true
    }

    pub const fn release(&mut self) {
        self.held = self.held.saturating_sub(1);
    }

    pub const fn held(&self) -> usize {
        self.held
    }

    pub const fn limit(&self) -> usize {
        self.limit
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct SlidingMinute {
    events: Vec<Millis>,
}

impl SlidingMinute {
    fn count(&self, now: Millis) -> u32 {
        let live = self
            .events
            .iter()
            .filter(|at| now.saturating_since(**at) < SLIDING_WINDOW_MS)
            .count();
        u32::try_from(live).unwrap_or(u32::MAX)
    }

    fn record(&mut self, now: Millis) {
        self.events
            .retain(|at| now.saturating_since(*at) < SLIDING_WINDOW_MS);
        self.events.push(now);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MutationLane {
    Destructive,
    Constructive,
}

impl MutationLane {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Destructive => "destructive",
            Self::Constructive => "constructive",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct IdentityTally {
    destructive: u32,
    constructive: u32,
    last_at: Millis,
}

impl Default for IdentityTally {
    fn default() -> Self {
        Self {
            destructive: 0,
            constructive: 0,
            last_at: Millis::ZERO,
        }
    }
}

pub const IDENTITY_TALLY_RETENTION_MS: u64 = 86_400_000;
pub const MAX_TRACKED_IDENTITIES: usize = 65_536;
const IDENTITY_TALLY_BYTES: u64 = 96;
const SLIDING_WINDOW_ENTRY_BYTES: u64 = 64;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct LaneWindows {
    destructive: SlidingMinute,
    constructive: SlidingMinute,
}

impl LaneWindows {
    const fn window(&self, lane: MutationLane) -> &SlidingMinute {
        match lane {
            MutationLane::Destructive => &self.destructive,
            MutationLane::Constructive => &self.constructive,
        }
    }

    const fn window_mut(&mut self, lane: MutationLane) -> &mut SlidingMinute {
        match lane {
            MutationLane::Destructive => &mut self.destructive,
            MutationLane::Constructive => &mut self.constructive,
        }
    }

    fn count(&self, lane: MutationLane, now: Millis) -> u32 {
        self.window(lane).count(now)
    }

    fn record(&mut self, lane: MutationLane, now: Millis) {
        self.window_mut(lane).record(now);
    }

    fn is_idle(&self, now: Millis) -> bool {
        self.destructive.count(now) == 0 && self.constructive.count(now) == 0
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct MutationLedger {
    per_room: BTreeMap<RoomKey, LaneWindows>,
    per_guild: BTreeMap<GuildId, LaneWindows>,
    global: LaneWindows,
    per_identity: BTreeMap<ConnectionKey, IdentityTally>,
    issued_total: u64,
    pruned_identities_total: u64,
}

impl MutationLedger {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn room_count(&self, room: RoomKey, now: Millis) -> u32 {
        self.room_count_in(room, MutationLane::Destructive, now)
    }

    pub fn room_count_in(&self, room: RoomKey, lane: MutationLane, now: Millis) -> u32 {
        self.per_room
            .get(&room)
            .map_or(0, |windows| windows.count(lane, now))
    }

    pub fn guild_count(&self, guild: GuildId, now: Millis) -> u32 {
        self.guild_count_in(guild, MutationLane::Destructive, now)
    }

    pub fn guild_count_in(&self, guild: GuildId, lane: MutationLane, now: Millis) -> u32 {
        self.per_guild
            .get(&guild)
            .map_or(0, |windows| windows.count(lane, now))
    }

    pub fn global_count(&self, now: Millis) -> u32 {
        self.global_count_in(MutationLane::Destructive, now)
    }

    pub fn global_count_in(&self, lane: MutationLane, now: Millis) -> u32 {
        self.global.count(lane, now)
    }

    pub fn identity_count(&self, key: &ConnectionKey) -> u32 {
        self.per_identity
            .get(key)
            .map_or(0, |tally| tally.destructive)
    }

    pub fn constructive_identity_count(&self, key: &ConnectionKey) -> u32 {
        self.per_identity
            .get(key)
            .map_or(0, |tally| tally.constructive)
    }

    pub fn tracked_identities(&self) -> usize {
        self.per_identity.len()
    }

    pub const fn pruned_identities_total(&self) -> u64 {
        self.pruned_identities_total
    }

    pub fn tracked_bytes(&self) -> u64 {
        let identities = (self.per_identity.len() as u64).saturating_mul(IDENTITY_TALLY_BYTES);
        let windows = (self.per_room.len() as u64)
            .saturating_add(self.per_guild.len() as u64)
            .saturating_add(1)
            .saturating_mul(SLIDING_WINDOW_ENTRY_BYTES);
        identities.saturating_add(windows)
    }

    pub fn record(&mut self, key: &ConnectionKey, lane: MutationLane, now: Millis) {
        self.per_room.entry(key.room).or_default().record(lane, now);
        if let Some(guild) = key.room.guild_id() {
            self.per_guild.entry(guild).or_default().record(lane, now);
        }
        self.global.record(lane, now);
        let tally = self.per_identity.entry(key.clone()).or_default();
        match lane {
            MutationLane::Destructive => {
                tally.destructive = tally.destructive.saturating_add(1);
            }
            MutationLane::Constructive => {
                tally.constructive = tally.constructive.saturating_add(1);
            }
        }
        tally.last_at = now;
        self.issued_total = self.issued_total.saturating_add(1);
    }

    pub const fn issued_total(&self) -> u64 {
        self.issued_total
    }

    pub fn refund_identity(&mut self, key: &ConnectionKey) -> bool {
        match self.per_identity.get_mut(key) {
            None => false,
            Some(tally) => {
                tally.destructive = tally.destructive.saturating_sub(1);
                if tally.destructive == 0 && tally.constructive == 0 {
                    self.per_identity.remove(key);
                }
                true
            }
        }
    }

    pub fn forget_room(&mut self, room: &RoomKey) {
        self.per_room.remove(room);
    }

    pub fn prune(&mut self, now: Millis) -> usize {
        self.per_room.retain(|_, windows| !windows.is_idle(now));
        self.per_guild.retain(|_, windows| !windows.is_idle(now));

        let before = self.per_identity.len();
        self.per_identity
            .retain(|_, tally| now.saturating_since(tally.last_at) < IDENTITY_TALLY_RETENTION_MS);

        while self.per_identity.len() > MAX_TRACKED_IDENTITIES {
            let Some(oldest) = self
                .per_identity
                .iter()
                .min_by_key(|(key, tally)| (tally.last_at, (*key).clone()))
                .map(|(key, _)| key.clone())
            else {
                break;
            };
            self.per_identity.remove(&oldest);
        }

        let pruned = before.saturating_sub(self.per_identity.len());
        self.pruned_identities_total = self.pruned_identities_total.saturating_add(pruned as u64);
        pruned
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BreakerLimits {
    pub max_divergent_fraction: f64,
    pub auto_reset_ms: u64,
    pub max_auto_resets: u32,
}

impl Default for BreakerLimits {
    fn default() -> Self {
        Self {
            max_divergent_fraction: 0.10,
            auto_reset_ms: 900_000,
            max_auto_resets: 2,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BreakerEvent {
    Steady,
    Tripped,
    AutoReset,
    HeldForManualReset,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct CircuitBreaker {
    tripped_since: Option<Millis>,
    below_half_since: Option<Millis>,
    auto_resets_used: u32,
    trips_total: u64,
}

impl CircuitBreaker {
    pub const fn new() -> Self {
        Self {
            tripped_since: None,
            below_half_since: None,
            auto_resets_used: 0,
            trips_total: 0,
        }
    }

    pub const fn is_tripped(&self) -> bool {
        self.tripped_since.is_some()
    }

    pub const fn trips_total(&self) -> u64 {
        self.trips_total
    }

    pub const fn auto_resets_used(&self) -> u32 {
        self.auto_resets_used
    }

    pub const fn manual_reset(&mut self) {
        self.tripped_since = None;
        self.below_half_since = None;
        self.auto_resets_used = 0;
    }

    pub const fn held_for_manual_reset(&self, limits: BreakerLimits) -> bool {
        self.tripped_since.is_some() && self.auto_resets_used >= limits.max_auto_resets
    }

    pub fn observe(
        &mut self,
        divergent_fraction: f64,
        now: Millis,
        limits: BreakerLimits,
    ) -> BreakerEvent {
        let over = divergent_fraction > limits.max_divergent_fraction;
        let calm = divergent_fraction < limits.max_divergent_fraction / 2.0;

        if calm {
            if self.below_half_since.is_none() {
                self.below_half_since = Some(now);
            }
        } else {
            self.below_half_since = None;
        }

        let Some(tripped_since) = self.tripped_since else {
            if over {
                self.tripped_since = Some(now);
                self.below_half_since = None;
                self.trips_total = self.trips_total.saturating_add(1);
                return BreakerEvent::Tripped;
            }
            return BreakerEvent::Steady;
        };

        if now.saturating_since(tripped_since) < limits.auto_reset_ms {
            return BreakerEvent::Steady;
        }

        let hysteresis_met = self
            .below_half_since
            .is_some_and(|since| now.saturating_since(since) >= BREAKER_HYSTERESIS_MS);

        if !hysteresis_met {
            return BreakerEvent::Steady;
        }

        if self.auto_resets_used >= limits.max_auto_resets {
            return BreakerEvent::HeldForManualReset;
        }

        self.auto_resets_used = self.auto_resets_used.saturating_add(1);
        self.tripped_since = None;
        self.below_half_since = None;
        BreakerEvent::AutoReset
    }
}

pub struct EnforceToken(());

impl std::fmt::Debug for EnforceToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("EnforceToken")
    }
}

impl PartialEq for EnforceToken {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for EnforceToken {}

#[derive(Debug, PartialEq, Eq)]
pub struct MutationAuthorization {
    pub connection: ConnectionKey,
    pub user_id: UserId,
    pub kind: ActionKind,
    pub location: Option<Location>,
    pub authorizing_turn: TurnId,
    pub preflight_turn: TurnId,
    pub preflight_at: Millis,
    token: EnforceToken,
}

impl MutationAuthorization {
    pub const fn token(&self) -> &EnforceToken {
        &self.token
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AuthorizationRequest {
    pub connection: ConnectionKey,
    pub user_id: UserId,
    pub kind: ActionKind,
    pub location: Option<Location>,
    pub authorizing_turn: TurnId,
    pub preflight_turn: TurnId,
    pub preflight_at: Millis,
    pub now: Millis,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AuthorizationOutcome {
    Granted(MutationAuthorization),
    Denied(AbortReason),
}

impl AuthorizationOutcome {
    pub fn granted(self) -> Option<MutationAuthorization> {
        match self {
            Self::Granted(authorization) => Some(authorization),
            Self::Denied(_) => None,
        }
    }

    pub const fn denial(&self) -> Option<AbortReason> {
        match self {
            Self::Granted(_) => None,
            Self::Denied(reason) => Some(*reason),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GovernorLimits {
    pub gateway_read_rps: u32,
    pub gateway_mutate_rpm: u32,
    pub livekit_read_rps: u32,
    pub livekit_read_rps_per_server: u32,
    pub livekit_mutate_rpm: u32,
    pub max_inflight_room_turns: usize,
    pub max_inflight_gateway: usize,
    pub max_inflight_livekit: usize,
    pub max_inflight_per_server: usize,
    pub max_mutations_per_room_per_min: u32,
    pub max_mutations_per_guild_per_min: u32,
    pub max_mutations_per_min: u32,
    pub max_action_attempts: u32,
    pub preflight_max_age_ms: u64,
    pub breaker: BreakerLimits,
}

impl Default for GovernorLimits {
    fn default() -> Self {
        Self {
            gateway_read_rps: 40,
            gateway_mutate_rpm: 30,
            livekit_read_rps: 40,
            livekit_read_rps_per_server: 40,
            livekit_mutate_rpm: 30,
            max_inflight_room_turns: 4,
            max_inflight_gateway: 8,
            max_inflight_livekit: 8,
            max_inflight_per_server: 2,
            max_mutations_per_room_per_min: 4,
            max_mutations_per_guild_per_min: 8,
            max_mutations_per_min: 30,
            max_action_attempts: 3,
            preflight_max_age_ms: 2_000,
            breaker: BreakerLimits::default(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BudgetView {
    pub warmup_complete: bool,
    pub coverage_pass_complete: bool,
    pub breaker_tripped: bool,
    pub destructive_capacity: bool,
    pub constructive_capacity: bool,
}

impl BudgetView {
    pub const fn unconstrained() -> Self {
        Self {
            warmup_complete: true,
            coverage_pass_complete: true,
            breaker_tripped: false,
            destructive_capacity: true,
            constructive_capacity: true,
        }
    }

    pub const fn destructive_block(&self) -> Option<AbortReason> {
        if !self.warmup_complete || !self.coverage_pass_complete {
            return Some(AbortReason::Warmup);
        }
        if self.breaker_tripped {
            return Some(AbortReason::Breaker);
        }
        if !self.destructive_capacity {
            return Some(AbortReason::Budget);
        }
        None
    }

    pub const fn constructive_block(&self) -> Option<AbortReason> {
        if self.constructive_capacity {
            None
        } else {
            Some(AbortReason::Budget)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Governor {
    configured_mode: ReconMode,
    runtime_override: Option<ReconMode>,
    clamps: ModeClamps,
    warmup: WarmupGate,
    limits: GovernorLimits,
    buckets: BTreeMap<BudgetClass, TokenBucket>,
    mutate_windows: BTreeMap<BudgetClass, SlidingLimiter>,
    room_turns: InflightSemaphore,
    gateway_inflight: InflightSemaphore,
    livekit_inflight: InflightSemaphore,
    per_server_inflight: BTreeMap<Location, InflightSemaphore>,
    per_server_read: BTreeMap<Location, TokenBucket>,
    mutations: MutationLedger,
    breaker: CircuitBreaker,
    denials: BTreeMap<AbortReason, u64>,
}

impl Governor {
    pub fn new(
        configured_mode: ReconMode,
        limits: GovernorLimits,
        warmup: WarmupGate,
        now: Millis,
    ) -> Self {
        let mut buckets = BTreeMap::new();
        buckets.insert(
            BudgetClass::GatewayRead,
            TokenBucket::per_second(limits.gateway_read_rps, now),
        );
        buckets.insert(
            BudgetClass::LiveKitRead,
            TokenBucket::per_second(limits.livekit_read_rps, now),
        );

        let mut mutate_windows = BTreeMap::new();
        mutate_windows.insert(
            BudgetClass::GatewayMutate,
            SlidingLimiter::new(limits.gateway_mutate_rpm),
        );
        mutate_windows.insert(
            BudgetClass::LiveKitMutate,
            SlidingLimiter::new(limits.livekit_mutate_rpm),
        );

        Self {
            configured_mode,
            runtime_override: None,
            clamps: ModeClamps::new(),
            warmup,
            limits,
            buckets,
            mutate_windows,
            room_turns: InflightSemaphore::new(limits.max_inflight_room_turns),
            gateway_inflight: InflightSemaphore::new(limits.max_inflight_gateway),
            livekit_inflight: InflightSemaphore::new(limits.max_inflight_livekit),
            per_server_inflight: BTreeMap::new(),
            per_server_read: BTreeMap::new(),
            mutations: MutationLedger::new(),
            breaker: CircuitBreaker::new(),
            denials: BTreeMap::new(),
        }
    }

    pub const fn configured_mode(&self) -> ReconMode {
        self.configured_mode
    }

    pub const fn runtime_override(&self) -> Option<ReconMode> {
        self.runtime_override
    }

    pub const fn set_runtime_override(&mut self, mode: Option<ReconMode>) {
        self.runtime_override = mode;
    }

    pub const fn clamps(&self) -> &ModeClamps {
        &self.clamps
    }

    pub const fn clamps_mut(&mut self) -> &mut ModeClamps {
        &mut self.clamps
    }

    pub fn effective_mode(&self) -> ReconMode {
        effective_mode(self.configured_mode, self.runtime_override, &self.clamps)
    }

    pub const fn warmup(&self) -> &WarmupGate {
        &self.warmup
    }

    pub const fn warmup_mut(&mut self) -> &mut WarmupGate {
        &mut self.warmup
    }

    pub const fn breaker(&self) -> &CircuitBreaker {
        &self.breaker
    }

    pub fn reset_breaker(&mut self) -> bool {
        let was_tripped = self.breaker.is_tripped();
        self.breaker.manual_reset();
        self.clamps
            .set(ModeClamp::BreakerTripped, self.breaker.is_tripped());
        was_tripped
    }

    pub const fn mutations(&self) -> &MutationLedger {
        &self.mutations
    }

    pub const fn mutations_mut(&mut self) -> &mut MutationLedger {
        &mut self.mutations
    }

    pub fn refund_identity(&mut self, key: &ConnectionKey) -> bool {
        self.mutations.refund_identity(key)
    }

    pub const fn limits(&self) -> GovernorLimits {
        self.limits
    }

    pub fn denials(&self) -> impl Iterator<Item = (&AbortReason, &u64)> {
        self.denials.iter()
    }

    pub fn denial_count(&self, reason: AbortReason) -> u64 {
        self.denials.get(&reason).copied().unwrap_or(0)
    }

    pub const fn pending_actions(&self) -> usize {
        0
    }

    pub fn observe_divergence(&mut self, divergent_fraction: f64, now: Millis) -> BreakerEvent {
        let event = self
            .breaker
            .observe(divergent_fraction, now, self.limits.breaker);
        self.clamps
            .set(ModeClamp::BreakerTripped, self.breaker.is_tripped());
        event
    }

    pub fn try_acquire(&mut self, class: BudgetClass, now: Millis) -> bool {
        if let Some(window) = self.mutate_windows.get_mut(&class) {
            return window.try_acquire(now);
        }
        self.buckets
            .get_mut(&class)
            .is_some_and(|bucket| bucket.try_acquire(now))
    }

    pub fn try_acquire_many(&mut self, class: BudgetClass, count: u32, now: Millis) -> bool {
        if self.mutate_windows.contains_key(&class) {
            return (0..count).all(|_| self.try_acquire(class, now));
        }
        self.buckets
            .get_mut(&class)
            .is_some_and(|bucket| bucket.try_acquire_many(count, now))
    }

    pub fn refund(&mut self, class: BudgetClass, count: u32) {
        if let Some(bucket) = self.buckets.get_mut(&class) {
            bucket.refund(count);
        }
    }

    pub fn has_capacity(&mut self, class: BudgetClass, now: Millis) -> bool {
        if let Some(window) = self.mutate_windows.get(&class) {
            return window.has_capacity(now);
        }
        self.buckets
            .get_mut(&class)
            .is_some_and(|bucket| bucket.has_capacity(now))
    }

    pub fn tokens(&mut self, class: BudgetClass, now: Millis) -> f64 {
        if let Some(window) = self.mutate_windows.get(&class) {
            return f64::from(window.remaining(now));
        }
        self.buckets
            .get_mut(&class)
            .map_or(0.0, |bucket| bucket.tokens(now))
    }

    pub fn try_acquire_server_read(
        &mut self,
        location: &Location,
        count: u32,
        now: Millis,
    ) -> bool {
        let rate = self.limits.livekit_read_rps_per_server;
        self.per_server_read
            .entry(location.clone())
            .or_insert_with(|| TokenBucket::per_second(rate, now))
            .try_acquire_many(count, now)
    }

    pub fn server_read_tokens(&mut self, location: &Location, now: Millis) -> f64 {
        let rate = self.limits.livekit_read_rps_per_server;
        self.per_server_read
            .entry(location.clone())
            .or_insert_with(|| TokenBucket::per_second(rate, now))
            .tokens(now)
    }

    pub fn refund_server_read(&mut self, location: &Location, count: u32) {
        if let Some(bucket) = self.per_server_read.get_mut(location) {
            bucket.refund(count);
        }
    }

    pub fn retain_servers(&mut self, keep: &[Location]) {
        self.per_server_read
            .retain(|location, bucket| keep.contains(location) || bucket.tokens < bucket.capacity);
        self.per_server_inflight
            .retain(|location, held| keep.contains(location) || held.held() > 0);
    }

    pub fn prune_mutations(&mut self, now: Millis) -> usize {
        self.mutations.prune(now)
    }

    pub fn forget_room(&mut self, room: &RoomKey) {
        self.mutations.forget_room(room);
    }

    pub const fn room_turns(&mut self) -> &mut InflightSemaphore {
        &mut self.room_turns
    }

    pub const fn gateway_inflight(&mut self) -> &mut InflightSemaphore {
        &mut self.gateway_inflight
    }

    pub const fn livekit_inflight(&mut self) -> &mut InflightSemaphore {
        &mut self.livekit_inflight
    }

    pub fn server_inflight(&mut self, location: &Location) -> &mut InflightSemaphore {
        self.per_server_inflight
            .entry(location.clone())
            .or_insert_with(|| InflightSemaphore::new(self.limits.max_inflight_per_server))
    }

    pub fn view(&mut self, now: Millis) -> BudgetView {
        let destructive_capacity = self.has_capacity(BudgetClass::GatewayMutate, now)
            || self.has_capacity(BudgetClass::LiveKitMutate, now);
        let constructive_capacity = self.has_capacity(BudgetClass::GatewayMutate, now);

        BudgetView {
            warmup_complete: self.warmup.elapsed_complete(now),
            coverage_pass_complete: self.warmup.coverage_pass_complete(),
            breaker_tripped: self.breaker.is_tripped(),
            destructive_capacity,
            constructive_capacity,
        }
    }

    fn deny(&mut self, reason: AbortReason) -> AuthorizationOutcome {
        *self.denials.entry(reason).or_insert(0) += 1;
        AuthorizationOutcome::Denied(reason)
    }

    pub fn authorize(&mut self, request: &AuthorizationRequest) -> AuthorizationOutcome {
        let now = request.now;

        if !self.warmup.is_complete(now) {
            return self.deny(AbortReason::Warmup);
        }

        if self.breaker.is_tripped() {
            return self.deny(AbortReason::Breaker);
        }

        if !self.effective_mode().allows_destructive() {
            return self.deny(AbortReason::Mode);
        }

        if request.preflight_turn < request.authorizing_turn {
            return self.deny(AbortReason::PreflightDisagreed);
        }

        if now.saturating_since(request.preflight_at) > self.limits.preflight_max_age_ms {
            return self.deny(AbortReason::PreflightStale);
        }

        if self.mutations.identity_count(&request.connection) >= self.limits.max_action_attempts {
            return self.deny(AbortReason::Budget);
        }

        if self.mutations.room_count(request.connection.room, now)
            >= self.limits.max_mutations_per_room_per_min
        {
            return self.deny(AbortReason::Budget);
        }

        if let Some(guild) = request.connection.room.guild_id()
            && self.mutations.guild_count(guild, now) >= self.limits.max_mutations_per_guild_per_min
        {
            return self.deny(AbortReason::Budget);
        }

        if self.mutations.global_count(now) >= self.limits.max_mutations_per_min {
            return self.deny(AbortReason::Budget);
        }

        if !self.try_acquire(BudgetClass::for_action(request.kind), now) {
            return self.deny(AbortReason::Budget);
        }

        self.mutations
            .record(&request.connection, MutationLane::Destructive, now);

        AuthorizationOutcome::Granted(MutationAuthorization {
            connection: request.connection.clone(),
            user_id: request.user_id,
            kind: request.kind,
            location: request.location.clone(),
            authorizing_turn: request.authorizing_turn,
            preflight_turn: request.preflight_turn,
            preflight_at: request.preflight_at,
            token: EnforceToken(()),
        })
    }

    pub fn authorize_constructive(
        &mut self,
        connection: &ConnectionKey,
        now: Millis,
    ) -> Result<(), AbortReason> {
        if !self.effective_mode().allows_constructive() {
            self.deny(AbortReason::Mode);
            return Err(AbortReason::Mode);
        }

        let lane = MutationLane::Constructive;

        if self.mutations.room_count_in(connection.room, lane, now)
            >= self.limits.max_mutations_per_room_per_min
        {
            self.deny(AbortReason::Budget);
            return Err(AbortReason::Budget);
        }

        if let Some(guild) = connection.room.guild_id()
            && self.mutations.guild_count_in(guild, lane, now)
                >= self.limits.max_mutations_per_guild_per_min
        {
            self.deny(AbortReason::Budget);
            return Err(AbortReason::Budget);
        }

        if self.mutations.global_count_in(lane, now) >= self.limits.max_mutations_per_min {
            self.deny(AbortReason::Budget);
            return Err(AbortReason::Budget);
        }

        if !self.try_acquire(BudgetClass::GatewayMutate, now) {
            self.deny(AbortReason::Budget);
            return Err(AbortReason::Budget);
        }

        self.mutations.record(connection, lane, now);
        Ok(())
    }
}
