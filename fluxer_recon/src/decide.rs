// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::budget::BudgetView;
use crate::config::ReconConfig;
use crate::evidence::{PendingJoin, Presence, RoomObservation, Side, UnknownReason};
use crate::gateway::Nonce;
use crate::guards::{AbortReason, DmPosture};
use crate::ids::{ConnectionId, ConnectionKey, Location, Millis, RoomKey, Scope, TurnId, UserId};
use crate::ledger::{
    ActionKind, ConnectionState, Fingerprint, FingerprintInput, Ledger, LocationIndex,
    RepairVerdict, WedgedReason, fingerprint,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DecideParams {
    pub required_corroborations: u32,
    pub min_corroboration_gap_ms: u64,
    pub max_corroboration_window_ms: u64,
    pub min_divergence_ms: u64,
    pub max_action_attempts: u32,
    pub pending_join_skew_ms: u64,
    pub dm_posture: DmPosture,
}

impl Default for DecideParams {
    fn default() -> Self {
        Self {
            required_corroborations: 3,
            min_corroboration_gap_ms: 1_000,
            max_corroboration_window_ms: 60_000,
            min_divergence_ms: 15_000,
            max_action_attempts: 3,
            pending_join_skew_ms: 5_000,
            dm_posture: DmPosture::default(),
        }
    }
}

impl DecideParams {
    pub const fn from_config(config: &ReconConfig) -> Self {
        Self {
            required_corroborations: config.required_corroborations,
            min_corroboration_gap_ms: config.min_corroboration_gap_ms,
            max_corroboration_window_ms: config.max_corroboration_window_ms,
            min_divergence_ms: config.min_divergence_ms,
            max_action_attempts: config.max_action_attempts,
            pending_join_skew_ms: config.pending_join_skew_ms,
            dm_posture: DmPosture {
                dm_gateway_eviction: config.dm_gateway_eviction,
                dm_media_eviction: config.dm_media_eviction,
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ActionClass {
    None,
    Constructive,
    Destructive,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DecisionAction {
    Hold,
    ConfirmConnection { nonce: Nonce },
    RepairState,
    RemoveGatewayState,
    RemoveParticipant { location: Location },
}

impl DecisionAction {
    pub const fn class(&self) -> ActionClass {
        match self {
            Self::Hold => ActionClass::None,
            Self::ConfirmConnection { .. } | Self::RepairState => ActionClass::Constructive,
            Self::RemoveGatewayState | Self::RemoveParticipant { .. } => ActionClass::Destructive,
        }
    }

    pub const fn label(&self) -> &'static str {
        match self {
            Self::Hold => "hold",
            Self::ConfirmConnection { .. } => "confirm_connection",
            Self::RepairState => "repair_state",
            Self::RemoveGatewayState => "remove_gateway_state",
            Self::RemoveParticipant { .. } => "remove_participant",
        }
    }

    pub const fn kind(&self) -> Option<ActionKind> {
        match self {
            Self::Hold | Self::ConfirmConnection { .. } | Self::RepairState => None,
            Self::RemoveGatewayState => Some(ActionKind::RemoveGatewayState),
            Self::RemoveParticipant { .. } => Some(ActionKind::RemoveParticipant),
        }
    }

    pub const fn is_destructive(&self) -> bool {
        matches!(self.class(), ActionClass::Destructive)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DecisionReason {
    SideUnknown(UnknownReason),
    FingerprintChanged,
    BothSidesPresent,
    BothSidesAbsent,
    DivergenceOpened,
    CorroborationRecorded,
    CorroborationSpacedTooTightly,
    CorroborationWindowExpired,
    DivergenceTooYoung,
    EvidenceComplete,
    PendingJoinOutstanding,
    RepairAttempted,
    RepairSucceeded,
    RepairMadeNoChange,
    RepairNotPossible,
    RepairNotDefinitive,
    RepairExhausted,
    ActionUnresolved,
    ActionsExhausted,
    ParticipantJoinedAfterDivergence,
    LocationUnavailable,
    WedgedHolding,
}

impl DecisionReason {
    pub const fn label(&self) -> &'static str {
        match self {
            Self::SideUnknown(_) => "side_unknown",
            Self::FingerprintChanged => "fingerprint_changed",
            Self::BothSidesPresent => "both_sides_present",
            Self::BothSidesAbsent => "both_sides_absent",
            Self::DivergenceOpened => "divergence_opened",
            Self::CorroborationRecorded => "corroboration_recorded",
            Self::CorroborationSpacedTooTightly => "corroboration_spaced_too_tightly",
            Self::CorroborationWindowExpired => "corroboration_window_expired",
            Self::DivergenceTooYoung => "divergence_too_young",
            Self::EvidenceComplete => "evidence_complete",
            Self::PendingJoinOutstanding => "pending_join_outstanding",
            Self::RepairAttempted => "repair_attempted",
            Self::RepairSucceeded => "repair_succeeded",
            Self::RepairMadeNoChange => "repair_made_no_change",
            Self::RepairNotPossible => "repair_not_possible",
            Self::RepairNotDefinitive => "repair_not_definitive",
            Self::RepairExhausted => "repair_exhausted",
            Self::ActionUnresolved => "action_unresolved",
            Self::ActionsExhausted => "actions_exhausted",
            Self::ParticipantJoinedAfterDivergence => "participant_joined_after_divergence",
            Self::LocationUnavailable => "location_unavailable",
            Self::WedgedHolding => "wedged_holding",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Decision {
    pub connection: ConnectionKey,
    pub user_id: UserId,
    pub action: DecisionAction,
    pub reason: DecisionReason,
    pub from: &'static str,
    pub to: &'static str,
    pub blocked_by: Option<AbortReason>,
}

impl Decision {
    pub const fn is_actionable(&self) -> bool {
        self.blocked_by.is_none() && !matches!(self.action, DecisionAction::Hold)
    }

    pub const fn is_destructive(&self) -> bool {
        self.action.is_destructive()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecisionSet {
    pub room: RoomKey,
    pub turn: TurnId,
    pub at: Millis,
    pub decisions: Vec<Decision>,
}

impl DecisionSet {
    pub fn empty(room: RoomKey, turn: TurnId, at: Millis) -> Self {
        Self {
            room,
            turn,
            at,
            decisions: Vec::new(),
        }
    }

    pub fn actionable(&self) -> impl Iterator<Item = &Decision> {
        self.decisions.iter().filter(|entry| entry.is_actionable())
    }

    pub fn destructive(&self) -> impl Iterator<Item = &Decision> {
        self.decisions.iter().filter(|entry| entry.is_destructive())
    }

    pub fn actionable_destructive(&self) -> impl Iterator<Item = &Decision> {
        self.decisions
            .iter()
            .filter(|entry| entry.is_actionable() && entry.is_destructive())
    }

    pub fn blocked(&self) -> impl Iterator<Item = (&Decision, AbortReason)> {
        self.decisions
            .iter()
            .filter_map(|entry| entry.blocked_by.map(|reason| (entry, reason)))
    }

    pub fn for_connection(&self, connection: &ConnectionId) -> Option<&Decision> {
        self.decisions
            .iter()
            .find(|entry| &entry.connection.connection == connection)
    }
}

struct Context<'a> {
    params: &'a DecideParams,
    room: RoomKey,
    now: Millis,
    fingerprint: Fingerprint,
    location: Option<LocationIndex>,
    location_value: Option<&'a Location>,
    joined_at: Option<Millis>,
    pending_join: Option<&'a PendingJoin>,
    budget: &'a BudgetView,
    action_attempts: u32,
    repair_attempts: u32,
    repair_verdict: Option<RepairVerdict>,
}

struct Outcome {
    state: ConnectionState,
    action: DecisionAction,
    reason: DecisionReason,
    blocked_by: Option<AbortReason>,
}

impl Outcome {
    const fn hold(state: ConnectionState, reason: DecisionReason) -> Self {
        Self {
            state,
            action: DecisionAction::Hold,
            reason,
            blocked_by: None,
        }
    }
}

pub fn decide(
    ledger: &mut Ledger,
    observation: &RoomObservation,
    now: Millis,
    budget: &BudgetView,
) -> DecisionSet {
    let params = *ledger.params();
    let room = observation.room;

    if ledger.ensure_room(room, now).is_err() {
        return DecisionSet::empty(room, observation.turn, now);
    }

    let no_candidates = observation.candidates.is_empty();
    let partially_unreadable = matches!(
        observation.authority.media.unknown_reason(),
        Some(UnknownReason::RoomPartiallyUnreadable)
    );
    if let Some(entry) = ledger.room_mut(&room) {
        entry.note_turn(observation.turn, now);
        entry.set_no_candidates(no_candidates);
        entry.set_partially_unreadable(partially_unreadable);
    }

    let mut work: Vec<(ConnectionId, UserId)> = observation
        .connections
        .iter()
        .map(|entry| (entry.connection.clone(), entry.user_id))
        .collect();
    if let Some(entry) = ledger.room(&room) {
        for tracked in entry.connections() {
            if !work.iter().any(|(id, _)| id == &tracked.connection) {
                work.push((tracked.connection.clone(), tracked.user_id));
            }
        }
    }
    work.sort();
    work.dedup();

    let unsighted = observation.unsighted_presence();
    let mut decisions = Vec::with_capacity(work.len());

    for (connection, user_id) in work {
        let observed = observation.observation_for(&connection);
        let presence: Side<Presence> =
            observed.map_or_else(|| unsighted.clone(), |entry| entry.presence.clone());
        let siblings = observed
            .map(|entry| entry.gateway_siblings.clone())
            .unwrap_or_default();
        let locations = observed
            .map(|entry| entry.media_locations.clone())
            .unwrap_or_default();
        let joined_at = observed.and_then(|entry| entry.participant_joined_at);

        let print = fingerprint(&FingerprintInput {
            room,
            connection: &connection,
            user_id,
            gateway_siblings: &siblings,
            media_locations: &locations,
            participant_joined_at: joined_at,
            census_epoch: observation.census_epoch,
            topology_epoch: observation.topology_epoch,
        });

        let location_index = locations
            .first()
            .and_then(|location| ledger.intern_location(location));
        if let (Some(index), Some(entry)) = (location_index, ledger.room_mut(&room)) {
            entry.note_location(index);
        }

        let pending_join = observation.pending_join_for(
            &connection,
            observation.wall_at,
            params.pending_join_skew_ms,
        );

        let Ok(entry) = ledger.upsert_connection(room, &connection, user_id, now) else {
            continue;
        };
        if let Some(index) = location_index {
            entry.last_location = Some(index);
        }

        let context = Context {
            params: &params,
            room,
            now,
            fingerprint: print,
            location: location_index,
            location_value: locations.first(),
            joined_at,
            pending_join,
            budget,
            action_attempts: u32::from(entry.action_attempts),
            repair_attempts: u32::from(entry.repair_attempts),
            repair_verdict: entry.last_repair_verdict,
        };

        let from = entry.state.label();
        let outcome = transition(&entry.state, &presence, &context);

        if matches!(
            outcome.state,
            ConnectionState::Nascent | ConnectionState::Consistent
        ) {
            entry.last_repair_verdict = None;
        }
        entry.state = outcome.state;

        decisions.push(Decision {
            connection: ConnectionKey::new(room, connection),
            user_id,
            action: outcome.action,
            reason: outcome.reason,
            from,
            to: entry.state.label(),
            blocked_by: outcome.blocked_by,
        });
    }

    DecisionSet {
        room,
        turn: observation.turn,
        at: now,
        decisions,
    }
}

fn transition(prior: &ConnectionState, presence: &Side<Presence>, ctx: &Context<'_>) -> Outcome {
    match (&presence.gateway, &presence.media) {
        (Presence::Unknown(reason), Presence::Unknown(_))
        | (Presence::Unknown(reason), Presence::Present)
        | (Presence::Unknown(reason), Presence::Absent)
        | (Presence::Present, Presence::Unknown(reason))
        | (Presence::Absent, Presence::Unknown(reason)) => Outcome::hold(
            ConnectionState::Nascent,
            DecisionReason::SideUnknown(reason.clone()),
        ),
        (Presence::Present, Presence::Present) => Outcome::hold(
            ConnectionState::Consistent,
            DecisionReason::BothSidesPresent,
        ),
        (Presence::Absent, Presence::Absent) => Outcome::hold(
            ConnectionState::Retired {
                at: retired_at(prior, ctx.now),
            },
            DecisionReason::BothSidesAbsent,
        ),
        (Presence::Present, Presence::Absent) => gateway_only_lane(prior, ctx),
        (Presence::Absent, Presence::Present) => media_only_lane(prior, ctx),
    }
}

const fn retired_at(prior: &ConnectionState, now: Millis) -> Millis {
    match prior {
        ConnectionState::Retired { at } => *at,
        ConnectionState::Nascent
        | ConnectionState::Consistent
        | ConnectionState::PendingJoin { .. }
        | ConnectionState::GatewayOnly { .. }
        | ConnectionState::MediaOnly { .. }
        | ConnectionState::Repairing { .. }
        | ConnectionState::ActionTaken { .. }
        | ConnectionState::Wedged { .. } => now,
    }
}

fn fingerprint_broke(prior: &ConnectionState, ctx: &Context<'_>) -> bool {
    prior
        .stored_fingerprint()
        .is_some_and(|stored| stored != ctx.fingerprint)
}

fn nascent_on_fingerprint_change() -> Outcome {
    Outcome::hold(ConnectionState::Nascent, DecisionReason::FingerprintChanged)
}

fn gateway_only_lane(prior: &ConnectionState, ctx: &Context<'_>) -> Outcome {
    if fingerprint_broke(prior, ctx) {
        return nascent_on_fingerprint_change();
    }

    match prior {
        ConnectionState::Nascent
        | ConnectionState::Consistent
        | ConnectionState::PendingJoin { .. }
        | ConnectionState::Repairing { .. }
        | ConnectionState::MediaOnly { .. }
        | ConnectionState::Retired { .. } => open_gateway_divergence(ctx),
        ConnectionState::GatewayOnly {
            since,
            corroborations,
            last_corroboration,
            fingerprint: _,
        } => corroborate_gateway(*since, *corroborations, *last_corroboration, ctx),
        ConnectionState::ActionTaken { kind, since, .. } => match kind {
            ActionKind::RemoveGatewayState => {
                if ctx.action_attempts >= ctx.params.max_action_attempts {
                    return wedged(
                        WedgedReason::ActionIneffective,
                        ctx,
                        DecisionReason::ActionsExhausted,
                    );
                }
                evaluate_gateway(
                    *since,
                    ctx.params.required_corroborations,
                    ctx.now,
                    ctx,
                    DecisionReason::ActionUnresolved,
                )
            }
            ActionKind::RemoveParticipant => open_gateway_divergence(ctx),
        },
        ConnectionState::Wedged {
            reason,
            at,
            fingerprint: print,
        } => Outcome::hold(
            ConnectionState::Wedged {
                reason: *reason,
                at: *at,
                fingerprint: *print,
            },
            DecisionReason::WedgedHolding,
        ),
    }
}

fn open_gateway_divergence(ctx: &Context<'_>) -> Outcome {
    Outcome::hold(
        ConnectionState::GatewayOnly {
            since: ctx.now,
            corroborations: 1,
            fingerprint: ctx.fingerprint,
            last_corroboration: ctx.now,
        },
        DecisionReason::DivergenceOpened,
    )
}

fn corroborate_gateway(
    since: Millis,
    corroborations: u32,
    last_corroboration: Millis,
    ctx: &Context<'_>,
) -> Outcome {
    if ctx.now.saturating_since(since) > ctx.params.max_corroboration_window_ms {
        return Outcome::hold(
            ConnectionState::GatewayOnly {
                since: ctx.now,
                corroborations: 1,
                fingerprint: ctx.fingerprint,
                last_corroboration: ctx.now,
            },
            DecisionReason::CorroborationWindowExpired,
        );
    }

    if ctx.now.saturating_since(last_corroboration) < ctx.params.min_corroboration_gap_ms {
        return evaluate_gateway(
            since,
            corroborations,
            last_corroboration,
            ctx,
            DecisionReason::CorroborationSpacedTooTightly,
        );
    }

    evaluate_gateway(
        since,
        corroborations.saturating_add(1),
        ctx.now,
        ctx,
        DecisionReason::CorroborationRecorded,
    )
}

fn evaluate_gateway(
    since: Millis,
    corroborations: u32,
    last_corroboration: Millis,
    ctx: &Context<'_>,
    progress: DecisionReason,
) -> Outcome {
    let state = ConnectionState::GatewayOnly {
        since,
        corroborations,
        fingerprint: ctx.fingerprint,
        last_corroboration,
    };

    if corroborations < ctx.params.required_corroborations {
        return Outcome::hold(state, progress);
    }

    if ctx.now.saturating_since(since) < ctx.params.min_divergence_ms {
        return Outcome::hold(state, DecisionReason::DivergenceTooYoung);
    }

    if ctx.action_attempts >= ctx.params.max_action_attempts {
        return wedged(
            WedgedReason::ActionIneffective,
            ctx,
            DecisionReason::ActionsExhausted,
        );
    }

    let blocked_by = if ctx.params.dm_posture.allows_gateway_removal(ctx.room) {
        ctx.budget.destructive_block()
    } else {
        Some(AbortReason::DmPosture)
    };

    Outcome {
        state,
        action: DecisionAction::RemoveGatewayState,
        reason: DecisionReason::EvidenceComplete,
        blocked_by,
    }
}

fn media_only_lane(prior: &ConnectionState, ctx: &Context<'_>) -> Outcome {
    if fingerprint_broke(prior, ctx) {
        return nascent_on_fingerprint_change();
    }

    match prior {
        ConnectionState::Nascent
        | ConnectionState::Consistent
        | ConnectionState::GatewayOnly { .. }
        | ConnectionState::Retired { .. } => open_media_divergence(ctx),
        ConnectionState::PendingJoin { last_confirm, .. } => match ctx.pending_join {
            None => open_media_divergence(ctx),
            Some(join) => {
                let state = ConnectionState::PendingJoin {
                    expires_at: join.expires_at,
                    nonce: join.nonce.clone(),
                    last_confirm: *last_confirm,
                };
                if ctx.now.saturating_since(*last_confirm) < ctx.params.min_corroboration_gap_ms {
                    return Outcome::hold(state, DecisionReason::PendingJoinOutstanding);
                }
                Outcome {
                    state,
                    action: DecisionAction::ConfirmConnection {
                        nonce: join.nonce.clone(),
                    },
                    reason: DecisionReason::PendingJoinOutstanding,
                    blocked_by: ctx.budget.constructive_block(),
                }
            }
        },
        ConnectionState::Repairing {
            since,
            last_attempt,
        } => repairing_lane(*since, *last_attempt, ctx),
        ConnectionState::MediaOnly {
            since,
            corroborations,
            last_corroboration,
            location,
            participant_joined_at,
            repair_verdict,
            fingerprint: _,
        } => corroborate_media(
            *since,
            *corroborations,
            *last_corroboration,
            *location,
            *participant_joined_at,
            *repair_verdict,
            ctx,
        ),
        ConnectionState::ActionTaken { kind, since, .. } => match kind {
            ActionKind::RemoveParticipant => {
                if ctx.action_attempts >= ctx.params.max_action_attempts {
                    return wedged(
                        WedgedReason::ActionIneffective,
                        ctx,
                        DecisionReason::ActionsExhausted,
                    );
                }
                reopen_media_divergence(*since, ctx, DecisionReason::ActionUnresolved)
            }
            ActionKind::RemoveGatewayState => open_media_divergence(ctx),
        },
        ConnectionState::Wedged {
            reason,
            at,
            fingerprint: print,
        } => Outcome::hold(
            ConnectionState::Wedged {
                reason: *reason,
                at: *at,
                fingerprint: *print,
            },
            DecisionReason::WedgedHolding,
        ),
    }
}

fn open_media_divergence(ctx: &Context<'_>) -> Outcome {
    if let Some(join) = ctx.pending_join {
        return Outcome {
            state: ConnectionState::PendingJoin {
                expires_at: join.expires_at,
                nonce: join.nonce.clone(),
                last_confirm: Millis::ZERO,
            },
            action: DecisionAction::ConfirmConnection {
                nonce: join.nonce.clone(),
            },
            reason: DecisionReason::PendingJoinOutstanding,
            blocked_by: ctx.budget.constructive_block(),
        };
    }

    match ctx.room.scope() {
        Scope::Guild => {
            if ctx.repair_attempts > ctx.params.max_action_attempts {
                return wedged(
                    WedgedReason::RepairExhausted,
                    ctx,
                    DecisionReason::RepairExhausted,
                );
            }
            Outcome {
                state: ConnectionState::Repairing {
                    since: ctx.now,
                    last_attempt: Millis::ZERO,
                },
                action: DecisionAction::RepairState,
                reason: DecisionReason::RepairAttempted,
                blocked_by: ctx.budget.constructive_block(),
            }
        }
        Scope::Dm => reopen_media_divergence(ctx.now, ctx, DecisionReason::DivergenceOpened),
    }
}

fn repairing_lane(since: Millis, last_attempt: Millis, ctx: &Context<'_>) -> Outcome {
    match ctx.repair_verdict {
        Some(RepairVerdict::Repaired) => {
            Outcome::hold(ConnectionState::Consistent, DecisionReason::RepairSucceeded)
        }
        Some(RepairVerdict::NotRepairable) => {
            reopen_media_divergence(since, ctx, DecisionReason::RepairNotPossible)
        }
        Some(RepairVerdict::NoChange) | None => {
            if ctx.repair_attempts > ctx.params.max_action_attempts {
                return wedged(
                    WedgedReason::RepairExhausted,
                    ctx,
                    DecisionReason::RepairExhausted,
                );
            }
            let state = ConnectionState::Repairing {
                since,
                last_attempt,
            };
            if ctx.now.saturating_since(last_attempt) < ctx.params.min_corroboration_gap_ms {
                return Outcome::hold(state, DecisionReason::RepairMadeNoChange);
            }
            Outcome {
                state,
                action: DecisionAction::RepairState,
                reason: DecisionReason::RepairAttempted,
                blocked_by: ctx.budget.constructive_block(),
            }
        }
    }
}

fn reopen_media_divergence(since: Millis, ctx: &Context<'_>, reason: DecisionReason) -> Outcome {
    let Some(location) = ctx.location else {
        return Outcome::hold(
            ConnectionState::Nascent,
            DecisionReason::LocationUnavailable,
        );
    };

    Outcome::hold(
        ConnectionState::MediaOnly {
            since,
            corroborations: 1,
            fingerprint: ctx.fingerprint,
            last_corroboration: ctx.now,
            location,
            participant_joined_at: ctx.joined_at.unwrap_or(ctx.now),
            repair_verdict: ctx.repair_verdict,
        },
        reason,
    )
}

#[allow(clippy::too_many_arguments)]
fn corroborate_media(
    since: Millis,
    corroborations: u32,
    last_corroboration: Millis,
    location: LocationIndex,
    participant_joined_at: Millis,
    repair_verdict: Option<RepairVerdict>,
    ctx: &Context<'_>,
) -> Outcome {
    let joined_at = ctx.joined_at.unwrap_or(participant_joined_at);

    if joined_at > since {
        return Outcome::hold(
            ConnectionState::MediaOnly {
                since: ctx.now,
                corroborations: 1,
                fingerprint: ctx.fingerprint,
                last_corroboration: ctx.now,
                location,
                participant_joined_at: joined_at,
                repair_verdict,
            },
            DecisionReason::ParticipantJoinedAfterDivergence,
        );
    }

    if ctx.now.saturating_since(since) > ctx.params.max_corroboration_window_ms {
        return Outcome::hold(
            ConnectionState::MediaOnly {
                since: ctx.now,
                corroborations: 1,
                fingerprint: ctx.fingerprint,
                last_corroboration: ctx.now,
                location,
                participant_joined_at: joined_at,
                repair_verdict,
            },
            DecisionReason::CorroborationWindowExpired,
        );
    }

    let spaced =
        ctx.now.saturating_since(last_corroboration) >= ctx.params.min_corroboration_gap_ms;
    let corroborations = if spaced {
        corroborations.saturating_add(1)
    } else {
        corroborations
    };
    let last_corroboration = if spaced { ctx.now } else { last_corroboration };
    let progress = if spaced {
        DecisionReason::CorroborationRecorded
    } else {
        DecisionReason::CorroborationSpacedTooTightly
    };

    let state = ConnectionState::MediaOnly {
        since,
        corroborations,
        fingerprint: ctx.fingerprint,
        last_corroboration,
        location,
        participant_joined_at: joined_at,
        repair_verdict,
    };

    if corroborations < ctx.params.required_corroborations {
        return Outcome::hold(state, progress);
    }

    if ctx.now.saturating_since(since) < ctx.params.min_divergence_ms {
        return Outcome::hold(state, DecisionReason::DivergenceTooYoung);
    }

    if matches!(ctx.room, RoomKey::Guild { .. })
        && repair_verdict != Some(RepairVerdict::NotRepairable)
    {
        return Outcome::hold(state, DecisionReason::RepairNotDefinitive);
    }

    if ctx.action_attempts >= ctx.params.max_action_attempts {
        return wedged(
            WedgedReason::ActionIneffective,
            ctx,
            DecisionReason::ActionsExhausted,
        );
    }

    let Some(target) = ctx.location_value else {
        return Outcome::hold(state, DecisionReason::LocationUnavailable);
    };

    let blocked_by = if ctx.params.dm_posture.allows_media_removal(ctx.room) {
        ctx.budget.destructive_block()
    } else {
        Some(AbortReason::DmPosture)
    };

    Outcome {
        state,
        action: DecisionAction::RemoveParticipant {
            location: target.clone(),
        },
        reason: DecisionReason::EvidenceComplete,
        blocked_by,
    }
}

fn wedged(reason: WedgedReason, ctx: &Context<'_>, decision: DecisionReason) -> Outcome {
    Outcome::hold(
        ConnectionState::Wedged {
            reason,
            at: ctx.now,
            fingerprint: ctx.fingerprint,
        },
        decision,
    )
}
