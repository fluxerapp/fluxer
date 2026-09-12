// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::VecDeque;
use std::sync::{Mutex, MutexGuard, PoisonError};

use serde::Serialize;

use crate::decide::{ActionClass, Decision, DecisionSet};
use crate::evidence::{ConnectionObservation, RoomObservation, SideAuthority};
use crate::guards::AbortReason;
use crate::health::ReconMode;
use crate::ids::{Millis, RoomKey};
use crate::ledger::ConnectionLedger;
use crate::metrics::scope_label;

pub const DEFAULT_JOURNAL_CAPACITY: usize = 512;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Disposition {
    Held,
    Blocked(AbortReason),
    WouldExecute,
    Executed,
}

impl Disposition {
    pub const fn label(self) -> &'static str {
        match self {
            Self::Held => "held",
            Self::Blocked(_) => "blocked",
            Self::WouldExecute => "would_execute",
            Self::Executed => "executed",
        }
    }

    pub const fn blocked_by(self) -> Option<AbortReason> {
        match self {
            Self::Blocked(reason) => Some(reason),
            Self::Held | Self::WouldExecute | Self::Executed => None,
        }
    }

    pub const fn mutates(self) -> bool {
        matches!(self, Self::Executed)
    }

    pub const fn is_counterfactual(self) -> bool {
        matches!(self, Self::WouldExecute)
    }
}

pub const fn action_class_label(class: ActionClass) -> &'static str {
    match class {
        ActionClass::None => "none",
        ActionClass::Constructive => "constructive",
        ActionClass::Destructive => "destructive",
    }
}

pub const fn mode_allows(class: ActionClass, mode: ReconMode) -> bool {
    match class {
        ActionClass::None => true,
        ActionClass::Constructive => mode.allows_constructive(),
        ActionClass::Destructive => mode.allows_destructive(),
    }
}

pub fn disposition(decision: &Decision, mode: ReconMode) -> Disposition {
    let class = decision.action.class();
    match (class, decision.blocked_by) {
        (ActionClass::None, _) => Disposition::Held,
        (_, Some(reason)) => Disposition::Blocked(reason),
        (class, None) if mode_allows(class, mode) => Disposition::Executed,
        (_, None) => Disposition::WouldExecute,
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize)]
pub struct EvidenceChain {
    pub gateway_presence: &'static str,
    pub media_presence: &'static str,
    pub gateway_unknown_reason: Option<&'static str>,
    pub media_unknown_reason: Option<&'static str>,
    pub gateway_authority: Option<&'static str>,
    pub media_authority: Option<&'static str>,
    pub gateway_siblings: Vec<String>,
    pub media_locations: Vec<String>,
    pub candidates: Vec<String>,
    pub gateway_state_has_connection_id: bool,
    pub participant_joined_at_ms: Option<u64>,
    pub user_has_pending_join: bool,
    pub corroborations: u32,
    pub divergence_since_ms: Option<u64>,
    pub fingerprint: Option<String>,
    pub action_attempts: u32,
    pub repair_attempts: u32,
    pub last_repair_verdict: Option<&'static str>,
}

const fn authority_reason(authority: &SideAuthority) -> Option<&'static str> {
    match authority {
        SideAuthority::Authoritative => None,
        SideAuthority::Unknown(reason) => Some(reason.label()),
    }
}

pub fn evidence_chain(
    observation: &RoomObservation,
    connection: &ConnectionObservation,
    ledger: Option<&ConnectionLedger>,
    pending_join_skew_ms: u64,
) -> EvidenceChain {
    EvidenceChain {
        gateway_presence: connection.presence.gateway.label(),
        media_presence: connection.presence.media.label(),
        gateway_unknown_reason: connection
            .presence
            .gateway
            .unknown_reason()
            .map(|reason| reason.label()),
        media_unknown_reason: connection
            .presence
            .media
            .unknown_reason()
            .map(|reason| reason.label()),
        gateway_authority: authority_reason(&observation.authority.gateway),
        media_authority: authority_reason(&observation.authority.media),
        gateway_siblings: connection
            .gateway_siblings
            .iter()
            .map(ToString::to_string)
            .collect(),
        media_locations: connection
            .media_locations
            .iter()
            .map(ToString::to_string)
            .collect(),
        candidates: observation
            .candidates
            .locations()
            .iter()
            .map(ToString::to_string)
            .collect(),
        gateway_state_has_connection_id: connection.gateway_state_has_connection_id,
        participant_joined_at_ms: connection.participant_joined_at.map(Millis::get),
        user_has_pending_join: observation.user_has_pending_join(
            connection.user_id,
            observation.wall_at,
            pending_join_skew_ms,
        ),
        corroborations: ledger.map_or(0, |entry| entry.state.corroborations()),
        divergence_since_ms: ledger
            .and_then(|entry| entry.state.divergence_since())
            .map(Millis::get),
        fingerprint: ledger
            .and_then(|entry| entry.state.stored_fingerprint())
            .map(|fingerprint| format!("{:016x}", fingerprint.get())),
        action_attempts: ledger.map_or(0, |entry| u32::from(entry.action_attempts)),
        repair_attempts: ledger.map_or(0, |entry| u32::from(entry.repair_attempts)),
        last_repair_verdict: ledger
            .and_then(|entry| entry.last_repair_verdict)
            .map(|verdict| verdict.label()),
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct DecisionRecord {
    #[serde(skip)]
    pub room: RoomKey,
    pub turn: u64,
    pub at_ms: u64,
    pub scope: &'static str,
    pub guild_id: Option<String>,
    pub channel_id: String,
    pub connection_id: String,
    pub user_id: String,
    pub from: &'static str,
    pub to: &'static str,
    pub action: &'static str,
    pub action_class: &'static str,
    pub reason: &'static str,
    pub mode: &'static str,
    pub disposition: &'static str,
    pub blocked_by: Option<&'static str>,
    pub would_mutate: bool,
    pub evidence: EvidenceChain,
}

impl DecisionRecord {
    pub fn new(
        decision: &Decision,
        turn: crate::ids::TurnId,
        at: Millis,
        mode: ReconMode,
        evidence: EvidenceChain,
    ) -> Self {
        let room = decision.connection.room;
        let disposition = disposition(decision, mode);
        Self {
            room,
            turn: turn.get(),
            at_ms: at.get(),
            scope: scope_label(room.scope()),
            guild_id: room.guild_id().map(|guild| guild.to_string()),
            channel_id: room.channel_id().to_string(),
            connection_id: decision.connection.connection.to_string(),
            user_id: decision.user_id.to_string(),
            from: decision.from,
            to: decision.to,
            action: decision.action.label(),
            action_class: action_class_label(decision.action.class()),
            reason: decision.reason.label(),
            mode: mode.as_str(),
            disposition: disposition.label(),
            blocked_by: disposition.blocked_by().map(AbortReason::label),
            would_mutate: disposition.is_counterfactual(),
            evidence,
        }
    }

    pub fn is_counterfactual(&self) -> bool {
        self.would_mutate
    }

    pub fn matches_room(&self, room: RoomKey) -> bool {
        self.room == room
    }

    pub fn to_json(&self) -> String {
        serde_json::to_string(self)
            .unwrap_or_else(|error| format!("{{\"serialisation_error\":\"{error}\"}}"))
    }
}

pub fn log_decision(record: &DecisionRecord) {
    if record.would_mutate {
        tracing::warn!(
            room_scope = record.scope,
            guild_id = record.guild_id.as_deref(),
            channel_id = record.channel_id,
            connection_id = record.connection_id,
            user_id = record.user_id,
            action = record.action,
            reason = record.reason,
            mode = record.mode,
            turn = record.turn,
            decision = record.to_json(),
            "recon would have mutated"
        );
        return;
    }

    tracing::info!(
        room_scope = record.scope,
        guild_id = record.guild_id.as_deref(),
        channel_id = record.channel_id,
        connection_id = record.connection_id,
        user_id = record.user_id,
        action = record.action,
        reason = record.reason,
        mode = record.mode,
        disposition = record.disposition,
        turn = record.turn,
        decision = record.to_json(),
        "recon decision"
    );
}

#[derive(Debug)]
pub struct DecisionJournal {
    capacity: usize,
    entries: Mutex<VecDeque<DecisionRecord>>,
}

impl Default for DecisionJournal {
    fn default() -> Self {
        Self::new(DEFAULT_JOURNAL_CAPACITY)
    }
}

impl DecisionJournal {
    pub fn new(capacity: usize) -> Self {
        Self {
            capacity: capacity.max(1),
            entries: Mutex::new(VecDeque::new()),
        }
    }

    fn lock(&self) -> MutexGuard<'_, VecDeque<DecisionRecord>> {
        self.entries.lock().unwrap_or_else(PoisonError::into_inner)
    }

    pub fn record(&self, record: DecisionRecord) {
        let mut entries = self.lock();
        if entries.len() >= self.capacity {
            entries.pop_front();
        }
        entries.push_back(record);
    }

    pub fn len(&self) -> usize {
        self.lock().len()
    }

    pub fn is_empty(&self) -> bool {
        self.lock().is_empty()
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn last_at_ms(&self) -> Option<u64> {
        self.lock().back().map(|record| record.at_ms)
    }

    pub fn recent(&self, limit: usize) -> Vec<DecisionRecord> {
        let entries = self.lock();
        entries.iter().rev().take(limit).cloned().collect()
    }

    pub fn for_room(&self, room: RoomKey, limit: usize) -> Vec<DecisionRecord> {
        let entries = self.lock();
        entries
            .iter()
            .rev()
            .filter(|record| record.matches_room(room))
            .take(limit)
            .cloned()
            .collect()
    }

    pub fn counterfactual_count(&self) -> usize {
        self.lock()
            .iter()
            .filter(|record| record.is_counterfactual())
            .count()
    }
}

pub struct ObservedSet<'a> {
    pub set: &'a DecisionSet,
    pub observation: &'a RoomObservation,
    pub mode: ReconMode,
    pub pending_join_skew_ms: u64,
}

pub fn journal_decisions(
    journal: &DecisionJournal,
    observed: &ObservedSet<'_>,
    ledger_of: impl Fn(&Decision) -> Option<ConnectionLedger>,
) -> Vec<DecisionRecord> {
    let mut recorded = Vec::with_capacity(observed.set.decisions.len());
    for decision in &observed.set.decisions {
        let entry = ledger_of(decision);
        let chain = match observed
            .observation
            .observation_for(&decision.connection.connection)
        {
            Some(connection) => evidence_chain(
                observed.observation,
                connection,
                entry.as_ref(),
                observed.pending_join_skew_ms,
            ),
            None => EvidenceChain::default(),
        };
        let record = DecisionRecord::new(
            decision,
            observed.set.turn,
            observed.set.at,
            observed.mode,
            chain,
        );
        log_decision(&record);
        journal.record(record.clone());
        recorded.push(record);
    }
    recorded
}
