// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VideoCodec} from 'livekit-client';

export type ScreenShareRemoteMigrationPhase = 'breaking' | 'candidate' | 'committed';

export interface ScreenShareRemoteMigrationState {
	migrationId: string;
	generation: number;
	previousTrackSid: string | null;
	candidateTrackSid: string | null;
	committedTrackSid: string | null;
	codec?: VideoCodec;
	phase: ScreenShareRemoteMigrationPhase;
	readySent: boolean;
}

export type ScreenShareRemoteMigrationEvent =
	| {
			type: 'migration.break';
			migrationId: string;
			generation: number;
			previousTrackSid: string | null;
			codec?: VideoCodec;
	  }
	| {
			type: 'migration.candidate';
			migrationId: string;
			generation: number;
			previousTrackSid: string | null;
			candidateTrackSid: string;
			codec?: VideoCodec;
			readySent?: boolean;
	  }
	| {
			type: 'migration.commit';
			migrationId: string;
			generation: number;
			previousTrackSid: string | null;
			candidateTrackSid: string;
	  }
	| {
			type: 'migration.abort';
			migrationId: string;
			candidateTrackSid: string | null;
	  }
	| {type: 'migration.candidateUnpublished'; trackSid: string}
	| {type: 'migration.committedUnpublished'; trackSid: string}
	| {type: 'migration.readySent'};

function isSameMigration(state: ScreenShareRemoteMigrationState | null, event: {migrationId: string}): boolean {
	return state?.migrationId === event.migrationId;
}

function shouldIgnoreCandidate(
	state: ScreenShareRemoteMigrationState | null,
	event: Extract<ScreenShareRemoteMigrationEvent, {type: 'migration.candidate'}>,
): boolean {
	if (!state) return false;
	if (event.generation < state.generation) return true;
	if (state.phase === 'committed' && event.generation <= state.generation) return true;
	return (
		state.phase === 'candidate' &&
		state.migrationId === event.migrationId &&
		state.candidateTrackSid === event.candidateTrackSid
	);
}

function shouldIgnoreBreak(
	state: ScreenShareRemoteMigrationState | null,
	event: Extract<ScreenShareRemoteMigrationEvent, {type: 'migration.break'}>,
): boolean {
	if (!state) return false;
	if (event.generation < state.generation) return true;
	if (state.phase === 'committed' && event.generation <= state.generation) return true;
	return state.phase === 'breaking' && state.migrationId === event.migrationId;
}

function shouldIgnoreCommit(
	state: ScreenShareRemoteMigrationState | null,
	event: Extract<ScreenShareRemoteMigrationEvent, {type: 'migration.commit'}>,
): boolean {
	if (!state) return false;
	if (event.generation < state.generation) return true;
	if (event.generation === state.generation && !isSameMigration(state, event)) return true;
	return state.phase === 'committed' && state.committedTrackSid === event.candidateTrackSid;
}

function transitionBreak(
	state: ScreenShareRemoteMigrationState | null,
	event: Extract<ScreenShareRemoteMigrationEvent, {type: 'migration.break'}>,
): ScreenShareRemoteMigrationState {
	const previousCommittedTrackSid = state?.committedTrackSid ?? null;
	return {
		migrationId: event.migrationId,
		generation: event.generation,
		previousTrackSid: event.previousTrackSid,
		candidateTrackSid: null,
		committedTrackSid: event.previousTrackSid ?? previousCommittedTrackSid,
		...(event.codec ? {codec: event.codec} : {}),
		phase: 'breaking',
		readySent: true,
	};
}

function transitionCandidate(
	state: ScreenShareRemoteMigrationState | null,
	event: Extract<ScreenShareRemoteMigrationEvent, {type: 'migration.candidate'}>,
): ScreenShareRemoteMigrationState {
	const previousCommittedTrackSid = state?.committedTrackSid ?? null;
	return {
		migrationId: event.migrationId,
		generation: event.generation,
		previousTrackSid: event.previousTrackSid,
		candidateTrackSid: event.candidateTrackSid,
		committedTrackSid: event.previousTrackSid ?? previousCommittedTrackSid,
		...(event.codec ? {codec: event.codec} : {}),
		phase: 'candidate',
		readySent: event.readySent === true,
	};
}

function transitionCommit(
	event: Extract<ScreenShareRemoteMigrationEvent, {type: 'migration.commit'}>,
): ScreenShareRemoteMigrationState {
	return {
		migrationId: event.migrationId,
		generation: event.generation,
		previousTrackSid: event.previousTrackSid,
		candidateTrackSid: null,
		committedTrackSid: event.candidateTrackSid,
		phase: 'committed',
		readySent: true,
	};
}

function transitionAbort(
	state: ScreenShareRemoteMigrationState | null,
	event: Extract<ScreenShareRemoteMigrationEvent, {type: 'migration.abort'}>,
): ScreenShareRemoteMigrationState | null {
	if (!state || state.migrationId !== event.migrationId || state.candidateTrackSid !== event.candidateTrackSid) {
		return state;
	}
	const committedTrackSid = state.committedTrackSid ?? state.previousTrackSid;
	if (!committedTrackSid) return null;
	return {
		...state,
		candidateTrackSid: null,
		committedTrackSid,
		phase: 'committed',
		readySent: true,
	};
}

function transitionCandidateUnpublished(
	state: ScreenShareRemoteMigrationState | null,
	trackSid: string,
): ScreenShareRemoteMigrationState | null {
	if (!state || state.candidateTrackSid !== trackSid) return state;
	const committedTrackSid = state.committedTrackSid ?? state.previousTrackSid;
	if (!committedTrackSid) return null;
	return {
		...state,
		candidateTrackSid: null,
		committedTrackSid,
		phase: 'committed',
	};
}

function transitionCommittedUnpublished(
	state: ScreenShareRemoteMigrationState | null,
	trackSid: string,
): ScreenShareRemoteMigrationState | null {
	if (!state || state.committedTrackSid !== trackSid) return state;
	if (state.phase === 'breaking') {
		return {
			...state,
			previousTrackSid: state.previousTrackSid === trackSid ? null : state.previousTrackSid,
			committedTrackSid: null,
		};
	}
	return null;
}

export function transitionRemoteScreenShareMigrationState(
	state: ScreenShareRemoteMigrationState | null,
	event: ScreenShareRemoteMigrationEvent,
): ScreenShareRemoteMigrationState | null {
	switch (event.type) {
		case 'migration.break':
			return shouldIgnoreBreak(state, event) ? state : transitionBreak(state, event);
		case 'migration.candidate':
			return shouldIgnoreCandidate(state, event) ? state : transitionCandidate(state, event);
		case 'migration.commit':
			return shouldIgnoreCommit(state, event) ? state : transitionCommit(event);
		case 'migration.abort':
			return transitionAbort(state, event);
		case 'migration.candidateUnpublished':
			return transitionCandidateUnpublished(state, event.trackSid);
		case 'migration.committedUnpublished':
			return transitionCommittedUnpublished(state, event.trackSid);
		case 'migration.readySent':
			return state && !state.readySent ? {...state, readySent: true} : state;
		default:
			return state;
	}
}
