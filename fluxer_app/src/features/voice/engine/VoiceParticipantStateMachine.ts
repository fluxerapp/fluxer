// SPDX-License-Identifier: AGPL-3.0-or-later

import {SPEAKING_REMOTE_ATTACK_MS, SPEAKING_REMOTE_RELEASE_MS} from '@app/features/voice/engine/VoiceSpeakingThreshold';
import {
	asVoiceConnectionQuality,
	type VoiceConnectionQuality,
	VoiceTrackSource,
} from '@app/features/voice/engine/VoiceTrackSource';
import {areOrderedStringArraysEqual} from '@app/features/voice/utils/StringArrayUtils';
import type {Participant, Room} from 'livekit-client';
import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export type LivekitParticipantSnapshot = Readonly<{
	identity: string;
	userId: string | null;
	connectionId: string | null;
	sid: string;
	isLocal: boolean;
	isSpeaking: boolean;
	isAudioLevelSpeaking: boolean;
	connectionQuality: VoiceConnectionQuality;
	metadata?: string;
	attributes: Readonly<Record<string, string>>;
	audioTrackSids: ReadonlyArray<string>;
	videoTrackSids: ReadonlyArray<string>;
	isMicrophoneEnabled: boolean;
	isCameraEnabled: boolean;
	isScreenShareEnabled: boolean;
	isScreenShareAudioEnabled: boolean;
	joinedAt: number | null;
	lastSpokeAt: number | null;
}>;

export interface VoiceParticipantMachineContext {
	participants: Readonly<Record<string, LivekitParticipantSnapshot>>;
}

export type VoiceParticipantEvent =
	| {type: 'participant.upsert'; snapshot: LivekitParticipantSnapshot}
	| {type: 'participant.remove'; identity: string}
	| {type: 'participant.removeConnection'; connectionId: string}
	| {type: 'participant.hydrate'; snapshots: ReadonlyArray<LivekitParticipantSnapshot>}
	| {type: 'participant.activeSpeakers'; identities: ReadonlyArray<string>}
	| {type: 'participant.setAudioLevelSpeaking'; identity: string; speaking: boolean; nowMs?: number}
	| {type: 'participant.clear'};

export interface RemoteSpeakingAnalyserState {
	identity: string;
	track: unknown;
	speaking: boolean;
	belowSinceMs: number | null;
	aboveSinceMs: number | null;
	playbackBoost: number;
	appliedBoost: number;
}

export type VoiceRemoteSpeakingCommand =
	| {type: 'setAudioLevelSpeaking'; identity: string; speaking: boolean}
	| {type: 'setPlaybackBoost'; identity: string; boost: number}
	| {type: 'clearPlaybackBoost'; identity: string}
	| {type: 'rehydrateRemoteAnalysers'};

export interface VoiceRemoteSpeakingMachineContext {
	analysers: ReadonlyMap<string, RemoteSpeakingAnalyserState>;
	analyserSuspendedByVisibility: boolean;
	commands: ReadonlyArray<VoiceRemoteSpeakingCommand>;
}

export type VoiceRemoteSpeakingEvent =
	| {type: 'remote.attach'; identity: string; track: unknown}
	| {type: 'remote.detach'; identity: string}
	| {type: 'remote.tick'; identity: string; rms: number; threshold: number; nowMs: number; trackEnded?: boolean}
	| {type: 'remote.visibilityHidden'}
	| {type: 'remote.visibilityVisible'}
	| {type: 'remote.clear'}
	| {type: 'remote.clearCommands'};

const EMPTY_PARTICIPANTS: Readonly<Record<string, LivekitParticipantSnapshot>> = {};
const EMPTY_REMOTE_COMMANDS: ReadonlyArray<VoiceRemoteSpeakingCommand> = [];
const REMOTE_PLAYBACK_TARGET_RMS = 0.09;
const REMOTE_PLAYBACK_MIN_RMS = 0.0025;
const REMOTE_PLAYBACK_MAX_BOOST = 3;
const REMOTE_PLAYBACK_MIN_CHANGE_RATIO = 1.03;
const REMOTE_PLAYBACK_BOOST_UP_COEFF = 0.08;
const REMOTE_PLAYBACK_BOOST_DOWN_COEFF = 0.35;
const REMOTE_PLAYBACK_SILENCE_HOLD_MS = 3000;
const REMOTE_PLAYBACK_SILENCE_RELEASE = 0.99;
const REMOTE_PLAYBACK_UNITY_EPSILON = 0.01;

export const extractParticipantUserId = (identity: string): string | null => {
	const match = identity.match(/^user_(\d+)(?:_(.+))?$/);
	return match ? match[1] : null;
};

export const extractParticipantConnectionId = (identity: string): string | null => {
	const match = identity.match(/^user_(\d+)_(.+)$/);
	return match ? match[2] : null;
};

const keysSorted = (m: Map<string, unknown>): ReadonlyArray<string> => Object.freeze([...m.keys()].sort());
const attrsClone = (a: Readonly<Record<string, string>>): Readonly<Record<string, string>> => Object.freeze({...a});

const attrsEqual = (a: Readonly<Record<string, string>>, b: Readonly<Record<string, string>>): boolean => {
	if (a === b) return true;
	const keysA = Object.keys(a);
	const keysB = Object.keys(b);
	if (keysA.length !== keysB.length) return false;
	for (let i = 0; i < keysA.length; i++) {
		const k = keysA[i];
		if (a[k] !== b[k]) return false;
	}
	return true;
};

export const areLivekitParticipantSnapshotsEqual = (
	a: LivekitParticipantSnapshot | undefined,
	b: LivekitParticipantSnapshot,
): boolean => {
	if (!a) return false;
	return (
		a.identity === b.identity &&
		a.userId === b.userId &&
		a.connectionId === b.connectionId &&
		a.sid === b.sid &&
		a.isLocal === b.isLocal &&
		a.isSpeaking === b.isSpeaking &&
		a.isAudioLevelSpeaking === b.isAudioLevelSpeaking &&
		a.connectionQuality === b.connectionQuality &&
		a.metadata === b.metadata &&
		a.isMicrophoneEnabled === b.isMicrophoneEnabled &&
		a.isCameraEnabled === b.isCameraEnabled &&
		a.isScreenShareEnabled === b.isScreenShareEnabled &&
		a.isScreenShareAudioEnabled === b.isScreenShareAudioEnabled &&
		a.joinedAt === b.joinedAt &&
		a.lastSpokeAt === b.lastSpokeAt &&
		areOrderedStringArraysEqual(a.audioTrackSids, b.audioTrackSids) &&
		areOrderedStringArraysEqual(a.videoTrackSids, b.videoTrackSids) &&
		attrsEqual(a.attributes, b.attributes)
	);
};

function isScreenShareAudioEnabled(p: Participant): boolean {
	for (const publication of p.audioTrackPublications.values()) {
		if (publication.source === VoiceTrackSource.ScreenShareAudio) {
			return !publication.isMuted;
		}
	}
	return false;
}

export const createLivekitParticipantSnapshot = (
	p: Participant,
	previous?: LivekitParticipantSnapshot,
): LivekitParticipantSnapshot => ({
	identity: p.identity,
	userId: extractParticipantUserId(p.identity),
	connectionId: extractParticipantConnectionId(p.identity),
	sid: p.sid,
	isLocal: p.isLocal,
	isSpeaking: p.isSpeaking,
	isAudioLevelSpeaking: previous?.isAudioLevelSpeaking ?? false,
	connectionQuality: asVoiceConnectionQuality(p.connectionQuality),
	metadata: p.metadata ?? undefined,
	attributes: attrsClone(p.attributes),
	audioTrackSids: keysSorted(p.audioTrackPublications),
	videoTrackSids: keysSorted(p.videoTrackPublications),
	isMicrophoneEnabled: p.isMicrophoneEnabled,
	isCameraEnabled: p.isCameraEnabled,
	isScreenShareEnabled: p.isScreenShareEnabled,
	isScreenShareAudioEnabled: isScreenShareAudioEnabled(p),
	joinedAt: p.joinedAt ? p.joinedAt.getTime() : null,
	lastSpokeAt: previous?.lastSpokeAt ?? null,
});

export function createLivekitParticipantSnapshotsFromRoom(
	room: Room,
	previous: Readonly<Record<string, LivekitParticipantSnapshot>> = EMPTY_PARTICIPANTS,
): Array<LivekitParticipantSnapshot> {
	const snapshots: Array<LivekitParticipantSnapshot> = [];
	if (room.localParticipant) {
		snapshots.push(createLivekitParticipantSnapshot(room.localParticipant, previous[room.localParticipant.identity]));
	}
	room.remoteParticipants.forEach((participant) => {
		snapshots.push(createLivekitParticipantSnapshot(participant, previous[participant.identity]));
	});
	return snapshots;
}

function upsertParticipant(
	context: VoiceParticipantMachineContext,
	snapshot: LivekitParticipantSnapshot,
): VoiceParticipantMachineContext {
	const existing = context.participants[snapshot.identity];
	if (areLivekitParticipantSnapshotsEqual(existing, snapshot)) return context;
	return {
		participants: {
			...context.participants,
			[snapshot.identity]: snapshot,
		},
	};
}

function removeParticipant(context: VoiceParticipantMachineContext, identity: string): VoiceParticipantMachineContext {
	if (!(identity in context.participants)) return context;
	const participants = {...context.participants};
	delete participants[identity];
	return {participants};
}

function removeConnectionParticipants(
	context: VoiceParticipantMachineContext,
	connectionId: string,
): VoiceParticipantMachineContext {
	let participants: Record<string, LivekitParticipantSnapshot> | null = null;
	for (const [identity, snapshot] of Object.entries(context.participants)) {
		if (snapshot.connectionId !== connectionId) continue;
		participants ??= {...context.participants};
		delete participants[identity];
	}
	return participants ? {participants} : context;
}

function hydrateParticipants(
	context: VoiceParticipantMachineContext,
	snapshots: ReadonlyArray<LivekitParticipantSnapshot>,
): VoiceParticipantMachineContext {
	const participants: Record<string, LivekitParticipantSnapshot> = {};
	let changed = Object.keys(context.participants).length !== snapshots.length;
	for (const snapshot of snapshots) {
		const existing = context.participants[snapshot.identity];
		participants[snapshot.identity] = areLivekitParticipantSnapshotsEqual(existing, snapshot) ? existing! : snapshot;
		if (participants[snapshot.identity] !== existing) changed = true;
	}
	if (!changed) return context;
	return {participants};
}

function setActiveSpeakers(
	context: VoiceParticipantMachineContext,
	identities: ReadonlyArray<string>,
): VoiceParticipantMachineContext {
	const speakerIds = new Set(identities);
	let changed = false;
	const participants = {...context.participants};
	for (const [identity, snapshot] of Object.entries(context.participants)) {
		const isSpeaking = speakerIds.has(identity);
		if (snapshot.isSpeaking !== isSpeaking) {
			participants[identity] = {...snapshot, isSpeaking};
			changed = true;
		}
	}
	return changed ? {participants} : context;
}

function setAudioLevelSpeaking(
	context: VoiceParticipantMachineContext,
	identity: string,
	isAudioLevelSpeaking: boolean,
	nowMs?: number,
): VoiceParticipantMachineContext {
	const existing = context.participants[identity];
	if (!existing) return context;
	const lastSpokeAt = isAudioLevelSpeaking ? (nowMs ?? existing.lastSpokeAt) : existing.lastSpokeAt;
	if (existing.isAudioLevelSpeaking === isAudioLevelSpeaking && existing.lastSpokeAt === lastSpokeAt) return context;
	return {
		participants: {
			...context.participants,
			[identity]: {...existing, isAudioLevelSpeaking, lastSpokeAt},
		},
	};
}

function remoteContext(
	analysers: ReadonlyMap<string, RemoteSpeakingAnalyserState> = new Map(),
	analyserSuspendedByVisibility = false,
	commands: ReadonlyArray<VoiceRemoteSpeakingCommand> = EMPTY_REMOTE_COMMANDS,
): VoiceRemoteSpeakingMachineContext {
	return {analysers, analyserSuspendedByVisibility, commands};
}

function appendRemoteCommands(
	context: VoiceRemoteSpeakingMachineContext,
	commands: ReadonlyArray<VoiceRemoteSpeakingCommand>,
): VoiceRemoteSpeakingMachineContext {
	if (commands.length === 0) return context;
	return {...context, commands: [...context.commands, ...commands]};
}

function detachRemoteAnalyser(
	context: VoiceRemoteSpeakingMachineContext,
	identity: string,
): VoiceRemoteSpeakingMachineContext {
	if (!context.analysers.has(identity)) return context;
	const analysers = new Map(context.analysers);
	analysers.delete(identity);
	return appendRemoteCommands({...context, analysers}, [
		{type: 'setAudioLevelSpeaking', identity, speaking: false},
		{type: 'clearPlaybackBoost', identity},
	]);
}

function attachRemoteAnalyser(
	context: VoiceRemoteSpeakingMachineContext,
	identity: string,
	track: unknown,
): VoiceRemoteSpeakingMachineContext {
	const existing = context.analysers.get(identity);
	if (existing?.track === track) return context;
	const analysers = new Map(context.analysers);
	analysers.set(identity, {
		identity,
		track,
		speaking: false,
		belowSinceMs: null,
		aboveSinceMs: null,
		playbackBoost: 1,
		appliedBoost: 1,
	});
	const commands: Array<VoiceRemoteSpeakingCommand> = [];
	if (existing?.speaking) commands.push({type: 'setAudioLevelSpeaking', identity, speaking: false});
	if (existing && existing.appliedBoost !== 1) commands.push({type: 'clearPlaybackBoost', identity});
	commands.push({type: 'setPlaybackBoost', identity, boost: 1});
	return appendRemoteCommands({...context, analysers}, commands);
}

function settleRemotePlaybackBoost(
	entry: RemoteSpeakingAnalyserState,
	nextBoost: number,
): {entry: RemoteSpeakingAnalyserState; command: VoiceRemoteSpeakingCommand | null} {
	const clampedBoost = Math.max(1, Math.min(REMOTE_PLAYBACK_MAX_BOOST, nextBoost));
	const ratio =
		clampedBoost >= entry.appliedBoost ? clampedBoost / entry.appliedBoost : entry.appliedBoost / clampedBoost;
	if (ratio < REMOTE_PLAYBACK_MIN_CHANGE_RATIO) {
		if (clampedBoost === entry.playbackBoost) return {entry, command: null};
		return {entry: {...entry, playbackBoost: clampedBoost}, command: null};
	}
	return {
		entry: {...entry, playbackBoost: clampedBoost, appliedBoost: clampedBoost},
		command: {type: 'setPlaybackBoost', identity: entry.identity, boost: clampedBoost},
	};
}

function snapRemotePlaybackBoost(entry: RemoteSpeakingAnalyserState): {
	entry: RemoteSpeakingAnalyserState;
	command: VoiceRemoteSpeakingCommand;
} {
	return {
		entry: {...entry, playbackBoost: 1, appliedBoost: 1},
		command: {type: 'clearPlaybackBoost', identity: entry.identity},
	};
}

function updateRemotePlaybackBoost(
	entry: RemoteSpeakingAnalyserState,
	rms: number,
	threshold: number,
	nowMs: number,
): {entry: RemoteSpeakingAnalyserState; command: VoiceRemoteSpeakingCommand | null} {
	const isActiveSpeech = Number.isFinite(rms) && rms >= REMOTE_PLAYBACK_MIN_RMS && rms >= threshold && entry.speaking;
	if (isActiveSpeech) {
		const desiredBoost = Math.max(1, Math.min(REMOTE_PLAYBACK_MAX_BOOST, REMOTE_PLAYBACK_TARGET_RMS / rms));
		const coefficient =
			desiredBoost > entry.playbackBoost ? REMOTE_PLAYBACK_BOOST_UP_COEFF : REMOTE_PLAYBACK_BOOST_DOWN_COEFF;
		return settleRemotePlaybackBoost(entry, entry.playbackBoost + (desiredBoost - entry.playbackBoost) * coefficient);
	}
	if (entry.playbackBoost <= 1) return {entry, command: null};
	if (entry.belowSinceMs === null || nowMs - entry.belowSinceMs < REMOTE_PLAYBACK_SILENCE_HOLD_MS) {
		return {entry, command: null};
	}
	const releasedBoost = 1 + (entry.playbackBoost - 1) * REMOTE_PLAYBACK_SILENCE_RELEASE;
	if (releasedBoost - 1 <= REMOTE_PLAYBACK_UNITY_EPSILON) return snapRemotePlaybackBoost(entry);
	return settleRemotePlaybackBoost(entry, releasedBoost);
}

function tickRemoteAnalyser(
	context: VoiceRemoteSpeakingMachineContext,
	event: Extract<VoiceRemoteSpeakingEvent, {type: 'remote.tick'}>,
): VoiceRemoteSpeakingMachineContext {
	const existing = context.analysers.get(event.identity);
	if (!existing) return context;
	if (event.trackEnded) return detachRemoteAnalyser(context, event.identity);

	const commands: Array<VoiceRemoteSpeakingCommand> = [];
	let entry = existing;
	if (event.rms >= event.threshold) {
		const aboveSinceMs = entry.aboveSinceMs ?? event.nowMs;
		entry = {
			...entry,
			belowSinceMs: null,
			aboveSinceMs,
		};
		if (!entry.speaking && event.nowMs - aboveSinceMs >= SPEAKING_REMOTE_ATTACK_MS) {
			entry = {...entry, speaking: true};
			commands.push({type: 'setAudioLevelSpeaking', identity: event.identity, speaking: true});
		}
	} else {
		const belowSinceMs = entry.belowSinceMs ?? event.nowMs;
		entry = {
			...entry,
			aboveSinceMs: null,
			belowSinceMs,
		};
		if (entry.speaking && event.nowMs - belowSinceMs >= SPEAKING_REMOTE_RELEASE_MS) {
			entry = {...entry, speaking: false};
			commands.push({type: 'setAudioLevelSpeaking', identity: event.identity, speaking: false});
		}
	}
	const boostUpdate = updateRemotePlaybackBoost(entry, event.rms, event.threshold, event.nowMs);
	entry = boostUpdate.entry;
	if (boostUpdate.command) commands.push(boostUpdate.command);
	if (entry === existing && commands.length === 0) return context;
	const analysers = new Map(context.analysers);
	analysers.set(event.identity, entry);
	return appendRemoteCommands({...context, analysers}, commands);
}

function detachAllRemoteAnalysers(
	context: VoiceRemoteSpeakingMachineContext,
	analyserSuspendedByVisibility: boolean,
): VoiceRemoteSpeakingMachineContext {
	if (context.analysers.size === 0) {
		return context.analyserSuspendedByVisibility === analyserSuspendedByVisibility
			? context
			: {...context, analyserSuspendedByVisibility};
	}
	const commands: Array<VoiceRemoteSpeakingCommand> = [];
	for (const identity of context.analysers.keys()) {
		commands.push({type: 'setAudioLevelSpeaking', identity, speaking: false});
		commands.push({type: 'clearPlaybackBoost', identity});
	}
	return appendRemoteCommands(remoteContext(new Map(), analyserSuspendedByVisibility, context.commands), commands);
}

function showRemoteAnalysers(context: VoiceRemoteSpeakingMachineContext): VoiceRemoteSpeakingMachineContext {
	if (!context.analyserSuspendedByVisibility) return context;
	return appendRemoteCommands({...context, analyserSuspendedByVisibility: false}, [{type: 'rehydrateRemoteAnalysers'}]);
}

export const voiceParticipantStateMachine = setup({
	types: {} as {
		context: VoiceParticipantMachineContext;
		events: VoiceParticipantEvent;
	},
	actions: {
		upsert: assign(({context, event}) =>
			event.type === 'participant.upsert' ? upsertParticipant(context, event.snapshot) : context,
		),
		remove: assign(({context, event}) =>
			event.type === 'participant.remove' ? removeParticipant(context, event.identity) : context,
		),
		removeConnection: assign(({context, event}) =>
			event.type === 'participant.removeConnection'
				? removeConnectionParticipants(context, event.connectionId)
				: context,
		),
		hydrate: assign(({context, event}) =>
			event.type === 'participant.hydrate' ? hydrateParticipants(context, event.snapshots) : context,
		),
		activeSpeakers: assign(({context, event}) =>
			event.type === 'participant.activeSpeakers' ? setActiveSpeakers(context, event.identities) : context,
		),
		setAudioLevelSpeaking: assign(({context, event}) =>
			event.type === 'participant.setAudioLevelSpeaking'
				? setAudioLevelSpeaking(context, event.identity, event.speaking, event.nowMs)
				: context,
		),
		clear: assign(() => ({participants: EMPTY_PARTICIPANTS})),
	},
}).createMachine({
	id: 'voiceParticipant',
	context: () => ({participants: EMPTY_PARTICIPANTS}),
	on: {
		'participant.upsert': {actions: 'upsert'},
		'participant.remove': {actions: 'remove'},
		'participant.removeConnection': {actions: 'removeConnection'},
		'participant.hydrate': {actions: 'hydrate'},
		'participant.activeSpeakers': {actions: 'activeSpeakers'},
		'participant.setAudioLevelSpeaking': {actions: 'setAudioLevelSpeaking'},
		'participant.clear': {actions: 'clear'},
	},
});

export const voiceRemoteSpeakingStateMachine = setup({
	types: {} as {
		context: VoiceRemoteSpeakingMachineContext;
		events: VoiceRemoteSpeakingEvent;
	},
	actions: {
		attach: assign(({context, event}) =>
			event.type === 'remote.attach' ? attachRemoteAnalyser(context, event.identity, event.track) : context,
		),
		detach: assign(({context, event}) =>
			event.type === 'remote.detach' ? detachRemoteAnalyser(context, event.identity) : context,
		),
		tick: assign(({context, event}) => (event.type === 'remote.tick' ? tickRemoteAnalyser(context, event) : context)),
		hide: assign(({context}) => detachAllRemoteAnalysers(context, true)),
		show: assign(({context}) => showRemoteAnalysers(context)),
		clear: assign(({context}) => detachAllRemoteAnalysers(context, false)),
		clearCommands: assign(({context}) => ({...context, commands: EMPTY_REMOTE_COMMANDS})),
	},
}).createMachine({
	id: 'voiceRemoteSpeaking',
	context: () => remoteContext(),
	on: {
		'remote.attach': {actions: 'attach'},
		'remote.detach': {actions: 'detach'},
		'remote.tick': {actions: 'tick'},
		'remote.visibilityHidden': {actions: 'hide'},
		'remote.visibilityVisible': {actions: 'show'},
		'remote.clear': {actions: 'clear'},
		'remote.clearCommands': {actions: 'clearCommands'},
	},
});

export type VoiceParticipantSnapshot = SnapshotFrom<typeof voiceParticipantStateMachine>;
export type VoiceRemoteSpeakingSnapshot = SnapshotFrom<typeof voiceRemoteSpeakingStateMachine>;

export function createVoiceParticipantSnapshot(): VoiceParticipantSnapshot {
	return getInitialSnapshot(voiceParticipantStateMachine);
}

export function transitionVoiceParticipantSnapshot(
	snapshot: VoiceParticipantSnapshot,
	event: VoiceParticipantEvent,
): VoiceParticipantSnapshot {
	return transition(voiceParticipantStateMachine, snapshot, event)[0] as VoiceParticipantSnapshot;
}

export function createVoiceRemoteSpeakingSnapshot(): VoiceRemoteSpeakingSnapshot {
	return getInitialSnapshot(voiceRemoteSpeakingStateMachine);
}

export function transitionVoiceRemoteSpeakingSnapshot(
	snapshot: VoiceRemoteSpeakingSnapshot,
	event: VoiceRemoteSpeakingEvent,
): VoiceRemoteSpeakingSnapshot {
	return transition(voiceRemoteSpeakingStateMachine, snapshot, event)[0] as VoiceRemoteSpeakingSnapshot;
}

export function clearVoiceRemoteSpeakingCommands(snapshot: VoiceRemoteSpeakingSnapshot): VoiceRemoteSpeakingSnapshot {
	return transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});
}

export const __TEST__ = {
	REMOTE_PLAYBACK_TARGET_RMS,
	REMOTE_PLAYBACK_MIN_RMS,
	REMOTE_PLAYBACK_MAX_BOOST,
	REMOTE_PLAYBACK_SILENCE_HOLD_MS,
};

export function findParticipantSnapshotByUserIdAndConnectionId(
	participants: Readonly<Record<string, LivekitParticipantSnapshot>>,
	userId: string,
	connectionId: string | null,
): LivekitParticipantSnapshot | undefined {
	for (const identity in participants) {
		const participant = participants[identity];
		if (!participant) continue;
		if (participant.userId === userId && participant.connectionId === connectionId) {
			return participant;
		}
	}
	return undefined;
}
