// SPDX-License-Identifier: AGPL-3.0-or-later

import {SPEAKING_REMOTE_ATTACK_MS, SPEAKING_REMOTE_RELEASE_MS} from '@app/features/voice/engine/VoiceSpeakingThreshold';
import type {VoiceConnectionQuality} from '@app/features/voice/engine/VoiceTrackSource';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

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

interface RemoteSpeakingAnalyserState {
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

interface VoiceRemoteSpeakingMachineContext {
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

const voiceRemoteSpeakingStateMachine = setup({
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
export type VoiceRemoteSpeakingSnapshot = SnapshotFrom<typeof voiceRemoteSpeakingStateMachine>;

export function createVoiceRemoteSpeakingSnapshot(): VoiceRemoteSpeakingSnapshot {
	return initialTransition(voiceRemoteSpeakingStateMachine)[0];
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
