// SPDX-License-Identifier: AGPL-3.0-or-later

import {SPEAKING_REMOTE_ATTACK_MS, SPEAKING_REMOTE_RELEASE_MS} from '@app/features/voice/engine/VoiceSpeakingThreshold';
import {
	VoiceConnectionQuality,
	type VoiceConnectionQuality as VoiceConnectionQualityType,
} from '@app/features/voice/engine/VoiceTrackSource';
import type {Participant, Room} from 'livekit-client';
import {describe, expect, it} from 'vitest';
import {
	__TEST__,
	createLivekitParticipantSnapshot,
	createLivekitParticipantSnapshotsFromRoom,
	createVoiceParticipantSnapshot,
	createVoiceRemoteSpeakingSnapshot,
	findParticipantSnapshotByUserIdAndConnectionId,
	transitionVoiceParticipantSnapshot,
	transitionVoiceRemoteSpeakingSnapshot,
	type VoiceRemoteSpeakingCommand,
	type VoiceRemoteSpeakingSnapshot,
} from './VoiceParticipantStateMachine';

const connectionQuality = VoiceConnectionQuality.Good;

function participant(
	identity: string,
	overrides: Omit<Partial<Participant>, 'connectionQuality'> & {
		connectionQuality?: VoiceConnectionQualityType;
		audioTrackSids?: ReadonlyArray<string>;
		videoTrackSids?: ReadonlyArray<string>;
	} = {},
): Participant {
	return {
		identity,
		sid: overrides.sid ?? `sid-${identity}`,
		isLocal: overrides.isLocal ?? false,
		isSpeaking: overrides.isSpeaking ?? false,
		connectionQuality: overrides.connectionQuality ?? connectionQuality,
		metadata: overrides.metadata,
		attributes: overrides.attributes ?? {},
		audioTrackPublications:
			overrides.audioTrackPublications ?? new Map((overrides.audioTrackSids ?? []).map((sid) => [sid, {}])),
		videoTrackPublications: new Map((overrides.videoTrackSids ?? []).map((sid) => [sid, {}])),
		isMicrophoneEnabled: overrides.isMicrophoneEnabled ?? false,
		isCameraEnabled: overrides.isCameraEnabled ?? false,
		isScreenShareEnabled: overrides.isScreenShareEnabled ?? false,
		joinedAt: overrides.joinedAt ?? null,
		lastSpokeAt: overrides.lastSpokeAt ?? null,
	} as Participant;
}

function room(localParticipant: Participant | null, remoteParticipants: ReadonlyArray<Participant>): Room {
	return {
		localParticipant,
		remoteParticipants: new Map(remoteParticipants.map((p) => [p.identity, p])),
	} as Room;
}

function commandsOf(snapshot: VoiceRemoteSpeakingSnapshot): ReadonlyArray<VoiceRemoteSpeakingCommand> {
	return snapshot.context.commands;
}

const LEVELLER_IDENTITY = 'user_1_a';
const LEVELLER_TICK_MS = 50;

function attachedRemoteSnapshot(): VoiceRemoteSpeakingSnapshot {
	const attached = transitionVoiceRemoteSpeakingSnapshot(createVoiceRemoteSpeakingSnapshot(), {
		type: 'remote.attach',
		identity: LEVELLER_IDENTITY,
		track: {},
	});
	return transitionVoiceRemoteSpeakingSnapshot(attached, {type: 'remote.clearCommands'});
}

function runRemoteTicks(
	snapshot: VoiceRemoteSpeakingSnapshot,
	options: {rms: number; threshold: number; ticks: number; startMs: number},
): {snapshot: VoiceRemoteSpeakingSnapshot; commands: Array<VoiceRemoteSpeakingCommand>; endMs: number} {
	let next = snapshot;
	let nowMs = options.startMs;
	const commands: Array<VoiceRemoteSpeakingCommand> = [];
	for (let i = 0; i < options.ticks; i++) {
		next = transitionVoiceRemoteSpeakingSnapshot(next, {
			type: 'remote.tick',
			identity: LEVELLER_IDENTITY,
			rms: options.rms,
			threshold: options.threshold,
			nowMs,
		});
		commands.push(...commandsOf(next));
		next = transitionVoiceRemoteSpeakingSnapshot(next, {type: 'remote.clearCommands'});
		nowMs += LEVELLER_TICK_MS;
	}
	return {snapshot: next, commands, endMs: nowMs};
}

function playbackBoostOf(snapshot: VoiceRemoteSpeakingSnapshot): number {
	return snapshot.context.analysers.get(LEVELLER_IDENTITY)?.playbackBoost ?? Number.NaN;
}

function appliedBoostOf(snapshot: VoiceRemoteSpeakingSnapshot): number {
	return snapshot.context.analysers.get(LEVELLER_IDENTITY)?.appliedBoost ?? Number.NaN;
}

describe('VoiceParticipantStateMachine participants', () => {
	it('keeps participant snapshot references stable when an upsert is equal', () => {
		const p = participant('user_42_conn-a', {
			audioTrackSids: ['audio-b', 'audio-a'],
			videoTrackSids: ['video-a'],
			attributes: {role: 'speaker'},
			joinedAt: new Date(100),
		});
		let snapshot = createVoiceParticipantSnapshot();
		snapshot = transitionVoiceParticipantSnapshot(snapshot, {
			type: 'participant.upsert',
			snapshot: createLivekitParticipantSnapshot(p),
		});
		const participantsRef = snapshot.context.participants;
		const participantRef = snapshot.context.participants[p.identity];

		snapshot = transitionVoiceParticipantSnapshot(snapshot, {
			type: 'participant.upsert',
			snapshot: createLivekitParticipantSnapshot(
				participant('user_42_conn-a', {
					audioTrackSids: ['audio-b', 'audio-a'],
					videoTrackSids: ['video-a'],
					attributes: {role: 'speaker'},
					joinedAt: new Date(100),
				}),
			),
		});

		expect(snapshot.context.participants).toBe(participantsRef);
		expect(snapshot.context.participants[p.identity]).toBe(participantRef);
		expect(participantRef?.audioTrackSids).toEqual(['audio-a', 'audio-b']);
	});

	it('derives screen-share audio state from audio publications', () => {
		const p = participant('user_42_conn-a', {
			audioTrackPublications: new Map([
				[
					'screen-audio',
					{
						source: 'screen_share_audio',
						isMuted: false,
					},
				],
			]) as Participant['audioTrackPublications'],
		});

		expect(createLivekitParticipantSnapshot(p).isScreenShareAudioEnabled).toBe(true);
	});

	it('hydrates from the room and removes stale participants', () => {
		let snapshot = createVoiceParticipantSnapshot();
		const alice = participant('user_1_a');
		const bob = participant('user_2_b');
		snapshot = transitionVoiceParticipantSnapshot(snapshot, {
			type: 'participant.hydrate',
			snapshots: [createLivekitParticipantSnapshot(alice), createLivekitParticipantSnapshot(bob)],
		});

		const hydrated = createLivekitParticipantSnapshotsFromRoom(room(alice, []), snapshot.context.participants);
		snapshot = transitionVoiceParticipantSnapshot(snapshot, {type: 'participant.hydrate', snapshots: hydrated});

		expect(Object.keys(snapshot.context.participants)).toEqual(['user_1_a']);
		expect(snapshot.context.participants['user_1_a']).toBeDefined();
		expect(snapshot.context.participants['user_2_b']).toBeUndefined();
	});

	it('parses multiple connections for the same user identity independently', () => {
		let snapshot = createVoiceParticipantSnapshot();
		for (const p of [participant('user_99_desktop'), participant('user_99_mobile')]) {
			snapshot = transitionVoiceParticipantSnapshot(snapshot, {
				type: 'participant.upsert',
				snapshot: createLivekitParticipantSnapshot(p),
			});
		}

		expect(
			findParticipantSnapshotByUserIdAndConnectionId(snapshot.context.participants, '99', 'desktop')?.identity,
		).toBe('user_99_desktop');
		expect(
			findParticipantSnapshotByUserIdAndConnectionId(snapshot.context.participants, '99', 'mobile')?.identity,
		).toBe('user_99_mobile');
		expect(findParticipantSnapshotByUserIdAndConnectionId(snapshot.context.participants, '99', null)).toBeUndefined();
	});

	it('diffs active speaker updates without creating missing participants', () => {
		let snapshot = createVoiceParticipantSnapshot();
		for (const p of [participant('user_1_a'), participant('user_2_b')]) {
			snapshot = transitionVoiceParticipantSnapshot(snapshot, {
				type: 'participant.upsert',
				snapshot: createLivekitParticipantSnapshot(p),
			});
		}
		snapshot = transitionVoiceParticipantSnapshot(snapshot, {
			type: 'participant.activeSpeakers',
			identities: ['user_1_a', 'user_3_c'],
		});

		expect(snapshot.context.participants['user_1_a']?.isSpeaking).toBe(true);
		expect(snapshot.context.participants['user_2_b']?.isSpeaking).toBe(false);
		expect(snapshot.context.participants['user_3_c']).toBeUndefined();

		snapshot = transitionVoiceParticipantSnapshot(snapshot, {
			type: 'participant.activeSpeakers',
			identities: ['user_2_b'],
		});
		expect(snapshot.context.participants['user_1_a']?.isSpeaking).toBe(false);
		expect(snapshot.context.participants['user_2_b']?.isSpeaking).toBe(true);
	});

	it('records lastSpokeAt from audio-level speaking updates', () => {
		let snapshot = createVoiceParticipantSnapshot();
		snapshot = transitionVoiceParticipantSnapshot(snapshot, {
			type: 'participant.upsert',
			snapshot: createLivekitParticipantSnapshot(participant('user_1_a')),
		});
		snapshot = transitionVoiceParticipantSnapshot(snapshot, {
			type: 'participant.setAudioLevelSpeaking',
			identity: 'user_1_a',
			speaking: true,
			nowMs: 1234,
		});
		expect(snapshot.context.participants['user_1_a']?.isAudioLevelSpeaking).toBe(true);
		expect(snapshot.context.participants['user_1_a']?.lastSpokeAt).toBe(1234);
		snapshot = transitionVoiceParticipantSnapshot(snapshot, {
			type: 'participant.upsert',
			snapshot: createLivekitParticipantSnapshot(
				participant('user_1_a', {lastSpokeAt: new Date(9999)}),
				snapshot.context.participants['user_1_a'],
			),
		});
		expect(snapshot.context.participants['user_1_a']?.lastSpokeAt).toBe(1234);
		snapshot = transitionVoiceParticipantSnapshot(snapshot, {
			type: 'participant.setAudioLevelSpeaking',
			identity: 'user_1_a',
			speaking: false,
			nowMs: 2000,
		});
		expect(snapshot.context.participants['user_1_a']?.isAudioLevelSpeaking).toBe(false);
		expect(snapshot.context.participants['user_1_a']?.lastSpokeAt).toBe(1234);
	});
});

describe('VoiceParticipantStateMachine remote speaking', () => {
	it('applies attack and release timing for audio-level speaking', () => {
		const track = {};
		const threshold = 0.015;
		let snapshot = createVoiceRemoteSpeakingSnapshot();
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.attach', identity: 'user_1_a', track});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});

		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.tick',
			identity: 'user_1_a',
			rms: 0.02,
			threshold,
			nowMs: 0,
		});
		expect(commandsOf(snapshot).some((command) => command.type === 'setAudioLevelSpeaking')).toBe(false);
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});

		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.tick',
			identity: 'user_1_a',
			rms: 0.02,
			threshold,
			nowMs: SPEAKING_REMOTE_ATTACK_MS + 1,
		});
		expect(commandsOf(snapshot)).toContainEqual({type: 'setAudioLevelSpeaking', identity: 'user_1_a', speaking: true});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});

		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.tick',
			identity: 'user_1_a',
			rms: 0,
			threshold,
			nowMs: 100,
		});
		expect(commandsOf(snapshot).some((command) => command.type === 'setAudioLevelSpeaking')).toBe(false);
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});

		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.tick',
			identity: 'user_1_a',
			rms: 0,
			threshold,
			nowMs: 100 + SPEAKING_REMOTE_RELEASE_MS + 1,
		});
		expect(commandsOf(snapshot)).toContainEqual({type: 'setAudioLevelSpeaking', identity: 'user_1_a', speaking: false});
	});

	it('detaches and clears remote state when a track ends', () => {
		let snapshot = createVoiceRemoteSpeakingSnapshot();
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.attach',
			identity: 'user_1_a',
			track: {},
		});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});

		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.tick',
			identity: 'user_1_a',
			rms: 0,
			threshold: 0,
			nowMs: 10,
			trackEnded: true,
		});

		expect(snapshot.context.analysers.has('user_1_a')).toBe(false);
		expect(commandsOf(snapshot)).toEqual([
			{type: 'setAudioLevelSpeaking', identity: 'user_1_a', speaking: false},
			{type: 'clearPlaybackBoost', identity: 'user_1_a'},
		]);
	});

	it('suspends on visibility hide and requests rehydrate on show', () => {
		let snapshot = createVoiceRemoteSpeakingSnapshot();
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.attach',
			identity: 'user_1_a',
			track: {},
		});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.attach',
			identity: 'user_2_b',
			track: {},
		});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});

		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.visibilityHidden'});
		expect(snapshot.context.analyserSuspendedByVisibility).toBe(true);
		expect(snapshot.context.analysers.size).toBe(0);
		expect(commandsOf(snapshot)).toEqual([
			{type: 'setAudioLevelSpeaking', identity: 'user_1_a', speaking: false},
			{type: 'clearPlaybackBoost', identity: 'user_1_a'},
			{type: 'setAudioLevelSpeaking', identity: 'user_2_b', speaking: false},
			{type: 'clearPlaybackBoost', identity: 'user_2_b'},
		]);

		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.visibilityVisible'});
		expect(snapshot.context.analyserSuspendedByVisibility).toBe(false);
		expect(commandsOf(snapshot)).toEqual([{type: 'rehydrateRemoteAnalysers'}]);
	});

	it('holds the playback boost through silence and releases it back to unity', () => {
		let snapshot = transitionVoiceRemoteSpeakingSnapshot(createVoiceRemoteSpeakingSnapshot(), {
			type: 'remote.attach',
			identity: LEVELLER_IDENTITY,
			track: {},
		});
		expect(commandsOf(snapshot)).toEqual([{type: 'setPlaybackBoost', identity: LEVELLER_IDENTITY, boost: 1}]);
		expect(playbackBoostOf(snapshot)).toBe(1);
		expect(appliedBoostOf(snapshot)).toBe(1);
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});

		const settled = runRemoteTicks(snapshot, {rms: 0.03, threshold: 0.006, ticks: 60, startMs: 0});
		expect(playbackBoostOf(settled.snapshot)).toBeGreaterThan(2.5);
		expect(settled.commands.some((command) => command.type === 'setPlaybackBoost')).toBe(true);

		const settledBoost = playbackBoostOf(settled.snapshot);
		const held = runRemoteTicks(settled.snapshot, {rms: 0, threshold: 0.006, ticks: 60, startMs: settled.endMs});
		expect(held.endMs - settled.endMs).toBe(__TEST__.REMOTE_PLAYBACK_SILENCE_HOLD_MS);
		expect(playbackBoostOf(held.snapshot)).toBe(settledBoost);
		expect(held.commands.some((command) => command.type === 'clearPlaybackBoost')).toBe(false);

		const released = runRemoteTicks(held.snapshot, {rms: 0, threshold: 0.006, ticks: 700, startMs: held.endMs});
		expect(released.commands.filter((command) => command.type === 'clearPlaybackBoost')).toHaveLength(1);
		expect(playbackBoostOf(released.snapshot)).toBe(1);
		expect(appliedBoostOf(released.snapshot)).toBe(1);

		snapshot = transitionVoiceRemoteSpeakingSnapshot(released.snapshot, {
			type: 'remote.detach',
			identity: LEVELLER_IDENTITY,
		});
		expect(commandsOf(snapshot)).toEqual([
			{type: 'setAudioLevelSpeaking', identity: LEVELLER_IDENTITY, speaking: false},
			{type: 'clearPlaybackBoost', identity: LEVELLER_IDENTITY},
		]);
	});

	it('holds the boost through an inter-syllabic pause instead of pumping', () => {
		const settled = runRemoteTicks(attachedRemoteSnapshot(), {rms: 0.02, threshold: 0.006, ticks: 120, startMs: 0});
		expect(playbackBoostOf(settled.snapshot)).toBeCloseTo(__TEST__.REMOTE_PLAYBACK_MAX_BOOST, 2);

		const settledBoost = playbackBoostOf(settled.snapshot);
		const paused = runRemoteTicks(settled.snapshot, {rms: 0.002, threshold: 0.006, ticks: 4, startMs: settled.endMs});
		expect(Math.abs(playbackBoostOf(paused.snapshot) - settledBoost)).toBeLessThan(settledBoost * 0.01);
		expect(paused.commands).toHaveLength(0);
	});

	it('integrates steps the emit guard suppresses', () => {
		const settled = runRemoteTicks(attachedRemoteSnapshot(), {rms: 0.086, threshold: 0.006, ticks: 200, startMs: 0});
		expect(playbackBoostOf(settled.snapshot)).toBeCloseTo(1.0465, 4);
		expect(appliedBoostOf(settled.snapshot)).toBeLessThan(playbackBoostOf(settled.snapshot));
	});

	it('targets roughly -21 dBFS of playback level', () => {
		const settled = runRemoteTicks(attachedRemoteSnapshot(), {rms: 0.045, threshold: 0.006, ticks: 200, startMs: 0});
		expect(playbackBoostOf(settled.snapshot)).toBeCloseTo(2, 4);
		expect(playbackBoostOf(settled.snapshot) * 0.045).toBeCloseTo(__TEST__.REMOTE_PLAYBACK_TARGET_RMS, 4);
	});

	it('never integrates on a zero threshold', () => {
		const settled = runRemoteTicks(attachedRemoteSnapshot(), {rms: 0, threshold: 0, ticks: 10, startMs: 0});
		expect(playbackBoostOf(settled.snapshot)).toBe(1);
		expect(settled.commands.some((command) => command.type === 'setPlaybackBoost')).toBe(false);
	});

	it('clears a stale boost when reattaching a different track', () => {
		const settled = runRemoteTicks(attachedRemoteSnapshot(), {rms: 0.02, threshold: 0.006, ticks: 60, startMs: 0});
		expect(appliedBoostOf(settled.snapshot)).toBeGreaterThan(1);

		const reattached = transitionVoiceRemoteSpeakingSnapshot(settled.snapshot, {
			type: 'remote.attach',
			identity: LEVELLER_IDENTITY,
			track: {},
		});
		expect(commandsOf(reattached)).toEqual([
			{type: 'setAudioLevelSpeaking', identity: LEVELLER_IDENTITY, speaking: false},
			{type: 'clearPlaybackBoost', identity: LEVELLER_IDENTITY},
			{type: 'setPlaybackBoost', identity: LEVELLER_IDENTITY, boost: 1},
		]);
		expect(playbackBoostOf(reattached)).toBe(1);
		expect(appliedBoostOf(reattached)).toBe(1);
	});

	it('clears stale speaking flags after detach and clear', () => {
		let snapshot = createVoiceRemoteSpeakingSnapshot();
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.attach',
			identity: 'user_1_a',
			track: {},
		});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.tick',
			identity: 'user_1_a',
			rms: 0.02,
			threshold: 0.015,
			nowMs: 0,
		});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.tick',
			identity: 'user_1_a',
			rms: 0.02,
			threshold: 0.015,
			nowMs: SPEAKING_REMOTE_ATTACK_MS + 1,
		});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.detach', identity: 'user_1_a'});
		expect(commandsOf(snapshot)).toContainEqual({type: 'setAudioLevelSpeaking', identity: 'user_1_a', speaking: false});

		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.attach',
			identity: 'user_1_a',
			track: {},
		});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
			type: 'remote.attach',
			identity: 'user_2_b',
			track: {},
		});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});
		for (const identity of ['user_1_a', 'user_2_b']) {
			snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
				type: 'remote.tick',
				identity,
				rms: 0.02,
				threshold: 0.015,
				nowMs: 0,
			});
			snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {
				type: 'remote.tick',
				identity,
				rms: 0.02,
				threshold: 0.015,
				nowMs: SPEAKING_REMOTE_ATTACK_MS + 1,
			});
		}
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clearCommands'});
		snapshot = transitionVoiceRemoteSpeakingSnapshot(snapshot, {type: 'remote.clear'});
		expect(commandsOf(snapshot)).toEqual([
			{type: 'setAudioLevelSpeaking', identity: 'user_1_a', speaking: false},
			{type: 'clearPlaybackBoost', identity: 'user_1_a'},
			{type: 'setAudioLevelSpeaking', identity: 'user_2_b', speaking: false},
			{type: 'clearPlaybackBoost', identity: 'user_2_b'},
		]);
		expect(snapshot.context.analysers.size).toBe(0);
	});
});
