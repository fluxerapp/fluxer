// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	getRoomFromMediaEngine,
	setVoiceEngineV2ParticipantAudioLevelSpeaking,
} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {
	clearVoiceRemoteSpeakingCommands,
	createVoiceRemoteSpeakingSnapshot,
	transitionVoiceRemoteSpeakingSnapshot,
	type VoiceRemoteSpeakingCommand,
	type VoiceRemoteSpeakingEvent,
	type VoiceRemoteSpeakingSnapshot,
} from '@app/features/voice/engine/VoiceParticipantStateMachine';
import {getSharedVoiceAudioContext} from '@app/features/voice/engine/VoiceSharedAudioContext';
import {getRemoteSpeakingThresholdRms} from '@app/features/voice/engine/VoiceSpeakingThreshold';
import {VoiceTrackKind, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import ParticipantVolume from '@app/features/voice/state/ParticipantVolume';
import {
	clearRemoteVoicePlaybackBoost,
	setRemoteVoicePlaybackBoost,
} from '@app/features/voice/state/RemoteVoicePlaybackBoost';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import type {Participant, RemoteAudioTrack, RemoteTrack, RemoteTrackPublication, Room} from 'livekit-client';
import {assertNonEmptyString, assertNonNegativeFinite, assertObjectLike} from './VoiceEngineV2AppAdapterAssertions';

const logger = new Logger('VoiceEngineV2AppRemoteSpeakingAdapter');
export const REMOTE_SPEAKING_ANALYSER_INTERVAL_MS = 50;
export const REMOTE_SPEAKING_ANALYSER_HANDLES_CAP = 256;

export function computeTimeDomainRms(samples: Float32Array): number {
	if (samples.length === 0) return 0;
	let sumSquares = 0;
	for (let i = 0; i < samples.length; i++) {
		const sample = samples[i]!;
		sumSquares += sample * sample;
	}
	return Math.sqrt(sumSquares / samples.length);
}

interface AnalyserHandle {
	identity: string;
	track: MediaStreamTrack;
	source: MediaStreamAudioSourceNode;
	analyser: AnalyserNode;
	samples: Float32Array<ArrayBuffer>;
	nextTickAtMs: number;
}

export interface VoiceEngineV2AppRemoteSpeakingAdapterOptions {
	autoSchedule?: boolean;
}

export class VoiceEngineV2AppRemoteSpeakingAdapter {
	private audioContext: AudioContext | null = null;
	private audioContextIsShared = false;
	private analysers = new Map<string, AnalyserHandle>();
	private installedVisibilityListener = false;
	private roomForRehydrate: Room | null = null;
	private machineSnapshot: VoiceRemoteSpeakingSnapshot = createVoiceRemoteSpeakingSnapshot();
	private autoScheduleTimerId: number | null = null;
	private readonly autoSchedule: boolean;

	constructor(options: VoiceEngineV2AppRemoteSpeakingAdapterOptions = {}) {
		this.autoSchedule = options.autoSchedule ?? true;
	}

	private transition(event: VoiceRemoteSpeakingEvent): ReadonlyArray<VoiceRemoteSpeakingCommand> {
		this.machineSnapshot = transitionVoiceRemoteSpeakingSnapshot(this.machineSnapshot, event);
		const commands = this.machineSnapshot.context.commands;
		this.machineSnapshot = clearVoiceRemoteSpeakingCommands(this.machineSnapshot);
		return commands;
	}

	private applyCommands(commands: ReadonlyArray<VoiceRemoteSpeakingCommand>): void {
		assert.ok(Array.isArray(commands), 'remote speaking commands must be an array');
		for (const command of commands) {
			switch (command.type) {
				case 'setAudioLevelSpeaking':
					setVoiceEngineV2ParticipantAudioLevelSpeaking(command.identity, command.speaking);
					break;
				case 'setPlaybackBoost':
					setRemoteVoicePlaybackBoost(command.identity, command.boost);
					this.reapplyParticipantVolume(command.identity);
					break;
				case 'clearPlaybackBoost':
					clearRemoteVoicePlaybackBoost(command.identity);
					this.reapplyParticipantVolume(command.identity);
					break;
				case 'rehydrateRemoteAnalysers':
					if (this.roomForRehydrate) this.hydrateFromRoom(this.roomForRehydrate);
					break;
			}
		}
	}

	private detachAllAdapterAnalysers(): void {
		for (const handle of this.analysers.values()) {
			this.disposeHandle(handle);
		}
		this.analysers.clear();
		this.closeAudioContext();
		this.stopAutoSchedule();
	}

	private ensureVisibilityListener(): void {
		if (this.installedVisibilityListener) return;
		if (typeof document === 'undefined') return;
		document.addEventListener('visibilitychange', () => {
			if (document.hidden) {
				if (this.machineSnapshot.context.analyserSuspendedByVisibility) return;
				this.detachAllAdapterAnalysers();
				this.applyCommands(this.transition({type: 'remote.visibilityHidden'}));
			} else {
				if (!this.machineSnapshot.context.analyserSuspendedByVisibility) return;
				this.applyCommands(this.transition({type: 'remote.visibilityVisible'}));
			}
		});
		this.installedVisibilityListener = true;
	}

	private getAudioContext(): AudioContext | null {
		if (typeof window === 'undefined') return null;
		if (this.audioContext && this.audioContext.state !== 'closed') {
			return this.audioContext;
		}
		const shared = getSharedVoiceAudioContext();
		if (shared) {
			this.audioContext = shared;
			this.audioContextIsShared = true;
			assert.notEqual(shared.state, 'closed', 'shared voice AudioContext for analyser must not be closed');
			return shared;
		}
		const AudioContextCtor =
			window.AudioContext ||
			(
				window as typeof window & {
					webkitAudioContext?: typeof AudioContext;
				}
			).webkitAudioContext;
		if (!AudioContextCtor) return null;
		try {
			this.audioContext = new AudioContextCtor({latencyHint: 'interactive'});
			this.audioContextIsShared = false;
			if (this.audioContext.state === 'suspended') {
				void this.audioContext.resume().catch((error) => {
					logger.debug('AudioContext resume rejected on remote speaking analyser', {error});
				});
			}
			return this.audioContext;
		} catch (error) {
			logger.warn('Failed to create AudioContext for remote speaking analyser', {error});
			return null;
		}
	}

	attachIfApplicable(participant: Participant, publication: RemoteTrackPublication, track: RemoteTrack): void {
		assertObjectLike<Participant>(participant, 'attachIfApplicable.participant');
		assertObjectLike<RemoteTrackPublication>(publication, 'attachIfApplicable.publication');
		assertObjectLike<RemoteTrack>(track, 'attachIfApplicable.track');
		assertNonEmptyString(participant.identity, 'attachIfApplicable.participant.identity');
		assert.ok(
			this.analysers.size <= REMOTE_SPEAKING_ANALYSER_HANDLES_CAP,
			'remote speaking analyser registry exceeded cap',
		);
		this.ensureVisibilityListener();
		if (this.machineSnapshot.context.analyserSuspendedByVisibility) return;
		if (participant.isLocal) return;
		if (publication.source !== VoiceTrackSource.Microphone) return;
		if (track.kind !== VoiceTrackKind.Audio) return;
		const audioTrack = track as RemoteAudioTrack;
		const mediaStreamTrack = audioTrack.mediaStreamTrack;
		if (!mediaStreamTrack || mediaStreamTrack.readyState === 'ended') return;
		const identity = participant.identity;
		const existing = this.analysers.get(identity);
		if (existing && existing.track === mediaStreamTrack) {
			return;
		}
		if (existing) {
			this.disposeHandle(existing);
		}
		const ctx = this.getAudioContext();
		if (!ctx) return;
		if (this.analysers.size >= REMOTE_SPEAKING_ANALYSER_HANDLES_CAP) {
			logger.warn('Remote speaking analyser registry full; refusing attach', {
				identity,
				cap: REMOTE_SPEAKING_ANALYSER_HANDLES_CAP,
			});
			return;
		}
		try {
			const source = ctx.createMediaStreamSource(new MediaStream([mediaStreamTrack]));
			const analyser = ctx.createAnalyser();
			analyser.fftSize = 512;
			analyser.smoothingTimeConstant = 0.2;
			source.connect(analyser);
			const handle: AnalyserHandle = {
				identity,
				track: mediaStreamTrack,
				source,
				analyser,
				samples: new Float32Array(new ArrayBuffer(analyser.fftSize * Float32Array.BYTES_PER_ELEMENT)),
				nextTickAtMs: this.nowMs() + REMOTE_SPEAKING_ANALYSER_INTERVAL_MS,
			};
			this.analysers.set(identity, handle);
			this.applyCommands(this.transition({type: 'remote.attach', identity, track: mediaStreamTrack}));
			this.ensureAutoSchedule();
			assert.ok(this.analysers.has(identity), 'attachIfApplicable post-condition: handle registered');
		} catch (error) {
			logger.warn('Failed to attach remote analyser', {identity, error});
		}
	}

	detachByIdentity(identity: string): void {
		assertNonEmptyString(identity, 'detachByIdentity.identity');
		const handle = this.analysers.get(identity);
		if (!handle) return;
		this.disposeHandle(handle);
		this.analysers.delete(identity);
		this.applyCommands(this.transition({type: 'remote.detach', identity}));
		if (this.analysers.size === 0) {
			this.closeAudioContext();
			this.stopAutoSchedule();
		}
		assert.ok(!this.analysers.has(identity), 'detachByIdentity post-condition: handle removed');
	}

	detachIfTrackMatches(participant: Participant, publication: RemoteTrackPublication): void {
		assertObjectLike<Participant>(participant, 'detachIfTrackMatches.participant');
		assertObjectLike<RemoteTrackPublication>(publication, 'detachIfTrackMatches.publication');
		assertNonEmptyString(participant.identity, 'detachIfTrackMatches.participant.identity');
		if (participant.isLocal) return;
		if (publication.source !== VoiceTrackSource.Microphone) return;
		const handle = this.analysers.get(participant.identity);
		if (!handle) return;
		const track = publication.track as RemoteAudioTrack | undefined;
		if (!track || track.mediaStreamTrack === handle.track) {
			this.detachByIdentity(participant.identity);
		}
	}

	clear(): void {
		assert.ok(
			this.analysers.size <= REMOTE_SPEAKING_ANALYSER_HANDLES_CAP,
			'clear pre-condition: analyser map cap respected',
		);
		this.detachAllAdapterAnalysers();
		this.applyCommands(this.transition({type: 'remote.clear'}));
		this.roomForRehydrate = null;
		assert.equal(this.analysers.size, 0, 'clear post-condition: no analysers');
	}

	private reapplyParticipantVolume(identity: string): void {
		const room = getRoomFromMediaEngine() ?? this.roomForRehydrate;
		const participant = room?.remoteParticipants.get(identity);
		if (!participant) return;
		try {
			ParticipantVolume.applySettingsToParticipant(participant);
		} catch (error) {
			logger.debug('Failed to reapply participant volume after playback boost update', {identity, error});
		}
	}

	hydrateFromRoom(room: Room): void {
		assertObjectLike<Room>(room, 'hydrateFromRoom.room');
		assert.ok(room.remoteParticipants instanceof Map, 'hydrateFromRoom.room.remoteParticipants must be a Map');
		this.ensureVisibilityListener();
		this.roomForRehydrate = room;
		if (this.machineSnapshot.context.analyserSuspendedByVisibility) return;
		room.remoteParticipants.forEach((participant) => {
			participant.audioTrackPublications.forEach((publication) => {
				const remotePub = publication as RemoteTrackPublication;
				if (!remotePub.isSubscribed) return;
				const track = remotePub.track;
				if (!track) return;
				this.attachIfApplicable(participant, remotePub, track);
			});
		});
	}

	tickHost(nowMs: number): void {
		assertNonNegativeFinite(nowMs, 'tickHost.nowMs');
		assert.ok(
			this.analysers.size <= REMOTE_SPEAKING_ANALYSER_HANDLES_CAP,
			'tickHost pre-condition: analyser cap respected',
		);
		if (this.machineSnapshot.context.analyserSuspendedByVisibility) return;
		const sizeBefore = this.analysers.size;
		const expired: Array<AnalyserHandle> = [];
		for (const handle of this.analysers.values()) {
			if (handle.nextTickAtMs <= nowMs) {
				expired.push(handle);
			}
		}
		for (const handle of expired) {
			if (this.analysers.get(handle.identity) !== handle) continue;
			this.processHandleTick(handle, nowMs);
		}
		assert.ok(this.analysers.size <= sizeBefore, 'tickHost post-condition: handles only removed');
	}

	private processHandleTick(handle: AnalyserHandle, nowMs: number): void {
		assertObjectLike<AnalyserHandle>(handle, 'processHandleTick.handle');
		assertNonNegativeFinite(nowMs, 'processHandleTick.nowMs');
		if (handle.track.readyState === 'ended') {
			this.disposeHandle(handle);
			this.analysers.delete(handle.identity);
			this.applyCommands(
				this.transition({
					type: 'remote.tick',
					identity: handle.identity,
					rms: 0,
					threshold: 0,
					nowMs,
					trackEnded: true,
				}),
			);
			if (this.analysers.size === 0) {
				this.closeAudioContext();
				this.stopAutoSchedule();
			}
			return;
		}
		handle.analyser.getFloatTimeDomainData(handle.samples);
		const rms = computeTimeDomainRms(handle.samples);
		const threshold = getRemoteSpeakingThresholdRms(VoiceSettings.getVadThreshold());
		this.applyCommands(this.transition({type: 'remote.tick', identity: handle.identity, rms, threshold, nowMs}));
		handle.nextTickAtMs = nowMs + REMOTE_SPEAKING_ANALYSER_INTERVAL_MS;
	}

	private disposeHandle(handle: AnalyserHandle): void {
		try {
			handle.source.disconnect();
		} catch (error) {
			logger.debug('Failed to disconnect remote analyser source node', {identity: handle.identity, error});
		}
		try {
			handle.analyser.disconnect();
		} catch (error) {
			logger.debug('Failed to disconnect remote analyser node', {identity: handle.identity, error});
		}
	}

	private nowMs(): number {
		if (typeof performance !== 'undefined' && typeof performance.now === 'function') {
			return performance.now();
		}
		return 0;
	}

	private ensureAutoSchedule(): void {
		if (!this.autoSchedule) return;
		if (this.autoScheduleTimerId !== null) return;
		if (typeof window === 'undefined') return;
		this.autoScheduleTimerId = window.setInterval(() => {
			this.tickHost(this.nowMs());
		}, REMOTE_SPEAKING_ANALYSER_INTERVAL_MS);
	}

	private stopAutoSchedule(): void {
		if (this.autoScheduleTimerId === null) return;
		if (typeof window === 'undefined') return;
		window.clearInterval(this.autoScheduleTimerId);
		this.autoScheduleTimerId = null;
	}

	get analyserCount(): number {
		return this.analysers.size;
	}

	hasAnalyserForIdentity(identity: string): boolean {
		assertNonEmptyString(identity, 'hasAnalyserForIdentity.identity');
		return this.analysers.has(identity);
	}

	private closeAudioContext(): void {
		if (!this.audioContext) return;
		if (this.audioContextIsShared) {
			this.audioContext = null;
			this.audioContextIsShared = false;
			return;
		}
		void this.audioContext.close().catch((error) => {
			logger.debug('Failed to close AudioContext during remote speaking analyser teardown', {error});
		});
		this.audioContext = null;
	}
}

const voiceEngineV2AppRemoteSpeakingAdapter = new VoiceEngineV2AppRemoteSpeakingAdapter();

export default voiceEngineV2AppRemoteSpeakingAdapter;
