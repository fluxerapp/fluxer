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
import {
	getSharedVoiceAudioContext,
	resolveAudioContextConstructor,
} from '@app/features/voice/engine/VoiceSharedAudioContext';
import {
	computeTimeDomainRms,
	createSpeakingAnalyser,
	createSpeakingSamples,
	readSpeakingThresholdRms,
	SPEAKING_ANALYSER_INTERVAL_MS,
} from '@app/features/voice/engine/VoiceSpeakingDetection';
import {VoiceTrackKind, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {
	assertNonEmptyString,
	assertNonNegativeFinite,
	assertObjectLike,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppAdapterAssertions';
import ParticipantVolume from '@app/features/voice/state/ParticipantVolume';
import {
	clearRemoteVoicePlaybackBoost,
	setRemoteVoicePlaybackBoost,
} from '@app/features/voice/state/RemoteVoicePlaybackBoost';
import type {Participant, RemoteAudioTrack, RemoteTrack, RemoteTrackPublication, Room} from 'livekit-client';

const logger = new Logger('VoiceEngineV2AppRemoteSpeakingAdapter');
const REMOTE_SPEAKING_ANALYSER_HANDLES_CAP = 256;

interface AnalyserHandle {
	identity: string;
	track: MediaStreamTrack;
	source: MediaStreamAudioSourceNode;
	analyser: AnalyserNode;
	samples: Float32Array<ArrayBuffer>;
	nextTickAtMs: number;
}

class VoiceEngineV2AppRemoteSpeakingAdapter {
	private audioContext: AudioContext | null = null;
	private audioContextIsShared = false;
	private analysers = new Map<string, AnalyserHandle>();
	private hydratedRoom: Room | null = null;
	private machineSnapshot: VoiceRemoteSpeakingSnapshot = createVoiceRemoteSpeakingSnapshot();
	private autoScheduleTimerId: number | null = null;

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
		const AudioContextCtor = resolveAudioContextConstructor();
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
			const analyser = createSpeakingAnalyser(ctx);
			source.connect(analyser);
			const handle: AnalyserHandle = {
				identity,
				track: mediaStreamTrack,
				source,
				analyser,
				samples: createSpeakingSamples(),
				nextTickAtMs: this.nowMs() + SPEAKING_ANALYSER_INTERVAL_MS,
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
		this.hydratedRoom = null;
		assert.equal(this.analysers.size, 0, 'clear post-condition: no analysers');
	}

	private reapplyParticipantVolume(identity: string): void {
		const room = getRoomFromMediaEngine() ?? this.hydratedRoom;
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
		this.hydratedRoom = room;
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
		const threshold = readSpeakingThresholdRms();
		this.applyCommands(this.transition({type: 'remote.tick', identity: handle.identity, rms, threshold, nowMs}));
		handle.nextTickAtMs = nowMs + SPEAKING_ANALYSER_INTERVAL_MS;
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
		if (this.autoScheduleTimerId !== null) return;
		if (typeof window === 'undefined') return;
		this.autoScheduleTimerId = window.setInterval(() => {
			this.tickHost(this.nowMs());
		}, SPEAKING_ANALYSER_INTERVAL_MS);
	}

	private stopAutoSchedule(): void {
		if (this.autoScheduleTimerId === null) return;
		if (typeof window === 'undefined') return;
		window.clearInterval(this.autoScheduleTimerId);
		this.autoScheduleTimerId = null;
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
