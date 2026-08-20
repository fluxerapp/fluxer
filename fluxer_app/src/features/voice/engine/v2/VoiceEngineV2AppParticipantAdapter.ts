// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {LivekitParticipantSnapshot} from '@app/features/voice/engine/VoiceParticipantStateMachine';
import {asVoiceConnectionQuality, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import type {
	VoiceEngineV2Controller,
	VoiceEngineV2Event,
	VoiceEngineV2Model,
	VoiceEngineV2Participant,
	VoiceEngineV2Track,
} from '@fluxer/voice_engine_v2';
import type {Participant, Room, TrackPublication} from 'livekit-client';
import {Track} from 'livekit-client';
import {assertNonNullObject, assertString} from './VoiceEngineV2AppAdapterAssertions';

const MAX_DISCARDED_CONNECTION_IDS = 4096;

export interface VoiceEngineV2AppParticipantTrackFlags {
	isMicrophoneEnabled?: boolean;
	isCameraEnabled?: boolean;
	isScreenShareEnabled?: boolean;
	isScreenShareAudioEnabled?: boolean;
}

export interface VoiceEngineV2AppParticipantAdapterOptions {
	controller: VoiceEngineV2Controller;
	getModel: () => VoiceEngineV2Model;
	ingest?: (event: VoiceEngineV2Event) => void;
	getCurrentConnectionId?: () => string | null | undefined;
}

export type VoiceEngineV2AppLivekitParticipantSnapshot = LivekitParticipantSnapshot &
	VoiceEngineV2Participant & {
		name: string;
	};

function extractParticipantUserId(identity: string): string | null {
	const match = identity.match(/^user_(\d+)(?:_(.+))?$/);
	return match ? match[1] : null;
}

function extractParticipantConnectionId(identity: string): string | null {
	const match = identity.match(/^user_(\d+)_(.+)$/);
	return match ? match[2] : null;
}

function isScreenShareAudioEnabled(participant: Participant): boolean {
	for (const publication of participant.audioTrackPublications.values()) {
		if (publication.source === VoiceTrackSource.ScreenShareAudio) {
			return !publication.isMuted;
		}
	}
	return false;
}

function v2TrackSource(source: unknown): string {
	switch (source) {
		case Track.Source.Microphone:
		case VoiceTrackSource.Microphone:
			return 'microphone';
		case Track.Source.Camera:
		case VoiceTrackSource.Camera:
			return 'camera';
		case Track.Source.ScreenShare:
		case VoiceTrackSource.ScreenShare:
			return 'screen';
		case Track.Source.ScreenShareAudio:
		case VoiceTrackSource.ScreenShareAudio:
			return 'screenAudio';
		default:
			return 'unknown';
	}
}

function snapshotFromParticipant(
	participant: Participant,
	previous?: VoiceEngineV2AppLivekitParticipantSnapshot,
): VoiceEngineV2AppLivekitParticipantSnapshot {
	return {
		identity: participant.identity,
		name: participant.name ?? previous?.name ?? '',
		userId: extractParticipantUserId(participant.identity),
		connectionId: extractParticipantConnectionId(participant.identity),
		sid: participant.sid,
		isLocal: participant.isLocal,
		isSpeaking: participant.isSpeaking,
		isAudioLevelSpeaking: previous?.isAudioLevelSpeaking ?? false,
		connectionQuality: asVoiceConnectionQuality(participant.connectionQuality),
		metadata: participant.metadata ?? undefined,
		attributes: Object.freeze({...participant.attributes}),
		audioTrackSids: Object.freeze([...participant.audioTrackPublications.keys()].sort()),
		videoTrackSids: Object.freeze([...participant.videoTrackPublications.keys()].sort()),
		isMicrophoneEnabled: participant.isMicrophoneEnabled,
		isCameraEnabled: participant.isCameraEnabled,
		isScreenShareEnabled: participant.isScreenShareEnabled,
		isScreenShareAudioEnabled: isScreenShareAudioEnabled(participant),
		joinedAt: participant.joinedAt ? participant.joinedAt.getTime() : null,
		lastSpokeAt: previous?.lastSpokeAt ?? null,
	};
}

function trackFromPublication(participant: Participant, publication: TrackPublication): VoiceEngineV2Track {
	const source = v2TrackSource(publication.source);
	return {
		participantIdentity: participant.identity,
		participantSid: participant.sid,
		trackSid: publication.trackSid,
		trackName: publication.trackName || source,
		kind: publication.kind === Track.Kind.Audio ? 'audio' : 'video',
		source,
		muted: publication.isMuted,
	};
}

function collectParticipantTracks(participant: Participant): Array<VoiceEngineV2Track> {
	const tracks: Array<VoiceEngineV2Track> = [];
	for (const publication of participant.audioTrackPublications.values()) {
		if (publication.trackSid) tracks.push(trackFromPublication(participant, publication));
	}
	for (const publication of participant.videoTrackPublications.values()) {
		if (publication.trackSid) tracks.push(trackFromPublication(participant, publication));
	}
	return tracks;
}

function updateTrackFlags(
	participant: VoiceEngineV2AppLivekitParticipantSnapshot,
	flags: VoiceEngineV2AppParticipantTrackFlags,
): VoiceEngineV2AppLivekitParticipantSnapshot {
	return {
		...participant,
		isMicrophoneEnabled: flags.isMicrophoneEnabled ?? participant.isMicrophoneEnabled,
		isCameraEnabled: flags.isCameraEnabled ?? participant.isCameraEnabled,
		isScreenShareEnabled: flags.isScreenShareEnabled ?? participant.isScreenShareEnabled,
		isScreenShareAudioEnabled: flags.isScreenShareAudioEnabled ?? participant.isScreenShareAudioEnabled,
	};
}

export class VoiceEngineV2AppParticipantAdapter {
	private readonly controller: VoiceEngineV2Controller;
	private readonly getModel: () => VoiceEngineV2Model;
	private readonly ingest: (event: VoiceEngineV2Event) => void;
	private readonly getCurrentConnectionId: () => string | null | undefined;
	private readonly discardedConnectionIds = new Set<string>();

	constructor(options: VoiceEngineV2AppParticipantAdapterOptions) {
		this.controller = options.controller;
		this.getModel = options.getModel;
		this.ingest = options.ingest ?? ((event) => this.controller.dispatch(event));
		this.getCurrentConnectionId = options.getCurrentConnectionId ?? (() => null);
	}

	get participants(): Readonly<Record<string, VoiceEngineV2AppLivekitParticipantSnapshot>> {
		return Object.freeze(
			Object.fromEntries(
				this.getModel().participants.map((participant) => [
					participant.identity,
					participant as unknown as VoiceEngineV2AppLivekitParticipantSnapshot,
				]),
			),
		);
	}

	isConnectionDiscarded(connectionId: string | null | undefined): boolean {
		if (connectionId == null) return false;
		assert.equal(typeof connectionId, 'string', 'connectionId must be string');
		return this.discardedConnectionIds.has(connectionId);
	}

	discardConnection(connectionId: string): void {
		assertString(connectionId, 'connectionId');
		assert.ok(
			this.discardedConnectionIds.size <= MAX_DISCARDED_CONNECTION_IDS,
			`discardedConnectionIds exceeded cap=${MAX_DISCARDED_CONNECTION_IDS}`,
		);
		this.discardedConnectionIds.add(connectionId);
		const participants = this.participants;
		for (const identity in participants) {
			const participant = participants[identity];
			if (!participant) continue;
			if (participant.connectionId === connectionId) {
				this.ingest({type: 'room.participantLeft', participantIdentity: participant.identity});
			}
		}
	}

	upsertParticipant(participant: Participant): void {
		assertNonNullObject(participant, 'participant');
		assertString(participant.identity, 'participant.identity');
		if (this.isConnectionDiscarded(extractParticipantConnectionId(participant.identity))) return;
		const previous = this.participants[participant.identity];
		const snapshot = snapshotFromParticipant(participant, previous);
		this.upsertSnapshot(snapshot);
		const nextTrackSids = new Set<string>();
		for (const track of collectParticipantTracks(participant)) {
			nextTrackSids.add(track.trackSid);
			this.ingest({type: 'room.trackPublished', track});
			this.ingest({type: track.muted ? 'room.trackMuted' : 'room.trackUnmuted', trackSid: track.trackSid});
		}
		for (const trackSid of [...(previous?.audioTrackSids ?? []), ...(previous?.videoTrackSids ?? [])]) {
			if (!nextTrackSids.has(trackSid)) {
				this.ingest({type: 'room.trackUnpublished', trackSid});
			}
		}
	}

	hydrateFromRoom(room: Room): void {
		assertNonNullObject(room, 'room');
		assertNonNullObject(room.localParticipant, 'room.localParticipant');
		this.upsertParticipant(room.localParticipant);
		room.remoteParticipants.forEach((participant) => this.upsertParticipant(participant));
	}

	updateActiveSpeakers(speakers: Array<Participant>): void {
		assert.ok(Array.isArray(speakers), 'speakers must be array');
		const speakerIdentities = new Set(speakers.map((speaker) => speaker.identity));
		const participants = this.participants;
		for (const identity in participants) {
			const participant = participants[identity];
			if (!participant) continue;
			const speaking = speakerIdentities.has(participant.identity);
			if (participant.isSpeaking === speaking) continue;
			this.upsertSnapshot({...participant, isSpeaking: speaking});
		}
	}

	patchParticipantTrackFlags(identity: string, flags: VoiceEngineV2AppParticipantTrackFlags): void {
		assertString(identity, 'identity');
		assertNonNullObject(flags, 'flags');
		const participant = this.participants[identity];
		if (!participant) return;
		if (this.isConnectionDiscarded(participant.connectionId)) return;
		this.upsertSnapshot(updateTrackFlags(participant, flags));
	}

	updateActiveSpeakersBySid(sids: ReadonlyArray<string>): void {
		assert.ok(Array.isArray(sids), 'sids must be array');
		const speakerSids = new Set(sids);
		const participants = this.participants;
		for (const identity in participants) {
			const participant = participants[identity];
			if (!participant) continue;
			const speaking = speakerSids.has(participant.sid);
			if (participant.isSpeaking === speaking) continue;
			this.upsertSnapshot({...participant, isSpeaking: speaking});
		}
	}

	removeParticipant(identity: string): void {
		assertString(identity, 'identity');
		this.ingest({type: 'room.participantLeft', participantIdentity: identity});
	}

	removeParticipantBySid(sid: string): void {
		assertString(sid, 'sid');
		const participant = this.getParticipantBySid(sid);
		if (participant) this.removeParticipant(participant.identity);
	}

	getParticipant(identity: string): VoiceEngineV2AppLivekitParticipantSnapshot | undefined {
		assertString(identity, 'identity');
		return this.participants[identity];
	}

	getParticipantBySid(sid: string): VoiceEngineV2AppLivekitParticipantSnapshot | undefined {
		assertString(sid, 'sid');
		const participants = this.participants;
		for (const identity in participants) {
			const participant = participants[identity];
			if (participant?.sid === sid) return participant;
		}
		return undefined;
	}

	getLocalParticipant(): VoiceEngineV2AppLivekitParticipantSnapshot | undefined {
		const participants = this.participants;
		const currentConnectionId = this.getCurrentConnectionId() ?? null;
		if (currentConnectionId) {
			const participant = this.getLocalParticipantByConnectionId(participants, currentConnectionId);
			if (participant) return participant;
		}
		let fallback: VoiceEngineV2AppLivekitParticipantSnapshot | undefined;
		for (const identity in participants) {
			const participant = participants[identity];
			if (!participant?.isLocal) continue;
			if (this.isConnectionDiscarded(participant.connectionId)) continue;
			if (!fallback) {
				fallback = participant;
				continue;
			}
			const fallbackJoinedAt = fallback.joinedAt ?? 0;
			const participantJoinedAt = participant.joinedAt ?? 0;
			if (participantJoinedAt >= fallbackJoinedAt) fallback = participant;
		}
		return fallback;
	}

	getParticipantByUserIdAndConnectionId(
		userId: string,
		connectionId: string | null,
	): VoiceEngineV2AppLivekitParticipantSnapshot | undefined {
		assertString(userId, 'userId');
		const participants = this.participants;
		for (const identity in participants) {
			const participant = participants[identity];
			if (!participant) continue;
			if (participant.userId !== userId) continue;
			if (participant.connectionId === connectionId) return participant;
		}
		return undefined;
	}

	extractConnectionId(identity: string): string | null {
		assertString(identity, 'identity');
		return extractParticipantConnectionId(identity);
	}

	clear(): void {
		const participants = this.participants;
		for (const identity in participants) {
			const participant = participants[identity];
			if (!participant) continue;
			this.ingest({type: 'room.participantLeft', participantIdentity: participant.identity});
		}
	}

	private upsertSnapshot(participant: VoiceEngineV2AppLivekitParticipantSnapshot): void {
		assertNonNullObject(participant, 'participant');
		assertString(participant.identity, 'participant.identity');
		this.ingest({type: 'room.participantJoined', participant});
	}

	private getLocalParticipantByConnectionId(
		participants: Readonly<Record<string, VoiceEngineV2AppLivekitParticipantSnapshot>>,
		connectionId: string,
	): VoiceEngineV2AppLivekitParticipantSnapshot | undefined {
		assertString(connectionId, 'connectionId');
		for (const identity in participants) {
			const participant = participants[identity];
			if (!participant?.isLocal) continue;
			if (participant.connectionId === connectionId) return participant;
		}
		return undefined;
	}
}

export function createVoiceEngineV2AppParticipantAdapter(
	options: VoiceEngineV2AppParticipantAdapterOptions,
): VoiceEngineV2AppParticipantAdapter {
	return new VoiceEngineV2AppParticipantAdapter(options);
}
