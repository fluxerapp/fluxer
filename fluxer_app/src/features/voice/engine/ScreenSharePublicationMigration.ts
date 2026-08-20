// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	createLocalMigrationReadinessState,
	isLocalMigrationReadinessComplete,
	type ScreenShareLocalMigrationReadinessState,
	type ScreenShareMigrationReadinessResult,
	type ScreenShareRemoteMigrationEvent,
	type ScreenShareRemoteMigrationState,
	selectLocalMigrationReadinessResult,
	transitionLocalMigrationReadinessState,
	transitionRemoteScreenShareMigrationState,
} from '@app/features/voice/engine/ScreenSharePublicationMigrationStateMachine';
import {Store} from '@app/features/voice/engine/Store';
import {selectVoiceMediaGraphViewerStreamKeys} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {getStreamKeyForParticipantIdentity} from '@app/features/voice/engine/VoiceStreamWatchState';
import {
	type LocalParticipant,
	type Participant,
	type RemoteParticipant,
	type RemoteTrack,
	type RemoteTrackPublication,
	type Room,
	RoomEvent,
	Track,
	TrackEvent,
	type VideoCodec,
} from 'livekit-client';

const logger = new Logger('ScreenSharePublicationMigration');

export const SCREEN_SHARE_PUBLICATION_MIGRATION_TOPIC = 'fluxer.rtc.screen-share-migration.v1';

const CANDIDATE_OP = 1;
const READY_OP = 2;
const COMMIT_OP = 3;
const ABORT_OP = 4;
const BREAK_OP = 5;
const TEXT_ENCODER = new TextEncoder();
const TEXT_DECODER = new TextDecoder();
const DEFAULT_READY_TIMEOUT_MS = 5000;
const REMOTE_READY_PROBE_TIMEOUT_MS = 6500;
const REMOTE_MIGRATION_STATE_TIMEOUT_MS = 10000;
const REMOTE_READY_PROBE_STATS_INTERVAL_MS = 200;
const CANDIDATE_TRACK_NAME_MARKER = '.candidate.';
export const REMOTE_MIGRATION_STATES_MAX = 256;
export const LOCAL_MIGRATION_SESSIONS_MAX = 16;
export const READY_PROBES_MAX = 64;

export interface ScreenShareMigrationCandidateMessage {
	op: typeof CANDIDATE_OP;
	d: {
		migration_id: string;
		generation: number;
		previous_track_sid: string | null;
		candidate_track_sid: string;
		codec: VideoCodec;
		reason: string;
	};
}

export interface ScreenShareMigrationBreakMessage {
	op: typeof BREAK_OP;
	d: {
		migration_id: string;
		generation: number;
		previous_track_sid: string | null;
		codec: VideoCodec;
		reason: string;
	};
}

export interface ScreenShareMigrationReadyMessage {
	op: typeof READY_OP;
	d: {
		migration_id: string;
		generation: number;
		candidate_track_sid: string;
	};
}

export interface ScreenShareMigrationCommitMessage {
	op: typeof COMMIT_OP;
	d: {
		migration_id: string;
		generation: number;
		previous_track_sid: string | null;
		candidate_track_sid: string;
	};
}

export interface ScreenShareMigrationAbortMessage {
	op: typeof ABORT_OP;
	d: {
		migration_id: string;
		generation: number;
		candidate_track_sid: string | null;
		reason: string;
	};
}

export type ScreenShareMigrationMessage =
	| ScreenShareMigrationCandidateMessage
	| ScreenShareMigrationBreakMessage
	| ScreenShareMigrationReadyMessage
	| ScreenShareMigrationCommitMessage
	| ScreenShareMigrationAbortMessage;

interface LocalMigrationInput {
	room: Room;
	publisherIdentity: string;
	migrationId: string;
	generation: number;
	previousTrackSid: string | null;
	candidateTrackSid: string;
	codec: VideoCodec;
	reason: string;
	targetIdentities: ReadonlyArray<string>;
}

interface LocalBreakBeforeMakeMigrationInput {
	migrationId: string;
	generation: number;
	previousTrackSid: string | null;
	codec: VideoCodec;
	reason: string;
}

interface ReadyProbe {
	dispose: () => void;
}

function createId(prefix: string): string {
	const cryptoObject = globalThis.crypto as Crypto | undefined;
	if (typeof cryptoObject?.randomUUID === 'function') return `${prefix}_${cryptoObject.randomUUID()}`;
	return `${prefix}_${Date.now().toString(36)}_${Math.random().toString(36).slice(2)}`;
}

function isObject(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object';
}

function isVideoCodec(value: unknown): value is VideoCodec {
	return value === 'av1' || value === 'h265' || value === 'h264' || value === 'vp9' || value === 'vp8';
}

function isStringOrNull(value: unknown): value is string | null {
	return typeof value === 'string' || value === null;
}

function isCandidateMessage(message: unknown): message is ScreenShareMigrationCandidateMessage {
	if (!isObject(message)) return false;
	if (message.op !== CANDIDATE_OP || !isObject(message.d)) return false;
	return (
		typeof message.d.migration_id === 'string' &&
		typeof message.d.generation === 'number' &&
		isStringOrNull(message.d.previous_track_sid) &&
		typeof message.d.candidate_track_sid === 'string' &&
		isVideoCodec(message.d.codec) &&
		typeof message.d.reason === 'string'
	);
}

function isBreakMessage(message: unknown): message is ScreenShareMigrationBreakMessage {
	if (!isObject(message)) return false;
	if (message.op !== BREAK_OP || !isObject(message.d)) return false;
	return (
		typeof message.d.migration_id === 'string' &&
		typeof message.d.generation === 'number' &&
		isStringOrNull(message.d.previous_track_sid) &&
		isVideoCodec(message.d.codec) &&
		typeof message.d.reason === 'string'
	);
}

function isReadyMessage(message: unknown): message is ScreenShareMigrationReadyMessage {
	if (!isObject(message)) return false;
	if (message.op !== READY_OP || !isObject(message.d)) return false;
	return (
		typeof message.d.migration_id === 'string' &&
		typeof message.d.generation === 'number' &&
		typeof message.d.candidate_track_sid === 'string'
	);
}

function isCommitMessage(message: unknown): message is ScreenShareMigrationCommitMessage {
	if (!isObject(message)) return false;
	if (message.op !== COMMIT_OP || !isObject(message.d)) return false;
	return (
		typeof message.d.migration_id === 'string' &&
		typeof message.d.generation === 'number' &&
		isStringOrNull(message.d.previous_track_sid) &&
		typeof message.d.candidate_track_sid === 'string'
	);
}

function isAbortMessage(message: unknown): message is ScreenShareMigrationAbortMessage {
	if (!isObject(message)) return false;
	if (message.op !== ABORT_OP || !isObject(message.d)) return false;
	return (
		typeof message.d.migration_id === 'string' &&
		typeof message.d.generation === 'number' &&
		isStringOrNull(message.d.candidate_track_sid) &&
		typeof message.d.reason === 'string'
	);
}

export function parseScreenShareMigrationMessage(payload: Uint8Array): ScreenShareMigrationMessage | null {
	try {
		const parsed = JSON.parse(TEXT_DECODER.decode(payload)) as unknown;
		if (!isObject(parsed)) return null;
		if (isCandidateMessage(parsed)) return parsed;
		if (isBreakMessage(parsed)) return parsed;
		if (isReadyMessage(parsed)) return parsed;
		if (isCommitMessage(parsed)) return parsed;
		if (isAbortMessage(parsed)) return parsed;
		return null;
	} catch {
		return null;
	}
}

export function encodeScreenShareMigrationMessage(message: ScreenShareMigrationMessage): Uint8Array {
	return TEXT_ENCODER.encode(JSON.stringify(message));
}

export function isScreenShareMigrationCandidatePublication(
	publication: {trackName?: string | null} | null | undefined,
): boolean {
	return typeof publication?.trackName === 'string' && publication.trackName.includes(CANDIDATE_TRACK_NAME_MARKER);
}

function getScreenSharePublicationBySid(
	participant: Pick<Participant, 'trackPublications'> | null | undefined,
	trackSid: string | null | undefined,
): RemoteTrackPublication | undefined {
	if (!participant || !trackSid) return undefined;
	const publication = participant.trackPublications.get(trackSid);
	if (!publication || publication.source !== Track.Source.ScreenShare) return undefined;
	return publication as RemoteTrackPublication;
}

function getRemoteScreenSharePublications(
	participant: Pick<Participant, 'trackPublications'> | null | undefined,
): Array<RemoteTrackPublication> {
	if (!participant) return [];
	return Array.from(participant.trackPublications.values()).filter(
		(publication): publication is RemoteTrackPublication => publication.source === Track.Source.ScreenShare,
	);
}

function getPublicationPreference(publication: RemoteTrackPublication): number {
	let preference = 1;
	if (!publication.isMuted) preference += 2;
	if (publication.isSubscribed) preference += 4;
	const track = publication.videoTrack?.mediaStreamTrack ?? publication.track?.mediaStreamTrack;
	if (track && track.readyState !== 'ended') preference += 8;
	return preference;
}

function selectBestScreenSharePublication(
	participant: Pick<Participant, 'trackPublications'> | null | undefined,
): RemoteTrackPublication | undefined {
	const publications = getRemoteScreenSharePublications(participant);
	const nonCandidatePublications = publications.filter(
		(publication) => !isScreenShareMigrationCandidatePublication(publication),
	);
	const candidates = nonCandidatePublications.length > 0 ? nonCandidatePublications : publications;
	let selected: RemoteTrackPublication | undefined;
	for (const publication of candidates) {
		if (!selected || getPublicationPreference(publication) >= getPublicationPreference(selected)) {
			selected = publication;
		}
	}
	return selected;
}

function uniquePublications(publications: ReadonlyArray<RemoteTrackPublication | null | undefined>) {
	const result: Array<RemoteTrackPublication> = [];
	const seen = new Set<string>();
	for (const publication of publications) {
		if (!publication?.trackSid || seen.has(publication.trackSid)) continue;
		seen.add(publication.trackSid);
		result.push(publication);
	}
	return result;
}

function safeSetSubscribed(publication: RemoteTrackPublication, subscribed: boolean): void {
	try {
		if (publication.isSubscribed !== subscribed) {
			publication.setSubscribed(subscribed);
		}
	} catch (error) {
		logger.warn('Failed to update migration publication subscription', {
			error,
			trackSid: publication.trackSid,
			subscribed,
		});
	}
}

function safeSetEnabled(publication: RemoteTrackPublication, enabled: boolean): void {
	try {
		publication.setEnabled(enabled);
	} catch (error) {
		logger.warn('Failed to update migration publication enabled state', {
			error,
			trackSid: publication.trackSid,
			enabled,
		});
	}
}

function publishMigrationMessage(
	participant: LocalParticipant,
	message: ScreenShareMigrationMessage,
	destinationIdentity?: string,
): Promise<void> {
	return participant.publishData(encodeScreenShareMigrationMessage(message), {
		reliable: true,
		topic: SCREEN_SHARE_PUBLICATION_MIGRATION_TOPIC,
		...(destinationIdentity ? {destinationIdentities: [destinationIdentity]} : {}),
	});
}

class ScreenShareLocalMigrationSession {
	readonly migrationId: string;
	readonly generation: number;
	readonly previousTrackSid: string | null;
	readonly candidateTrackSid: string;
	private readinessState: ScreenShareLocalMigrationReadinessState;
	private waitResolve: ((result: ScreenShareMigrationReadinessResult) => void) | null = null;
	private waitTimer: NodeJS.Timeout | null = null;

	constructor(input: LocalMigrationInput) {
		this.migrationId = input.migrationId;
		this.generation = input.generation;
		this.previousTrackSid = input.previousTrackSid;
		this.candidateTrackSid = input.candidateTrackSid;
		this.readinessState = createLocalMigrationReadinessState(input.targetIdentities);
	}

	markReady(participantIdentity: string): void {
		this.readinessState = transitionLocalMigrationReadinessState(this.readinessState, {
			type: 'watcher.ready',
			participantIdentity,
		});
		this.resolveIfComplete(false);
	}

	waitForReady(timeoutMs = DEFAULT_READY_TIMEOUT_MS): Promise<ScreenShareMigrationReadinessResult> {
		if (isLocalMigrationReadinessComplete(this.readinessState)) {
			return Promise.resolve(this.buildResult(false));
		}
		return new Promise((resolve) => {
			this.waitResolve = resolve;
			this.waitTimer = setTimeout(() => {
				this.waitTimer = null;
				this.finish(true);
			}, timeoutMs);
		});
	}

	finish(timedOut = false): ScreenShareMigrationReadinessResult {
		if (this.waitTimer) {
			clearTimeout(this.waitTimer);
			this.waitTimer = null;
		}
		const result = this.buildResult(timedOut);
		this.waitResolve?.(result);
		this.waitResolve = null;
		return result;
	}

	private resolveIfComplete(timedOut: boolean): void {
		if (!isLocalMigrationReadinessComplete(this.readinessState)) return;
		this.finish(timedOut);
	}

	private buildResult(timedOut: boolean): ScreenShareMigrationReadinessResult {
		return selectLocalMigrationReadinessResult(this.readinessState, timedOut);
	}
}

class ScreenSharePublicationMigration extends Store {
	private room: Room | null = null;
	private bindDisposer: (() => void) | null = null;
	private guildId: string | null = null;
	private channelId: string | null = null;
	private remoteStatesByIdentity = new Map<string, ScreenShareRemoteMigrationState>();
	private localSessionsById = new Map<string, ScreenShareLocalMigrationSession>();
	private readyProbesByKey = new Map<string, ReadyProbe>();
	private remoteStateExpiryTimersByIdentity = new Map<string, NodeJS.Timeout>();
	version = 0;

	createMigrationId(): string {
		return createId('ssm');
	}

	getDefaultReadyTimeoutMs(): number {
		return DEFAULT_READY_TIMEOUT_MS;
	}

	getRemoteMigrationStateTimeoutMs(): number {
		return REMOTE_MIGRATION_STATE_TIMEOUT_MS;
	}

	shouldAbortLocalMigrationForReadiness(readiness: ScreenShareMigrationReadinessResult): boolean {
		return readiness.timedOut && readiness.missingIdentities.length > 0;
	}

	createCandidateTrackName(baseName: string | undefined, migrationId: string, generation: number): string {
		const prefix = baseName && baseName.length > 0 ? baseName : 'screen_share';
		return `${prefix}${CANDIDATE_TRACK_NAME_MARKER}${generation}.${migrationId}`;
	}

	bind(room: Room, options: {guildId?: string | null; channelId?: string | null} = {}): () => void {
		this.dispose();
		this.room = room;
		this.guildId = options.guildId ?? null;
		this.channelId = options.channelId ?? null;
		const onDataReceived = (
			payload: Uint8Array,
			participant: Participant | undefined,
			_kind: unknown,
			topic?: string,
		): void => {
			if (topic !== SCREEN_SHARE_PUBLICATION_MIGRATION_TOPIC || !participant) return;
			this.handleDataMessage(room, participant, payload);
		};
		const onTrackPublished = (publication: RemoteTrackPublication, participant: RemoteParticipant): void => {
			if (publication.source !== Track.Source.ScreenShare) return;
			this.ensureRemoteCandidateSubscription(room, participant);
		};
		const onTrackSubscribed = (
			track: RemoteTrack,
			publication: RemoteTrackPublication,
			participant: RemoteParticipant,
		): void => {
			if (publication.source !== Track.Source.ScreenShare) return;
			this.handleRemoteTrackSubscribed(room, participant, publication, track);
		};
		const onTrackUnpublished = (publication: RemoteTrackPublication, participant: RemoteParticipant): void => {
			if (publication.source !== Track.Source.ScreenShare) return;
			this.handleRemoteTrackUnpublished(participant.identity, publication);
		};
		room.on(RoomEvent.DataReceived, onDataReceived);
		room.on(RoomEvent.TrackPublished, onTrackPublished);
		room.on(RoomEvent.TrackSubscribed, onTrackSubscribed);
		room.on(RoomEvent.TrackUnpublished, onTrackUnpublished);
		this.bindDisposer = () => {
			room.off(RoomEvent.DataReceived, onDataReceived);
			room.off(RoomEvent.TrackPublished, onTrackPublished);
			room.off(RoomEvent.TrackSubscribed, onTrackSubscribed);
			room.off(RoomEvent.TrackUnpublished, onTrackUnpublished);
		};
		return this.bindDisposer;
	}

	dispose(): void {
		this.bindDisposer?.();
		this.bindDisposer = null;
		for (const probe of this.readyProbesByKey.values()) {
			probe.dispose();
		}
		this.readyProbesByKey.clear();
		for (const session of this.localSessionsById.values()) {
			session.finish(true);
		}
		this.localSessionsById.clear();
		for (const timer of this.remoteStateExpiryTimersByIdentity.values()) {
			clearTimeout(timer);
		}
		this.remoteStateExpiryTimersByIdentity.clear();
		this.remoteStatesByIdentity.clear();
		this.room = null;
		this.guildId = null;
		this.channelId = null;
		this.bumpVersion();
	}

	beginLocalMigration(input: LocalMigrationInput): ScreenShareLocalMigrationSession {
		const session = new ScreenShareLocalMigrationSession(input);
		if (this.hasLocalSessionCapacity(input.migrationId)) {
			this.localSessionsById.set(input.migrationId, session);
		}
		this.commitRemoteMigrationState(input.publisherIdentity, {
			type: 'migration.candidate',
			migrationId: input.migrationId,
			generation: input.generation,
			previousTrackSid: input.previousTrackSid,
			candidateTrackSid: input.candidateTrackSid,
			codec: input.codec,
			readySent: true,
		});
		return session;
	}

	markLocalMigrationBreaking(publisherIdentity: string, message: ScreenShareMigrationBreakMessage['d']): void {
		this.applyBreak(publisherIdentity, message);
	}

	finishLocalMigration(migrationId: string): void {
		const session = this.localSessionsById.get(migrationId);
		session?.finish(false);
		this.localSessionsById.delete(migrationId);
	}

	markLocalMigrationCommitted(publisherIdentity: string, message: ScreenShareMigrationCommitMessage['d']): void {
		this.applyCommit(publisherIdentity, message);
		this.finishLocalMigration(message.migration_id);
	}

	markLocalMigrationAborted(publisherIdentity: string, message: ScreenShareMigrationAbortMessage['d']): void {
		this.applyAbort(publisherIdentity, message);
		this.finishLocalMigration(message.migration_id);
	}

	async publishCandidate(room: Room, input: LocalMigrationInput): Promise<void> {
		await publishMigrationMessage(room.localParticipant, {
			op: CANDIDATE_OP,
			d: {
				migration_id: input.migrationId,
				generation: input.generation,
				previous_track_sid: input.previousTrackSid,
				candidate_track_sid: input.candidateTrackSid,
				codec: input.codec,
				reason: input.reason,
			},
		});
	}

	async publishBreak(room: Room, input: LocalBreakBeforeMakeMigrationInput): Promise<void> {
		await publishMigrationMessage(room.localParticipant, {
			op: BREAK_OP,
			d: {
				migration_id: input.migrationId,
				generation: input.generation,
				previous_track_sid: input.previousTrackSid,
				codec: input.codec,
				reason: input.reason,
			},
		});
	}

	async publishCommit(room: Room, message: ScreenShareMigrationCommitMessage['d']): Promise<void> {
		await publishMigrationMessage(room.localParticipant, {
			op: COMMIT_OP,
			d: message,
		});
	}

	async publishAbort(room: Room, message: ScreenShareMigrationAbortMessage['d']): Promise<void> {
		await publishMigrationMessage(room.localParticipant, {
			op: ABORT_OP,
			d: message,
		});
	}

	selectScreenSharePublication(
		participant: Pick<Participant, 'identity' | 'trackPublications'> | null | undefined,
	): RemoteTrackPublication | undefined {
		if (!participant) return undefined;
		const state = this.remoteStatesByIdentity.get(participant.identity);
		if (state?.phase === 'candidate') {
			const previous = getScreenSharePublicationBySid(participant, state.previousTrackSid);
			if (previous) return previous;
		}
		if (state?.committedTrackSid) {
			const committed = getScreenSharePublicationBySid(participant, state.committedTrackSid);
			if (committed) return committed;
		}
		return selectBestScreenSharePublication(participant);
	}

	getPreloadScreenSharePublications(
		participant: Pick<Participant, 'identity' | 'trackPublications'> | null | undefined,
	): Array<RemoteTrackPublication> {
		if (!participant) return [];
		const state = this.remoteStatesByIdentity.get(participant.identity);
		if (state?.phase !== 'candidate' || !state.candidateTrackSid) return [];
		const candidate = getScreenSharePublicationBySid(participant, state.candidateTrackSid);
		return candidate ? [candidate] : [];
	}

	getManagedScreenSharePublications(
		participant: Pick<Participant, 'identity' | 'trackPublications'> | null | undefined,
	): Array<RemoteTrackPublication> {
		return uniquePublications([
			this.selectScreenSharePublication(participant),
			...this.getPreloadScreenSharePublications(participant),
		]);
	}

	isScreenShareMigrationPending(participant: Pick<Participant, 'identity'> | null | undefined): boolean {
		if (!participant) return false;
		const phase = this.remoteStatesByIdentity.get(participant.identity)?.phase;
		return phase === 'breaking' || phase === 'candidate';
	}

	isScreenShareBuffering(participant: Pick<Participant, 'identity'> | null | undefined): boolean {
		return this.isScreenShareMigrationPending(participant);
	}

	getScreenSharePublicationsToDisable(
		participant: Pick<Participant, 'identity' | 'trackPublications'> | null | undefined,
	): Array<RemoteTrackPublication> {
		if (!participant) return [];
		const keep = new Set(
			this.getManagedScreenSharePublications(participant).map((publication) => publication.trackSid),
		);
		return getRemoteScreenSharePublications(participant).filter((publication) => {
			if (keep.has(publication.trackSid)) return false;
			if (isScreenShareMigrationCandidatePublication(publication)) return false;
			return true;
		});
	}

	private handleDataMessage(room: Room, participant: Participant, payload: Uint8Array): void {
		const message = parseScreenShareMigrationMessage(payload);
		if (!message) return;
		switch (message.op) {
			case BREAK_OP:
				this.applyBreak(participant.identity, message.d);
				break;
			case CANDIDATE_OP:
				this.applyCandidate(participant.identity, message.d);
				this.ensureRemoteCandidateSubscription(room, participant as RemoteParticipant);
				break;
			case READY_OP:
				this.applyReady(participant.identity, message.d);
				break;
			case COMMIT_OP:
				this.applyCommit(participant.identity, message.d);
				break;
			case ABORT_OP:
				this.applyAbort(participant.identity, message.d);
				break;
		}
	}

	private applyCandidate(participantIdentity: string, message: ScreenShareMigrationCandidateMessage['d']): void {
		this.commitRemoteMigrationState(participantIdentity, {
			type: 'migration.candidate',
			migrationId: message.migration_id,
			generation: message.generation,
			previousTrackSid: message.previous_track_sid,
			candidateTrackSid: message.candidate_track_sid,
			codec: message.codec,
		});
	}

	private applyBreak(participantIdentity: string, message: ScreenShareMigrationBreakMessage['d']): void {
		this.commitRemoteMigrationState(participantIdentity, {
			type: 'migration.break',
			migrationId: message.migration_id,
			generation: message.generation,
			previousTrackSid: message.previous_track_sid,
			codec: message.codec,
		});
	}

	private applyReady(participantIdentity: string, message: ScreenShareMigrationReadyMessage['d']): void {
		const session = this.localSessionsById.get(message.migration_id);
		if (!session) return;
		if (session.generation !== message.generation || session.candidateTrackSid !== message.candidate_track_sid) return;
		session.markReady(participantIdentity);
	}

	private applyCommit(participantIdentity: string, message: ScreenShareMigrationCommitMessage['d']): void {
		this.commitRemoteMigrationState(participantIdentity, {
			type: 'migration.commit',
			migrationId: message.migration_id,
			generation: message.generation,
			previousTrackSid: message.previous_track_sid,
			candidateTrackSid: message.candidate_track_sid,
		});
		this.disposeReadyProbe(participantIdentity, message.candidate_track_sid);
	}

	private applyAbort(participantIdentity: string, message: ScreenShareMigrationAbortMessage['d']): void {
		const existing = this.remoteStatesByIdentity.get(participantIdentity);
		if (!existing || existing.migrationId !== message.migration_id) return;
		const roomParticipant = this.getParticipantByIdentity(participantIdentity);
		const candidate = getScreenSharePublicationBySid(roomParticipant, message.candidate_track_sid);
		if (candidate) safeSetSubscribed(candidate, false);
		this.disposeReadyProbe(participantIdentity, message.candidate_track_sid);
		this.commitRemoteMigrationState(participantIdentity, {
			type: 'migration.abort',
			migrationId: message.migration_id,
			candidateTrackSid: message.candidate_track_sid,
		});
	}

	private handleRemoteTrackSubscribed(
		room: Room,
		participant: RemoteParticipant,
		publication: RemoteTrackPublication,
		track: RemoteTrack,
	): void {
		const state = this.remoteStatesByIdentity.get(participant.identity);
		if (state?.phase !== 'candidate' || state.candidateTrackSid !== publication.trackSid) return;
		this.ensureRemoteCandidateReadyProbe(room, participant.identity, state, publication, track);
	}

	private handleRemoteTrackUnpublished(participantIdentity: string, publication: RemoteTrackPublication): void {
		const state = this.remoteStatesByIdentity.get(participantIdentity);
		if (!state) return;
		if (state.candidateTrackSid === publication.trackSid) {
			this.disposeReadyProbe(participantIdentity, publication.trackSid);
			this.commitRemoteMigrationState(participantIdentity, {
				type: 'migration.candidateUnpublished',
				trackSid: publication.trackSid,
			});
			return;
		}
		if (state.committedTrackSid === publication.trackSid) {
			this.commitRemoteMigrationState(participantIdentity, {
				type: 'migration.committedUnpublished',
				trackSid: publication.trackSid,
			});
		}
	}

	private ensureRemoteCandidateSubscription(room: Room, participant: RemoteParticipant): void {
		const state = this.remoteStatesByIdentity.get(participant.identity);
		if (state?.phase !== 'candidate' || !state.candidateTrackSid) return;
		if (
			!this.isWatchingParticipantStream(participant.identity) &&
			!this.inheritsScreenShareSubscription(participant, state)
		) {
			return;
		}
		const publication = getScreenSharePublicationBySid(participant, state.candidateTrackSid);
		if (!publication) return;
		safeSetSubscribed(publication, true);
		safeSetEnabled(publication, true);
		if (publication.track) {
			this.ensureRemoteCandidateReadyProbe(
				room,
				participant.identity,
				state,
				publication,
				publication.track as RemoteTrack,
			);
		}
	}

	private ensureRemoteCandidateReadyProbe(
		room: Room,
		participantIdentity: string,
		state: ScreenShareRemoteMigrationState,
		publication: RemoteTrackPublication,
		track: RemoteTrack,
	): void {
		if (state.readySent || !state.candidateTrackSid) return;
		const candidateTrackSid = state.candidateTrackSid;
		const key = this.getReadyProbeKey(participantIdentity, candidateTrackSid);
		if (this.readyProbesByKey.has(key)) return;
		if (!this.hasReadyProbeCapacity(participantIdentity, candidateTrackSid)) return;
		const markReady = (): void => {
			const current = this.remoteStatesByIdentity.get(participantIdentity);
			if (
				!current ||
				current.migrationId !== state.migrationId ||
				current.generation !== state.generation ||
				current.candidateTrackSid !== candidateTrackSid ||
				current.readySent
			) {
				return;
			}
			this.commitRemoteMigrationState(participantIdentity, {type: 'migration.readySent'});
			this.disposeReadyProbe(participantIdentity, candidateTrackSid);
			void publishMigrationMessage(
				room.localParticipant,
				{
					op: READY_OP,
					d: {
						migration_id: state.migrationId,
						generation: state.generation,
						candidate_track_sid: candidateTrackSid,
					},
				},
				participantIdentity,
			).catch((error) => {
				logger.warn('Failed to publish screen share migration readiness ACK', {
					error,
					participantIdentity,
					trackSid: candidateTrackSid,
				});
			});
		};
		this.readyProbesByKey.set(
			key,
			this.createReadyProbe(track, publication, markReady, () =>
				this.disposeReadyProbe(participantIdentity, candidateTrackSid),
			),
		);
	}

	private createReadyProbe(
		track: RemoteTrack,
		publication: RemoteTrackPublication,
		onReady: () => void,
		onExpired: () => void,
	): ReadyProbe {
		let disposed = false;
		let video: HTMLVideoElement | null = null;
		let statsInterval: NodeJS.Timeout | null = null;
		const cleanups: Array<() => void> = [];
		const complete = (): void => {
			if (disposed) return;
			onReady();
		};
		const dispose = (): void => {
			if (disposed) return;
			disposed = true;
			if (statsInterval) {
				clearInterval(statsInterval);
				statsInterval = null;
			}
			for (const cleanup of cleanups.splice(0)) {
				cleanup();
			}
			if (video) {
				try {
					track.detach(video);
				} catch {}
				video.remove();
				video = null;
			}
		};
		const expireTimer = setTimeout(() => {
			if (disposed) return;
			onExpired();
		}, REMOTE_READY_PROBE_TIMEOUT_MS);
		cleanups.push(() => clearTimeout(expireTimer));

		const checkStats = (): void => {
			const statsPromise = track.getRTCStatsReport?.();
			if (!statsPromise) return;
			void statsPromise
				.then((report) => {
					if (!report) return;
					if (disposed) return;
					for (const value of report.values()) {
						const stats = value as RTCStats & {
							kind?: string;
							mediaType?: string;
							framesDecoded?: number;
							framesReceived?: number;
						};
						if (stats.type !== 'inbound-rtp' || (stats.kind ?? stats.mediaType) !== 'video') continue;
						if ((stats.framesDecoded ?? 0) > 0 || (stats.framesReceived ?? 0) > 0) {
							complete();
							return;
						}
					}
				})
				.catch(() => {});
		};
		statsInterval = setInterval(checkStats, REMOTE_READY_PROBE_STATS_INTERVAL_MS);
		checkStats();

		if (typeof document === 'undefined') {
			return {dispose};
		}

		video = document.createElement('video');
		video.muted = true;
		video.autoplay = true;
		video.playsInline = true;
		video.style.position = 'fixed';
		video.style.left = '-2px';
		video.style.top = '-2px';
		video.style.width = '1px';
		video.style.height = '1px';
		video.style.opacity = '0';
		video.style.pointerEvents = 'none';
		video.setAttribute('aria-hidden', 'true');
		const onVideoReady = (): void => {
			if (!video) return;
			if (video.readyState >= 2 && video.videoWidth > 0 && video.videoHeight > 0) {
				complete();
			}
		};
		for (const event of ['loadeddata', 'canplay', 'playing'] as const) {
			video.addEventListener(event, onVideoReady);
			cleanups.push(() => video?.removeEventListener(event, onVideoReady));
		}
		const onPlaybackStarted = (): void => complete();
		const onDimensionsChanged = (): void => complete();
		track.on(TrackEvent.VideoPlaybackStarted, onPlaybackStarted);
		track.on(TrackEvent.VideoDimensionsChanged, onDimensionsChanged);
		cleanups.push(() => {
			track.off(TrackEvent.VideoPlaybackStarted, onPlaybackStarted);
			track.off(TrackEvent.VideoDimensionsChanged, onDimensionsChanged);
		});
		document.body?.append(video);
		try {
			track.attach(video);
			void video.play().catch(() => {});
		} catch (error) {
			logger.debug('Failed to attach hidden screen share migration readiness probe', {
				error,
				trackSid: publication.trackSid,
			});
		}
		const requestVideoFrameCallback = video.requestVideoFrameCallback?.bind(video);
		if (requestVideoFrameCallback) {
			const handle = requestVideoFrameCallback(() => complete());
			const cancel = video.cancelVideoFrameCallback?.bind(video);
			if (cancel) cleanups.push(() => cancel(handle));
		}
		onVideoReady();
		return {dispose};
	}

	private disposeReadyProbe(participantIdentity: string, trackSid: string | null | undefined): void {
		if (!trackSid) return;
		const key = this.getReadyProbeKey(participantIdentity, trackSid);
		const probe = this.readyProbesByKey.get(key);
		if (!probe) return;
		this.readyProbesByKey.delete(key);
		probe.dispose();
	}

	private getReadyProbeKey(participantIdentity: string, trackSid: string): string {
		return `${participantIdentity}:${trackSid}`;
	}

	private clearRemoteMigrationExpiry(participantIdentity: string): void {
		const timer = this.remoteStateExpiryTimersByIdentity.get(participantIdentity);
		if (!timer) return;
		clearTimeout(timer);
		this.remoteStateExpiryTimersByIdentity.delete(participantIdentity);
	}

	private scheduleRemoteMigrationExpiry(participantIdentity: string, state: ScreenShareRemoteMigrationState): void {
		this.clearRemoteMigrationExpiry(participantIdentity);
		if (state.phase === 'committed') return;
		const timer = setTimeout(() => {
			const current = this.remoteStatesByIdentity.get(participantIdentity);
			if (
				!current ||
				current.phase === 'committed' ||
				current.migrationId !== state.migrationId ||
				current.generation !== state.generation
			) {
				return;
			}
			logger.warn('Expiring stale screen share migration state', {
				participantIdentity,
				migrationId: current.migrationId,
				generation: current.generation,
				phase: current.phase,
			});
			const next = transitionRemoteScreenShareMigrationState(current, {
				type: 'migration.abort',
				migrationId: current.migrationId,
				candidateTrackSid: current.candidateTrackSid,
			});
			this.setRemoteMigrationState(participantIdentity, next);
		}, REMOTE_MIGRATION_STATE_TIMEOUT_MS);
		this.remoteStateExpiryTimersByIdentity.set(participantIdentity, timer);
		assert.ok(
			this.remoteStateExpiryTimersByIdentity.size <= REMOTE_MIGRATION_STATES_MAX,
			'remote state expiry timers must stay within cap',
		);
	}

	private isWatchingParticipantStream(participantIdentity: string): boolean {
		const streamKey = getStreamKeyForParticipantIdentity(this.guildId, this.channelId, participantIdentity);
		return (
			streamKey != null &&
			selectVoiceMediaGraphViewerStreamKeys(voiceMediaGraphStore.getGraphSnapshot()).includes(streamKey)
		);
	}

	private inheritsScreenShareSubscription(
		participant: Pick<Participant, 'trackPublications'>,
		state: ScreenShareRemoteMigrationState,
	): boolean {
		const previous =
			getScreenSharePublicationBySid(participant, state.previousTrackSid) ??
			getScreenSharePublicationBySid(participant, state.committedTrackSid);
		return previous?.isSubscribed === true;
	}

	private getParticipantByIdentity(participantIdentity: string): Participant | null {
		const room = this.room;
		if (!room) return null;
		if (room.localParticipant.identity === participantIdentity) return room.localParticipant;
		return room.remoteParticipants.get(participantIdentity) ?? null;
	}

	private setRemoteMigrationState(participantIdentity: string, next: ScreenShareRemoteMigrationState | null): void {
		if (next) {
			if (!this.hasRemoteStateCapacity(participantIdentity)) return;
			this.remoteStatesByIdentity.set(participantIdentity, next);
			this.scheduleRemoteMigrationExpiry(participantIdentity, next);
		} else {
			this.remoteStatesByIdentity.delete(participantIdentity);
			this.clearRemoteMigrationExpiry(participantIdentity);
		}
		this.bumpVersion();
	}

	private hasRemoteStateCapacity(participantIdentity: string): boolean {
		assert.ok(this.remoteStatesByIdentity.size <= REMOTE_MIGRATION_STATES_MAX, 'remote states must stay within cap');
		if (this.remoteStatesByIdentity.has(participantIdentity)) return true;
		if (this.remoteStatesByIdentity.size < REMOTE_MIGRATION_STATES_MAX) return true;
		logger.error('Refusing remote screen share migration state beyond cap; a stale entry may have leaked', {
			participantIdentity,
			trackedCount: this.remoteStatesByIdentity.size,
			cap: REMOTE_MIGRATION_STATES_MAX,
		});
		return false;
	}

	private hasLocalSessionCapacity(migrationId: string): boolean {
		assert.ok(this.localSessionsById.size <= LOCAL_MIGRATION_SESSIONS_MAX, 'local sessions must stay within cap');
		if (this.localSessionsById.has(migrationId)) return true;
		if (this.localSessionsById.size < LOCAL_MIGRATION_SESSIONS_MAX) return true;
		logger.error('Refusing local screen share migration session beyond cap; a stale session may have leaked', {
			migrationId,
			trackedCount: this.localSessionsById.size,
			cap: LOCAL_MIGRATION_SESSIONS_MAX,
		});
		return false;
	}

	private hasReadyProbeCapacity(participantIdentity: string, trackSid: string): boolean {
		assert.ok(this.readyProbesByKey.size <= READY_PROBES_MAX, 'ready probes must stay within cap');
		if (this.readyProbesByKey.size < READY_PROBES_MAX) return true;
		logger.error('Refusing screen share migration ready probe beyond cap; a stale probe may have leaked', {
			participantIdentity,
			trackSid,
			trackedCount: this.readyProbesByKey.size,
			cap: READY_PROBES_MAX,
		});
		return false;
	}

	private commitRemoteMigrationState(participantIdentity: string, event: ScreenShareRemoteMigrationEvent): void {
		const current = this.remoteStatesByIdentity.get(participantIdentity) ?? null;
		const next = transitionRemoteScreenShareMigrationState(current, event);
		if (next === current) return;
		this.setRemoteMigrationState(participantIdentity, next);
	}

	private bumpVersion(): void {
		this.update(() => {
			this.version++;
		});
	}
}

const instance = new ScreenSharePublicationMigration();

if (typeof window !== 'undefined') {
	(
		window as typeof window & {
			_screenSharePublicationMigration?: ScreenSharePublicationMigration;
		}
	)._screenSharePublicationMigration = instance;
}

export type {ScreenShareMigrationReadinessResult, ScreenShareLocalMigrationSession};

export default instance;
