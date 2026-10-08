// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {assertNonNullObject, assertString} from '@app/features/voice/engine/v2/VoiceEngineV2AppAdapterAssertions';
import {
	selectVoiceEngineV2Model,
	type VoiceEngineV2AudioControls,
	type VoiceEngineV2AudioMode,
	type VoiceEngineV2ConnectionStatus,
	type VoiceEngineV2MediaStatus,
	type VoiceEngineV2Model,
	type VoiceEngineV2Participant,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2';

export type VoiceEngineV2AppProjectionSource = VoiceEngineV2Model | VoiceEngineV2Snapshot;

export type VoiceEngineV2AppParticipantSnapshot = Readonly<{
	identity: string;
	name?: string;
	userId: string | null;
	connectionId: string | null;
	sid: string;
	isLocal: boolean;
	isSpeaking: boolean;
	isAudioLevelSpeaking: boolean;
	connectionQuality?: string;
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

export interface VoiceEngineV2AppConnectionProjection {
	status: VoiceEngineV2ConnectionStatus;
	connected: boolean;
	connecting: boolean;
	disconnecting: boolean;
	reconnecting: boolean;
	failed: boolean;
	canPublishMedia: boolean;
	tearingDown: boolean;
	guildId: string | null;
	channelId: string | null;
	userId: string | null;
	sessionId: string | null;
	roomSid: string | null;
	roomName: string | null;
	serverRegion: string | null;
}

export interface VoiceEngineV2AppConnectionFallback {
	connected?: boolean;
	connecting?: boolean;
	reconnecting?: boolean;
	guildId?: string | null;
	channelId?: string | null;
	userId?: string | null;
	sessionId?: string | null;
}

interface VoiceEngineV2AppLocalMediaProjection {
	microphone: VoiceEngineV2MediaStatus;
	camera: VoiceEngineV2MediaStatus;
	screen: VoiceEngineV2MediaStatus;
	screenAudio: VoiceEngineV2MediaStatus;
	audio: VoiceEngineV2AudioControls;
	audioMode: VoiceEngineV2AudioMode;
	hasActiveLocalMedia: boolean;
	canPublishMedia: boolean;
	effectiveMicrophoneEnabled: boolean;
	localSpeakingOverride: boolean | null;
	locallyMuted: boolean;
	locallyDeafened: boolean;
	pushToTalkActive: boolean;
	pushToMuteActive: boolean;
	inputVolume: number;
	outputVolume: number;
	screenCaptureId: string | null;
}

export interface VoiceEngineV2AppParticipantProjection {
	participants: Array<VoiceEngineV2AppParticipantSnapshot>;
	participantIdentities: Array<string>;
}

export interface VoiceEngineV2AppParticipantSpeakingSnapshot {
	isSpeaking?: boolean | null;
	isAudioLevelSpeaking?: boolean | null;
}

export type VoiceEngineV2AppVoiceMuteReason = 'guild' | 'permission' | 'voice_push_to_talk' | 'self' | null;

interface VoiceEngineV2AppMuteReasonVoiceState {
	mute?: boolean | null;
}

export interface VoiceEngineV2AppMuteReasonInput {
	voiceState: VoiceEngineV2AppMuteReasonVoiceState | null;
	permissionMuted: boolean;
	audio: VoiceEngineV2AudioControls;
}

export function selectVoiceEngineV2AppConnection(
	source: VoiceEngineV2AppProjectionSource,
): VoiceEngineV2AppConnectionProjection {
	assertNonNullObject(source, 'source');
	const model = voiceEngineV2AppModel(source);
	assertNonNullObject(model.connection, 'model.connection');
	const voiceState = model.connection.gateway.selfVoiceState;
	return {
		status: model.connection.status,
		connected: model.connection.connected,
		connecting: model.connection.connecting,
		disconnecting: model.connection.status === 'disconnecting',
		reconnecting: model.connection.reconnecting,
		failed: model.connection.failed,
		canPublishMedia: model.canPublishMedia,
		tearingDown: model.tearingDown,
		guildId: voiceState?.guildId ?? null,
		channelId: voiceState?.channelId ?? null,
		userId: voiceState?.userId ?? null,
		sessionId: voiceState?.sessionId ?? null,
		roomSid: model.connection.liveKit.roomSid,
		roomName: model.connection.liveKit.roomName,
		serverRegion: model.connection.liveKit.serverRegion,
	};
}

function isProjectionDefinitiveConnection(projection: VoiceEngineV2AppConnectionProjection): boolean {
	if (projection.connected) return true;
	if (projection.connecting) return true;
	if (projection.reconnecting) return true;
	if (projection.channelId) return true;
	return false;
}

function isFallbackEmpty(fallback: VoiceEngineV2AppConnectionFallback): boolean {
	if (fallback.connected) return false;
	if (fallback.connecting) return false;
	if (fallback.reconnecting) return false;
	if (fallback.channelId) return false;
	return true;
}

export function selectVoiceEngineV2AppConnectionWithFallback(
	source: VoiceEngineV2AppProjectionSource,
	fallback: VoiceEngineV2AppConnectionFallback,
): VoiceEngineV2AppConnectionProjection {
	assertNonNullObject(source, 'source');
	assertNonNullObject(fallback, 'fallback');
	const projection = selectVoiceEngineV2AppConnection(source);
	if (isProjectionDefinitiveConnection(projection)) return projection;
	if (isFallbackEmpty(fallback)) return projection;
	const connected = fallback.connected ?? projection.connected;
	const connecting = fallback.connecting ?? projection.connecting;
	const reconnecting = fallback.reconnecting ?? projection.reconnecting;
	return {
		...projection,
		status: connected ? 'connected' : connecting ? 'connecting' : reconnecting ? 'reconnecting' : projection.status,
		connected,
		connecting,
		reconnecting,
		canPublishMedia: connected || projection.canPublishMedia,
		guildId: fallback.guildId ?? projection.guildId,
		channelId: fallback.channelId ?? projection.channelId,
		userId: fallback.userId ?? projection.userId,
		sessionId: fallback.sessionId ?? projection.sessionId,
	};
}

function selectVoiceEngineV2AppLocalMedia(
	source: VoiceEngineV2AppProjectionSource,
): VoiceEngineV2AppLocalMediaProjection {
	assertNonNullObject(source, 'source');
	const model = voiceEngineV2AppModel(source);
	assertNonNullObject(model.media, 'model.media');
	const audio = model.media.audio;
	assertNonNullObject(audio, 'audio');
	return {
		microphone: model.media.microphone,
		camera: model.media.camera,
		screen: model.media.screen,
		screenAudio: model.media.screenAudio,
		audio,
		audioMode: audio.mode,
		hasActiveLocalMedia: model.hasActiveLocalMedia,
		canPublishMedia: model.canPublishMedia,
		effectiveMicrophoneEnabled: model.media.effectiveMicrophoneEnabled,
		localSpeakingOverride: model.media.localSpeakingOverride,
		locallyMuted: audio.locallyMuted,
		locallyDeafened: audio.locallyDeafened,
		pushToTalkActive: audio.pushToTalkActive,
		pushToMuteActive: audio.pushToMuteActive,
		inputVolume: audio.inputVolume,
		outputVolume: audio.outputVolume,
		screenCaptureId: model.media.screenCaptureId,
	};
}

export function selectVoiceEngineV2AppEffectiveSelfMuteForVoiceStatePayload(
	source: VoiceEngineV2AppProjectionSource,
): boolean {
	assertNonNullObject(source, 'source');
	return selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls(selectVoiceEngineV2AppLocalMedia(source).audio);
}

export function selectVoiceEngineV2AppIntentSelfMuteForVoiceStatePayload(
	source: VoiceEngineV2AppProjectionSource,
): boolean {
	assertNonNullObject(source, 'source');
	return selectVoiceEngineV2AppIntentSelfMuteFromAudioControls(selectVoiceEngineV2AppLocalMedia(source).audio);
}

function selectVoiceEngineV2AppIntentSelfMuteFromAudioControls(audio: VoiceEngineV2AudioControls): boolean {
	assertNonNullObject(audio, 'audio');
	const intentMuted = audio.locallyMuted || audio.mutedByPermission;
	assert.equal(typeof intentMuted, 'boolean', 'intent self-mute must be a boolean');
	return intentMuted;
}

function isPushToTalkSilent(audio: VoiceEngineV2AudioControls): boolean {
	if (audio.mode !== 'pushToTalk') return false;
	return !audio.pushToTalkActive;
}

function isPushToMuteActive(audio: VoiceEngineV2AudioControls): boolean {
	if (audio.mode !== 'pushToMute') return false;
	return audio.pushToMuteActive;
}

function isSelfMutedByAudio(audio: VoiceEngineV2AudioControls): boolean {
	if (audio.locallyMuted) return true;
	return isPushToMuteActive(audio);
}

export function selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls(audio: VoiceEngineV2AudioControls): boolean {
	assertNonNullObject(audio, 'audio');
	if (audio.locallyMuted) return true;
	if (isPushToTalkSilent(audio)) return true;
	return isPushToMuteActive(audio);
}

export function selectVoiceEngineV2AppMuteReason(
	input: VoiceEngineV2AppMuteReasonInput,
): VoiceEngineV2AppVoiceMuteReason {
	assertNonNullObject(input, 'input');
	assertNonNullObject(input.audio, 'input.audio');
	if (input.voiceState?.mute === true) return 'guild';
	if (input.permissionMuted) return 'permission';
	if (isSelfMutedByAudio(input.audio)) return 'self';
	if (isPushToTalkSilent(input.audio)) return 'voice_push_to_talk';
	return null;
}

export function selectVoiceEngineV2AppParticipants(
	source: VoiceEngineV2AppProjectionSource,
): VoiceEngineV2AppParticipantProjection {
	assertNonNullObject(source, 'source');
	const participants = sortVoiceEngineV2Participants([...voiceEngineV2AppModel(source).participants]);
	assert.ok(Array.isArray(participants), 'participants must be array');
	return {
		participants,
		participantIdentities: participants.map((participant) => participant.identity),
	};
}

export function selectVoiceEngineV2AppParticipant(
	source: VoiceEngineV2AppProjectionSource,
	participantIdentity: string,
): VoiceEngineV2AppParticipantSnapshot | null {
	assertNonNullObject(source, 'source');
	assertString(participantIdentity, 'participantIdentity');
	return (
		(voiceEngineV2AppModel(source).participants.find((participant) => participant.identity === participantIdentity) as
			| VoiceEngineV2AppParticipantSnapshot
			| undefined) ?? null
	);
}

export function isVoiceEngineV2AppParticipantSpeaking(
	participant: VoiceEngineV2AppParticipantSpeakingSnapshot | null | undefined,
): boolean {
	if (!participant) return false;
	if (participant.isSpeaking) return true;
	return Boolean(participant.isAudioLevelSpeaking);
}

function voiceEngineV2AppModel(source: VoiceEngineV2AppProjectionSource): VoiceEngineV2Model {
	return isVoiceEngineV2Snapshot(source) ? selectVoiceEngineV2Model(source) : source;
}

function isVoiceEngineV2Snapshot(source: VoiceEngineV2AppProjectionSource): source is VoiceEngineV2Snapshot {
	return 'nextOperationId' in source;
}

function sortVoiceEngineV2Participants(
	participants: Array<VoiceEngineV2Participant>,
): Array<VoiceEngineV2AppParticipantSnapshot> {
	return participants.sort(
		(left, right) => compareStrings(left.identity, right.identity) || compareStrings(left.sid, right.sid),
	) as Array<VoiceEngineV2AppParticipantSnapshot>;
}

function compareStrings(left: string, right: string): number {
	if (left < right) return -1;
	if (left > right) return 1;
	return 0;
}
