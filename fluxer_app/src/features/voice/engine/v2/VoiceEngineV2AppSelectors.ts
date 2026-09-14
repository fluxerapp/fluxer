// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {assertNonNullObject, assertString} from '@app/features/voice/engine/v2/VoiceEngineV2AppAdapterAssertions';
import {
	selectVoiceEngineV2Model,
	summarizeVoiceEngineV2Stats,
	type VoiceEngineV2AudioControls,
	type VoiceEngineV2AudioInputDevice,
	type VoiceEngineV2AudioMode,
	type VoiceEngineV2AudioOutputDevice,
	type VoiceEngineV2CameraDevice,
	type VoiceEngineV2ConnectionStatus,
	type VoiceEngineV2DeviceInventory,
	type VoiceEngineV2E2eeState,
	type VoiceEngineV2InboundVideoTrack,
	type VoiceEngineV2MediaStatus,
	type VoiceEngineV2Model,
	type VoiceEngineV2Participant,
	type VoiceEngineV2PermissionResult,
	type VoiceEngineV2Snapshot,
	type VoiceEngineV2Stats,
	type VoiceEngineV2StatsSummary,
	type VoiceEngineV2Track,
	type VoiceEngineV2TrackSource,
	type VoiceEngineV2WatchedStream,
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

export interface VoiceEngineV2AppLocalMediaProjection {
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

export interface VoiceEngineV2AppTrackProjection {
	tracks: Array<VoiceEngineV2Track>;
	audioTracks: Array<VoiceEngineV2Track>;
	videoTracks: Array<VoiceEngineV2Track>;
	microphoneTracks: Array<VoiceEngineV2Track>;
	cameraTracks: Array<VoiceEngineV2Track>;
	screenTracks: Array<VoiceEngineV2Track>;
	screenAudioTracks: Array<VoiceEngineV2Track>;
	inboundVideoTracks: Array<VoiceEngineV2InboundVideoTrack>;
}

export interface VoiceEngineV2AppWatchedStreamProjection {
	streams: Array<VoiceEngineV2WatchedStream>;
	enabledStreams: Array<VoiceEngineV2WatchedStream>;
	disabledStreams: Array<VoiceEngineV2WatchedStream>;
}

export interface VoiceEngineV2AppStatsProjection {
	stats: VoiceEngineV2Stats | null;
	summary: VoiceEngineV2StatsSummary | null;
	hasStats: boolean;
	rttMs: number | null;
	outboundTrackCount: number;
	inboundTrackCount: number;
	droppedNativeVideoFrames: number;
	droppedVideoFrameCallbacks: number;
	failureCode: string | null;
}

export interface VoiceEngineV2AppDeviceProjection {
	devices: VoiceEngineV2DeviceInventory;
	permissions: Record<string, VoiceEngineV2PermissionResult>;
	selectedAudioInput: VoiceEngineV2AudioInputDevice | null;
	selectedAudioOutput: VoiceEngineV2AudioOutputDevice | null;
	selectedCamera: VoiceEngineV2CameraDevice | null;
	hasAudioInput: boolean;
	hasAudioOutput: boolean;
	hasCamera: boolean;
	microphonePermission: VoiceEngineV2PermissionResult | null;
	cameraPermission: VoiceEngineV2PermissionResult | null;
	screenPermission: VoiceEngineV2PermissionResult | null;
}

export interface VoiceEngineV2AppE2eeProjection extends VoiceEngineV2E2eeState {
	enabled: boolean;
	pending: boolean;
	failed: boolean;
}

export interface VoiceEngineV2AppParticipantSpeakingSnapshot {
	isSpeaking?: boolean | null;
	isAudioLevelSpeaking?: boolean | null;
}

export type VoiceEngineV2AppVoiceMuteReason = 'guild' | 'permission' | 'voice_push_to_talk' | 'self' | null;

export interface VoiceEngineV2AppMuteReasonVoiceState {
	mute?: boolean | null;
}

export interface VoiceEngineV2AppMuteReasonInput {
	voiceState: VoiceEngineV2AppMuteReasonVoiceState | null;
	permissionMuted: boolean;
	audio: VoiceEngineV2AudioControls;
}

export interface VoiceEngineV2AppViewProjection {
	connection: VoiceEngineV2AppConnectionProjection;
	localMedia: VoiceEngineV2AppLocalMediaProjection;
	participants: VoiceEngineV2AppParticipantProjection;
	tracks: VoiceEngineV2AppTrackProjection;
	watchedStreams: VoiceEngineV2AppWatchedStreamProjection;
	stats: VoiceEngineV2AppStatsProjection;
	devices: VoiceEngineV2AppDeviceProjection;
	e2ee: VoiceEngineV2AppE2eeProjection;
}

export function selectVoiceEngineV2AppView(source: VoiceEngineV2AppProjectionSource): VoiceEngineV2AppViewProjection {
	assertNonNullObject(source, 'source');
	const view = {
		connection: selectVoiceEngineV2AppConnection(source),
		localMedia: selectVoiceEngineV2AppLocalMedia(source),
		participants: selectVoiceEngineV2AppParticipants(source),
		tracks: selectVoiceEngineV2AppTracks(source),
		watchedStreams: selectVoiceEngineV2AppWatchedStreams(source),
		stats: selectVoiceEngineV2AppStats(source),
		devices: selectVoiceEngineV2AppDevices(source),
		e2ee: selectVoiceEngineV2AppE2ee(source),
	};
	assertNonNullObject(view.connection, 'view.connection');
	assertNonNullObject(view.localMedia, 'view.localMedia');
	return view;
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

export function selectVoiceEngineV2AppLocalMedia(
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

export function selectVoiceEngineV2AppIntentSelfMuteFromAudioControls(audio: VoiceEngineV2AudioControls): boolean {
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

export function selectVoiceEngineV2AppTracks(
	source: VoiceEngineV2AppProjectionSource,
): VoiceEngineV2AppTrackProjection {
	assertNonNullObject(source, 'source');
	const model = voiceEngineV2AppModel(source);
	const tracks = sortVoiceEngineV2Tracks([...model.tracks]);
	const inboundVideoTracks = sortVoiceEngineV2InboundVideoTracks([...model.inboundVideoTracks]);
	assert.ok(Array.isArray(tracks), 'tracks must be array');
	return {
		tracks,
		audioTracks: tracks.filter((track) => track.kind === 'audio'),
		videoTracks: tracks.filter((track) => track.kind === 'video'),
		microphoneTracks: tracks.filter((track) => track.source === 'microphone'),
		cameraTracks: tracks.filter((track) => track.source === 'camera'),
		screenTracks: tracks.filter((track) => track.source === 'screen'),
		screenAudioTracks: tracks.filter((track) => track.source === 'screenAudio'),
		inboundVideoTracks,
	};
}

export function selectVoiceEngineV2AppTracksForParticipant(
	source: VoiceEngineV2AppProjectionSource,
	participantIdentity: string,
): Array<VoiceEngineV2Track> {
	assertNonNullObject(source, 'source');
	assertString(participantIdentity, 'participantIdentity');
	return selectVoiceEngineV2AppTracks(source).tracks.filter(
		(track) => track.participantIdentity === participantIdentity,
	);
}

export function selectVoiceEngineV2AppTrackForSource(
	source: VoiceEngineV2AppProjectionSource,
	participantIdentity: string,
	trackSource: VoiceEngineV2TrackSource | string,
): VoiceEngineV2Track | null {
	assertNonNullObject(source, 'source');
	assertString(participantIdentity, 'participantIdentity');
	return (
		selectVoiceEngineV2AppTracks(source).tracks.find(
			(track) => track.participantIdentity === participantIdentity && track.source === trackSource,
		) ?? null
	);
}

export function selectVoiceEngineV2AppInboundVideoTrack(
	source: VoiceEngineV2AppProjectionSource,
	trackSid: string,
): VoiceEngineV2InboundVideoTrack | null {
	assertNonNullObject(source, 'source');
	assertString(trackSid, 'trackSid');
	return selectVoiceEngineV2AppTracks(source).inboundVideoTracks.find((track) => track.trackSid === trackSid) ?? null;
}

export function selectVoiceEngineV2AppWatchedStreams(
	source: VoiceEngineV2AppProjectionSource,
): VoiceEngineV2AppWatchedStreamProjection {
	assertNonNullObject(source, 'source');
	const streams = sortVoiceEngineV2WatchedStreams([...voiceEngineV2AppModel(source).watchedStreams]);
	assert.ok(Array.isArray(streams), 'streams must be array');
	return {
		streams,
		enabledStreams: streams.filter((stream) => stream.enabled),
		disabledStreams: streams.filter((stream) => !stream.enabled),
	};
}

export function selectVoiceEngineV2AppWatchedStream(
	source: VoiceEngineV2AppProjectionSource,
	participantIdentity: string,
	trackSource: VoiceEngineV2TrackSource | string,
): VoiceEngineV2WatchedStream | null {
	assertNonNullObject(source, 'source');
	assertString(participantIdentity, 'participantIdentity');
	return (
		selectVoiceEngineV2AppWatchedStreams(source).streams.find(
			(stream) => stream.participantIdentity === participantIdentity && stream.source === trackSource,
		) ?? null
	);
}

export function selectVoiceEngineV2AppStats(source: VoiceEngineV2AppProjectionSource): VoiceEngineV2AppStatsProjection {
	assertNonNullObject(source, 'source');
	const model = voiceEngineV2AppModel(source);
	const snapshot = voiceEngineV2AppSnapshot(source);
	const stats = model.stats;
	return {
		stats,
		summary: stats ? summarizeVoiceEngineV2Stats(stats) : null,
		hasStats: stats !== null,
		rttMs: stats?.rttMs ?? null,
		outboundTrackCount: stats?.outbound.length ?? 0,
		inboundTrackCount: stats?.inbound.length ?? 0,
		droppedNativeVideoFrames: stats?.droppedNativeVideoFrames ?? 0,
		droppedVideoFrameCallbacks: stats?.droppedVideoFrameCallbacks ?? 0,
		failureCode: snapshot?.statsFailure?.code ?? null,
	};
}

export function selectVoiceEngineV2AppDevices(
	source: VoiceEngineV2AppProjectionSource,
): VoiceEngineV2AppDeviceProjection {
	assertNonNullObject(source, 'source');
	const model = voiceEngineV2AppModel(source);
	assertNonNullObject(model.devices, 'model.devices');
	const selectedAudioInput =
		model.devices.audioInputs.find((device) => device.deviceId === model.devices.selectedAudioInputId) ?? null;
	const selectedAudioOutput =
		model.devices.audioOutputs.find((device) => device.deviceId === model.devices.selectedAudioOutputId) ?? null;
	const selectedCamera =
		model.devices.cameras.find((device) => device.deviceId === model.devices.selectedCameraId) ?? null;
	return {
		devices: model.devices,
		permissions: model.permissions,
		selectedAudioInput,
		selectedAudioOutput,
		selectedCamera,
		hasAudioInput: model.devices.audioInputs.length > 0,
		hasAudioOutput: model.devices.audioOutputs.length > 0,
		hasCamera: model.devices.cameras.length > 0,
		microphonePermission: model.permissions.microphone ?? null,
		cameraPermission: model.permissions.camera ?? null,
		screenPermission: model.permissions.screen ?? null,
	};
}

export function selectVoiceEngineV2AppE2ee(source: VoiceEngineV2AppProjectionSource): VoiceEngineV2AppE2eeProjection {
	assertNonNullObject(source, 'source');
	const e2ee = voiceEngineV2AppModel(source).media.e2ee;
	assertNonNullObject(e2ee, 'e2ee');
	return {
		status: e2ee.status,
		keyId: e2ee.keyId,
		failure: e2ee.failure,
		enabled: e2ee.status === 'enabled',
		pending: e2ee.status === 'pendingKey',
		failed: e2ee.status === 'failed',
	};
}

function voiceEngineV2AppModel(source: VoiceEngineV2AppProjectionSource): VoiceEngineV2Model {
	return isVoiceEngineV2Snapshot(source) ? selectVoiceEngineV2Model(source) : source;
}

function voiceEngineV2AppSnapshot(source: VoiceEngineV2AppProjectionSource): VoiceEngineV2Snapshot | null {
	return isVoiceEngineV2Snapshot(source) ? source : null;
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

function sortVoiceEngineV2Tracks(tracks: Array<VoiceEngineV2Track>): Array<VoiceEngineV2Track> {
	return tracks.sort(
		(left, right) =>
			compareStrings(left.participantIdentity, right.participantIdentity) ||
			compareStrings(left.source, right.source) ||
			compareStrings(left.trackSid, right.trackSid),
	);
}

function sortVoiceEngineV2InboundVideoTracks(
	tracks: Array<VoiceEngineV2InboundVideoTrack>,
): Array<VoiceEngineV2InboundVideoTrack> {
	return tracks.sort(
		(left, right) =>
			compareStrings(left.participantIdentity ?? '', right.participantIdentity ?? '') ||
			compareStrings(left.source, right.source) ||
			compareStrings(left.trackSid, right.trackSid),
	);
}

function sortVoiceEngineV2WatchedStreams(
	streams: Array<VoiceEngineV2WatchedStream>,
): Array<VoiceEngineV2WatchedStream> {
	return streams.sort(
		(left, right) =>
			compareStrings(left.participantIdentity, right.participantIdentity) || compareStrings(left.source, right.source),
	);
}

function compareStrings(left: string, right: string): number {
	if (left < right) return -1;
	if (left > right) return 1;
	return 0;
}
