// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {parseStreamKey} from '@app/features/voice/components/StreamKeys';
import type {VoiceStateSyncPartial} from '@app/features/voice/engine/VoiceStateSyncTypes';
import {
	buildVoiceParticipantIdentity,
	parseVoiceParticipantIdentity,
} from '@app/features/voice/utils/VoiceParticipantIdentity';
import type {
	VoiceEngineV2Controller,
	VoiceEngineV2Model,
	VoiceEngineV2Participant,
	VoiceEngineV2ScreenEncodingOptions,
	VoiceEngineV2Snapshot,
	VoiceEngineV2WatchedStream,
} from '@fluxer/voice_engine_v2';
import type {Participant, Room} from 'livekit-client';

const logger = new Logger('VoiceMediaEngineBridge');

interface MediaEngineAccess {
	channelId?: string | null;
	connectionId?: string | null;
	connected?: boolean;
	connecting?: boolean;
	guildId?: string | null;
	reconnecting?: boolean;
	room?: Room;
	voiceEngineV2Controller?: VoiceEngineV2Controller;
	voiceEngineV2Model?: VoiceEngineV2Model;
	voiceEngineV2Snapshot?: VoiceEngineV2Snapshot;
	getAllVoiceStatesInChannel?: (
		guildId: string,
		channelId: string,
	) => Readonly<Record<string, VoiceMediaEngineVoiceState>>;
	getVoiceStateByConnectionId?: (connectionId: string) => VoiceMediaEngineVoiceState | null;
	syncLocalVoiceStateWithServer?: (partial: VoiceStateSyncPartial) => void;
	upsertParticipant?: (participant: Participant) => void;
}

export interface VoiceMediaEngineConnectionContext {
	guildId: string | null;
	channelId: string | null;
	connectionId: string | null;
	connected: boolean;
	connecting: boolean;
	disconnecting: boolean;
	reconnecting: boolean;
}

export interface VoiceMediaEngineVoiceState {
	guild_id?: string | null;
	channel_id?: string | null;
	connection_id?: string | null;
	user_id: string;
	session_id?: string | null;
	self_mute: boolean;
	self_deaf: boolean;
	self_video: boolean;
	self_stream: boolean;
	mute?: boolean;
	deaf?: boolean;
	suppress?: boolean;
	viewer_stream_keys?: ReadonlyArray<string>;
}

type VoiceEngineV2ParticipantWithAppPresence = VoiceEngineV2Participant & {
	userId?: string | null;
	connectionId?: string | null;
	isLocal?: boolean;
	isSpeaking?: boolean;
	isAudioLevelSpeaking?: boolean;
	isMicrophoneEnabled?: boolean;
	isCameraEnabled?: boolean;
	isScreenShareEnabled?: boolean;
	isScreenShareAudioEnabled?: boolean;
	lastSpokeAt?: number | null;
};

function getMediaEngine(): MediaEngineAccess | null {
	try {
		if (!('_mediaEngine' in window)) return null;
		return (
			(
				window as {
					_mediaEngine?: MediaEngineAccess;
				}
			)._mediaEngine ?? null
		);
	} catch (error) {
		logger.error('Failed to access media engine store', error);
		return null;
	}
}

export function getVoiceConnectionContextFromMediaEngine(): VoiceMediaEngineConnectionContext | null {
	const store = getMediaEngine();
	if (!store) return null;
	const modelConnection = store.voiceEngineV2Model?.connection;
	const modelVoiceState = modelConnection?.gateway.selfVoiceState;
	return {
		guildId: modelVoiceState?.guildId ?? store.guildId ?? null,
		channelId: modelVoiceState?.channelId ?? store.channelId ?? null,
		connectionId: store.connectionId ?? null,
		connected: store.connected ?? modelConnection?.connected ?? false,
		connecting: store.connecting ?? modelConnection?.connecting ?? false,
		disconnecting: modelConnection?.status === 'disconnecting',
		reconnecting: store.reconnecting ?? modelConnection?.reconnecting ?? false,
	};
}

export function getAllVoiceStatesInChannelFromMediaEngine(
	guildId: string,
	channelId: string,
): Readonly<Record<string, VoiceMediaEngineVoiceState>> {
	return getMediaEngine()?.getAllVoiceStatesInChannel?.(guildId, channelId) ?? {};
}

export function getVoiceStateByConnectionIdFromMediaEngine(
	connectionId: string | null | undefined,
): VoiceMediaEngineVoiceState | null {
	if (!connectionId) return null;
	return getMediaEngine()?.getVoiceStateByConnectionId?.(connectionId) ?? null;
}

export function getVoiceEngineV2SnapshotFromMediaEngine(): VoiceEngineV2Snapshot | null {
	return getMediaEngine()?.voiceEngineV2Snapshot ?? null;
}

export interface VoiceEngineV2ScreenEncodingUpdateFromMediaEngine {
	captureId?: string;
	width: number;
	height: number;
	frameRate?: number;
	maxBitrateBps?: number;
}

function getVoiceEngineV2ActiveScreenCaptureId(controller: VoiceEngineV2Controller): string | null {
	return controller.snapshot.screen.desired?.captureId ?? controller.snapshot.screen.published?.captureId ?? null;
}

export function updateVoiceEngineV2ScreenEncodingFromMediaEngine(
	options: VoiceEngineV2ScreenEncodingUpdateFromMediaEngine,
): boolean {
	const controller = getMediaEngine()?.voiceEngineV2Controller;
	if (!controller) return false;
	const captureId = options.captureId ?? getVoiceEngineV2ActiveScreenCaptureId(controller);
	if (!captureId) return false;
	const update: VoiceEngineV2ScreenEncodingOptions = {
		captureId,
		width: options.width,
		height: options.height,
		frameRate: options.frameRate,
		maxBitrateBps: options.maxBitrateBps,
	};
	controller.updateScreenEncoding(update);
	return true;
}

export function setVoiceEngineV2ParticipantAudioLevelSpeaking(
	identity: string,
	speaking: boolean,
	nowMs: number = Date.now(),
): boolean {
	const store = getMediaEngine();
	const controller = store?.voiceEngineV2Controller;
	const participant = store?.voiceEngineV2Model?.participants.find((entry) => entry.identity === identity) as
		| VoiceEngineV2ParticipantWithAppPresence
		| undefined;
	if (!controller || !participant) return false;
	const nextParticipant = {
		...participant,
		isAudioLevelSpeaking: speaking,
		lastSpokeAt: speaking ? nowMs : (participant.lastSpokeAt ?? null),
	};
	controller.dispatch({type: 'room.participantJoined', participant: nextParticipant});
	return true;
}

export function syncLocalVoiceStateWithServer(partial: VoiceStateSyncPartial): void {
	try {
		const store = getMediaEngine();
		store?.syncLocalVoiceStateWithServer?.(partial);
	} catch (error) {
		logger.error('Failed to sync voice state with server', error);
	}
}

function resolveParticipantIdentityForStreamKey(store: MediaEngineAccess, connectionId: string): string | null {
	for (const participant of store.voiceEngineV2Model?.participants ?? []) {
		const parsed = parseVoiceParticipantIdentity(participant.identity);
		if (parsed.connectionId === connectionId) return participant.identity;
	}
	const voiceState = store.getVoiceStateByConnectionId?.(connectionId);
	if (voiceState?.user_id) return buildVoiceParticipantIdentity(voiceState.user_id, connectionId);
	return null;
}

function streamKeysToWatchedStreams(
	store: MediaEngineAccess,
	keys: ReadonlyArray<string>,
): {streams: Array<VoiceEngineV2WatchedStream>; unresolvedCount: number} {
	const streams: Array<VoiceEngineV2WatchedStream> = [];
	const seen = new Set<string>();
	let unresolvedCount = 0;
	for (const key of keys) {
		const parsed = parseStreamKey(key);
		if (!parsed?.connectionId) continue;
		const participantIdentity = resolveParticipantIdentityForStreamKey(store, parsed.connectionId);
		if (!participantIdentity) {
			unresolvedCount += 1;
			continue;
		}
		const streamKey = `${participantIdentity}:screen`;
		if (seen.has(streamKey)) continue;
		seen.add(streamKey);
		streams.push({
			participantIdentity,
			source: 'screen',
			trackSid: null,
			quality: 'high',
			enabled: true,
		});
	}
	return {streams, unresolvedCount};
}

export function syncVoiceEngineV2WatchedStreamKeys(keys: ReadonlyArray<string>): void {
	try {
		const store = getMediaEngine();
		const controller = store?.voiceEngineV2Controller;
		if (!store || !controller) return;
		const {streams, unresolvedCount} = streamKeysToWatchedStreams(store, keys);
		if (unresolvedCount > 0) return;
		controller.replaceWatchedStreams(streams);
	} catch (error) {
		logger.error('Failed to sync watched stream keys with voice engine v2', error);
	}
}

export function getRoomFromMediaEngine(): Room | null {
	try {
		const store = getMediaEngine();
		return store?.room ?? null;
	} catch (error) {
		logger.error('Failed to get room from media engine store', error);
		return null;
	}
}

export function updateLocalParticipantFromRoom(roomOverride?: Room | null): void {
	try {
		const store = getMediaEngine();
		if (!store?.upsertParticipant) return;
		const room = roomOverride ?? store.room ?? null;
		if (!room?.localParticipant) return;
		store.upsertParticipant(room.localParticipant);
	} catch (error) {
		logger.error('Failed to update local participant', error);
	}
}
