// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {VoiceSignalSendResult} from '@app/features/gateway/transport/GatewaySocket';

export const VOICE_MESH_TOKEN = 'p2p';
export const VOICE_MESH_ENDPOINT = 'wss://p2p.invalid';

export const VOICE_MESH_SOURCES = ['microphone', 'camera', 'screen_share', 'screen_share_audio'] as const;

export type VoiceMeshSource = (typeof VOICE_MESH_SOURCES)[number];

export type VoiceMeshTrackInfo = {
	src: VoiceMeshSource;
	sid: string;
	m: boolean;
	w?: number;
	h?: number;
};

export type VoiceMeshSignalData =
	| {t: 'offer'; g: number; sdp: string}
	| {t: 'answer'; g: number; sdp: string}
	| {t: 'ice'; g: number; c: RTCIceCandidateInit | null}
	| {t: 'need-offer'; g: number; restart?: true}
	| {t: 'tracks'; v: number; tracks: Array<VoiceMeshTrackInfo>}
	| {t: 'want'; v: number; sids: Array<string>};

export interface VoiceMeshSignalTarget {
	guildId: string | null;
	channelId: string;
	to: string;
}

export interface VoiceMeshIncomingSignal {
	guildId: string | null;
	channelId: string;
	from: string;
	userId: string;
	data: VoiceMeshSignalData;
}

type VoiceMeshSignalListener = (signal: VoiceMeshIncomingSignal) => void;

const listeners = new Set<VoiceMeshSignalListener>();

export function voiceMeshTrackSid(connectionId: string, src: VoiceMeshSource): string {
	return `TR_${connectionId}_${src}`;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isCounter(value: unknown): value is number {
	return typeof value === 'number' && Number.isSafeInteger(value) && value >= 0;
}

function isOptionalDimension(value: unknown): value is number | undefined {
	return value === undefined || (typeof value === 'number' && Number.isFinite(value) && value >= 0);
}

function isVoiceMeshSource(value: unknown): value is VoiceMeshSource {
	return VOICE_MESH_SOURCES.includes(value as VoiceMeshSource);
}

function isOptionalString(value: unknown): value is string | null | undefined {
	return value === undefined || value === null || typeof value === 'string';
}

function isOptionalIndex(value: unknown): value is number | null | undefined {
	return value === undefined || value === null || isCounter(value);
}

function parseCandidate(value: unknown): RTCIceCandidateInit | null | undefined {
	if (value === null) return null;
	if (!isRecord(value) || typeof value.candidate !== 'string') return undefined;
	const {candidate, sdpMid, sdpMLineIndex, usernameFragment} = value;
	if (!isOptionalString(sdpMid) || !isOptionalString(usernameFragment) || !isOptionalIndex(sdpMLineIndex)) {
		return undefined;
	}
	return {candidate, sdpMid, sdpMLineIndex, usernameFragment};
}

function parseTrack(value: unknown): VoiceMeshTrackInfo | null {
	if (!isRecord(value)) return null;
	const {src, sid, m, w, h} = value;
	if (!isVoiceMeshSource(src) || typeof sid !== 'string' || typeof m !== 'boolean') return null;
	if (!isOptionalDimension(w) || !isOptionalDimension(h)) return null;
	return {src, sid, m, ...(w !== undefined && {w}), ...(h !== undefined && {h})};
}

export function parseVoiceMeshSignalData(value: unknown): VoiceMeshSignalData | null {
	if (!isRecord(value)) return null;
	switch (value.t) {
		case 'offer':
		case 'answer':
			if (!isCounter(value.g) || typeof value.sdp !== 'string') return null;
			return {t: value.t, g: value.g, sdp: value.sdp};
		case 'ice': {
			if (!isCounter(value.g)) return null;
			const c = parseCandidate(value.c);
			if (c === undefined) return null;
			return {t: 'ice', g: value.g, c};
		}
		case 'need-offer':
			if (!isCounter(value.g)) return null;
			if (value.restart !== undefined && value.restart !== true) return null;
			return {t: 'need-offer', g: value.g, ...(value.restart === true && {restart: true})};
		case 'tracks': {
			if (!isCounter(value.v) || !Array.isArray(value.tracks)) return null;
			const tracks: Array<VoiceMeshTrackInfo> = [];
			for (const entry of value.tracks) {
				const track = parseTrack(entry);
				if (!track) return null;
				tracks.push(track);
			}
			return {t: 'tracks', v: value.v, tracks};
		}
		case 'want': {
			if (!isCounter(value.v) || !Array.isArray(value.sids)) return null;
			const sids: Array<unknown> = value.sids;
			if (!sids.every((sid): sid is string => typeof sid === 'string')) return null;
			return {t: 'want', v: value.v, sids};
		}
		default:
			return null;
	}
}

export function sendVoiceMeshSignal(target: VoiceMeshSignalTarget, data: VoiceMeshSignalData): VoiceSignalSendResult {
	const socket = GatewayConnection.socket;
	if (!socket) return VoiceSignalSendResult.Unsent;
	return socket.sendVoiceSignal({guild_id: target.guildId, channel_id: target.channelId, to: target.to, data});
}

export function subscribeVoiceMeshSignals(listener: VoiceMeshSignalListener): () => void {
	listeners.add(listener);
	return () => {
		listeners.delete(listener);
	};
}

export function dispatchVoiceMeshSignal(signal: VoiceMeshIncomingSignal): void {
	for (const listener of listeners) {
		listener(signal);
	}
}
