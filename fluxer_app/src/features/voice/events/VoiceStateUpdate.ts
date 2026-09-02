// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {SoundType} from '@app/features/notification/utils/SoundUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as SoundCommands from '@app/features/ui/commands/SoundCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {
	discardVoiceJoinChimeSequence,
	playSelfJoinChimeOnce,
	startVoiceJoinChimeSequence,
} from '@app/features/voice/engine/VoiceSelfJoinChime';
import VoiceRegionTeleport from '@app/features/voice/state/VoiceRegionTeleport';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';

const logger = new Logger('VoiceStateUpdate');

export const JOIN_CHIME_DUPLICATE_WINDOW_MS = 2000;
const RECENT_JOIN_CHIME_MAX_ENTRIES = 64;

interface VoiceStateUpdatePayload {
	user_id: string;
	channel_id?: string | null;
	guild_id?: string | null;
	session_id?: string;
	connection_id?: string;
	self_mute?: boolean;
	self_deaf?: boolean;
	self_video?: boolean;
	self_stream?: boolean;
	mute?: boolean;
	deaf?: boolean;
	suppress?: boolean;
}

interface JoinChimePayloadSummary {
	channel_id: string | null;
	connection_id: string | null;
	session_id: string | null;
}

interface RecentJoinChime {
	playedAt: number;
	summary: JoinChimePayloadSummary;
}

const recentJoinChimesByConnectionId = new Map<string, RecentJoinChime>();

export function resetRecentJoinChimesForTests(): void {
	recentJoinChimesByConnectionId.clear();
}

function summarizeJoinChimePayload(data: VoiceStateUpdatePayload): JoinChimePayloadSummary {
	return {
		channel_id: data.channel_id ?? null,
		connection_id: data.connection_id ?? null,
		session_id: data.session_id ?? null,
	};
}

function pruneRecentJoinChimes(now: number): void {
	for (const [connectionId, entry] of recentJoinChimesByConnectionId) {
		if (now - entry.playedAt >= JOIN_CHIME_DUPLICATE_WINDOW_MS) {
			recentJoinChimesByConnectionId.delete(connectionId);
		}
	}
	while (recentJoinChimesByConnectionId.size >= RECENT_JOIN_CHIME_MAX_ENTRIES) {
		const oldestKey = recentJoinChimesByConnectionId.keys().next().value;
		if (oldestKey === undefined) break;
		recentJoinChimesByConnectionId.delete(oldestKey);
	}
}

function shouldSuppressDuplicateJoinChime(data: VoiceStateUpdatePayload, now: number): boolean {
	if (!data.connection_id) return false;
	const recent = recentJoinChimesByConnectionId.get(data.connection_id);
	if (!recent || now - recent.playedAt >= JOIN_CHIME_DUPLICATE_WINDOW_MS) return false;
	logger.warn('Suppressed duplicate gateway join chime within dedupe window', {
		connectionId: data.connection_id,
		windowMs: JOIN_CHIME_DUPLICATE_WINDOW_MS,
		previousPayload: JSON.stringify(recent.summary),
		incomingPayload: JSON.stringify(summarizeJoinChimePayload(data)),
	});
	return true;
}

function rememberJoinChime(data: VoiceStateUpdatePayload, now: number): void {
	if (!data.connection_id) return;
	pruneRecentJoinChimes(now);
	recentJoinChimesByConnectionId.set(data.connection_id, {
		playedAt: now,
		summary: summarizeJoinChimePayload(data),
	});
}

function shouldPlayJoinChime(data: VoiceStateUpdatePayload): boolean {
	if (!MediaEngine.connected) return false;
	if (!data.channel_id) return false;
	if (!data.connection_id) return false;
	if (MediaEngine.isVoiceConnectionIgnored(data.connection_id)) return false;
	if (data.channel_id !== MediaEngine.channelId) return false;
	const previousState = MediaEngine.getVoiceStateByConnectionId(data.connection_id);
	if (previousState?.channel_id === data.channel_id) return false;
	return true;
}

function shouldBypassSelfDeafenedForJoinChime(data: VoiceStateUpdatePayload): boolean {
	return data.connection_id === MediaEngine.connectionId;
}

function shouldPlayLeaveChime(data: VoiceStateUpdatePayload): boolean {
	if (!MediaEngine.connected) return false;
	if (!MediaEngine.channelId) return false;
	if (!data.connection_id) return false;
	if (MediaEngine.isVoiceConnectionIgnored(data.connection_id)) return false;
	const previousState = MediaEngine.getVoiceStateByConnectionId(data.connection_id);
	if (previousState?.channel_id !== MediaEngine.channelId) return false;
	if (data.channel_id === previousState.channel_id) return false;
	return true;
}

export function handleVoiceStateUpdate(data: VoiceStateUpdatePayload, _context: GatewayHandlerContext): void {
	const guildId = data.guild_id ?? null;
	const voiceState = data as VoiceState;
	if (guildId && voiceState.member) {
		GuildMembers.hydrateIfMissing(guildId, voiceState.member as GuildMemberData);
	}
	const now = Date.now();
	const teleportingInPlace = VoiceRegionTeleport.shouldSuppressRejoinSounds();
	const playJoinChime =
		!teleportingInPlace && shouldPlayJoinChime(data) && !shouldSuppressDuplicateJoinChime(data, now);
	const playLeaveChime = !teleportingInPlace && !playJoinChime && shouldPlayLeaveChime(data);
	const previousState = data.connection_id ? MediaEngine.getVoiceStateByConnectionId(data.connection_id) : null;
	if (previousState && previousState.channel_id !== data.channel_id) {
		const otherConnectionRemains = Object.values(
			MediaEngine.getAllVoiceStatesInChannel(previousState.guild_id, previousState.channel_id),
		).some((state) => state.user_id === data.user_id && state.connection_id !== data.connection_id);
		if (!otherConnectionRemains) {
			discardVoiceJoinChimeSequence({userId: data.user_id, channelId: previousState.channel_id});
		}
	}
	MediaEngine.handleGatewayVoiceStateUpdate(guildId, voiceState);
	if (playJoinChime) {
		if (!data.channel_id) return;
		rememberJoinChime(data, now);
		void startVoiceJoinChimeSequence(
			{userId: data.user_id, channelId: data.channel_id},
			data.connection_id ?? null,
			(signal) =>
				shouldBypassSelfDeafenedForJoinChime(data)
					? playSelfJoinChimeOnce(data.connection_id, 'gateway', signal)
					: SoundCommands.playOneShotSoundImmediatelyBypassingSelfDeafened(SoundType.UserJoin, signal),
		);
	} else if (playLeaveChime) {
		if (data.connection_id) {
			recentJoinChimesByConnectionId.delete(data.connection_id);
		}
		SoundCommands.playSoundBypassingSelfDeafened(SoundType.UserLeave);
	}
}
