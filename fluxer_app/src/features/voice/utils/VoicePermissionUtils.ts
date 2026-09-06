// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import Permission from '@app/features/permissions/state/Permission';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';

export interface VoiceChannelPermissions {
	canSpeak: boolean;
	canStream: boolean;
	canUseVideo: boolean;
	canConnect: boolean;
	canPrioritySpeaker: boolean;
	canUseVad: boolean;
}

export const DEFAULT_VOICE_CHANNEL_PERMISSIONS: VoiceChannelPermissions = {
	canSpeak: true,
	canStream: true,
	canUseVideo: true,
	canConnect: true,
	canPrioritySpeaker: false,
	canUseVad: true,
};

export type VoicePermissionMuteReason = 'server_suppress' | 'missing_speak_permission' | null;

export interface VoicePermissionMuteState {
	muted: boolean;
	reason: VoicePermissionMuteReason;
}

export function getVoiceChannelPermissions(channelId: string | null | undefined): VoiceChannelPermissions | null {
	if (!channelId) return null;
	const permissions = Permission.getChannelPermissions(channelId);
	if (permissions === undefined) return null;
	return {
		canSpeak: (permissions & Permissions.SPEAK) === Permissions.SPEAK,
		canStream: (permissions & Permissions.STREAM) === Permissions.STREAM,
		canUseVideo: (permissions & Permissions.STREAM) === Permissions.STREAM,
		canConnect: (permissions & Permissions.CONNECT) === Permissions.CONNECT,
		canPrioritySpeaker: (permissions & Permissions.PRIORITY_SPEAKER) === Permissions.PRIORITY_SPEAKER,
		canUseVad: (permissions & Permissions.USE_VAD) === Permissions.USE_VAD,
	};
}

export function isVoiceSpeakPermissionDenied(
	guildId: string | null | undefined,
	channelId: string | null | undefined,
): boolean {
	if (!channelId) return false;
	const channel = Channels.getChannel(channelId);
	const effectiveGuildId = guildId ?? channel?.guildId ?? null;
	if (!effectiveGuildId || channel?.guildId !== effectiveGuildId) return false;
	const voicePermissions = getVoiceChannelPermissions(channelId);
	return voicePermissions !== null && !voicePermissions.canSpeak;
}

export function getVoicePermissionMuteState(
	voiceState: Pick<VoiceState, 'suppress'> | null | undefined,
	guildId: string | null | undefined,
	channelId: string | null | undefined,
): VoicePermissionMuteState {
	if (voiceState?.suppress ?? false) return {muted: true, reason: 'server_suppress'};
	if (isVoiceSpeakPermissionDenied(guildId, channelId)) return {muted: true, reason: 'missing_speak_permission'};
	return {muted: false, reason: null};
}

export function isVoicePermissionMuteActive(
	voiceState: Pick<VoiceState, 'suppress'> | null | undefined,
	guildId: string | null | undefined,
	channelId: string | null | undefined,
): boolean {
	return getVoicePermissionMuteState(voiceState, guildId, channelId).muted;
}

export function isVoiceServerMuteActive(
	voiceState: Pick<VoiceState, 'mute' | 'suppress'> | null | undefined,
	guildId: string | null | undefined,
	channelId: string | null | undefined,
): boolean {
	return (voiceState?.mute ?? false) || isVoicePermissionMuteActive(voiceState, guildId, channelId);
}

export function isParticipantVoicePermissionMuted({
	voiceState,
	guildId,
	channelId,
	isCurrentUser,
}: {
	voiceState: Pick<VoiceState, 'suppress'> | null | undefined;
	guildId: string | null | undefined;
	channelId: string | null | undefined;
	isCurrentUser: boolean;
}): boolean {
	return (voiceState?.suppress ?? false) || (isCurrentUser && isVoiceSpeakPermissionDenied(guildId, channelId));
}

export function applyVoiceSpeakPermissionToSelfMute(
	guildId: string | null | undefined,
	channelId: string | null | undefined,
	selfMute: boolean,
): boolean {
	return isVoiceSpeakPermissionDenied(guildId, channelId) || selfMute;
}

export function shouldPrepareMicrophoneForVoiceConnect({
	guildId,
	channelId,
	selfMute,
	selfDeaf,
	hasUserSetMute,
	mutedByPermission,
}: {
	guildId: string | null | undefined;
	channelId: string | null | undefined;
	selfMute: boolean;
	selfDeaf: boolean;
	hasUserSetMute?: boolean;
	mutedByPermission?: boolean;
}): boolean {
	const canRequestForPermissionMute = mutedByPermission === true && hasUserSetMute !== true;
	return (!selfMute || canRequestForPermissionMute) && !selfDeaf && !isVoiceSpeakPermissionDenied(guildId, channelId);
}

export function resolveVoiceStateSelfMute({
	guildId,
	channelId,
	microphoneGranted,
	requestedSelfMute,
	effectiveSelfMute,
}: {
	guildId: string | null | undefined;
	channelId: string | null | undefined;
	microphoneGranted: boolean;
	requestedSelfMute?: boolean;
	effectiveSelfMute: boolean;
}): boolean {
	if (isVoiceSpeakPermissionDenied(guildId, channelId)) return true;
	if (!microphoneGranted) return true;
	return requestedSelfMute ?? effectiveSelfMute;
}
