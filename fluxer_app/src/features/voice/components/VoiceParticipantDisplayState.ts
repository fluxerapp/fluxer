// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {
	isVoiceEngineV2AppParticipantSpeaking,
	type VoiceEngineV2AppParticipantSpeakingSnapshot,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';

interface VoiceParticipantDisplaySnapshot extends VoiceEngineV2AppParticipantSpeakingSnapshot {
	isMicrophoneEnabled?: boolean | null;
	isCameraEnabled?: boolean | null;
	isScreenShareEnabled?: boolean | null;
}

export interface ResolveVoiceParticipantSpeakingArgs {
	participant: VoiceParticipantDisplaySnapshot | null | undefined;
	voiceState: VoiceState | null;
	isLocalConnection: boolean;
	localSelfMute: boolean;
	permissionMuted: boolean;
}

export interface ResolveVoiceParticipantDisplayStateArgs extends ResolveVoiceParticipantSpeakingArgs {
	localSelfDeaf?: boolean;
	localSelfVideo?: boolean;
	localSelfStream?: boolean;
}

export interface VoiceParticipantDisplayState {
	speaking: boolean;
	selfMute: boolean;
	selfDeaf: boolean;
	guildMute: boolean;
	guildDeaf: boolean;
	cameraOn: boolean;
	streaming: boolean;
}

function resolveSelfMute({
	participant,
	voiceState,
	isLocalConnection,
	localSelfMute,
}: Pick<
	ResolveVoiceParticipantSpeakingArgs,
	'participant' | 'voiceState' | 'isLocalConnection' | 'localSelfMute'
>): boolean {
	if (isLocalConnection) return localSelfMute;
	return voiceState?.self_mute ?? !(participant?.isMicrophoneEnabled ?? true);
}

export function resolveVoiceParticipantSpeaking(args: ResolveVoiceParticipantSpeakingArgs): boolean {
	if (!isVoiceEngineV2AppParticipantSpeaking(args.participant)) return false;
	if (resolveSelfMute(args)) return false;
	if (args.permissionMuted) return false;
	if (args.voiceState?.mute ?? false) return false;
	return true;
}

export function resolveVoiceParticipantDisplayState({
	participant,
	voiceState,
	isLocalConnection,
	localSelfMute,
	localSelfDeaf = false,
	localSelfVideo = false,
	localSelfStream = false,
	permissionMuted,
}: ResolveVoiceParticipantDisplayStateArgs): VoiceParticipantDisplayState {
	const speaking = resolveVoiceParticipantSpeaking({
		participant,
		voiceState,
		isLocalConnection,
		localSelfMute,
		permissionMuted,
	});
	const selfMute = resolveSelfMute({participant, voiceState, isLocalConnection, localSelfMute}) || permissionMuted;
	const selfDeaf = isLocalConnection ? localSelfDeaf : (voiceState?.self_deaf ?? false);
	const remoteCameraOn = voiceState?.self_video ?? participant?.isCameraEnabled ?? false;
	const remoteStreaming = voiceState?.self_stream ?? participant?.isScreenShareEnabled ?? false;
	return {
		speaking,
		selfMute,
		selfDeaf,
		guildMute: voiceState?.mute ?? false,
		guildDeaf: voiceState?.deaf ?? false,
		cameraOn: remoteCameraOn || (isLocalConnection && localSelfVideo),
		streaming: remoteStreaming || (isLocalConnection && localSelfStream),
	};
}

export interface VoiceParticipantAvatarEntryVoiceState {
	speaking: boolean;
	selfMute: boolean;
	selfDeaf: boolean;
}

export interface ResolveVoiceParticipantAvatarEntryVoiceStateArgs {
	snapshot: VoiceParticipantDisplaySnapshot & {isLocal: boolean};
	voiceState: VoiceState | null;
	permissionMuted: boolean;
	localEffectiveSelfMute: boolean;
	localSelfDeaf: boolean;
}

export function resolveVoiceParticipantAvatarEntryVoiceState({
	snapshot,
	voiceState,
	permissionMuted,
	localEffectiveSelfMute,
	localSelfDeaf,
}: ResolveVoiceParticipantAvatarEntryVoiceStateArgs): VoiceParticipantAvatarEntryVoiceState {
	const display = resolveVoiceParticipantDisplayState({
		participant: snapshot,
		voiceState,
		isLocalConnection: snapshot.isLocal,
		localSelfMute: localEffectiveSelfMute,
		localSelfDeaf,
		permissionMuted,
	});
	return {
		speaking: display.speaking,
		selfMute: display.selfMute || display.guildMute,
		selfDeaf: display.selfDeaf || display.guildDeaf,
	};
}
