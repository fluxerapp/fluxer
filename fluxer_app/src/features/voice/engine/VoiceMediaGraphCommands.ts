// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {
	VoiceMediaGraphRemoteSubscriptionCommand,
	VoiceMediaGraphVideoQuality,
} from '@app/features/voice/engine/VoiceMediaGraphSubscriptionTypes';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';

export function buildVoiceMediaGraphNativeScreenShareSubscriptionCommands(args: {
	participantIdentity: string;
	subscribed: boolean;
	enabled?: boolean;
	quality?: VoiceMediaGraphVideoQuality;
}): Array<VoiceMediaGraphRemoteSubscriptionCommand> {
	assert.ok(args.participantIdentity.length > 0, 'participantIdentity is required');
	if (!args.subscribed) {
		return [
			{
				participantIdentity: args.participantIdentity,
				source: VoiceTrackSource.ScreenShare,
				subscribed: false,
				enabled: false,
			},
			{
				participantIdentity: args.participantIdentity,
				source: VoiceTrackSource.ScreenShareAudio,
				subscribed: false,
				enabled: false,
			},
		];
	}
	return [
		{
			participantIdentity: args.participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			subscribed: true,
			enabled: args.enabled ?? true,
			...(args.quality !== undefined ? {quality: args.quality} : {}),
		},
		{
			participantIdentity: args.participantIdentity,
			source: VoiceTrackSource.ScreenShareAudio,
			subscribed: true,
			enabled: true,
		},
	];
}

export function buildVoiceMediaGraphNativeScreenShareEnabledCommand(args: {
	participantIdentity: string;
	enabled: boolean;
}): VoiceMediaGraphRemoteSubscriptionCommand {
	assert.ok(args.participantIdentity.length > 0, 'participantIdentity is required');
	return {
		participantIdentity: args.participantIdentity,
		source: VoiceTrackSource.ScreenShare,
		subscribed: true,
		enabled: args.enabled,
	};
}

export function buildVoiceMediaGraphNativeScreenShareQualityCommand(args: {
	participantIdentity: string;
	enabled: boolean;
	quality: VoiceMediaGraphVideoQuality;
}): VoiceMediaGraphRemoteSubscriptionCommand | null {
	if (!args.enabled) return null;
	assert.ok(args.participantIdentity.length > 0, 'participantIdentity is required');
	return {
		participantIdentity: args.participantIdentity,
		source: VoiceTrackSource.ScreenShare,
		subscribed: true,
		quality: args.quality,
	};
}

export function buildVoiceMediaGraphNativeCameraSubscriptionCommand(args: {
	participantIdentity: string;
	subscribed: boolean;
	enabled?: boolean;
	quality?: VoiceMediaGraphVideoQuality;
}): VoiceMediaGraphRemoteSubscriptionCommand {
	assert.ok(args.participantIdentity.length > 0, 'participantIdentity is required');
	return {
		participantIdentity: args.participantIdentity,
		source: VoiceTrackSource.Camera,
		subscribed: args.subscribed,
		enabled: args.enabled ?? args.subscribed,
		...(args.quality !== undefined ? {quality: args.quality} : {}),
	};
}

export function buildVoiceMediaGraphNativeCameraQualityCommand(args: {
	participantIdentity: string;
	quality: VoiceMediaGraphVideoQuality;
}): VoiceMediaGraphRemoteSubscriptionCommand {
	assert.ok(args.participantIdentity.length > 0, 'participantIdentity is required');
	return {
		participantIdentity: args.participantIdentity,
		source: VoiceTrackSource.Camera,
		subscribed: true,
		quality: args.quality,
	};
}
