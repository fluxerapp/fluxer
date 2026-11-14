// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {VOICE_CHANNEL_CAMERA_USER_LIMIT} from '@fluxer/constants/src/LimitConstants';

interface VoiceCameraCapacityVoiceState {
	user_id: string;
	self_video?: boolean;
}

export interface VoiceCameraUserCapInput {
	voiceStates: Readonly<Record<string, VoiceCameraCapacityVoiceState>>;
	currentUserId: string | null;
	isOwnCameraEnabled: boolean;
}

function countDistinctCameraUsers(voiceStates: Readonly<Record<string, VoiceCameraCapacityVoiceState>>): number {
	const cameraUserIds = new Set<string>();
	let voiceStateCount = 0;
	for (const connectionId in voiceStates) {
		const voiceState = voiceStates[connectionId];
		if (!voiceState) continue;
		voiceStateCount += 1;
		if (voiceState.self_video !== true) continue;
		if (voiceState.user_id.length === 0) continue;
		cameraUserIds.add(voiceState.user_id);
	}
	assert.ok(cameraUserIds.size <= voiceStateCount, 'camera users cannot exceed voice states');
	return cameraUserIds.size;
}

export function isCameraUserCapBlocked(input: VoiceCameraUserCapInput): boolean {
	if (input.isOwnCameraEnabled) return false;
	if (input.currentUserId) {
		for (const connectionId in input.voiceStates) {
			const voiceState = input.voiceStates[connectionId];
			if (!voiceState) continue;
			if (voiceState.user_id !== input.currentUserId) continue;
			if (voiceState.self_video === true) return false;
		}
	}
	return countDistinctCameraUsers(input.voiceStates) >= VOICE_CHANNEL_CAMERA_USER_LIMIT;
}
