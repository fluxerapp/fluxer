// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	transitionVoiceControlBarState,
	type VoiceControlBarSignals,
} from '@app/features/voice/components/VoiceControlBarStateMachine';
import {describe, expect, it} from 'vitest';

function signals(overrides: Partial<VoiceControlBarSignals>): VoiceControlBarSignals {
	return {
		isDeafened: false,
		isGuildMuted: false,
		isGuildDeafened: false,
		isPermissionMuted: false,
		isPushToTalkEffective: false,
		isSelfMuted: false,
		effectiveMuted: false,
		canStream: true,
		isCameraEnabled: false,
		isCameraLimitReached: false,
		isCameraUserCapReached: false,
		isScreenShareEnabled: false,
		screenShareDisabled: false,
		...overrides,
	};
}

describe('voice control bar mute button in push to talk', () => {
	it('enables the toggle and shows the idle gate as unmuted with the hold hint', () => {
		expect(transitionVoiceControlBarState(signals({isPushToTalkEffective: true, effectiveMuted: true})).mute).toEqual({
			disabled: false,
			pressed: false,
			label: 'pushToTalkHoldHint',
		});
	});

	it('offers unmute when self-muted', () => {
		expect(
			transitionVoiceControlBarState(signals({isPushToTalkEffective: true, isSelfMuted: true, effectiveMuted: true}))
				.mute,
		).toEqual({disabled: false, pressed: true, label: 'unmute'});
	});

	it('keeps server and permission mutes locked', () => {
		expect(
			transitionVoiceControlBarState(signals({isPushToTalkEffective: true, isGuildMuted: true, effectiveMuted: true}))
				.mute,
		).toEqual({disabled: true, pressed: true, label: 'moderatorMuted'});
		expect(
			transitionVoiceControlBarState(
				signals({isPushToTalkEffective: true, isPermissionMuted: true, effectiveMuted: true}),
			).mute,
		).toEqual({disabled: true, pressed: true, label: 'permissionMuted'});
		expect(
			transitionVoiceControlBarState(
				signals({isPushToTalkEffective: true, isGuildDeafened: true, effectiveMuted: true}),
			).mute,
		).toEqual({disabled: true, pressed: true, label: 'moderatorDeafened'});
	});

	it('keeps voice activity labels unchanged', () => {
		expect(transitionVoiceControlBarState(signals({})).mute).toEqual({disabled: false, pressed: false, label: 'mute'});
		expect(transitionVoiceControlBarState(signals({isSelfMuted: true, effectiveMuted: true})).mute).toEqual({
			disabled: false,
			pressed: true,
			label: 'unmute',
		});
	});
});
