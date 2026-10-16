// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	selectUserAreaMicrophoneState,
	type UserAreaMicrophoneSignals,
} from '@app/features/app/components/layout/UserAreaState';
import {describe, expect, it} from 'vitest';

function signals(overrides: Partial<UserAreaMicrophoneSignals>): UserAreaMicrophoneSignals {
	return {
		effectiveAudioMuted: false,
		effectiveAudioDeafened: false,
		isGuildMuted: false,
		isGuildDeafened: false,
		isPermissionMuted: false,
		muteReason: null,
		isPushToTalkEffective: false,
		isPushToMuteEffective: false,
		isPushToMuteHeld: false,
		...overrides,
	};
}

describe('selectUserAreaMicrophoneState', () => {
	it('shows the idle push to talk gate as unmuted without locking the toggle', () => {
		expect(
			selectUserAreaMicrophoneState(signals({isPushToTalkEffective: true, muteReason: 'voice_push_to_talk'})),
		).toEqual({effectiveMuted: false, muteToggleLocked: false});
	});

	it('shows a self-muted push to talk user as muted', () => {
		expect(
			selectUserAreaMicrophoneState(
				signals({isPushToTalkEffective: true, effectiveAudioMuted: true, muteReason: 'self'}),
			),
		).toEqual({effectiveMuted: true, muteToggleLocked: false});
	});

	it('shows a push to talk user muted by the failure latch as muted', () => {
		expect(selectUserAreaMicrophoneState(signals({isPushToTalkEffective: true, muteReason: 'self'}))).toEqual({
			effectiveMuted: true,
			muteToggleLocked: false,
		});
	});

	it('lets a self-deafened push to talk user click the toggle', () => {
		expect(
			selectUserAreaMicrophoneState(
				signals({isPushToTalkEffective: true, effectiveAudioMuted: true, effectiveAudioDeafened: true}),
			),
		).toEqual({effectiveMuted: true, muteToggleLocked: false});
	});

	it('locks the toggle for server and permission mutes in push to talk', () => {
		for (const overrides of [
			{isGuildMuted: true, muteReason: 'guild' as const},
			{isGuildDeafened: true},
			{isPermissionMuted: true, muteReason: 'permission' as const},
		]) {
			expect(selectUserAreaMicrophoneState(signals({isPushToTalkEffective: true, ...overrides}))).toEqual({
				effectiveMuted: true,
				muteToggleLocked: true,
			});
		}
	});

	it('keeps voice activity and push to mute behaviour unchanged', () => {
		expect(selectUserAreaMicrophoneState(signals({}))).toEqual({effectiveMuted: false, muteToggleLocked: false});
		expect(selectUserAreaMicrophoneState(signals({effectiveAudioMuted: true, muteReason: 'self'}))).toEqual({
			effectiveMuted: true,
			muteToggleLocked: false,
		});
		expect(selectUserAreaMicrophoneState(signals({isPushToMuteEffective: true, isPushToMuteHeld: true}))).toEqual({
			effectiveMuted: true,
			muteToggleLocked: false,
		});
		expect(selectUserAreaMicrophoneState(signals({isGuildMuted: true}))).toEqual({
			effectiveMuted: true,
			muteToggleLocked: true,
		});
	});
});
