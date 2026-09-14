// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	selectUserAreaMicrophoneState,
	type UserAreaMicrophoneSignals,
} from '@app/features/app/components/layout/UserAreaState';
import {describe, expect, it} from 'vitest';

const baseSignals: UserAreaMicrophoneSignals = {
	effectiveAudioMuted: false,
	effectiveAudioDeafened: false,
	isGuildMuted: false,
	isGuildDeafened: false,
	isPermissionMuted: false,
	muteReason: null,
	isPushToTalkEffective: false,
	isPushToTalkHeld: false,
	isPushToMuteEffective: false,
	isPushToMuteHeld: false,
};

describe('selectUserAreaMicrophoneState', () => {
	it('follows normal effective mute state without transmit hold modes', () => {
		expect(selectUserAreaMicrophoneState(baseSignals)).toEqual({
			effectiveMuted: false,
			muteToggleLocked: false,
		});
		expect(selectUserAreaMicrophoneState({...baseSignals, effectiveAudioMuted: true})).toMatchObject({
			effectiveMuted: true,
		});
	});

	it('shows push-to-talk released as muted', () => {
		expect(selectUserAreaMicrophoneState({...baseSignals, isPushToTalkEffective: true})).toEqual({
			effectiveMuted: true,
			muteToggleLocked: true,
		});
	});

	it('treats push-to-talk without an effective binding as disabled', () => {
		expect(selectUserAreaMicrophoneState({...baseSignals, isPushToTalkEffective: false})).toEqual({
			effectiveMuted: false,
			muteToggleLocked: false,
		});
	});

	it('shows push-to-talk held as unmuted even with persisted self mute state', () => {
		expect(
			selectUserAreaMicrophoneState({
				...baseSignals,
				effectiveAudioMuted: true,
				muteReason: 'self',
				isPushToTalkEffective: true,
				isPushToTalkHeld: true,
			}),
		).toEqual({
			effectiveMuted: false,
			muteToggleLocked: true,
		});
	});

	it('does not show push-to-talk held as unmuted through hard mute states', () => {
		expect(
			selectUserAreaMicrophoneState({
				...baseSignals,
				isPermissionMuted: true,
				isPushToTalkEffective: true,
				isPushToTalkHeld: true,
			}),
		).toEqual({
			effectiveMuted: true,
			muteToggleLocked: true,
		});
	});

	it('shows push-to-mute held as muted while leaving the mute toggle available', () => {
		expect(
			selectUserAreaMicrophoneState({
				...baseSignals,
				isPushToMuteEffective: true,
				isPushToMuteHeld: true,
			}),
		).toEqual({
			effectiveMuted: true,
			muteToggleLocked: false,
		});
	});
});
