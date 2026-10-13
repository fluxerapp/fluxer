// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Room} from 'livekit-client';
import {afterEach, describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? ''}}),
}));

vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({default: {}}));

const {default: Keybind} = await import('@app/features/input/state/InputKeybind');
const {isVoiceActivityGateEnabled} = await import('@app/features/voice/utils/VoiceInputProcessor');
const {bindVoiceEngineV2AppAudioPreferencesSync} = await import(
	'@app/features/voice/engine/v2/VoiceEngineV2AppAudioPreferencesSyncBinding'
);

function createAdapter() {
	return {
		requestMicrophoneRefresh: vi.fn(async () => {}),
		configureVoiceInput: vi.fn(),
		handleInputKeybindChange: vi.fn(),
		applyAllLocalAudioPreferences: vi.fn(),
	};
}

describe('voice engine v2 audio preferences sync binding', () => {
	let dispose: (() => void) | null = null;

	afterEach(() => {
		dispose?.();
		dispose = null;
		Keybind.setTransmitMode('voice_activity');
	});

	it('reconfigures voice input when transmit mode flips without a push-to-mute keybind', () => {
		Keybind.setTransmitMode('voice_activity');
		expect(Keybind.isPushToMuteEffective()).toBe(false);
		const gateBefore = isVoiceActivityGateEnabled();
		const adapter = createAdapter();
		dispose = bindVoiceEngineV2AppAudioPreferencesSync({} as Room, adapter, {warn: vi.fn()});

		Keybind.setTransmitMode('voice_push_to_talk');

		expect(isVoiceActivityGateEnabled()).toBe(false);
		expect(gateBefore).not.toBe(isVoiceActivityGateEnabled());
		expect(adapter.configureVoiceInput).toHaveBeenCalledTimes(1);
		expect(adapter.handleInputKeybindChange).not.toHaveBeenCalled();

		Keybind.setTransmitMode('voice_activity');

		expect(isVoiceActivityGateEnabled()).toBe(gateBefore);
		expect(adapter.configureVoiceInput).toHaveBeenCalledTimes(2);
		expect(adapter.handleInputKeybindChange).not.toHaveBeenCalled();
	});
});
