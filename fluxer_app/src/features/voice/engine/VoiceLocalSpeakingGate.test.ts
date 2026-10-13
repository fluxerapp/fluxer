// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type LocalSpeakingOverrideInput,
	resolveLocalSpeakingOverrideState,
} from '@app/features/voice/engine/VoiceLocalSpeakingGate';
import {describe, expect, it} from 'vitest';

function input(overrides: Partial<LocalSpeakingOverrideInput>): LocalSpeakingOverrideInput {
	return {
		pushToTalkActive: true,
		pushToMuteActive: false,
		pushToMuteHeld: false,
		selfDeaf: false,
		effectiveSelfMute: false,
		hasMicrophonePublication: true,
		microphonePublicationMuted: false,
		...overrides,
	};
}

describe('resolveLocalSpeakingOverrideState in push to talk', () => {
	it('never lights the speaking indicator for a self-muted user holding the key', () => {
		expect(resolveLocalSpeakingOverrideState(input({effectiveSelfMute: true}))).toBe(false);
		expect(resolveLocalSpeakingOverrideState(input({effectiveSelfMute: true, microphonePublicationMuted: true}))).toBe(
			false,
		);
	});

	it('lights the speaking indicator for an unmuted user holding the key', () => {
		expect(resolveLocalSpeakingOverrideState(input({}))).toBe(true);
	});
});
