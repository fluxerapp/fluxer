// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	isLocalSpeakingSuppressed,
	type LocalSpeakingSuppressionInput,
} from '@app/features/voice/engine/VoiceLocalSpeakingGate';
import {describe, expect, it} from 'vitest';

function input(overrides: Partial<LocalSpeakingSuppressionInput>): LocalSpeakingSuppressionInput {
	return {
		selfDeaf: false,
		effectiveSelfMute: false,
		hasMicrophonePublication: true,
		microphonePublicationMuted: false,
		...overrides,
	};
}

describe('isLocalSpeakingSuppressed', () => {
	it('lets the measured outgoing audio decide for a live, unmuted microphone', () => {
		expect(isLocalSpeakingSuppressed(input({}))).toBe(false);
	});

	it('suppresses the indicator whenever nothing is sent', () => {
		expect(isLocalSpeakingSuppressed(input({effectiveSelfMute: true}))).toBe(true);
		expect(isLocalSpeakingSuppressed(input({selfDeaf: true}))).toBe(true);
		expect(isLocalSpeakingSuppressed(input({hasMicrophonePublication: false}))).toBe(true);
		expect(isLocalSpeakingSuppressed(input({microphonePublicationMuted: true}))).toBe(true);
	});
});
