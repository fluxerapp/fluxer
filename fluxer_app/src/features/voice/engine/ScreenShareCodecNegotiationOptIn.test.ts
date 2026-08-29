// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, it, vi} from 'vitest';

let av1OptIn = false;
let hevcOptIn = false;

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		getScreenShareEncoderMode: () => 'software',
		getPreferredScreenShareCodec: () => 'auto',
		getScreenShareAv1OptIn: () => av1OptIn,
		getScreenShareHevcOptIn: () => hevcOptIn,
	},
}));

const {getScreenShareCodecPreferenceOrder} = await import('./ScreenShareCodecNegotiation');

describe('screen-share codec preference order opt-in filter', () => {
	beforeEach(() => {
		av1OptIn = false;
		hevcOptIn = false;
	});

	it('excludes AV1 and HEVC from the automatic preference order by default', () => {
		const order = getScreenShareCodecPreferenceOrder('auto');
		expect(order).not.toContain('av1');
		expect(order).not.toContain('h265');
		expect(order).toContain('vp9');
	});

	it('includes AV1 only once its opt-in is on', () => {
		expect(getScreenShareCodecPreferenceOrder('auto')).not.toContain('av1');
		av1OptIn = true;
		expect(getScreenShareCodecPreferenceOrder('auto')).toContain('av1');
	});

	it('includes HEVC only once its opt-in is on', () => {
		expect(getScreenShareCodecPreferenceOrder('auto')).not.toContain('h265');
		hevcOptIn = true;
		expect(getScreenShareCodecPreferenceOrder('auto')).toContain('h265');
	});
});
