// SPDX-License-Identifier: AGPL-3.0-or-later

import {isScreenShareCodecUpgrade} from '@app/features/voice/utils/ScreenShareCodecSelection';
import {describe, expect, it} from 'vitest';

describe('isScreenShareCodecUpgrade', () => {
	it('treats an earlier ranked codec than the published one as an upgrade', () => {
		expect(isScreenShareCodecUpgrade(['vp9', 'h264', 'vp8'], 'h264', 'vp9')).toBe(true);
	});

	it('treats a later ranked codec than the published one as a downgrade', () => {
		expect(isScreenShareCodecUpgrade(['vp9', 'h264', 'vp8'], 'vp9', 'h264')).toBe(false);
	});

	it('treats a codec missing from the order as a downgrade', () => {
		expect(isScreenShareCodecUpgrade(['vp9', 'vp8'], 'h264', 'vp9')).toBe(false);
		expect(isScreenShareCodecUpgrade(['vp9', 'vp8'], 'vp9', 'h264')).toBe(false);
	});
});
