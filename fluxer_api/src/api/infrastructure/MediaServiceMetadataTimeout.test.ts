// SPDX-License-Identifier: AGPL-3.0-or-later

import {metadataRequestTimeoutMs} from '@app/api/infrastructure/MediaService';
import {describe, expect, it} from 'vitest';

const MIB = 1024 * 1024;

describe('metadataRequestTimeoutMs', () => {
	it.each([[undefined], [0], [100 * MIB], [500 * MIB]])(
		'keeps the 30 second window for an input of %s bytes that media-proxy buffers',
		(sizeBytes) => {
			expect(metadataRequestTimeoutMs(sizeBytes)).toBe(30_000);
		},
	);

	it.each([
		[500 * MIB + 1, 62_000],
		[700 * MIB, 74_000],
		[2 * 1024 * MIB, 158_000],
		[8 * 1024 * MIB, 542_000],
	])('widens the window for an input of %s bytes that media-proxy copies to disk first', (sizeBytes, timeoutMs) => {
		expect(metadataRequestTimeoutMs(sizeBytes)).toBe(timeoutMs);
	});
});
