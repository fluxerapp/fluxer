// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {describe, expect, it} from 'vitest';
import {maxEncodingBitrate} from './publishUtils.ts';

describe('maxEncodingBitrate', () => {
	it('returns the highest layer bitrate for a simulcast ladder', () => {
		expect(maxEncodingBitrate([{maxBitrate: 150_000}, {maxBitrate: 500_000}, {maxBitrate: 1_700_000}])).toBe(1_700_000);
	});

	it('returns the single encoding bitrate for a non-simulcast publish', () => {
		expect(maxEncodingBitrate([{maxBitrate: 3_000_000}])).toBe(3_000_000);
	});

	it('returns zero when no encoding declares a bitrate', () => {
		expect(maxEncodingBitrate([{}])).toBe(0);
		expect(maxEncodingBitrate([])).toBe(0);
		expect(maxEncodingBitrate(undefined)).toBe(0);
	});
});
