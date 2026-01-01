/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {describe, expect, test} from 'vitest';
import {
	compare,
	extractTimestamp,
	fromTimestamp,
	fromTimestampWithSequence,
	SnowflakeSequence,
} from '~/utils/SnowflakeUtils';

describe('SnowflakeUtils', () => {
	test('extractTimestamp round-trips generated snowflakes without precision loss', () => {
		const timestamp = Date.now();
		const snowflake = fromTimestamp(timestamp);

		expect(extractTimestamp(snowflake)).toBe(timestamp);
	});

	test('extractTimestamp returns NaN for invalid inputs instead of throwing', () => {
		expect(extractTimestamp('not-a-valid-snowflake')).toBeNaN();
		expect(extractTimestamp('123')).toBeNaN();
	});

	test('fromTimestampWithSequence preserves the timestamp while the sequence makes later IDs larger', () => {
		const timestamp = Date.now();
		const sequence = new SnowflakeSequence();

		const first = fromTimestampWithSequence(timestamp, sequence);
		const second = fromTimestampWithSequence(timestamp, sequence);

		expect(extractTimestamp(first)).toBe(timestamp);
		expect(extractTimestamp(second)).toBe(timestamp);
		expect(compare(second, first)).toBeGreaterThan(0);
	});

	test('compare orders snowflakes by length and handles null values safely', () => {
		expect(compare(null, null)).toBe(0);
		expect(compare(null, '1')).toBe(-1);
		expect(compare('2', null)).toBe(1);
		expect(compare('99999999999999999', '1')).toBeGreaterThan(0);
		expect(compare('1', '2')).toBeLessThan(0);
	});
});
