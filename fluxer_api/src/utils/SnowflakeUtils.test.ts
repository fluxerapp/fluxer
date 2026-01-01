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

import {describe, expect, it} from 'vitest';
import {FLUXER_EPOCH} from '~/Constants';
import {extractTimestamp, getSnowflake} from './SnowflakeUtils';

describe('SnowflakeUtils', () => {
	describe('getSnowflake', () => {
		it('should generate snowflake from timestamp', () => {
			const timestamp = Date.now();
			const snowflake = getSnowflake(timestamp);

			expect(typeof snowflake).toBe('bigint');
			expect(snowflake > 0n).toBe(true);
		});

		it('should generate different snowflakes for different timestamps', () => {
			const timestamp1 = Date.now();
			const timestamp2 = timestamp1 + 1000;

			const snowflake1 = getSnowflake(timestamp1);
			const snowflake2 = getSnowflake(timestamp2);

			expect(snowflake1).not.toBe(snowflake2);
			expect(snowflake2 > snowflake1).toBe(true);
		});

		it('should use current timestamp when no timestamp provided', () => {
			const beforeCall = Date.now();
			const snowflake = getSnowflake();
			const afterCall = Date.now();

			const extractedTimestamp = extractTimestamp(snowflake);

			expect(extractedTimestamp).toBeGreaterThanOrEqual(beforeCall);
			expect(extractedTimestamp).toBeLessThanOrEqual(afterCall);
		});

		it('should handle FLUXER_EPOCH correctly', () => {
			const snowflake = getSnowflake(FLUXER_EPOCH);
			expect(snowflake).toBe(0n);
		});
	});

	describe('extractTimestamp', () => {
		it('should extract timestamp from snowflake', () => {
			const originalTimestamp = Date.now();
			const snowflake = getSnowflake(originalTimestamp);
			const extractedTimestamp = extractTimestamp(snowflake);

			expect(extractedTimestamp).toBe(originalTimestamp);
		});

		it('should handle zero snowflake', () => {
			const extractedTimestamp = extractTimestamp(0n);
			expect(extractedTimestamp).toBe(FLUXER_EPOCH);
		});

		it('should be inverse of getSnowflake', () => {
			const timestamp = 1609459200000;
			const snowflake = getSnowflake(timestamp);
			const extracted = extractTimestamp(snowflake);

			expect(extracted).toBe(timestamp);
		});
	});

	describe('roundtrip conversion', () => {
		it('should maintain timestamp integrity through conversion', () => {
			const timestamps = [FLUXER_EPOCH, FLUXER_EPOCH + 1000, Date.now(), Date.now() + 86400000];

			timestamps.forEach((timestamp) => {
				const snowflake = getSnowflake(timestamp);
				const extracted = extractTimestamp(snowflake);
				expect(extracted).toBe(timestamp);
			});
		});
	});
});
