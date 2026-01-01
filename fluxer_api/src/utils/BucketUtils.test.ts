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

import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {FLUXER_EPOCH} from '~/Constants';
import {makeBucket, makeBuckets} from './BucketUtils';

describe('BucketUtils', () => {
	describe('makeBucket', () => {
		beforeEach(() => {
			vi.useFakeTimers();
		});

		afterEach(() => {
			vi.useRealTimers();
		});

		it('should create bucket from snowflake', () => {
			const snowflake = 1000000000000000n << 22n;
			const bucket = makeBucket(snowflake);

			expect(typeof bucket).toBe('number');
			expect(bucket).toBeGreaterThanOrEqual(0);
		});

		it('should create bucket from null snowflake using current time', () => {
			const mockTime = FLUXER_EPOCH + 1000000000;
			vi.setSystemTime(mockTime);

			const bucket = makeBucket(null);

			expect(typeof bucket).toBe('number');
			expect(bucket).toBeGreaterThanOrEqual(0);
		});

		it('should create same bucket for snowflakes in same time period', () => {
			const baseTimestamp = 1000000n;
			const snowflake1 = baseTimestamp << 22n;
			const snowflake2 = (baseTimestamp + 1000n) << 22n;

			const bucket1 = makeBucket(snowflake1);
			const bucket2 = makeBucket(snowflake2);

			expect(bucket1).toBe(bucket2);
		});

		it('should create different buckets for snowflakes in different time periods', () => {
			const bucketSizeMs = 1000 * 60 * 60 * 24 * 10;
			const baseTimestamp = BigInt(bucketSizeMs);
			const snowflake1 = baseTimestamp << 22n;
			const snowflake2 = (baseTimestamp + BigInt(bucketSizeMs * 2)) << 22n;

			const bucket1 = makeBucket(snowflake1);
			const bucket2 = makeBucket(snowflake2);

			expect(bucket2).toBeGreaterThan(bucket1);
		});

		it('should handle zero snowflake', () => {
			const bucket = makeBucket(0n);
			expect(bucket).toBe(0);
		});
	});

	describe('makeBuckets', () => {
		beforeEach(() => {
			vi.useFakeTimers();
		});

		afterEach(() => {
			vi.useRealTimers();
		});

		it('should create array of buckets between two snowflakes', () => {
			const bucketSizeMs = 1000 * 60 * 60 * 24 * 10;
			const startTimestamp = BigInt(bucketSizeMs);
			const endTimestamp = BigInt(bucketSizeMs * 3);
			const startSnowflake = startTimestamp << 22n;
			const endSnowflake = endTimestamp << 22n;

			const buckets = makeBuckets(startSnowflake, endSnowflake);

			expect(Array.isArray(buckets)).toBe(true);
			expect(buckets.length).toBeGreaterThan(0);
			expect(buckets[0]).toBeLessThanOrEqual(buckets[buckets.length - 1]);
		});

		it('should create single bucket when start and end are in same period', () => {
			const baseTimestamp = 1000000n;
			const startSnowflake = baseTimestamp << 22n;
			const endSnowflake = (baseTimestamp + 1000n) << 22n;

			const buckets = makeBuckets(startSnowflake, endSnowflake);

			expect(buckets).toHaveLength(1);
		});

		it('should handle null endId by using current time', () => {
			const mockTime = FLUXER_EPOCH + 1000000000;
			vi.setSystemTime(mockTime);

			const startSnowflake = 1000n << 22n;
			const buckets = makeBuckets(startSnowflake, null);

			expect(Array.isArray(buckets)).toBe(true);
			expect(buckets.length).toBeGreaterThanOrEqual(1);
		});

		it('should handle both null values by using current time', () => {
			const mockTime = FLUXER_EPOCH + 1000000000;
			vi.setSystemTime(mockTime);

			const buckets = makeBuckets(null, null);

			expect(buckets).toHaveLength(1);
		});

		it('should include all buckets in range', () => {
			const bucketSizeMs = 1000 * 60 * 60 * 24 * 10;
			const startTimestamp = BigInt(0);
			const endTimestamp = BigInt(bucketSizeMs * 2);
			const startSnowflake = startTimestamp << 22n;
			const endSnowflake = endTimestamp << 22n;

			const buckets = makeBuckets(startSnowflake, endSnowflake);

			expect(buckets).toEqual([0, 1, 2]);
		});

		it('should handle reverse order (endId before startId)', () => {
			const bucketSizeMs = 1000 * 60 * 60 * 24 * 10;
			const startTimestamp = BigInt(bucketSizeMs * 2);
			const endTimestamp = BigInt(bucketSizeMs);
			const startSnowflake = startTimestamp << 22n;
			const endSnowflake = endTimestamp << 22n;

			const buckets = makeBuckets(startSnowflake, endSnowflake);

			expect(Array.isArray(buckets)).toBe(true);
		});
	});
});
