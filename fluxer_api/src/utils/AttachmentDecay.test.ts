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
import {
	computeDecay,
	computeRenewalThresholdDays,
	computeRenewalWindowDays,
	DEFAULT_DECAY_CONSTANTS,
	DEFAULT_RENEWAL_CONSTANTS,
	extendExpiry,
	maybeRenewExpiry,
} from './AttachmentDecay';

const toBytes = (mb: number) => mb * 1024 * 1024;

describe('computeDecay', () => {
	const uploadDate = new Date('2024-01-01T00:00:00.000Z');

	it('returns null when size exceeds plan limit', () => {
		const sizeBytes = toBytes(DEFAULT_DECAY_CONSTANTS.PLAN_MB + 1);
		const result = computeDecay({sizeBytes, uploadedAt: uploadDate});
		expect(result).toBeNull();
	});

	it('uses MAX_DAYS for small files at or below MIN_MB', () => {
		const sizeBytes = toBytes(DEFAULT_DECAY_CONSTANTS.MIN_MB);
		const result = computeDecay({sizeBytes, uploadedAt: uploadDate});
		expect(result).not.toBeNull();
		expect(result!.days).toBe(DEFAULT_DECAY_CONSTANTS.MAX_DAYS);

		const expectedExpiry = new Date(uploadDate);
		expectedExpiry.setUTCDate(expectedExpiry.getUTCDate() + DEFAULT_DECAY_CONSTANTS.MAX_DAYS);
		expect(result!.expiresAt.toISOString()).toBe(expectedExpiry.toISOString());
	});

	it('uses MIN_DAYS for large files at or above MAX_MB', () => {
		const sizeBytes = toBytes(DEFAULT_DECAY_CONSTANTS.MAX_MB);
		const result = computeDecay({sizeBytes, uploadedAt: uploadDate});
		expect(result).not.toBeNull();
		expect(result!.days).toBe(DEFAULT_DECAY_CONSTANTS.MIN_DAYS);
	});

	it('blends lifetime between MIN_MB and MAX_MB', () => {
		const sizeBytes = toBytes((DEFAULT_DECAY_CONSTANTS.MIN_MB + DEFAULT_DECAY_CONSTANTS.MAX_MB) / 2);
		const result = computeDecay({sizeBytes, uploadedAt: uploadDate});
		expect(result).not.toBeNull();
		expect(result!.days).toBeGreaterThan(DEFAULT_DECAY_CONSTANTS.MIN_DAYS);
		expect(result!.days).toBeLessThan(DEFAULT_DECAY_CONSTANTS.MAX_DAYS);
	});

	it('computes cost proportionally to size and lifetime', () => {
		const sizeMb = 100;
		const sizeBytes = toBytes(sizeMb);
		const result = computeDecay({sizeBytes, uploadedAt: uploadDate});
		expect(result).not.toBeNull();

		const sizeTb = sizeBytes / 1024 ** 4;
		const lifetimeMonths = result!.days / 30;
		const expectedCost = sizeTb * DEFAULT_DECAY_CONSTANTS.PRICE_PER_TB_PER_MONTH * lifetimeMonths;
		expect(result!.cost).toBeCloseTo(expectedCost, 4);
	});
});

describe('extendExpiry', () => {
	it('returns newer expiry when current is older', () => {
		const current = new Date('2024-01-01T00:00:00.000Z');
		const next = new Date('2024-02-01T00:00:00.000Z');
		expect(extendExpiry(current, next)).toEqual(next);
	});

	it('keeps current expiry when it is already later', () => {
		const current = new Date('2024-03-01T00:00:00.000Z');
		const next = new Date('2024-02-01T00:00:00.000Z');
		expect(extendExpiry(current, next)).toEqual(current);
	});

	it('handles null current expiry', () => {
		const next = new Date('2024-02-01T00:00:00.000Z');
		expect(extendExpiry(null, next)).toEqual(next);
	});
});

describe('maybeRenewExpiry with size-aware windows', () => {
	const baseUpload = new Date('2024-01-01T00:00:00.000Z');

	it('uses a short window for large files and clamps to max', () => {
		const sizeMb = DEFAULT_DECAY_CONSTANTS.MAX_MB;
		const decay = computeDecay({sizeBytes: toBytes(sizeMb), uploadedAt: baseUpload});
		expect(decay).not.toBeNull();
		const windowDays = computeRenewalWindowDays(sizeMb);
		const thresholdDays = computeRenewalThresholdDays(windowDays);
		const maxExpiry = new Date(baseUpload);
		maxExpiry.setUTCDate(maxExpiry.getUTCDate() + decay!.days + windowDays);

		const now = new Date(baseUpload);
		now.setUTCDate(now.getUTCDate() + (decay!.days - thresholdDays));

		const renewed = maybeRenewExpiry({
			currentExpiry: decay!.expiresAt,
			now,
			windowDays,
			thresholdDays,
			maxExpiry,
		});

		expect(windowDays).toBe(DEFAULT_RENEWAL_CONSTANTS.MIN_WINDOW_DAYS);
		expect(renewed).not.toBeNull();
		if (renewed) {
			const expected = new Date(now);
			expected.setUTCDate(expected.getUTCDate() + windowDays);
			expect(renewed.toISOString()).toBe(expected.toISOString());
		}
	});

	it('caps renewal so total lifetime cannot exceed the size budget', () => {
		const sizeMb = DEFAULT_DECAY_CONSTANTS.MAX_MB;
		const decay = computeDecay({sizeBytes: toBytes(sizeMb), uploadedAt: baseUpload});
		expect(decay).not.toBeNull();
		const windowDays = computeRenewalWindowDays(sizeMb);
		const thresholdDays = computeRenewalThresholdDays(windowDays);
		const maxExpiry = new Date(baseUpload);
		maxExpiry.setUTCDate(maxExpiry.getUTCDate() + decay!.days + windowDays);

		const current = new Date(baseUpload);
		current.setUTCDate(current.getUTCDate() + decay!.days + windowDays - 1);

		const now = new Date(current);
		now.setUTCDate(now.getUTCDate() - thresholdDays + 1);

		const renewed = maybeRenewExpiry({
			currentExpiry: current,
			now,
			windowDays,
			thresholdDays,
			maxExpiry,
		});

		expect(renewed).not.toBeNull();
		if (renewed) {
			expect(renewed.toISOString()).toBe(maxExpiry.toISOString());
		}
	});

	it('uses a longer window for small files and extends when near threshold', () => {
		const sizeMb = DEFAULT_DECAY_CONSTANTS.MIN_MB;
		const decay = computeDecay({sizeBytes: toBytes(sizeMb), uploadedAt: baseUpload});
		expect(decay).not.toBeNull();
		const windowDays = computeRenewalWindowDays(sizeMb);
		const thresholdDays = computeRenewalThresholdDays(windowDays);
		const maxExpiry = new Date(baseUpload);
		maxExpiry.setUTCDate(maxExpiry.getUTCDate() + decay!.days + windowDays);

		const now = new Date(baseUpload);
		now.setUTCDate(now.getUTCDate() + (decay!.days - thresholdDays + 1));

		const renewed = maybeRenewExpiry({
			currentExpiry: decay!.expiresAt,
			now,
			windowDays,
			thresholdDays,
			maxExpiry,
		});

		expect(windowDays).toBe(DEFAULT_RENEWAL_CONSTANTS.MAX_WINDOW_DAYS);
		expect(thresholdDays).toBeGreaterThan(0);
		expect(renewed).not.toBeNull();
		if (renewed) {
			const expected = new Date(now);
			expected.setUTCDate(expected.getUTCDate() + windowDays);
			expect(renewed.toISOString()).toBe(expected.toISOString());
		}
	});

	it('does not extend when outside the renewal threshold', () => {
		const sizeMb = 100;
		const decay = computeDecay({sizeBytes: toBytes(sizeMb), uploadedAt: baseUpload});
		expect(decay).not.toBeNull();
		const windowDays = computeRenewalWindowDays(sizeMb);
		const thresholdDays = computeRenewalThresholdDays(windowDays);
		const maxExpiry = new Date(baseUpload);
		maxExpiry.setUTCDate(maxExpiry.getUTCDate() + decay!.days + windowDays);

		const now = new Date(baseUpload);
		now.setUTCDate(now.getUTCDate() + (decay!.days - thresholdDays - 5));

		const renewed = maybeRenewExpiry({
			currentExpiry: decay!.expiresAt,
			now,
			windowDays,
			thresholdDays,
			maxExpiry,
		});

		expect(renewed).toBeNull();
	});
});
