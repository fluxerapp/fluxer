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

export interface AttachmentDecayInput {
	sizeBytes: bigint | number;
	uploadedAt: Date;
	curve?: number;
	pricePerTBPerMonth?: number;
}

export interface AttachmentDecayResult {
	expiresAt: Date;
	days: number;
	cost: number;
}

export const DEFAULT_DECAY_CONSTANTS = {
	MIN_MB: 5,
	MAX_MB: 500,
	MIN_DAYS: 14,
	MAX_DAYS: 365 * 3,
	PLAN_MB: 500,
	CURVE: 0.5,
	PRICE_PER_TB_PER_MONTH: 0.0081103 * 1000,
};

export const DEFAULT_RENEWAL_CONSTANTS = {
	RENEW_THRESHOLD_DAYS: 30,
	RENEW_WINDOW_DAYS: 30,
	MIN_WINDOW_DAYS: 7,
	MAX_WINDOW_DAYS: 30,
	MIN_THRESHOLD_DAYS: 3,
	MAX_THRESHOLD_DAYS: 14,
};

function toMb(sizeBytes: bigint | number): number {
	const n = typeof sizeBytes === 'bigint' ? Number(sizeBytes) : sizeBytes;
	return n / 1024 / 1024;
}

export function computeDecay({
	sizeBytes,
	uploadedAt,
	curve = DEFAULT_DECAY_CONSTANTS.CURVE,
	pricePerTBPerMonth = DEFAULT_DECAY_CONSTANTS.PRICE_PER_TB_PER_MONTH,
}: AttachmentDecayInput): AttachmentDecayResult | null {
	const constants = DEFAULT_DECAY_CONSTANTS;
	const sizeMB = toMb(sizeBytes);

	if (sizeMB > constants.PLAN_MB) return null;

	let lifetimeDays: number;
	if (sizeMB <= constants.MIN_MB) {
		lifetimeDays = constants.MAX_DAYS;
	} else if (sizeMB >= constants.MAX_MB) {
		lifetimeDays = constants.MIN_DAYS;
	} else {
		const linearFrac = (sizeMB - constants.MIN_MB) / (constants.MAX_MB - constants.MIN_MB);
		const logFrac = Math.log(sizeMB / constants.MIN_MB) / Math.log(constants.MAX_MB / constants.MIN_MB);
		const blend = (1 - curve) * linearFrac + curve * logFrac;
		lifetimeDays = constants.MAX_DAYS - blend * (constants.MAX_DAYS - constants.MIN_DAYS);
	}

	const expiresAt = new Date(uploadedAt);
	expiresAt.setUTCDate(expiresAt.getUTCDate() + lifetimeDays);

	const sizeTB = (typeof sizeBytes === 'bigint' ? Number(sizeBytes) : sizeBytes) / 1024 / 1024 / 1024 / 1024;
	const lifetimeMonths = lifetimeDays / 30;
	const cost = sizeTB * pricePerTBPerMonth * lifetimeMonths;

	return {
		expiresAt,
		cost,
		days: Math.round(lifetimeDays),
	};
}

const MS_PER_DAY = 1000 * 60 * 60 * 24;

export function computeRenewalWindowDays(
	sizeMB: number,
	{
		minMB = DEFAULT_DECAY_CONSTANTS.MIN_MB,
		maxMB = DEFAULT_DECAY_CONSTANTS.MAX_MB,
	}: {minMB?: number; maxMB?: number} = {},
	{
		minWindowDays = DEFAULT_RENEWAL_CONSTANTS.MIN_WINDOW_DAYS,
		maxWindowDays = DEFAULT_RENEWAL_CONSTANTS.MAX_WINDOW_DAYS,
	}: {minWindowDays?: number; maxWindowDays?: number} = {},
): number {
	if (sizeMB <= minMB) return maxWindowDays;
	if (sizeMB >= maxMB) return minWindowDays;

	const frac = Math.log(sizeMB / minMB) / Math.log(maxMB / minMB);
	const window = maxWindowDays - frac * (maxWindowDays - minWindowDays);
	return Math.round(window);
}

export function computeRenewalThresholdDays(
	windowDays: number,
	{
		minThresholdDays = DEFAULT_RENEWAL_CONSTANTS.MIN_THRESHOLD_DAYS,
		maxThresholdDays = DEFAULT_RENEWAL_CONSTANTS.MAX_THRESHOLD_DAYS,
	}: {minThresholdDays?: number; maxThresholdDays?: number} = {},
): number {
	const clampedWindow = Math.max(
		DEFAULT_RENEWAL_CONSTANTS.MIN_WINDOW_DAYS,
		Math.min(DEFAULT_RENEWAL_CONSTANTS.MAX_WINDOW_DAYS, windowDays),
	);
	const threshold = Math.round(clampedWindow / 2);
	return Math.max(minThresholdDays, Math.min(maxThresholdDays, threshold));
}

export function computeCost({
	sizeBytes,
	lifetimeDays,
	pricePerTBPerMonth = DEFAULT_DECAY_CONSTANTS.PRICE_PER_TB_PER_MONTH,
}: {
	sizeBytes: bigint | number;
	lifetimeDays: number;
	pricePerTBPerMonth?: number;
}): number {
	const sizeTB = (typeof sizeBytes === 'bigint' ? Number(sizeBytes) : sizeBytes) / 1024 / 1024 / 1024 / 1024;
	const lifetimeMonths = lifetimeDays / 30;
	return sizeTB * pricePerTBPerMonth * lifetimeMonths;
}

export function getExpiryBucket(expiresAt: Date): number {
	return Number(
		`${expiresAt.getUTCFullYear()}${String(expiresAt.getUTCMonth() + 1).padStart(2, '0')}${String(
			expiresAt.getUTCDate(),
		).padStart(2, '0')}`,
	);
}

export function extendExpiry(currentExpiry: Date | null, newlyComputed: Date): Date {
	if (!currentExpiry) return newlyComputed;
	return currentExpiry > newlyComputed ? currentExpiry : newlyComputed;
}

export function maybeRenewExpiry({
	currentExpiry,
	now,
	thresholdDays = DEFAULT_RENEWAL_CONSTANTS.RENEW_THRESHOLD_DAYS,
	windowDays = DEFAULT_RENEWAL_CONSTANTS.RENEW_WINDOW_DAYS,
	maxExpiry,
}: {
	currentExpiry: Date | null;
	now: Date;
	thresholdDays?: number;
	windowDays?: number;
	maxExpiry?: Date;
}): Date | null {
	if (!currentExpiry) return null;
	if (windowDays <= 0) return null;

	const remainingMs = currentExpiry.getTime() - now.getTime();
	if (remainingMs > thresholdDays * MS_PER_DAY) {
		return null;
	}

	const targetMs = now.getTime() + windowDays * MS_PER_DAY;
	const cappedTargetMs = maxExpiry ? Math.min(maxExpiry.getTime(), targetMs) : targetMs;

	if (cappedTargetMs <= currentExpiry.getTime()) {
		return null;
	}

	const target = new Date(now);
	target.setTime(cappedTargetMs);
	return target;
}
