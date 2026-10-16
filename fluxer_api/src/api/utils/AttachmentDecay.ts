// SPDX-License-Identifier: AGPL-3.0-or-later

import {requireIntegerInRange} from '@app/api/utils/IntegerOptions';
import {getValidTimestamp} from '@app/api/utils/TimestampUtils';
import {ms} from 'itty-time';

export interface AttachmentDecayRules {
	minMb: number;
	maxMb: number;
	maxEligibleMb: number;
	minDays: number;
	maxDays: number;
	curve: number;
	pricePerTBPerMonth?: number;
}

interface AttachmentDecayInput {
	sizeBytes: bigint | number;
	uploadedAt: Date;
	rules?: AttachmentDecayRules;
}

interface AttachmentDecayResult {
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

function logRatio(value: number, base: number): number {
	const ratio = value / base;
	return Number.isFinite(ratio) ? Math.log(ratio) : Math.log(value) - Math.log(base);
}

export function computeDecay({sizeBytes, uploadedAt, rules}: AttachmentDecayInput): AttachmentDecayResult | null {
	const constants = {
		minMb: rules?.minMb ?? DEFAULT_DECAY_CONSTANTS.MIN_MB,
		maxMb: rules?.maxMb ?? DEFAULT_DECAY_CONSTANTS.MAX_MB,
		maxEligibleMb: rules?.maxEligibleMb ?? DEFAULT_DECAY_CONSTANTS.PLAN_MB,
		minDays: rules?.minDays ?? DEFAULT_DECAY_CONSTANTS.MIN_DAYS,
		maxDays: rules?.maxDays ?? DEFAULT_DECAY_CONSTANTS.MAX_DAYS,
		curve: rules?.curve ?? DEFAULT_DECAY_CONSTANTS.CURVE,
		pricePerTBPerMonth: rules?.pricePerTBPerMonth ?? DEFAULT_DECAY_CONSTANTS.PRICE_PER_TB_PER_MONTH,
	};
	const sizeMB = toMb(sizeBytes);
	if (sizeMB > constants.maxEligibleMb) return null;
	let lifetimeDays: number;
	if (sizeMB <= constants.minMb) {
		lifetimeDays = constants.maxDays;
	} else if (sizeMB >= constants.maxMb) {
		lifetimeDays = constants.minDays;
	} else {
		const linearFrac = (sizeMB - constants.minMb) / (constants.maxMb - constants.minMb);
		const logFrac = logRatio(sizeMB, constants.minMb) / logRatio(constants.maxMb, constants.minMb);
		const blend = (1 - constants.curve) * linearFrac + constants.curve * logFrac;
		lifetimeDays = constants.maxDays - blend * (constants.maxDays - constants.minDays);
	}
	const expiresAt = new Date(getValidTimestamp(uploadedAt, 'Attachment upload date'));
	expiresAt.setUTCDate(expiresAt.getUTCDate() + lifetimeDays);
	getValidTimestamp(expiresAt, 'Attachment decay expiry');
	return {
		expiresAt,
		cost: computeCost({sizeBytes, lifetimeDays, pricePerTBPerMonth: constants.pricePerTBPerMonth}),
		days: Math.round(lifetimeDays),
	};
}

const MS_PER_DAY = ms('1 day');

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
	getValidTimestamp(expiresAt, 'Attachment expiry');
	const bucket = Number(
		`${expiresAt.getUTCFullYear()}${String(expiresAt.getUTCMonth() + 1).padStart(2, '0')}${String(expiresAt.getUTCDate()).padStart(2, '0')}`,
	);
	return requireIntegerInRange('Attachment expiry bucket', bucket, -2_147_483_648, 2_147_483_647);
}

export function extendExpiry(currentExpiry: Date | null, newlyComputed: Date): Date {
	const newTimestamp = getValidTimestamp(newlyComputed, 'Computed attachment expiry');
	if (!currentExpiry) return newlyComputed;
	return getValidTimestamp(currentExpiry, 'Current attachment expiry') > newTimestamp ? currentExpiry : newlyComputed;
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
	const currentTimestamp = getValidTimestamp(currentExpiry, 'Current attachment expiry');
	const nowTimestamp = getValidTimestamp(now, 'Attachment renewal time');
	const remainingMs = currentTimestamp - nowTimestamp;
	if (remainingMs > thresholdDays * MS_PER_DAY) {
		return null;
	}
	const targetMs = nowTimestamp + windowDays * MS_PER_DAY;
	const cappedTargetMs = maxExpiry
		? Math.min(getValidTimestamp(maxExpiry, 'Maximum attachment expiry'), targetMs)
		: targetMs;
	if (cappedTargetMs <= currentTimestamp) {
		return null;
	}
	const target = new Date(cappedTargetMs);
	return getValidTimestamp(target, 'Renewed attachment expiry') > currentTimestamp ? target : null;
}
