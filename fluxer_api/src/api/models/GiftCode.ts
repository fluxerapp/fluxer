// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '../BrandedTypes';
import type {GiftCodeDurationType, GiftCodeRow} from '../database/types/PaymentTypes';
import {addMonthsClamp} from '../stripe/StripeUtils';

interface GiftCodeDuration {
	durationType: GiftCodeDurationType;
	durationQuantity: number;
}

const DURATION_TYPES = new Set<GiftCodeDurationType>(['days', 'weeks', 'months', 'years']);
const MILLISECONDS_PER_DAY = 24 * 60 * 60 * 1000;
const DAYS_PER_WEEK = 7;

function ensureDurationQuantity(value: number): number {
	if (!Number.isInteger(value) || value < 0) {
		throw new Error(`Gift code duration quantity must be a non-negative integer. Received: ${value}`);
	}
	return value;
}

function isGiftCodeDurationType(value: string): value is GiftCodeDurationType {
	return DURATION_TYPES.has(value as GiftCodeDurationType);
}

export function mapGiftDurationMonthsToFields(durationMonths: number): GiftCodeDuration {
	const normalisedDurationMonths = ensureDurationQuantity(durationMonths);
	if (normalisedDurationMonths !== 0 && normalisedDurationMonths % 12 === 0) {
		return {
			durationType: 'years',
			durationQuantity: normalisedDurationMonths / 12,
		};
	}
	return {
		durationType: 'months',
		durationQuantity: normalisedDurationMonths,
	};
}

function normaliseGiftCodeDuration(row: GiftCodeRow): GiftCodeDuration {
	const durationType = row.duration_type ?? null;
	const durationQuantity = row.duration_quantity ?? null;
	if (durationType !== null || durationQuantity !== null) {
		if (durationType === null || durationQuantity === null) {
			throw new Error('Gift code duration_type and duration_quantity must both be set when either is present');
		}
		if (!isGiftCodeDurationType(durationType)) {
			throw new Error(`Gift code duration_type is invalid: ${durationType}`);
		}
		return {
			durationType,
			durationQuantity: ensureDurationQuantity(durationQuantity),
		};
	}
	const durationMonths = row.duration_months;
	if (durationMonths === null || durationMonths === undefined) {
		throw new Error('Gift code duration is missing from both duration_type/duration_quantity and duration_months');
	}
	return mapGiftDurationMonthsToFields(durationMonths);
}

export function mapGiftCodeDurationToMonths(
	durationType: GiftCodeDurationType,
	durationQuantity: number,
): number | null {
	const normalisedDurationQuantity = ensureDurationQuantity(durationQuantity);
	switch (durationType) {
		case 'months':
			return normalisedDurationQuantity;
		case 'years':
			return normalisedDurationQuantity * 12;
		case 'days':
		case 'weeks':
			return null;
		default:
			throw new Error('Unsupported gift code duration type');
	}
}

export function addGiftCodeDuration(
	baseDate: Date,
	durationType: GiftCodeDurationType,
	durationQuantity: number,
): Date | null {
	const normalisedDurationQuantity = ensureDurationQuantity(durationQuantity);
	if (normalisedDurationQuantity === 0) {
		return null;
	}
	switch (durationType) {
		case 'days':
			return new Date(baseDate.getTime() + normalisedDurationQuantity * MILLISECONDS_PER_DAY);
		case 'weeks':
			return new Date(baseDate.getTime() + normalisedDurationQuantity * DAYS_PER_WEEK * MILLISECONDS_PER_DAY);
		case 'months':
			return addMonthsClamp(baseDate, normalisedDurationQuantity);
		case 'years':
			return addMonthsClamp(baseDate, normalisedDurationQuantity * 12);
		default:
			throw new Error('Unsupported gift code duration type');
	}
}

export class GiftCode {
	readonly code: string;
	readonly durationType: GiftCodeDurationType;
	readonly durationQuantity: number;
	readonly durationMonths: number | null;
	readonly createdAt: Date;
	readonly createdByUserId: UserID;
	readonly redeemedAt: Date | null;
	readonly redeemedByUserId: UserID | null;
	readonly stripePaymentIntentId: string | null;
	readonly visionarySequenceNumber: number | null;
	readonly checkoutSessionId: string | null;
	readonly revokedAt: Date | null;
	readonly version: number;

	constructor(row: GiftCodeRow) {
		const duration = normaliseGiftCodeDuration(row);
		this.code = row.code;
		this.durationType = duration.durationType;
		this.durationQuantity = duration.durationQuantity;
		this.durationMonths = mapGiftCodeDurationToMonths(duration.durationType, duration.durationQuantity);
		this.createdAt = row.created_at;
		this.createdByUserId = row.created_by_user_id as UserID;
		this.redeemedAt = row.redeemed_at ?? null;
		this.redeemedByUserId = row.redeemed_by_user_id ? (row.redeemed_by_user_id as UserID) : null;
		this.stripePaymentIntentId = row.stripe_payment_intent_id ?? null;
		this.visionarySequenceNumber = row.visionary_sequence_number ?? null;
		this.checkoutSessionId = row.checkout_session_id ?? null;
		this.revokedAt = row.revoked_at ?? null;
		this.version = row.version;
	}

	toRow(): GiftCodeRow {
		return {
			code: this.code,
			duration_months: this.durationMonths,
			duration_type: this.durationType,
			duration_quantity: this.durationQuantity,
			created_at: this.createdAt,
			created_by_user_id: this.createdByUserId,
			redeemed_at: this.redeemedAt,
			redeemed_by_user_id: this.redeemedByUserId,
			stripe_payment_intent_id: this.stripePaymentIntentId,
			visionary_sequence_number: this.visionarySequenceNumber,
			checkout_session_id: this.checkoutSessionId,
			revoked_at: this.revokedAt,
			version: this.version,
		};
	}
}
