// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Gift, GiftDurationType} from '@app/features/gift/commands/GiftCommands';
import {getPremiumProductName} from '@app/features/premium/utils/PremiumUtils';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const VISIONARY_LIFETIME_DESCRIPTOR = msg({
	message: 'Visionary (lifetime {premiumProductName})',
	comment: 'Gift duration label for a lifetime Visionary benefit that includes the premium product name.',
});
const OTHER_OF_DESCRIPTOR = msg({
	message: '{durationQuantity, plural, one {# day} other {# days}} of {premiumProductName}',
	comment: 'Gift duration label. PREMIUM_PRODUCT_NAME is the paid plan name.',
});
const OTHER_OF_2_DESCRIPTOR = msg({
	message: '{durationQuantity, plural, one {# week} other {# weeks}} of {premiumProductName}',
	comment: 'Gift duration label. PREMIUM_PRODUCT_NAME is the paid plan name.',
});
const OTHER_OF_3_DESCRIPTOR = msg({
	message: '{durationQuantity, plural, one {# month} other {# months}} of {premiumProductName}',
	comment: 'Gift duration label. PREMIUM_PRODUCT_NAME is the paid plan name.',
});
const OTHER_OF_4_DESCRIPTOR = msg({
	message: '{durationQuantity, plural, one {# year} other {# years}} of {premiumProductName}',
	comment: 'Gift duration label. PREMIUM_PRODUCT_NAME is the paid plan name.',
});
const LIFETIME_DESCRIPTOR = msg({
	message: 'Lifetime {premiumProductName}',
	comment: 'Gift duration label for a lifetime premium gift.',
});

interface GiftDurationTextConfig {
	lifetime: string;
	days: (durationQuantity: number) => string;
	weeks: (durationQuantity: number) => string;
	months: (durationQuantity: number) => string;
	years: (durationQuantity: number) => string;
}

export interface GiftDurationPayload {
	duration_type: GiftDurationType;
	duration_quantity: number;
}

function resolveGiftDuration(gift: Gift | GiftDurationPayload): {
	durationType: GiftDurationType;
	durationQuantity: number;
} {
	return {
		durationType: gift.duration_type,
		durationQuantity: gift.duration_quantity,
	};
}

export function formatGiftDurationText(
	durationType: GiftDurationType,
	durationQuantity: number,
	config: GiftDurationTextConfig,
): string {
	if (durationQuantity === 0) {
		return config.lifetime;
	}
	switch (durationType) {
		case 'days':
			return config.days(durationQuantity);
		case 'weeks':
			return config.weeks(durationQuantity);
		case 'months':
			return config.months(durationQuantity);
		case 'years':
			return config.years(durationQuantity);
		default:
			throw new Error('Unsupported gift duration type');
	}
}

export function getPlutoniumDurationConfig(i18n: I18n): GiftDurationTextConfig {
	return {
		lifetime: i18n._(VISIONARY_LIFETIME_DESCRIPTOR, {premiumProductName: getPremiumProductName()}),
		days: (durationQuantity: number) =>
			i18n._(OTHER_OF_DESCRIPTOR, {durationQuantity, premiumProductName: getPremiumProductName()}),
		weeks: (durationQuantity: number) =>
			i18n._(OTHER_OF_2_DESCRIPTOR, {durationQuantity, premiumProductName: getPremiumProductName()}),
		months: (durationQuantity: number) =>
			i18n._(OTHER_OF_3_DESCRIPTOR, {durationQuantity, premiumProductName: getPremiumProductName()}),
		years: (durationQuantity: number) =>
			i18n._(OTHER_OF_4_DESCRIPTOR, {durationQuantity, premiumProductName: getPremiumProductName()}),
	};
}

export function getPremiumDurationConfig(i18n: I18n): GiftDurationTextConfig {
	return {
		lifetime: i18n._(LIFETIME_DESCRIPTOR, {premiumProductName: getPremiumProductName()}),
		days: (durationQuantity: number) =>
			i18n._(OTHER_OF_DESCRIPTOR, {durationQuantity, premiumProductName: getPremiumProductName()}),
		weeks: (durationQuantity: number) =>
			i18n._(OTHER_OF_2_DESCRIPTOR, {durationQuantity, premiumProductName: getPremiumProductName()}),
		months: (durationQuantity: number) =>
			i18n._(OTHER_OF_3_DESCRIPTOR, {durationQuantity, premiumProductName: getPremiumProductName()}),
		years: (durationQuantity: number) =>
			i18n._(OTHER_OF_4_DESCRIPTOR, {durationQuantity, premiumProductName: getPremiumProductName()}),
	};
}

export function getGiftDurationText(i18n: I18n, gift: Gift | GiftDurationPayload): string {
	const duration = resolveGiftDuration(gift);
	return formatGiftDurationText(duration.durationType, duration.durationQuantity, getPlutoniumDurationConfig(i18n));
}

export function getPremiumGiftDurationText(i18n: I18n, gift: Gift | GiftDurationPayload): string {
	const duration = resolveGiftDuration(gift);
	return formatGiftDurationText(duration.durationType, duration.durationQuantity, getPremiumDurationConfig(i18n));
}

export function extractGiftCode(input: string): string {
	const trimmed = input.trim();
	const lastSlashIndex = trimmed.lastIndexOf('/');
	if (lastSlashIndex === -1) {
		return trimmed;
	}
	return trimmed.slice(lastSlashIndex + 1);
}
