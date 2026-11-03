// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	OPERATOR_PASS_CURRENCIES,
	OPERATOR_PASS_MAX_QUANTITY,
	type OperatorPassCurrency,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

export type OperatorPassPriceTable = Readonly<Record<OperatorPassCurrency, number>>;

export interface OperatorPassReleaseListWindow {
	cutoffAt: Date;
	publishBy: Date;
}

export type OperatorPassReleaseListTable = Readonly<Record<number, OperatorPassReleaseListWindow>>;

interface OperatorPassWithdrawalCountryRule {
	days: number;
	enabled: boolean;
}

export type OperatorPassWithdrawalCountryTable = Readonly<Record<string, OperatorPassWithdrawalCountryRule>>;

export const OPERATOR_PASS_WITHDRAWAL_RULE_VERSION = 'withdrawal-countries-2026-10-27';

const OPERATOR_PASS_DEFAULT_TRANSFER_LOCK_HOURS = 0;

const DEFAULT_WITHDRAWAL_COUNTRIES: OperatorPassWithdrawalCountryTable = {BR: {days: 7, enabled: true}};

const CURRENCY_WITHDRAWAL_COUNTRY: Readonly<Partial<Record<string, string>>> = {BRL: 'BR'};

export const OPERATOR_PASS_EARLY_BIRD_PRICES_MINOR: OperatorPassPriceTable = {
	USD: 19900,
	EUR: 19900,
	BRL: 102000,
	DKK: 149000,
	INR: 1910000,
	NOK: 215000,
	PLN: 76000,
	SEK: 225000,
	TRY: 971000,
};

function getOperatorPassStandardPriceEnvName(currency: OperatorPassCurrency): string {
	return `FLUXER_OPERATOR_PASS_STANDARD_PRICE_${currency}`;
}

export function parseOperatorPassStandardPrices(
	source: Partial<Record<string, number>> | undefined,
): OperatorPassPriceTable | null {
	const missing = OPERATOR_PASS_CURRENCIES.filter((currency) => source?.[currency.toLowerCase()] === undefined);
	if (missing.length === OPERATOR_PASS_CURRENCIES.length) {
		return null;
	}
	if (missing.length > 0) {
		throw new Error(
			`${missing.map(getOperatorPassStandardPriceEnvName).join(', ')} must be set, the standard price needs every currency or none`,
		);
	}
	const prices = {} as Record<OperatorPassCurrency, number>;
	for (const currency of OPERATOR_PASS_CURRENCIES) {
		const amount = source?.[currency.toLowerCase()];
		if (
			typeof amount !== 'number' ||
			!Number.isSafeInteger(amount) ||
			amount <= OPERATOR_PASS_EARLY_BIRD_PRICES_MINOR[currency]
		) {
			throw new Error(
				`${getOperatorPassStandardPriceEnvName(currency)} must be a whole amount in minor units above the early bird price`,
			);
		}
		prices[currency] = amount;
	}
	return prices;
}

function defaultReleaseListWindow(year: number): OperatorPassReleaseListWindow {
	return {
		cutoffAt: new Date(Date.UTC(year + 1, 0, 1) - 1),
		publishBy: new Date(Date.UTC(year + 1, 3, 1) - 1),
	};
}

export function parseOperatorPassReleaseLists(source: string | undefined): OperatorPassReleaseListTable {
	const trimmed = (source ?? '').trim();
	if (trimmed.length === 0) {
		return {};
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(trimmed);
	} catch {
		throw new Error(
			'FLUXER_OPERATOR_PASS_RELEASE_LISTS must be JSON such as {"2026":{"cutoff":"2026-12-15T23:59:59Z"}}',
		);
	}
	if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) {
		throw new Error('FLUXER_OPERATOR_PASS_RELEASE_LISTS must be a JSON object keyed by year');
	}
	const table: Record<number, OperatorPassReleaseListWindow> = {};
	for (const [key, value] of Object.entries(parsed as Record<string, unknown>)) {
		const year = Number(key);
		if (!Number.isInteger(year) || year < 2026 || year > 9999) {
			throw new Error(`FLUXER_OPERATOR_PASS_RELEASE_LISTS has an invalid year ${key}`);
		}
		const entry = (value ?? {}) as {cutoff?: unknown; publish_by?: unknown};
		const fallback = defaultReleaseListWindow(year);
		const cutoffAt = typeof entry.cutoff === 'string' ? new Date(entry.cutoff) : fallback.cutoffAt;
		const publishBy = typeof entry.publish_by === 'string' ? new Date(entry.publish_by) : fallback.publishBy;
		if (Number.isNaN(cutoffAt.getTime()) || Number.isNaN(publishBy.getTime())) {
			throw new Error(`FLUXER_OPERATOR_PASS_RELEASE_LISTS has an invalid date for ${year}`);
		}
		if (cutoffAt.getTime() > fallback.cutoffAt.getTime()) {
			throw new Error(
				`FLUXER_OPERATOR_PASS_RELEASE_LISTS cutoff for ${year} must be no later than 31 December ${year}`,
			);
		}
		if (publishBy.getTime() > fallback.publishBy.getTime() || publishBy.getTime() < cutoffAt.getTime()) {
			throw new Error(
				`FLUXER_OPERATOR_PASS_RELEASE_LISTS publish_by for ${year} must be after the cutoff and no later than 31 March ${year + 1}`,
			);
		}
		table[year] = {cutoffAt, publishBy};
	}
	return table;
}

export function getOperatorPassReleaseListWindow(
	table: OperatorPassReleaseListTable,
	year: number,
): OperatorPassReleaseListWindow {
	return table[year] ?? defaultReleaseListWindow(year);
}

export function parseOperatorPassTransferLockHours(value: number | undefined): number {
	if (value === undefined) {
		return OPERATOR_PASS_DEFAULT_TRANSFER_LOCK_HOURS;
	}
	if (!Number.isInteger(value) || value < 0 || value > 24 * 90) {
		throw new Error('FLUXER_OPERATOR_PASS_TRANSFER_LOCK_HOURS must be a whole number of hours between 0 and 2160');
	}
	return value;
}

export function parseOperatorPassWithdrawalCountries(source: string | undefined): OperatorPassWithdrawalCountryTable {
	const trimmed = (source ?? '').trim();
	if (trimmed.length === 0) {
		return DEFAULT_WITHDRAWAL_COUNTRIES;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(trimmed);
	} catch {
		throw new Error('FLUXER_OPERATOR_PASS_WITHDRAWAL_COUNTRIES must be JSON such as {"BR":{"days":7,"enabled":true}}');
	}
	if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) {
		throw new Error('FLUXER_OPERATOR_PASS_WITHDRAWAL_COUNTRIES must be a JSON object keyed by country code');
	}
	const table: Record<string, OperatorPassWithdrawalCountryRule> = {};
	for (const [key, value] of Object.entries(parsed as Record<string, unknown>)) {
		const country = key.trim().toUpperCase();
		const rule = (value ?? {}) as {days?: unknown; enabled?: unknown};
		if (
			!/^[A-Z]{2}$/u.test(country) ||
			typeof rule.days !== 'number' ||
			!Number.isInteger(rule.days) ||
			rule.days < 1 ||
			rule.days > 90
		) {
			throw new Error(`FLUXER_OPERATOR_PASS_WITHDRAWAL_COUNTRIES has an invalid rule for ${key}`);
		}
		table[country] = {days: rule.days, enabled: rule.enabled !== false};
	}
	return table;
}

const OPERATOR_PASS_LOCALIZED_CURRENCIES: ReadonlyArray<OperatorPassCurrency> = OPERATOR_PASS_CURRENCIES.filter(
	(currency) => currency !== 'USD' && currency !== 'EUR',
);

export function resolveOperatorPassDisabledCurrencies(
	enabled: Partial<Record<string, boolean>> | undefined,
): ReadonlySet<OperatorPassCurrency> {
	const disabled = new Set<OperatorPassCurrency>();
	for (const currency of OPERATOR_PASS_LOCALIZED_CURRENCIES) {
		if (enabled?.[currency.toLowerCase()] === false) {
			disabled.add(currency);
		}
	}
	return disabled;
}

const OPERATOR_PASS_DEFAULT_MAX_QUANTITY = 25;

export function parseOperatorPassMaxQuantity(value: number | undefined): number {
	if (value === undefined) {
		return OPERATOR_PASS_DEFAULT_MAX_QUANTITY;
	}
	if (!Number.isInteger(value) || value < 1 || value > OPERATOR_PASS_MAX_QUANTITY) {
		throw new Error(`FLUXER_OPERATOR_PASS_MAX_QUANTITY must be a whole number from 1 to ${OPERATOR_PASS_MAX_QUANTITY}`);
	}
	return value;
}

const OPERATOR_PASS_DEFAULT_WALL_TIER_MINIMUMS: ReadonlyArray<number> = [1, 2, 5, 10];

export function parseOperatorPassWallTiers(source: string | undefined): ReadonlyArray<number> {
	const trimmed = (source ?? '').trim();
	if (trimmed.length === 0) {
		return OPERATOR_PASS_DEFAULT_WALL_TIER_MINIMUMS;
	}
	const minimums = trimmed.split(',').map((value) => Number(value.trim()));
	const valid =
		minimums[0] === 1 &&
		minimums.every((value, index) => Number.isSafeInteger(value) && (index === 0 || value > minimums[index - 1]!));
	if (!valid) {
		throw new Error(
			'FLUXER_OPERATOR_PASS_WALL_TIERS must be rising whole numbers of passes starting at 1, such as 1,2,5,10',
		);
	}
	return minimums;
}

export function getOperatorPassWallTier(count: number, minimums: ReadonlyArray<number>): number {
	let tier = 1;
	for (const [index, minimum] of minimums.entries()) {
		if (count >= minimum) tier = index + 1;
	}
	return tier;
}

export interface OperatorPassWithdrawalRuleMatch {
	country: string;
	days: number;
}

export function resolveOperatorPassWithdrawalRule(
	table: OperatorPassWithdrawalCountryTable,
	countries: ReadonlyArray<string | null | undefined>,
	currency: string | null | undefined,
): OperatorPassWithdrawalRuleMatch | null {
	const candidates = [...countries, currency ? CURRENCY_WITHDRAWAL_COUNTRY[currency.toUpperCase()] : null];
	for (const candidate of candidates) {
		const country = candidate?.trim().toUpperCase();
		const rule = country ? table[country] : undefined;
		if (country && rule?.enabled) {
			return {country, days: rule.days};
		}
	}
	return null;
}
