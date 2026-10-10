// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_COMPACT_MAX_FRACTION_DIGITS,
	DEFAULT_NUMBER_FALLBACK,
	DEFAULT_NUMBER_LOCALE,
} from '@pkgs/number_utils/src/NumberConstants';
import {getNumberFormatter} from '@pkgs/number_utils/src/NumberFormatterCache';
import {parseNumberInput} from '@pkgs/number_utils/src/NumberParsing';
import type {
	CompactNumberFormatOptions,
	NumberFormatBaseOptions,
	NumberFormatOptions,
	NumberInput,
} from '@pkgs/number_utils/src/NumberTypes';

interface ResolvedNumberFormatBaseOptions {
	locale: string;
	fallbackValue: number;
}

interface CompactFormatOptionsInput {
	maximumFractionDigits?: number;
	minimumFractionDigits?: number;
}

function resolveBaseOptions(options?: NumberFormatBaseOptions): ResolvedNumberFormatBaseOptions {
	return {
		locale: options?.locale ?? DEFAULT_NUMBER_LOCALE,
		fallbackValue: options?.fallbackValue ?? DEFAULT_NUMBER_FALLBACK,
	};
}

function resolveNumberFormatOptions(optionsOrLocale: NumberFormatOptions | string | undefined): NumberFormatOptions {
	if (typeof optionsOrLocale === 'string') {
		return {locale: optionsOrLocale};
	}
	return optionsOrLocale ?? {};
}

function resolveCompactFormatOptions(
	optionsOrLocale: CompactNumberFormatOptions | string | undefined,
	maximumFractionDigits: number | undefined,
): CompactNumberFormatOptions {
	if (typeof optionsOrLocale === 'string') {
		return {
			locale: optionsOrLocale,
			maximumFractionDigits,
		};
	}
	if (optionsOrLocale === undefined) {
		if (maximumFractionDigits === undefined) {
			return {};
		}
		return {maximumFractionDigits};
	}
	if (maximumFractionDigits === undefined) {
		return optionsOrLocale;
	}
	return {
		...optionsOrLocale,
		maximumFractionDigits: optionsOrLocale.maximumFractionDigits ?? maximumFractionDigits,
	};
}

function formatNumberValue(
	value: NumberInput,
	resolvedOptions: ResolvedNumberFormatBaseOptions,
	numberFormatOptions: Intl.NumberFormatOptions = {},
): string {
	const parsedValue = parseNumberInput(value, resolvedOptions.fallbackValue);
	return getNumberFormatter(resolvedOptions.locale, numberFormatOptions).format(parsedValue);
}

function buildCompactFormatOptions(options: CompactFormatOptionsInput): Intl.NumberFormatOptions {
	const numberFormatOptions: Intl.NumberFormatOptions = {
		notation: 'compact',
		maximumFractionDigits: options.maximumFractionDigits ?? DEFAULT_COMPACT_MAX_FRACTION_DIGITS,
	};
	if (options.minimumFractionDigits !== undefined) {
		numberFormatOptions.minimumFractionDigits = options.minimumFractionDigits;
	}
	return numberFormatOptions;
}

export function formatNumber(value: NumberInput, locale?: string): string;
export function formatNumber(value: NumberInput, options?: NumberFormatOptions): string;
export function formatNumber(value: NumberInput, optionsOrLocale: NumberFormatOptions | string = {}): string {
	const options = resolveNumberFormatOptions(optionsOrLocale);
	const resolvedOptions = resolveBaseOptions(options);
	return formatNumberValue(value, resolvedOptions, options.numberFormatOptions);
}

export function formatCompactNumber(value: NumberInput, locale?: string, maximumFractionDigits?: number): string;
export function formatCompactNumber(value: NumberInput, options?: CompactNumberFormatOptions): string;
export function formatCompactNumber(
	value: NumberInput,
	optionsOrLocale: CompactNumberFormatOptions | string = {},
	maximumFractionDigits?: number,
): string {
	const options = resolveCompactFormatOptions(optionsOrLocale, maximumFractionDigits);
	const resolvedOptions = resolveBaseOptions(options);
	return formatNumberValue(value, resolvedOptions, buildCompactFormatOptions(options));
}
