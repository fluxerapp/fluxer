// SPDX-License-Identifier: AGPL-3.0-or-later

export type NumberInput = number | string | null | undefined;

export interface NumberFormatBaseOptions {
	locale?: string;
	fallbackValue?: number;
}

export interface NumberFormatOptions extends NumberFormatBaseOptions {
	numberFormatOptions?: Intl.NumberFormatOptions;
}

export interface CompactNumberFormatOptions extends NumberFormatBaseOptions {
	maximumFractionDigits?: number;
	minimumFractionDigits?: number;
}
