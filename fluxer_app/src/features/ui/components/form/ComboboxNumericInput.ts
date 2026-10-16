// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';

const DIGIT_GROUP_SEPARATOR = /(?<=\d)[,.'\u2019\s\u00a0\u202f](?=\d{3}(?!\d))/g;

export function parseNumericInput(inputValue: string): number | undefined {
	const numericMatch = inputValue
		.trim()
		.replace(DIGIT_GROUP_SEPARATOR, '')
		.match(/([0-9]+(?:[.,][0-9]+)?)/);
	if (!numericMatch) return undefined;
	const parsedValue = Number(numericMatch[1].replace(',', '.'));
	return Number.isFinite(parsedValue) ? parsedValue : undefined;
}

export function resolveNearestNumericOption(
	inputValue: string,
	options: ReadonlyArray<ComboboxOption<number>>,
): number | undefined {
	const parsedValue = parseNumericInput(inputValue);
	if (parsedValue === undefined) return undefined;
	return options.reduce((nearest, option) =>
		Math.abs(option.value - parsedValue) < Math.abs(nearest.value - parsedValue) ? option : nearest,
	).value;
}
