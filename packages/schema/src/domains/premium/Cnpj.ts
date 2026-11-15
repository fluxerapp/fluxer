// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

const FIRST_DIGIT_WEIGHTS = [5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2];
const SECOND_DIGIT_WEIGHTS = [6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2];

function checkDigit(base: string, weights: ReadonlyArray<number>): number {
	const sum = weights.reduce((total, weight, index) => total + (base.charCodeAt(index) - 48) * weight, 0);
	const remainder = sum % 11;
	return remainder < 2 ? 0 : 11 - remainder;
}

export function normalizeCnpj(value: string): string | null {
	const cnpj = value.toUpperCase().replace(/[\s./-]/g, '');
	if (!/^[0-9A-Z]{12}[0-9]{2}$/.test(cnpj) || /^(.)\1{13}$/.test(cnpj)) {
		return null;
	}
	const first = checkDigit(cnpj.slice(0, 12), FIRST_DIGIT_WEIGHTS);
	const second = checkDigit(cnpj.slice(0, 12) + String(first), SECOND_DIGIT_WEIGHTS);
	return cnpj.endsWith(`${first}${second}`) ? cnpj : null;
}

export const CnpjSchema = z
	.string()
	.max(32)
	.refine((value) => normalizeCnpj(value) !== null, {message: 'Enter a valid CNPJ'})
	.transform((value) => normalizeCnpj(value)!)
	.describe(
		'CNPJ of the Brazilian company the purchase is for, with or without punctuation. Letters are allowed in the first 12 characters. Used only with is_business when the purchase falls under Brazilian law',
	);
