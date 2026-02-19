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

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	normalizeString,
	withOpenApiType,
	withStringLengthRangeValidation,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const WHITESPACE_REGEX = /\s+/g;
const NON_FILENAME_CHARS_REGEX = /[^\p{L}\p{N}\p{M}_.-]/gu;
const FILENAME_SAFE_REGEX = /^[\p{L}\p{N}\p{M}_.-]+$/u;
const WINDOWS_RESERVED_NAMES = /^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(\.|$)/i;

function isValidBase64(value: string): boolean {
	if (value.length % 4 !== 0) {
		return false;
	}

	let padding = 0;
	for (let i = value.length - 1; i >= 0; i--) {
		if (value.charCodeAt(i) !== 61) {
			break;
		}
		padding++;
	}

	if (padding > 2) {
		return false;
	}

	const boundary = value.length - padding;

	for (let i = 0; i < boundary; i++) {
		const code = value.charCodeAt(i);
		const isUpper = code >= 65 && code <= 90;
		const isLower = code >= 97 && code <= 122;
		const isDigit = code >= 48 && code <= 57;
		const isPlus = code === 43;
		const isSlash = code === 47;

		if (!(isUpper || isLower || isDigit || isPlus || isSlash)) {
			return false;
		}
	}

	for (let i = boundary; i < value.length; i++) {
		if (value.charCodeAt(i) !== 61) {
			return false;
		}
	}

	try {
		const decoded = Buffer.from(value, 'base64');
		if (decoded.length === 0) {
			return value === '';
		}

		return decoded.toString('base64') === value;
	} catch {
		return false;
	}
}

export function normalizeFilename(value: string): string {
	let normalized = normalizeString(value);

	// biome-ignore lint/suspicious/noControlCharactersInRegex: null byte filtering is intentional for security
	normalized = normalized.replace(/\x00/g, '');

	normalized = normalized.replace(/[/\\]/g, '_');

	normalized = normalized.replace(/\.{2,}/g, '_');

	while (normalized.includes('..')) {
		normalized = normalized.replace(/\.\./g, '_');
	}

	normalized = normalized.replace(/[<>:"|?*]/g, '');

	if (WINDOWS_RESERVED_NAMES.test(normalized)) {
		normalized = `_${normalized}`;
	}

	normalized = normalized.replace(WHITESPACE_REGEX, '_');

	normalized = normalized.replace(NON_FILENAME_CHARS_REGEX, '');

	normalized = normalized.replace(/\.\./g, '_');
	normalized = normalized.replace(/[/\\]/g, '_');

	if (!normalized || /^[._]+$/.test(normalized)) {
		normalized = 'unnamed';
	}

	return normalized;
}

export const FilenameType = withStringLengthRangeValidation(
	z.string(),
	1,
	255,
	ValidationErrorCodes.FILENAME_LENGTH_INVALID,
)
	.transform(normalizeFilename)
	.refine((value) => value.length >= 1, ValidationErrorCodes.FILENAME_EMPTY_AFTER_NORMALIZATION)
	.refine((value) => FILENAME_SAFE_REGEX.test(value), ValidationErrorCodes.FILENAME_INVALID_CHARACTERS);

export function createBase64StringType(minLength = 1, maxLength = 256) {
	return withOpenApiType(
		z
			.string()
			.superRefine((value, ctx) => {
				const normalized = normalizeString(value);
				const commaIndex = normalized.indexOf(',');
				const base64 = commaIndex !== -1 ? normalized.slice(commaIndex + 1) : normalized;

				if (base64.length < minLength || base64.length > maxLength) {
					ctx.addIssue({
						code: 'custom',
						message: ValidationErrorCodes.BASE64_LENGTH_INVALID,
						params: {min: minLength, maxLength},
					});
					return z.NEVER;
				}

				if (base64.length < 1 || !isValidBase64(base64)) {
					ctx.addIssue({
						code: 'custom',
						message: ValidationErrorCodes.INVALID_BASE64_FORMAT,
					});
					return z.NEVER;
				}
			})
			.transform((value) => {
				const normalized = normalizeString(value);
				const commaIndex = normalized.indexOf(',');
				return commaIndex !== -1 ? normalized.slice(commaIndex + 1) : normalized;
			}),
		'Base64ImageType',
	);
}
