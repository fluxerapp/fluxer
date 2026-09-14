// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	normalizeString,
	withOpenApiType,
	withStringLengthRangeValidation,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const WHITESPACE_REGEX = /\s+/g;
const NON_FILENAME_CHARS_REGEX = /[^\p{L}\p{N}\p{M}_.-]/gu;
const WINDOWS_RESERVED_NAMES = /^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(\.|$)/i;
const BASE64_ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

export function isValidBase64(value: string): boolean {
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
	if (padding === 0) {
		return true;
	}
	const finalValue = BASE64_ALPHABET.indexOf(value.charAt(boundary - 1));
	return padding === 1 ? finalValue % 4 === 0 : finalValue % 16 === 0;
}

export function normalizeFilename(value: string): string {
	let normalized = normalizeString(value);
	normalized = normalized.replaceAll(String.fromCharCode(0), '');
	normalized = normalized.replace(/[/\\]/g, '_');
	normalized = normalized.replace(/\.{2,}/g, '.');
	normalized = normalized.replace(/[<>:"|?*]/g, '');
	if (WINDOWS_RESERVED_NAMES.test(normalized)) {
		normalized = `_${normalized}`;
	}
	normalized = normalized.replace(WHITESPACE_REGEX, '_');
	normalized = normalized.replace(NON_FILENAME_CHARS_REGEX, '');
	normalized = normalized.replace(/\.\./g, '_');
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
	.pipe(z.string());

export function base64LengthForBytes(maxBytes: number): number {
	return 4 * Math.ceil(maxBytes / 3);
}

export function createBase64StringType(minLength = 1, maxLength = 256) {
	return withOpenApiType(
		z
			.string()
			.overwrite((value) => {
				const normalized = normalizeString(value);
				const commaIndex = normalized.indexOf(',');
				return commaIndex !== -1 ? normalized.slice(commaIndex + 1) : normalized;
			})
			.superRefine((base64, ctx) => {
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
			}),
		'Base64ImageType',
	);
}
