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

import validator from 'validator';
import {z} from 'zod';
import {Config} from '~/Config';

const RTL_OVERRIDE_REGEX = /\u202E/g;
// biome-ignore lint/suspicious/noControlCharactersInRegex: this is fine
const FORM_FEED_REGEX = /\u000C/g;
const EMAIL_LOCAL_REGEX = /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+$/;
const DISCRIMINATOR_REGEX = /^\d{1,4}$/;
const FILENAME_SAFE_REGEX = /^[\p{L}\p{N}\p{M}_.-]+$/u;
const VANITY_URL_REGEX = /^[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$/;
const ISO_TIMESTAMP_REGEX = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:?\d{2})?$/;
const WHITESPACE_REGEX = /\s+/g;
const MULTIPLE_HYPHENS_REGEX = /-{2,}/g;
const NON_FILENAME_CHARS_REGEX = /[^\p{L}\p{N}\p{M}_.-]/gu;

const PROTOCOLS = ['http', 'https'];
const TRUE_VALUES = ['true', 'True', '1'];

const DISALLOWED_CHARS = new Set(' !"#$%&\'()*+,/:;<=>?@[\\]^`{|}~');

const URL_VALIDATOR_OPTIONS = {
	require_protocol: true,
	require_host: true,
	disallow_auth: true,
	allow_trailing_dot: false,
	allow_protocol_relative_urls: false,
	allow_fragments: false,
	validate_length: true,
	protocols: ['http', 'https'] as Array<string>,
} as const;

export const PHONE_E164_REGEX = /^\+[1-9]\d{1,14}$/;
const PHONE_E164_ERROR_MESSAGE = 'Phone number must be in E.164 format (e.g., +1234567890)';

const normalizeString = (value: string): string => {
	return value.replace(RTL_OVERRIDE_REGEX, '').replace(FORM_FEED_REGEX, '').trim();
};

const isValidBase64 = (value: string): boolean => {
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
};

const C0_C1_CTRL_REGEX =
	// biome-ignore lint/suspicious/noControlCharactersInRegex: this is fine
	/[\u0000-\u0008\u000B\u000C\u000E-\u001F\u007F\u0080-\u009F]/g;

const SURROGATES_REGEX = /[\uD800-\uDFFF]/g;

const JOIN_CONTROLS_REGEX = /(?:\u200C|\u200D)/g;

const WJ_BOM_REGEX = /(?:\u2060|\uFEFF)/g;

const BIDI_CTRL_REGEX = /[\u200E\u200F\u202A-\u202E\u2066-\u2069]/g;

const MISC_INVISIBLES_REGEX = /[\u00AD\u180E\uFFFE\uFFFF]/g;

const TAG_CHARS_REGEX = /[\u{E0000}-\u{E007F}]/gu;

const VARIATION_SELECTORS_BASIC = /[\uFE00-\uFE0F]/g;
const VARIATION_SELECTORS_IDEOGRAPHIC = /[\u{E0100}-\u{E01EF}]/gu;

const UNICODE_SPACES_REGEX = /[\s\u00A0\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000]/g;

const normalizeWhitespace = (s: string): string => s.replace(UNICODE_SPACES_REGEX, ' ').replace(/\s+/g, ' ').trim();

const stripInvisibles = (s: string): string =>
	s
		.replace(C0_C1_CTRL_REGEX, '')
		.replace(JOIN_CONTROLS_REGEX, '')
		.replace(WJ_BOM_REGEX, '')
		.replace(BIDI_CTRL_REGEX, '')
		.replace(MISC_INVISIBLES_REGEX, '')
		.replace(TAG_CHARS_REGEX, '');

const stripVariationSelectors = (s: string): string =>
	s.replace(VARIATION_SELECTORS_BASIC, '').replace(VARIATION_SELECTORS_IDEOGRAPHIC, '');

const sanitizeUsername = (value: string): string => {
	let s = normalizeString(value);
	s = s.replace(SURROGATES_REGEX, '');
	s = stripInvisibles(s);
	s = stripVariationSelectors(s);
	s = normalizeWhitespace(s);
	return s;
};

const sanitizeChannelName = (value: string): string => {
	let s = normalizeString(value);
	s = stripInvisibles(s);
	s = normalizeWhitespace(s);
	return s;
};

export const EmailType = z
	.email('Invalid email format')
	.transform(normalizeString)
	.refine((value) => value.length >= 1 && value.length <= 254, 'Email length must be between 1 and 254 characters')
	.refine((value) => {
		const atIndex = value.indexOf('@');
		if (atIndex === -1) return false;
		const local = value.slice(0, atIndex);
		return EMAIL_LOCAL_REGEX.test(local);
	}, 'Invalid email local part');

export const DiscriminatorType = z
	.string()
	.regex(DISCRIMINATOR_REGEX, 'Discriminator must be 1-4 digits')
	.superRefine((value, ctx) => {
		const num = Number.parseInt(value, 10);
		if (num < 0 || num > 9999) {
			ctx.addIssue({
				code: 'custom',
				message: 'Discriminator must be between 0 and 9999',
			});
			return z.NEVER;
		}
	})
	.transform((value) => Number.parseInt(value, 10));

const FLUXER_TAG_REGEX = /^[a-zA-Z0-9_]+$/;

export const UsernameType = z
	.string()
	.transform((value) => value.trim())
	.refine((value) => value.length >= 1 && value.length <= 32, 'Username length must be between 1 and 32 characters')
	.refine(
		(value) => FLUXER_TAG_REGEX.test(value),
		'Username can only contain Latin letters (a-z, A-Z), numbers (0-9), and underscores (_)',
	)
	.refine((value) => {
		const lowerValue = value.toLowerCase();
		return lowerValue !== 'everyone' && lowerValue !== 'here';
	}, 'Username cannot be "everyone" or "here"')
	.refine((value) => {
		const lowerValue = value.toLowerCase();
		return !lowerValue.includes('fluxer') && !lowerValue.includes('system message');
	}, 'Username cannot contain "fluxer" or "system message"');

export const GlobalNameType = z
	.string()
	.transform(sanitizeUsername)
	.refine((value) => value.length >= 1 && value.length <= 32, 'Global name length must be between 1 and 32 characters')
	.refine((value) => {
		const lowerValue = value.toLowerCase();
		return lowerValue !== 'everyone' && lowerValue !== 'here';
	}, 'Global name cannot be "everyone" or "here"')
	.refine((value) => {
		const lowerValue = value.toLowerCase();
		return !lowerValue.includes('system message');
	}, 'Global name cannot contain "system message"');

const createUrlSchema = (allowFragments: boolean) => {
	return z
		.string()
		.transform(normalizeString)
		.refine((value) => value.length >= 1 && value.length <= 2048, 'URL length must be between 1 and 2048 characters')
		.refine((value) => {
			if (!value.startsWith('http://') && !value.startsWith('https://')) {
				return false;
			}
			try {
				const url = new URL(value);
				return PROTOCOLS.includes(url.protocol.slice(0, -1));
			} catch {
				return false;
			}
		}, 'Invalid URL format')
		.refine(
			(value) =>
				validator.isURL(value, {
					...URL_VALIDATOR_OPTIONS,
					allow_fragments: allowFragments,
					require_tld: Config.nodeEnv !== 'development',
				}),
			'Invalid URL format',
		);
};

export const URLType = createUrlSchema(false);
export const URLWithFragmentType = createUrlSchema(true);

export const AttachmentURLType = z
	.string()
	.transform(normalizeString)
	.refine((value) => value.length >= 1 && value.length <= 2048, 'URL length must be between 1 and 2048 characters')
	.refine((value) => {
		if (value.startsWith('attachment://')) {
			const filename = value.slice(13);
			if (filename.length === 0) {
				return false;
			}
			return FILENAME_SAFE_REGEX.test(filename);
		}

		if (!value.startsWith('http://') && !value.startsWith('https://')) {
			return false;
		}
		try {
			const url = new URL(value);
			return PROTOCOLS.includes(url.protocol.slice(0, -1));
		} catch {
			return false;
		}
	}, 'Invalid URL format or attachment reference')
	.refine((value) => {
		if (value.startsWith('attachment://')) {
			return true;
		}
		return validator.isURL(value, {
			...URL_VALIDATOR_OPTIONS,
			require_tld: Config.nodeEnv !== 'development',
		});
	}, 'Invalid URL format');

const WINDOWS_RESERVED_NAMES = /^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(\.|$)/i;

function normalizeFilename(value: string): string {
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

export const FilenameType = z
	.string()
	.refine((value) => value.length >= 1 && value.length <= 255, 'Filename length must be between 1 and 255 characters')
	.transform(normalizeFilename)
	.refine((value) => value.length >= 1, 'Filename cannot be empty after normalization')
	.refine((value) => FILENAME_SAFE_REGEX.test(value), 'Filename contains invalid characters');

export const Int64Type = z.union([z.string(), z.number().int()]).transform((value, ctx) => {
	if (typeof value === 'number' && !Number.isSafeInteger(value)) {
		ctx.addIssue({
			code: 'custom',
			message: 'Invalid integer format',
		});
		return z.NEVER;
	}

	const normalized = typeof value === 'number' ? value.toString() : value;
	const trimmed = normalized.trim();

	try {
		const bigInt = BigInt(trimmed);
		if (bigInt < -9223372036854775808n || bigInt > 9223372036854775807n) {
			ctx.addIssue({
				code: 'custom',
				message: 'Integer out of valid int64 range',
			});
			return z.NEVER;
		}
		return bigInt;
	} catch {
		ctx.addIssue({
			code: 'custom',
			message: 'Invalid integer format',
		});
		return z.NEVER;
	}
});

export const ColorType = z
	.number()
	.int()
	.min(0x000000, 'Color value must be at least 0x000000')
	.max(0xffffff, 'Color value must not exceed 0xffffff');

export const Int32Type = z.number().int().min(0).max(2147483647);

const INTEGER_STRING_REGEX = /^[+-]?\d+$/;

const coerceNumericStringToNumber = (value: unknown): unknown => {
	if (typeof value !== 'string') {
		return value;
	}

	const trimmed = value.trim();
	if (trimmed.length === 0 || !INTEGER_STRING_REGEX.test(trimmed)) {
		return value;
	}

	const parsed = Number(trimmed);
	return Number.isNaN(parsed) ? value : parsed;
};

export const coerceNumberFromString = <T extends z.ZodNumber>(schema: T) =>
	z.preprocess((value) => coerceNumericStringToNumber(value), schema);

export const QueryBooleanType = z
	.string()
	.trim()
	.optional()
	.default('false')
	.transform((value) => TRUE_VALUES.includes(value));

export const createQueryIntegerType = ({defaultValue = 0, minValue = 0, maxValue = 2147483647} = {}) =>
	z
		.string()
		.trim()
		.optional()
		.default(defaultValue.toString())
		.superRefine((value, ctx) => {
			const num = Number.parseInt(value, 10);
			if (!Number.isInteger(num) || num < minValue || num > maxValue) {
				ctx.addIssue({
					code: 'custom',
					message: `Invalid integer: must be between ${minValue} and ${maxValue}`,
				});
				return z.NEVER;
			}
		})
		.transform((value) => Number.parseInt(value, 10));

export const DateTimeType = z.union([
	z
		.string()
		.regex(ISO_TIMESTAMP_REGEX, 'Must be a valid ISO timestamp')
		.superRefine((value, ctx) => {
			const date = new Date(value);
			if (Number.isNaN(date.getTime())) {
				ctx.addIssue({
					code: 'custom',
					message: 'Invalid date',
				});
				return z.NEVER;
			}
		})
		.transform((value) => new Date(value)),
	z
		.number()
		.int()
		.min(0)
		.max(8640000000000000)
		.superRefine((value, ctx) => {
			const date = new Date(value);
			if (Number.isNaN(date.getTime())) {
				ctx.addIssue({
					code: 'custom',
					message: 'Invalid date',
				});
				return z.NEVER;
			}
		})
		.transform((value) => new Date(value)),
]);

export const WebhookNameType = z
	.string()
	.transform(normalizeString)
	.refine(
		(value) => value.length >= 1 && value.length <= 80,
		'Webhook name length must be between 1 and 80 characters',
	);

export const PasswordType = z
	.string()
	.transform(normalizeString)
	.refine((value) => value.length >= 8 && value.length <= 256, 'Password length must be between 8 and 256 characters');

export const PhoneNumberType = z
	.string()
	.transform(normalizeString)
	.refine((value) => PHONE_E164_REGEX.test(value), PHONE_E164_ERROR_MESSAGE);

export const createStringType = (minLength = 1, maxLength = 256) =>
	z
		.string()
		.transform(normalizeString)
		.refine(
			(value: string) => value.length >= minLength && value.length <= maxLength,
			minLength === maxLength
				? `String must be exactly ${minLength} characters`
				: `String length must be between ${minLength} and ${maxLength} characters`,
		);

export const createUnboundedStringType = () => z.string().transform(normalizeString);

export const createBase64StringType = (minLength = 1, maxLength = 256) =>
	z
		.string()
		.superRefine((value, ctx) => {
			const normalized = normalizeString(value);
			const commaIndex = normalized.indexOf(',');
			const base64 = commaIndex !== -1 ? normalized.slice(commaIndex + 1) : normalized;

			if (base64.length < minLength || base64.length > maxLength) {
				ctx.addIssue({
					code: 'custom',
					message: `Base64 string length must be between ${minLength} and ${maxLength} characters`,
				});
				return z.NEVER;
			}

			if (base64.length < 1 || !isValidBase64(base64)) {
				ctx.addIssue({
					code: 'custom',
					message: 'Value must be a valid base64-encoded string',
				});
				return z.NEVER;
			}
		})
		.transform((value) => {
			const normalized = normalizeString(value);
			const commaIndex = normalized.indexOf(',');
			return commaIndex !== -1 ? normalized.slice(commaIndex + 1) : normalized;
		});

export const ChannelNameType = z
	.string()
	.superRefine((value, ctx) => {
		const normalized = normalizeString(value);
		const processed =
			normalized
				.toLowerCase()
				.replace(WHITESPACE_REGEX, '-')
				.split('')
				.filter((char) => !DISALLOWED_CHARS.has(char))
				.join('') || '-';
		if (processed.length < 1) {
			ctx.addIssue({
				code: 'custom',
				message: 'Channel name cannot be empty after normalization',
			});
			return z.NEVER;
		}
	})
	.transform((value) => {
		const normalized = normalizeString(value);
		return (
			normalized
				.toLowerCase()
				.replace(WHITESPACE_REGEX, '-')
				.split('')
				.filter((char) => !DISALLOWED_CHARS.has(char))
				.join('') || '-'
		);
	})
	.refine(
		(value) => value.length >= 1 && value.length <= 100,
		'Channel name length must be between 1 and 100 characters',
	);

export const GeneralChannelNameType = z
	.string()
	.transform((value) => {
		let sanitized = sanitizeChannelName(value);
		sanitized = sanitized.replace(WHITESPACE_REGEX, ' ');
		return sanitized;
	})
	.pipe(
		z
			.string()
			.refine((v) => v.trim().length > 0, 'Name cannot be empty after normalization')
			.min(1, 'Name length must be between 1 and 100 characters')
			.max(100, 'Name length must be between 1 and 100 characters'),
	);

export const VanityURLCodeType = z
	.string()
	.superRefine((value, ctx) => {
		const normalized = normalizeString(value);
		const processed = normalized.toLowerCase().replace(WHITESPACE_REGEX, '-').replace(MULTIPLE_HYPHENS_REGEX, '-');
		if (!VANITY_URL_REGEX.test(processed)) {
			ctx.addIssue({
				code: 'custom',
				message: 'Vanity URL can only contain lowercase letters (a-z), digits (0-9), and hyphens (-)',
			});
			return z.NEVER;
		}
	})
	.transform((value) => {
		const normalized = normalizeString(value);
		return normalized.toLowerCase().replace(WHITESPACE_REGEX, '-').replace(MULTIPLE_HYPHENS_REGEX, '-');
	})
	.refine(
		(value) => value.length >= 2 && value.length <= 32,
		'Vanity URL code length must be between 2 and 32 characters',
	);

const AUDIT_LOG_REASON_MAX_LENGTH = 512;

export const AuditLogReasonType = z
	.string()
	.nullable()
	.optional()
	.transform((value) => {
		if (!value || value.trim().length === 0) {
			return null;
		}
		const normalized = normalizeString(value);
		if (normalized.length < 1 || normalized.length > AUDIT_LOG_REASON_MAX_LENGTH) {
			return null;
		}
		return normalized;
	});

export const SudoVerificationSchema = z.object({
	password: PasswordType.optional(),
	mfa_method: z.enum(['totp', 'sms', 'webauthn']).optional(),
	mfa_code: createStringType(1, 32).optional(),
	webauthn_response: z.any().optional(),
	webauthn_challenge: createStringType().optional(),
});

export {z};
