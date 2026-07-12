// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ChannelOverwriteTypes,
	ChannelOverwriteTypesDescriptions,
	ChannelTypes,
} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	createInt32EnumType,
	createNamedLiteralUnion,
	MAX_STRING_PROCESSING_LENGTH,
	normalizeString,
	normalizeWhitespace,
	stripInvisibles,
	withOpenApiType,
	withStringLengthRangeValidation,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ChannelTypeSchema = withOpenApiType(
	createInt32EnumType(
		[
			[ChannelTypes.GUILD_TEXT, 'GUILD_TEXT', 'A text channel within a guild'],
			[ChannelTypes.DM, 'DM', 'A direct message between users'],
			[ChannelTypes.GUILD_VOICE, 'GUILD_VOICE', 'A voice channel within a guild'],
			[ChannelTypes.GROUP_DM, 'GROUP_DM', 'A group direct message between users'],
			[ChannelTypes.GUILD_CATEGORY, 'GUILD_CATEGORY', 'A category that contains channels'],
			[ChannelTypes.GUILD_THREAD, 'GUILD_THREAD', 'A thread sub-channel within a guild text channel'],
			[ChannelTypes.GUILD_LINK, 'GUILD_LINK', 'A link channel for external resources'],
			[ChannelTypes.DM_PERSONAL_NOTES, 'DM_PERSONAL_NOTES', 'Personal notes DM channel'],
		],
		'The type of the channel',
	),
	'ChannelType',
);
export const ChannelOverwriteTypeSchema = withOpenApiType(
	createNamedLiteralUnion(
		[
			[ChannelOverwriteTypes.ROLE, 'ROLE', ChannelOverwriteTypesDescriptions.ROLE],
			[ChannelOverwriteTypes.MEMBER, 'MEMBER', ChannelOverwriteTypesDescriptions.MEMBER],
		] as const,
		'The type of entity the overwrite applies to',
	),
	'ChannelOverwriteType',
);
const WHITESPACE_REGEX = /\s+/g;
const MULTIPLE_HYPHENS_REGEX = /-{2,}/g;
const VANITY_URL_REGEX = /^[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$/;
const DISALLOWED_CHARS = new Set(' !"#$%&\'()*+,/:;<=>?@[\\]^`{|}~');

function sanitizeChannelName(value: string): string {
	if (value.length > MAX_STRING_PROCESSING_LENGTH) {
		throw new Error(ValidationErrorCodes.STRING_LENGTH_INVALID);
	}
	let s = normalizeString(value);
	s = stripInvisibles(s);
	s = normalizeWhitespace(s);
	return s;
}

export const ChannelNameType = z
	.string()
	.superRefine((value, ctx) => {
		if (value.length > MAX_STRING_PROCESSING_LENGTH) {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.STRING_LENGTH_INVALID,
				params: {min: 1, max: 100},
			});
			return z.NEVER;
		}
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
				message: ValidationErrorCodes.CHANNEL_NAME_EMPTY_AFTER_NORMALIZATION,
			});
			return z.NEVER;
		}
	})
	.transform((value) => {
		if (value.length > MAX_STRING_PROCESSING_LENGTH) {
			throw new Error(ValidationErrorCodes.STRING_LENGTH_INVALID);
		}
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
	.pipe(withStringLengthRangeValidation(z.string(), 1, 100, ValidationErrorCodes.STRING_LENGTH_INVALID));
export const GeneralChannelNameType = z
	.string()
	.superRefine((value, ctx) => {
		if (value.length > MAX_STRING_PROCESSING_LENGTH) {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.STRING_LENGTH_INVALID,
				params: {min: 1, max: 100},
			});
			return z.NEVER;
		}
	})
	.transform((value) => {
		let sanitized = sanitizeChannelName(value);
		sanitized = sanitized.replace(WHITESPACE_REGEX, ' ');
		return sanitized;
	})
	.refine((v) => v.trim().length > 0, ValidationErrorCodes.NAME_EMPTY_AFTER_NORMALIZATION)
	.pipe(withStringLengthRangeValidation(z.string(), 1, 100, ValidationErrorCodes.STRING_LENGTH_INVALID));
export const VanityURLCodeType = z
	.string()
	.superRefine((value, ctx) => {
		const normalized = normalizeString(value);
		const processed = normalized.toLowerCase().replace(WHITESPACE_REGEX, '-').replace(MULTIPLE_HYPHENS_REGEX, '-');
		if (!VANITY_URL_REGEX.test(processed)) {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.VANITY_URL_INVALID_CHARACTERS,
			});
			return z.NEVER;
		}
	})
	.transform((value) => {
		const normalized = normalizeString(value);
		return normalized.toLowerCase().replace(WHITESPACE_REGEX, '-').replace(MULTIPLE_HYPHENS_REGEX, '-');
	})
	.pipe(withStringLengthRangeValidation(z.string(), 2, 32, ValidationErrorCodes.VANITY_URL_CODE_LENGTH_INVALID));
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
