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

import {GUILD_REPORT_CATEGORIES, MESSAGE_REPORT_CATEGORIES, USER_REPORT_CATEGORIES} from '~/constants/ReportCategories';
import {createStringType, EmailType, Int64Type, z} from '~/Schema';

const FLUXER_TAG_REGEX = /^([^#]{1,32})#([0-9]{4})$/;

const FLUXER_TAG_TYPE = z
	.string()
	.min(3)
	.max(37)
	.refine((value) => FLUXER_TAG_REGEX.test(value), 'Fluxer tag must be in the format username#1234');

const EU_COUNTRY_CODES = [
	'AT',
	'BE',
	'BG',
	'HR',
	'CY',
	'CZ',
	'DK',
	'EE',
	'FI',
	'FR',
	'DE',
	'GR',
	'HU',
	'IE',
	'IT',
	'LV',
	'LT',
	'LU',
	'MT',
	'NL',
	'PL',
	'PT',
	'RO',
	'SK',
	'SI',
	'ES',
	'SE',
] as const;

const EU_COUNTRY_CODE_ENUM = z.enum(EU_COUNTRY_CODES);
export const ReportMessageRequest = z.object({
	channel_id: Int64Type,
	message_id: Int64Type,
	category: z.enum(MESSAGE_REPORT_CATEGORIES),
	additional_info: z.optional(createStringType(0, 1000)),
});

export const ReportUserRequest = z.object({
	user_id: Int64Type,
	category: z.enum(USER_REPORT_CATEGORIES),
	additional_info: z.optional(createStringType(0, 1000)),
	guild_id: z.optional(Int64Type),
});

export const ReportGuildRequest = z.object({
	guild_id: Int64Type,
	category: z.enum(GUILD_REPORT_CATEGORIES),
	additional_info: z.optional(createStringType(0, 1000)),
});

const DSA_VERIFICATION_CODE_TYPE = createStringType(9, 9).refine(
	(value) => /^[A-Z0-9]{4}-[A-Z0-9]{4}$/.test(value),
	'Verification code must have the format XXXX-XXXX (uppercase letters and digits)',
);

export const DsaReportEmailSendRequest = z.object({
	email: EmailType,
});

export const DsaReportEmailVerifyRequest = z.object({
	email: EmailType,
	code: DSA_VERIFICATION_CODE_TYPE,
});

const DsaReportBase = z.object({
	ticket: createStringType(1, 128),
	additional_info: z.optional(createStringType(0, 1000)),
	reporter_full_legal_name: createStringType(1, 160),
	reporter_country_of_residence: EU_COUNTRY_CODE_ENUM,
	reporter_fluxer_tag: z.optional(FLUXER_TAG_TYPE),
});

const DsaReportMessage = DsaReportBase.extend({
	report_type: z.literal('message'),
	category: z.enum(MESSAGE_REPORT_CATEGORIES),
	message_link: createStringType(1, 2048),
	reported_user_tag: z.optional(FLUXER_TAG_TYPE),
});

const DsaReportUser = DsaReportBase.extend({
	report_type: z.literal('user'),
	category: z.enum(USER_REPORT_CATEGORIES),
	user_id: Int64Type.optional(),
	user_tag: z.optional(FLUXER_TAG_TYPE),
}).superRefine((value, ctx) => {
	if (!value.user_id && !value.user_tag) {
		ctx.addIssue({
			code: 'custom',
			message: 'Either user_id or user_tag must be provided for user reports',
			path: [],
		});
	}
});

const DsaReportGuild = DsaReportBase.extend({
	report_type: z.literal('guild'),
	category: z.enum(GUILD_REPORT_CATEGORIES),
	guild_id: Int64Type,
	invite_code: z.optional(createStringType(1, 64)),
});

export const DsaReportRequest = z.discriminatedUnion('report_type', [DsaReportMessage, DsaReportUser, DsaReportGuild]);

export type DsaReportRequest = z.infer<typeof DsaReportRequest>;

export type ReportMessageRequest = z.infer<typeof ReportMessageRequest>;
export type ReportUserRequest = z.infer<typeof ReportUserRequest>;
export type ReportGuildRequest = z.infer<typeof ReportGuildRequest>;
