// SPDX-License-Identifier: AGPL-3.0-or-later

import {ReportFlowAnswerFields} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {
	createNamedStringLiteralUnion,
	createStringType,
	SnowflakeStringType,
	SnowflakeType,
	withFieldDescription,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {EmailType} from '@fluxer/schema/src/primitives/UserValidators';
import {z} from 'zod';

const MessageReportCategoryEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['harassment', 'harassment', 'Content that harasses, bullies, or intimidates users'],
			['hate_speech', 'hate_speech', 'Content promoting hatred against protected groups'],
			['violent_content', 'violent_content', 'Content depicting or promoting violence'],
			['spam', 'spam', 'Unsolicited bulk messages or promotional content'],
			['nsfw_violation', 'nsfw_violation', 'Adult content posted outside age-restricted channels'],
			['illegal_activity', 'illegal_activity', 'Content promoting or facilitating illegal activities'],
			['doxxing', 'doxxing', 'Content revealing private personal information'],
			['self_harm', 'self_harm', 'Content promoting self-harm or suicide'],
			['child_safety', 'child_safety', 'Content that endangers minors or depicts child abuse'],
			['malicious_links', 'malicious_links', 'Links to malware, phishing, or other malicious sites'],
			['impersonation', 'impersonation', 'Content falsely claiming to be another person or entity'],
			['other', 'other', 'Other violations not covered by specific categories'],
		],
		'Category of the message report',
	),
	'MessageReportCategory',
);
const UserReportCategoryEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['harassment', 'harassment', 'User engages in harassment, bullying, or intimidation'],
			['hate_speech', 'hate_speech', 'User promotes hatred against protected groups'],
			['spam_account', 'spam_account', 'Account used for spamming or bulk messaging'],
			['impersonation', 'impersonation', 'User falsely claims to be another person or entity'],
			['underage_user', 'underage_user', 'User appears to be under the minimum required age'],
			['inappropriate_profile', 'inappropriate_profile', 'Profile contains inappropriate or offensive content'],
			['other', 'other', 'Other violations not covered by specific categories'],
		],
		'Category of the user report',
	),
	'UserReportCategory',
);
const GuildReportCategoryEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['harassment', 'harassment', 'Guild facilitates harassment, bullying, or intimidation'],
			['hate_speech', 'hate_speech', 'Guild promotes hatred against protected groups'],
			['extremist_community', 'extremist_community', 'Guild promotes extremist or terrorist ideologies'],
			['illegal_activity', 'illegal_activity', 'Guild promotes or facilitates illegal activities'],
			['child_safety', 'child_safety', 'Guild endangers minors or hosts child abuse content'],
			['raid_coordination', 'raid_coordination', 'Guild coordinates attacks on other communities'],
			['spam', 'spam', 'Guild used for spamming or bulk messaging'],
			['malware_distribution', 'malware_distribution', 'Guild distributes malware or malicious software'],
			['other', 'other', 'Other violations not covered by specific categories'],
		],
		'Category of the guild report',
	),
	'GuildReportCategory',
);
export const ReportResponse = z.object({
	report_id: SnowflakeStringType.describe('The unique identifier for this report'),
	status: z.string().describe('Current status of the report (pending, resolved)'),
	reported_at: z.string().describe('ISO 8601 timestamp when the report was submitted'),
});

export type ReportResponse = z.infer<typeof ReportResponse>;

export const OkResponse = z.object({
	ok: z.boolean().describe('Whether the operation was successful'),
});

export type OkResponse = z.infer<typeof OkResponse>;

export const TicketResponse = z.object({
	ticket: z.string().describe('A temporary ticket token for subsequent operations'),
});

export type TicketResponse = z.infer<typeof TicketResponse>;

const FLUXER_TAG_REGEX = /^([^#]{1,32})#([0-9]{4})$/;
const FLUXER_TAG_TYPE = z
	.string()
	.min(3)
	.max(37)
	.refine((value) => FLUXER_TAG_REGEX.test(value), 'Fluxer tag must be in the format username#1234')
	.describe('A Fluxer username tag in the format username#1234');
const EuCountryCodeEnum = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['AT', 'AT', 'Austria'],
			['BE', 'BE', 'Belgium'],
			['BG', 'BG', 'Bulgaria'],
			['HR', 'HR', 'Croatia'],
			['CY', 'CY', 'Cyprus'],
			['CZ', 'CZ', 'Czechia'],
			['DK', 'DK', 'Denmark'],
			['EE', 'EE', 'Estonia'],
			['FI', 'FI', 'Finland'],
			['FR', 'FR', 'France'],
			['DE', 'DE', 'Germany'],
			['GR', 'GR', 'Greece'],
			['HU', 'HU', 'Hungary'],
			['IE', 'IE', 'Ireland'],
			['IT', 'IT', 'Italy'],
			['LV', 'LV', 'Latvia'],
			['LT', 'LT', 'Lithuania'],
			['LU', 'LU', 'Luxembourg'],
			['MT', 'MT', 'Malta'],
			['NL', 'NL', 'Netherlands'],
			['PL', 'PL', 'Poland'],
			['PT', 'PT', 'Portugal'],
			['RO', 'RO', 'Romania'],
			['SK', 'SK', 'Slovakia'],
			['SI', 'SI', 'Slovenia'],
			['ES', 'ES', 'Spain'],
			['SE', 'SE', 'Sweden'],
		] as const,
		'EU country code of residence',
	),
	'EuCountryCode',
);
export const ReportMessageRequest = z.object({
	channel_id: SnowflakeType.describe('ID of the accessible channel containing the reported message'),
	message_id: SnowflakeType.describe('ID of the accessible message being reported'),
	category: MessageReportCategoryEnum,
});

export type ReportMessageRequest = z.infer<typeof ReportMessageRequest>;

export const ReportUserRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user being reported'),
	category: UserReportCategoryEnum,
	guild_id: z.optional(SnowflakeType).describe('ID of the guild where the violation occurred'),
});

export type ReportUserRequest = z.infer<typeof ReportUserRequest>;

export const ReportGuildRequest = z.object({
	guild_id: SnowflakeType.describe('ID of the guild being reported'),
	category: GuildReportCategoryEnum,
	invite_code: z
		.optional(createStringType(1, 64))
		.describe('Invite code proving access to the guild (required when not a member of a non-discoverable guild)'),
});

export type ReportGuildRequest = z.infer<typeof ReportGuildRequest>;

const DSA_VERIFICATION_CODE_TYPE = createStringType(9, 9).refine(
	(value) => /^[A-Z0-9]{4}-[A-Z0-9]{4}$/.test(value),
	'Verification code must have the format XXXX-XXXX (uppercase letters and digits)',
);
export const DsaReportEmailSendRequest = z.object({
	email: EmailType.describe('Email address to send the DSA verification code to'),
});

export type DsaReportEmailSendRequest = z.infer<typeof DsaReportEmailSendRequest>;

export const DsaReportEmailVerifyRequest = z.object({
	email: EmailType.describe('Email address that received the verification code'),
	code: DSA_VERIFICATION_CODE_TYPE.describe('Verification code received via email'),
});

export type DsaReportEmailVerifyRequest = z.infer<typeof DsaReportEmailVerifyRequest>;

interface DsaReportRefinementInput {
	category?: string | undefined;
	reporter_full_legal_name?: string | undefined;
	additional_info?: string | undefined;
	revision_hash?: string | undefined;
	steps?: Array<unknown> | undefined;
	good_faith_confirmed?: true | undefined;
}

function requireDsaReportField(ctx: z.RefinementCtx, field: string, message: string): void {
	ctx.addIssue({code: 'custom', message, path: [field]});
}

function refineDsaReport(value: DsaReportRefinementInput, ctx: z.RefinementCtx): void {
	if (value.steps === undefined) {
		if (value.category === undefined) {
			requireDsaReportField(ctx, 'category', 'category is required when steps are not provided');
		}
		if (value.reporter_full_legal_name === undefined) {
			requireDsaReportField(
				ctx,
				'reporter_full_legal_name',
				'reporter_full_legal_name is required when steps are not provided',
			);
		}
		return;
	}
	if (value.revision_hash === undefined) {
		requireDsaReportField(ctx, 'revision_hash', 'revision_hash is required with steps');
	}
	if (value.good_faith_confirmed !== true) {
		requireDsaReportField(ctx, 'good_faith_confirmed', 'good_faith_confirmed is required with steps');
	}
	if (!value.additional_info) {
		requireDsaReportField(ctx, 'additional_info', 'additional_info is required with steps');
	}
}

function refineDsaUserTarget(
	value: {user_id?: bigint | undefined; user_tag?: string | undefined},
	ctx: z.RefinementCtx,
): void {
	if (!value.user_id && !value.user_tag) {
		ctx.addIssue({
			code: 'custom',
			message: 'Either user_id or user_tag must be provided for user reports',
			path: [],
		});
	}
}

const DsaReportCommonFields = {
	ticket: createStringType(1, 128).describe('Verification ticket obtained from email verification'),
	reporter_country_of_residence: withFieldDescription(EuCountryCodeEnum, 'EU country code of the reporter residence'),
};
const DSA_REPORT_LEGAL_NAME_TYPE = z
	.optional(createStringType(1, 160))
	.describe('Full legal name of the person filing the report, required unless the report is about child sexual abuse');
const DsaReportFlowFields = {
	revision_hash: ReportFlowAnswerFields.revision_hash.describe(
		'revision_hash of the DSA report flow the reporter answered',
	),
	steps: ReportFlowAnswerFields.steps.describe(
		'Screens of the DSA report flow and the answers chosen on each, in order',
	),
	good_faith_confirmed: z
		.literal(true)
		.describe('Confirms in good faith that the information and allegations in the notice are accurate and complete'),
	locale: ReportFlowAnswerFields.locale,
};
const DsaReportBase = z.object({
	...DsaReportCommonFields,
	additional_info: z.optional(createStringType(0, 1000)).describe('Additional context or details about the report'),
	reporter_full_legal_name: DSA_REPORT_LEGAL_NAME_TYPE,
	revision_hash: DsaReportFlowFields.revision_hash.optional(),
	steps: DsaReportFlowFields.steps.optional(),
	good_faith_confirmed: DsaReportFlowFields.good_faith_confirmed.optional(),
	locale: DsaReportFlowFields.locale,
});
const DsaReportFlowBase = z.object({
	...DsaReportCommonFields,
	...DsaReportFlowFields,
	additional_info: createStringType(1, 1000).describe(
		'Explanation of the problem. If the reporter believes the content is illegal, which law it breaks and why',
	),
	reporter_full_legal_name: DSA_REPORT_LEGAL_NAME_TYPE,
});
const DsaReportMessageTarget = {
	report_type: z.literal('message').describe('Type of report'),
	message_link: createStringType(1, 2048).describe('Link to the message being reported'),
	reported_user_tag: z.optional(FLUXER_TAG_TYPE).describe('Fluxer tag of the user who sent the message'),
};
const DsaReportUserTarget = {
	report_type: z.literal('user').describe('Type of report'),
	user_id: SnowflakeType.optional().describe('ID of the user being reported'),
	user_tag: z.optional(FLUXER_TAG_TYPE).describe('Fluxer tag of the user being reported'),
};
const DsaReportGuildTarget = {
	report_type: z.literal('guild').describe('Type of report'),
	guild_id: SnowflakeType.describe('ID of the guild being reported'),
	invite_code: z.optional(createStringType(1, 64)).describe('Invite code used to access the guild'),
};

const DsaReportMessageRequest = DsaReportBase.extend({
	...DsaReportMessageTarget,
	category: MessageReportCategoryEnum.optional(),
}).superRefine(refineDsaReport);

const DsaReportUserRequest = DsaReportBase.extend({
	...DsaReportUserTarget,
	category: UserReportCategoryEnum.optional(),
}).superRefine((value, ctx) => {
	refineDsaReport(value, ctx);
	refineDsaUserTarget(value, ctx);
});

const DsaReportGuildRequest = DsaReportBase.extend({
	...DsaReportGuildTarget,
	category: GuildReportCategoryEnum.optional(),
}).superRefine(refineDsaReport);

export const DsaReportRequest = z.discriminatedUnion('report_type', [
	DsaReportMessageRequest,
	DsaReportUserRequest,
	DsaReportGuildRequest,
]);

export type DsaReportRequest = z.infer<typeof DsaReportRequest>;

export const DsaReportFlowRequest = z.discriminatedUnion('report_type', [
	DsaReportFlowBase.extend(DsaReportMessageTarget),
	DsaReportFlowBase.extend(DsaReportUserTarget).superRefine(refineDsaUserTarget),
	DsaReportFlowBase.extend(DsaReportGuildTarget),
]);

export type DsaReportFlowRequest = z.infer<typeof DsaReportFlowRequest>;
