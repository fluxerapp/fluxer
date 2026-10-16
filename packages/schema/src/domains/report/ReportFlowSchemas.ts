// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createNamedStringLiteralUnion,
	createStringType,
	SnowflakeType,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ReportFlowTargetType = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['message', 'message', 'A message'],
			['user', 'user', 'A user profile'],
			['guild', 'guild', 'A community, on the DSA form only'],
		],
		'Kind of entity a report flow is about',
	),
	'ReportFlowTargetType',
);

export type ReportFlowTargetType = z.infer<typeof ReportFlowTargetType>;

export const ReportFlowSurface = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['in_app', 'in_app', 'The in-app report flow'],
			['dsa', 'dsa', 'The public Digital Services Act report form'],
		],
		'Where the flow is shown',
	),
	'ReportFlowSurface',
);

export type ReportFlowSurface = z.infer<typeof ReportFlowSurface>;

export const ReportFlowOutcomeType = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['screen', 'screen', 'Go to another screen'],
			['submit', 'submit', 'Show the summary, then file a report'],
			['end', 'end', 'Finish without filing a report'],
			['link', 'link', 'Open an external page'],
		],
		'What choosing an option does',
	),
	'ReportFlowOutcomeType',
);

export type ReportFlowOutcomeType = z.infer<typeof ReportFlowOutcomeType>;

export const ReportFlowPathParams = z.object({target_type: ReportFlowTargetType});

export type ReportFlowPathParams = z.infer<typeof ReportFlowPathParams>;

export const ReportFlowQuery = z.object({
	surface: ReportFlowSurface.default('in_app'),
	locale: createStringType(1, 35)
		.optional()
		.describe(
			'Language tag of the client UI. The server picks the closest supported locale and reports it in the locale field',
		),
});

export type ReportFlowQuery = z.infer<typeof ReportFlowQuery>;

export const ReportFlowOutcome = z.object({
	type: ReportFlowOutcomeType,
	screen_id: z.string().nullable().describe('Next screen when type is screen'),
	reason: z.string().nullable().describe('Report reason when type is submit'),
	notice_id: z.string().nullable().describe('Notice to show when type is end, null for the thank-you screen'),
	url: z.string().nullable().describe('Page to open when type is link'),
});

export type ReportFlowOutcome = z.infer<typeof ReportFlowOutcome>;

export const ReportFlowOption = z.object({id: z.string(), label: z.string(), outcome: ReportFlowOutcome});

export type ReportFlowOption = z.infer<typeof ReportFlowOption>;

export const ReportFlowChecklistItem = z.object({
	id: z.string(),
	label: z.string(),
	description: z.string().nullable(),
});

export type ReportFlowChecklistItem = z.infer<typeof ReportFlowChecklistItem>;

export const ReportFlowChecklist = z.object({
	items: z.array(ReportFlowChecklistItem),
	min_checked: z.number().int(),
	outcome: ReportFlowOutcome,
});

export type ReportFlowChecklist = z.infer<typeof ReportFlowChecklist>;

export const ReportFlowScreen = z.object({
	id: z.string(),
	title: z.string(),
	subtitle: z.string().nullable(),
	urgent: z.boolean(),
	options: z.array(ReportFlowOption),
	options_heading: z.string().nullable(),
	checklist: ReportFlowChecklist.nullable(),
	next_screen_id: z.string().nullable().describe('Set on an info screen, which has a Next button instead of choices'),
});

export type ReportFlowScreen = z.infer<typeof ReportFlowScreen>;

export const ReportFlowNotice = z.object({id: z.string(), title: z.string(), body: z.string()});

export type ReportFlowNotice = z.infer<typeof ReportFlowNotice>;

export const ReportFlowResponse = z.object({
	target_type: ReportFlowTargetType,
	surface: ReportFlowSurface,
	revision_hash: z.string(),
	locale: z.string().describe('Locale the copy was rendered in'),
	start_screen_id: z.string(),
	guidelines_url: z.string().nullable().describe('Community guidelines page, null when the instance has none'),
	screens: z.array(ReportFlowScreen),
	notices: z.array(ReportFlowNotice),
});

export type ReportFlowResponse = z.infer<typeof ReportFlowResponse>;

const REPORT_FLOW_ID = createStringType(1, 48);

export const ReportFlowStep = z.object({
	screen_id: REPORT_FLOW_ID,
	option_id: REPORT_FLOW_ID.optional(),
	item_ids: z.array(REPORT_FLOW_ID).min(1).max(32).optional(),
});

export type ReportFlowStep = z.infer<typeof ReportFlowStep>;

export const ReportFlowAnswerFields = {
	revision_hash: createStringType(1, 64),
	steps: z.array(ReportFlowStep).min(1).max(16),
	locale: createStringType(1, 35).optional().describe('Language tag the reporter saw the flow in'),
};

export const ReportFlowMessageSubmissionRequest = z.object({
	...ReportFlowAnswerFields,
	channel_id: SnowflakeType,
	message_id: SnowflakeType,
});

export type ReportFlowMessageSubmissionRequest = z.infer<typeof ReportFlowMessageSubmissionRequest>;

export const ReportFlowUserSubmissionRequest = z.object({
	...ReportFlowAnswerFields,
	user_id: SnowflakeType,
	guild_id: z.optional(SnowflakeType),
});

export type ReportFlowUserSubmissionRequest = z.infer<typeof ReportFlowUserSubmissionRequest>;
