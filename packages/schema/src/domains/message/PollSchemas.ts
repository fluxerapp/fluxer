// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MAX_POLL_DURATION_SECONDS,
	MAX_POLL_OPTION_TEXT_LENGTH,
	MAX_POLL_OPTIONS,
	MAX_POLL_TITLE_LENGTH,
	MIN_POLL_DURATION_SECONDS,
	MIN_POLL_OPTIONS,
} from '@fluxer/constants/src/LimitConstants';
import {DateTimeType} from '@fluxer/schema/src/primitives/QueryValidators';
import {
	coerceNumberFromString,
	createStringType,
	Int32Type,
	SnowflakeStringType,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';
import {ClientAttachmentRequest, ClientUploadedAttachmentRequest} from './AttachmentSchemas';

const PollDurationSecondsRequest = coerceNumberFromString(
	z.number().int().min(MIN_POLL_DURATION_SECONDS).max(MAX_POLL_DURATION_SECONDS),
).describe('Poll duration in seconds');

const PollOptionRequest = z.object({
	text: createStringType(1, MAX_POLL_OPTION_TEXT_LENGTH).describe('Text shown for this poll option'),
	attachment_id: coerceNumberFromString(Int32Type)
		.nullish()
		.describe('Client-side attachment identifier for an optional image associated with this option'),
});

export const PollRequest = z
	.object({
		title: createStringType(1, MAX_POLL_TITLE_LENGTH).describe('Poll title or question'),
		options: z.array(PollOptionRequest).min(MIN_POLL_OPTIONS).max(MAX_POLL_OPTIONS).describe('Poll answer options'),
		duration_seconds: PollDurationSecondsRequest.optional().describe('Relative poll duration in seconds'),
		expires_at: DateTimeType.optional().describe('Absolute poll close timestamp'),
		anonymous: z.boolean().default(false).describe('Whether voter identities are hidden from normal users'),
		allow_ranked_choice: z.boolean().default(false).describe('Whether voters can rank multiple options'),
		allow_custom_answers: z.boolean().default(false).describe('Whether users can add custom answer options'),
	})
	.superRefine((value, ctx) => {
		if (value.duration_seconds === undefined && value.expires_at === undefined) {
			ctx.addIssue({
				code: 'custom',
				message: 'Polls must include duration_seconds or expires_at',
				path: ['duration_seconds'],
			});
		}
		if (value.duration_seconds !== undefined && value.expires_at !== undefined) {
			ctx.addIssue({
				code: 'custom',
				message: 'Polls cannot include both duration_seconds and expires_at',
				path: ['expires_at'],
			});
		}
		const seenOptions = new Set<string>();
		for (const [index, option] of value.options.entries()) {
			const normalized = option.text.toLowerCase();
			if (seenOptions.has(normalized)) {
				ctx.addIssue({
					code: 'custom',
					message: 'Poll options must be unique',
					path: ['options', index, 'text'],
				});
			}
			seenOptions.add(normalized);
		}
	});

export type PollRequest = z.infer<typeof PollRequest>;

export const PollCustomOptionRequest = z.object({
	text: createStringType(1, MAX_POLL_OPTION_TEXT_LENGTH).describe('Text shown for the custom poll option'),
	attachments: z
		.array(z.union([ClientUploadedAttachmentRequest, ClientAttachmentRequest]))
		.max(1)
		.optional()
		.describe('Optional image upload to associate with this custom poll option'),
});

export type PollCustomOptionRequest = z.infer<typeof PollCustomOptionRequest>;

export const PollVoteRequest = z
	.object({
		option_ids: z.array(SnowflakeType).min(1).max(MAX_POLL_OPTIONS).describe('Selected option IDs in preference order'),
	})
	.superRefine((value, ctx) => {
		const seenOptions = new Set<string>();
		for (const [index, optionId] of value.option_ids.entries()) {
			const optionKey = optionId.toString();
			if (seenOptions.has(optionKey)) {
				ctx.addIssue({
					code: 'custom',
					message: 'Poll vote option IDs must be unique',
					path: ['option_ids', index],
				});
			}
			seenOptions.add(optionKey);
		}
	});

export type PollVoteRequest = z.infer<typeof PollVoteRequest>;

const PollVoteResponse = z.object({
	user_id: SnowflakeStringType.describe('User who submitted this vote'),
	option_ids: z.array(SnowflakeStringType).min(1).max(MAX_POLL_OPTIONS).describe('Selected option IDs in vote order'),
	created_at: z.iso.datetime().describe('ISO8601 timestamp for when this vote was submitted'),
});

export const PollOptionResponse = z.object({
	id: SnowflakeStringType.describe('Poll option ID'),
	text: z.string().describe('Text shown for this poll option'),
	attachment_id: SnowflakeStringType.nullish().describe(
		'Attachment ID for an optional image associated with this option',
	),
	vote_count: Int32Type.describe('Visible vote count for this option'),
	rank_counts: z
		.array(Int32Type)
		.nullish()
		.describe('Ranked-choice selection counts by rank, where index 0 is first choice'),
	ranked_score: Int32Type.nullish().describe('Borda-style aggregate score for ranked-choice poll results'),
	me: z.boolean().optional().describe('Whether the current user selected this option'),
	voter_ids: z.array(SnowflakeStringType).nullish().describe('Visible voter IDs for non-anonymous poll results'),
});

export type PollOptionResponse = z.infer<typeof PollOptionResponse>;

export const PollResponse = z.object({
	id: SnowflakeStringType.describe('Poll ID'),
	title: z.string().describe('Poll title or question'),
	options: z.array(PollOptionResponse).min(MIN_POLL_OPTIONS).max(MAX_POLL_OPTIONS).describe('Poll answer options'),
	expires_at: z.iso.datetime().describe('ISO8601 timestamp when this poll closes'),
	closed: z.boolean().describe('Whether this poll is closed to new votes'),
	anonymous: z.boolean().describe('Whether voter identities are hidden from normal users'),
	allow_ranked_choice: z.boolean().describe('Whether voters can rank multiple options'),
	allow_custom_answers: z.boolean().describe('Whether users can add custom answer options'),
	votes: z.array(PollVoteResponse).nullish().describe('Visible full vote records, usually only for moderation views'),
});

export type PollResponse = z.infer<typeof PollResponse>;
