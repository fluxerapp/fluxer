// SPDX-License-Identifier: AGPL-3.0-or-later

import {PollLayoutTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	DEFAULT_POLL_DURATION_HOURS,
	MAX_POLL_ANSWER_TEXT_LENGTH,
	MAX_POLL_ANSWERS,
	MAX_POLL_DURATION_HOURS,
	MAX_POLL_QUESTION_LENGTH,
	MIN_POLL_DURATION_HOURS,
} from '@fluxer/constants/src/LimitConstants';
import {ClientAttachmentReferenceRequest} from '@fluxer/schema/src/domains/message/AttachmentSchemas';
import {MessageAttachmentResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {createStringType, Int32Type, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

/**
 * A poll question or answer is "media": optional text and/or a single image attachment.
 * At least one of text/attachment must be present — enforced via .refine below.
 */
const PollMediaRequestShape = z.object({
	text: createStringType(1, MAX_POLL_ANSWER_TEXT_LENGTH).nullish().describe('The text content of this poll media'),
	attachment: ClientAttachmentReferenceRequest.nullish().describe(
		'An optional image attached to this poll question/answer',
	),
});

const PollQuestionRequest = z.object({
	text: createStringType(1, MAX_POLL_QUESTION_LENGTH).nullish().describe('The poll question text'),
	attachment: ClientAttachmentReferenceRequest.nullish().describe('An optional image for the poll question'),
});

export const PollAnswerRequest = PollMediaRequestShape.refine((value) => Boolean(value.text || value.attachment), {
	message: 'A poll answer must have either text or an attachment',
});

export type PollAnswerRequest = z.infer<typeof PollAnswerRequest>;

/**
 * Client-supplied request for creating a poll attached to a new message.
 * Mirrors the "Initiation" flow described in the Polls spec: title, duration,
 * ranked-choice toggle, anonymous toggle, and optional custom-answer support.
 */
export const PollCreateRequest = z
	.object({
		question: PollQuestionRequest,
		answers: z
			.array(PollAnswerRequest)
			.min(1, 'A poll must have at least one answer')
			.max(MAX_POLL_ANSWERS, `A poll cannot have more than ${MAX_POLL_ANSWERS} answers`)
			.describe('The selectable answers for this poll'),
		duration: z
			.number()
			.int()
			.min(MIN_POLL_DURATION_HOURS)
			.max(MAX_POLL_DURATION_HOURS)
			.default(DEFAULT_POLL_DURATION_HOURS)
			.describe('How long the poll stays open, in hours'),
		allow_multiselect: z
			.boolean()
			.default(false)
			.describe('Whether voters may select more than one answer (ranked choice)'),
		layout_type: z
			.literal(PollLayoutTypes.DEFAULT)
			.optional()
			.default(PollLayoutTypes.DEFAULT)
			.describe('The visual layout to render this poll with'),
	})
	.refine((value) => Boolean(value.question.text || value.question.attachment), {
		message: 'A poll question must have either text or an attachment',
		path: ['question'],
	});

export type PollCreateRequest = z.infer<typeof PollCreateRequest>;

/**
 * Request body for casting (or retracting) a vote on a poll answer.
 */
export const PollVoteRequest = z.object({
	answer_id: Int32Type.describe('The ID of the answer being voted for'),
});

export type PollVoteRequest = z.infer<typeof PollVoteRequest>;

// ── Responses ────────────────────────────────────────────────────────────────

const PollMediaResponse = z.object({
	text: z.string().nullish().describe('The text content of this poll media'),
	attachment: MessageAttachmentResponse.nullish().describe('An optional image attached to this poll media'),
});

export const PollAnswerResponse = z.object({
	answer_id: Int32Type.describe('The unique identifier of this answer within the poll'),
	poll_media: PollMediaResponse.describe('The displayable content for this answer'),
});

export type PollAnswerResponse = z.infer<typeof PollAnswerResponse>;

const PollAnswerCountResponse = z.object({
	answer_id: Int32Type.describe('The answer this count applies to'),
	count: Int32Type.describe('The number of votes for this answer'),
	me_voted: z.boolean().describe('Whether the requesting user voted for this answer'),
});

const PollResultsResponse = z.object({
	is_finalized: z.boolean().describe('Whether the poll has closed and results are final'),
	answer_counts: z.array(PollAnswerCountResponse).nullish().describe('Per-answer vote counts'),
});

export const PollResponse = z.object({
	question: PollMediaResponse.describe('The poll question'),
	answers: z.array(PollAnswerResponse).describe('The selectable answers for this poll'),
	expiry: z.iso.datetime().nullish().describe('The ISO 8601 timestamp when voting closes'),
	allow_multiselect: z.boolean().describe('Whether this is a ranked-choice / multi-select poll'),
	layout_type: Int32Type.describe('The visual layout of this poll'),
	results: PollResultsResponse.nullish().describe('Aggregated vote results, present once available'),
});

export type PollResponse = z.infer<typeof PollResponse>;

export const PollVotersResponse = z.object({
	users: z.array(SnowflakeStringType).describe('User IDs who voted for the requested answer'),
});

export type PollVotersResponse = z.infer<typeof PollVotersResponse>;
