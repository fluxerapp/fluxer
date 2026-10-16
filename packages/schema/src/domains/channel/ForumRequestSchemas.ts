// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	FORUM_TAG_NAME_MAX_LENGTH,
	FORUM_TOPIC_MAX_LENGTH,
	ForumLayoutTypes,
	ForumSortOrderTypes,
	MAX_APPLIED_TAGS_PER_THREAD,
	POST_DATA_MAX_IDS,
	THREAD_NAME_MAX_LENGTH,
	THREAD_NAME_MIN_LENGTH,
	THREAD_RATE_LIMIT_PER_USER_MAX,
	THREAD_SEARCH_MAX_LIMIT,
	THREAD_SEARCH_MAX_OFFSET,
	THREAD_SEARCH_MAX_TAGS,
	THREAD_SEARCH_MIN_LIMIT,
	THREAD_SEARCH_NAME_MAX_LENGTH,
} from '@fluxer/constants/src/ThreadConstants';
import {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {
	StartThreadRequest,
	ThreadAutoArchiveDurationSchema,
} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {MessageRequestSchema} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {MessageResponseSchema} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {createQueryIntegerType} from '@fluxer/schema/src/primitives/QueryValidators';
import {
	createInt32EnumType,
	createStringType,
	Int32Type,
	SnowflakeStringType,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {withSchemaMetadata} from '@fluxer/schema/src/SchemaMetadata';
import {z} from 'zod';

const EXPERIMENT = {experiment: 'channel_threads'} as const;
const EMOJI_NAME_MAX_LENGTH = 64;

export const ForumTagRequest = withSchemaMetadata(
	z.object({
		name: createStringType(1, FORUM_TAG_NAME_MAX_LENGTH).describe('The name of the tag (1-50 characters)'),
		moderated: z.boolean().optional().describe('Whether only moderators can add or remove this tag'),
		emoji_id: SnowflakeType.nullish().describe('The ID of a custom guild emoji'),
		emoji_name: createStringType(1, EMOJI_NAME_MAX_LENGTH).nullish().describe('The unicode character of the emoji'),
	}),
	EXPERIMENT,
);

export type ForumTagRequest = z.infer<typeof ForumTagRequest>;

export const ForumTagUpdateRequest = withSchemaMetadata(
	ForumTagRequest.extend({
		id: SnowflakeType.optional().describe('The ID of an existing tag to keep'),
	}),
	EXPERIMENT,
);

export type ForumTagUpdateRequest = z.infer<typeof ForumTagUpdateRequest>;

export const DefaultReactionEmojiRequest = withSchemaMetadata(
	z.object({
		emoji_id: SnowflakeType.nullish().describe('The ID of a custom guild emoji'),
		emoji_name: createStringType(1, EMOJI_NAME_MAX_LENGTH).nullish().describe('The unicode character of the emoji'),
	}),
	EXPERIMENT,
);

export type DefaultReactionEmojiRequest = z.infer<typeof DefaultReactionEmojiRequest>;

export const ForumSortOrderSchema = createInt32EnumType(
	[
		[ForumSortOrderTypes.LATEST_ACTIVITY, 'LATEST_ACTIVITY', 'Sort posts by latest activity'],
		[ForumSortOrderTypes.CREATION_TIME, 'CREATION_TIME', 'Sort posts by creation time'],
	],
	'The default sort order of posts',
	'ForumSortOrderType',
);

export const ForumLayoutSchema = createInt32EnumType(
	[
		[ForumLayoutTypes.DEFAULT, 'DEFAULT', 'No default layout'],
		[ForumLayoutTypes.LIST, 'LIST', 'Display posts as a list'],
		[ForumLayoutTypes.GRID, 'GRID', 'Display posts as a collection of tiles'],
	],
	'The default layout of a forum channel',
	'ForumLayoutType',
);

export const ForumTagSettingSchema = z
	.enum(['match_some', 'match_all'])
	.describe('How posts are filtered when searching by several tags');

export const ThreadParentDefaultsRequestFields = {
	default_auto_archive_duration: ThreadAutoArchiveDurationSchema.nullish().describe(
		'Default auto archive duration in minutes for new threads',
	),
	default_thread_rate_limit_per_user: Int32Type.max(THREAD_RATE_LIMIT_PER_USER_MAX)
		.nullish()
		.describe('Slowmode in seconds copied onto new threads (0-21600)'),
};

export const ForumChannelRequestFields = {
	...ThreadParentDefaultsRequestFields,
	topic: createStringType(1, FORUM_TOPIC_MAX_LENGTH)
		.nullish()
		.describe(`The guidelines of the channel (1-${FORUM_TOPIC_MAX_LENGTH} characters)`),
	available_tags: z.array(ForumTagUpdateRequest).optional().describe('Tags that can be applied to posts (max 20)'),
	default_reaction_emoji: DefaultReactionEmojiRequest.nullish().describe('Default reaction shown on posts'),
	default_sort_order: ForumSortOrderSchema.nullish(),
	default_tag_setting: ForumTagSettingSchema.nullish(),
	flags: Int32Type.optional().describe('Channel flags'),
};

const ThreadNameType = createStringType(THREAD_NAME_MIN_LENGTH, THREAD_NAME_MAX_LENGTH).describe(
	'The name of the post (1-100 characters)',
);

export const ForumThreadMessageRequest = withSchemaMetadata(
	MessageRequestSchema.omit({message_reference: true, favorite_meme_id: true, tts: true, nonce: true}),
	EXPERIMENT,
);

export type ForumThreadMessageRequest = z.infer<typeof ForumThreadMessageRequest>;

const AppliedTagsType = z
	.array(SnowflakeType)
	.max(MAX_APPLIED_TAGS_PER_THREAD)
	.describe('IDs of the tags applied to the post (max 5)');

export const StartForumThreadRequest = withSchemaMetadata(
	z.object({
		name: ThreadNameType,
		type: Int32Type.optional().describe('The type of thread, which is always a public thread in a forum'),
		auto_archive_duration: ThreadAutoArchiveDurationSchema.optional(),
		rate_limit_per_user: Int32Type.max(THREAD_RATE_LIMIT_PER_USER_MAX)
			.optional()
			.describe('Seconds a user has to wait before sending another message (0-21600)'),
		applied_tags: AppliedTagsType.optional(),
		message: ForumThreadMessageRequest.describe('The first message of the post'),
	}),
	EXPERIMENT,
);

export type StartForumThreadRequest = z.infer<typeof StartForumThreadRequest>;

export const StartThreadRequestBody = z.union([StartThreadRequest, StartForumThreadRequest]);

export const StartThreadMultipartRequest = withSchemaMetadata(
	z.object({
		payload_json: z.string().describe('The JSON-encoded request body'),
		'files[0]': z
			.file()
			.optional()
			.describe(
				'A file for the first message of a post, referenced by attachment id 0. Further files go in files[1], files[2] and so on',
			),
	}),
	EXPERIMENT,
);

export const StartForumThreadResponse = withSchemaMetadata(
	ThreadChannelResponse.extend({
		message: MessageResponseSchema.optional().describe('The first message of the post'),
	}),
	EXPERIMENT,
);

export type StartForumThreadResponse = z.infer<typeof StartForumThreadResponse>;

export const ThreadAppliedTagsRequestFields = {
	applied_tags: AppliedTagsType.optional(),
};

export const ThreadPostDataRequest = withSchemaMetadata(
	z.object({
		thread_ids: z
			.array(SnowflakeType)
			.min(1)
			.max(POST_DATA_MAX_IDS)
			.describe(`The IDs of the posts to get data for (max ${POST_DATA_MAX_IDS})`),
	}),
	EXPERIMENT,
);

export type ThreadPostDataRequest = z.infer<typeof ThreadPostDataRequest>;

export const ThreadPostDataEntryResponse = z.object({
	owner: GuildMemberResponse.nullable().describe('The guild member who created the post'),
	first_message: MessageResponseSchema.nullable().describe('The first message of the post'),
});

export const ThreadPostDataResponse = withSchemaMetadata(
	z.object({
		threads: z
			.record(SnowflakeStringType, ThreadPostDataEntryResponse)
			.describe('A mapping of post IDs to their post data'),
	}),
	EXPERIMENT,
);

export type ThreadPostDataResponse = z.infer<typeof ThreadPostDataResponse>;

const TagQueryType = z.preprocess(
	(value) => (value === undefined || Array.isArray(value) ? value : [value]),
	z.array(SnowflakeType).max(THREAD_SEARCH_MAX_TAGS),
);

const THREAD_SEARCH_ARCHIVED_VALUES = ['true', 'false', 'True', 'False', '1', '0'];

export const ThreadSearchSortBySchema = z
	.enum(['last_message_time', 'archive_time', 'relevance', 'creation_time'])
	.describe('The sorting algorithm to use');

export const ThreadSearchSortOrderSchema = z.enum(['asc', 'desc']).describe('The direction to sort');

export const ThreadSearchQuery = withSchemaMetadata(
	z.object({
		name: z
			.string()
			.max(THREAD_SEARCH_NAME_MAX_LENGTH)
			.optional()
			.describe('Text to look for in thread names (max 100 characters)'),
		slop: createQueryIntegerType({defaultValue: 2, minValue: 0, maxValue: 100}).describe(
			'Accepted for compatibility and ignored (max 100, default 2)',
		),
		tag: TagQueryType.optional().describe('Tag IDs to filter by (max 20)'),
		tag_setting: ForumTagSettingSchema.optional(),
		archived: z
			.string()
			.optional()
			.refine((value) => value === undefined || THREAD_SEARCH_ARCHIVED_VALUES.includes(value))
			.transform((value) => (value === undefined ? undefined : value === 'true' || value === 'True' || value === '1'))
			.describe('Whether to return only archived or only active threads (default both)'),
		sort_by: ThreadSearchSortBySchema.default('last_message_time'),
		sort_order: ThreadSearchSortOrderSchema.default('desc'),
		limit: createQueryIntegerType({
			defaultValue: THREAD_SEARCH_MAX_LIMIT,
			minValue: THREAD_SEARCH_MIN_LIMIT,
			maxValue: THREAD_SEARCH_MAX_LIMIT,
		}).describe('Max number of threads to return (1-25, default 25)'),
		offset: createQueryIntegerType({defaultValue: 0, minValue: 0, maxValue: THREAD_SEARCH_MAX_OFFSET}).describe(
			'Number of threads to skip (max 9975)',
		),
		max_id: SnowflakeType.optional().describe('Get threads before this thread ID'),
		min_id: SnowflakeType.optional().describe('Get threads after this thread ID'),
	}),
	EXPERIMENT,
);

export type ThreadSearchQuery = z.infer<typeof ThreadSearchQuery>;

export const ThreadSearchResponse = withSchemaMetadata(
	z.object({
		threads: z.array(ThreadChannelResponse).describe('The threads that match the search'),
		members: z.array(ThreadMemberResponse).describe('A thread member object for each returned thread the user joined'),
		has_more: z.boolean().describe('Whether more threads could be returned by a later request'),
		total_results: Int32Type.describe('The total number of threads that match the search'),
		first_messages: z
			.array(MessageResponseSchema)
			.optional()
			.describe('The first message of each returned post, in forum and media channels only'),
	}),
	EXPERIMENT,
);

export type ThreadSearchResponse = z.infer<typeof ThreadSearchResponse>;

export const SearchIndexNotReadyResponse = withSchemaMetadata(
	z.object({
		code: z.literal('SEARCH_INDEX_NOT_READY').describe('Machine-readable code of the response'),
		message: z.string().describe('Human-readable description of the response'),
		documents_indexed: Int32Type.describe('Always 0 while the index is being built'),
		retry_after: z.number().describe('Seconds to wait before retrying the search'),
	}),
	EXPERIMENT,
);

export type SearchIndexNotReadyResponse = z.infer<typeof SearchIndexNotReadyResponse>;

export const ThreadSearchResult = withSchemaMetadata(
	z.union([ThreadSearchResponse, SearchIndexNotReadyResponse]),
	EXPERIMENT,
);

export type ThreadSearchResult = z.infer<typeof ThreadSearchResult>;

export const ChannelIdTagIdParam = z.object({
	channel_id: SnowflakeType.describe('The ID of the channel'),
	tag_id: SnowflakeType.describe('The ID of the forum tag'),
});
