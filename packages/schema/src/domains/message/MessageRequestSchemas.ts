// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageFlags, MessageFlagsDescriptions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MESSAGE_LENGTH_NON_PREMIUM, MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {
	ClientAttachmentReferenceRequest,
	ClientAttachmentRequest,
	ClientUploadedAttachmentRequest,
} from '@fluxer/schema/src/domains/message/AttachmentSchemas';
import {AllowedMentionsRequest, MessageReferenceRequest} from '@fluxer/schema/src/domains/message/SharedMessageSchemas';
import {createQueryIntegerType, DateTimeType} from '@fluxer/schema/src/primitives/QueryValidators';
import {
	ColorType,
	createBitflagInt32Type,
	createNamedStringLiteralUnion,
	createStringType,
	createUnboundedStringType,
	Int32Type,
	SnowflakeType,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {AttachmentURLType, URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {z} from 'zod';

const RICH_EMBED_AUTHOR_NAME_MAX_LENGTH = 256 as const;
const RICH_EMBED_MEDIA_DESCRIPTION_MAX_LENGTH = 4096 as const;
export const RICH_EMBED_FOOTER_TEXT_MAX_LENGTH = 2048 as const;
const RICH_EMBED_FIELD_NAME_MAX_LENGTH = 256 as const;
export const RICH_EMBED_FIELD_VALUE_MAX_LENGTH = 1024 as const;
export const RICH_EMBED_TITLE_MAX_LENGTH = 256 as const;
export const RICH_EMBED_DESCRIPTION_MAX_LENGTH = 4096 as const;
const RICH_EMBED_FIELDS_MAX = 25 as const;

function omitEmbedObjectWithoutRequiredField(value: unknown, requiredField: string): unknown {
	if (!value || typeof value !== 'object' || Array.isArray(value)) {
		return value;
	}
	const record = value as Record<string, unknown>;
	if (record[requiredField] === undefined || record[requiredField] === null) {
		return undefined;
	}
	return value;
}

function omitEmptyString(value: unknown): unknown {
	return value === '' ? undefined : value;
}

const RichEmbedAuthorRequestShape = z.object({
	name: createStringType(1, RICH_EMBED_AUTHOR_NAME_MAX_LENGTH).describe('Name of the embed author'),
	url: URLType.nullish().describe('URL to link from the author name'),
	icon_url: URLType.nullish().describe('URL of the author icon'),
});
export const RichEmbedAuthorRequest = z.preprocess(
	(value) => omitEmbedObjectWithoutRequiredField(value, 'name'),
	RichEmbedAuthorRequestShape.nullish(),
);

export type RichEmbedAuthorRequest = z.infer<typeof RichEmbedAuthorRequest>;

export const RichEmbedMediaRequestShape = z.object({
	url: AttachmentURLType.describe('URL of the media (image, video, etc.)'),
	description: createStringType(1, RICH_EMBED_MEDIA_DESCRIPTION_MAX_LENGTH)
		.nullish()
		.describe('Alt text description of the media'),
});
export const RichEmbedMediaRequest = z.preprocess(
	(value) => omitEmbedObjectWithoutRequiredField(value, 'url'),
	RichEmbedMediaRequestShape.nullish(),
);

export type RichEmbedMediaRequest = z.infer<typeof RichEmbedMediaRequest>;

const RichEmbedFooterRequestShape = z.object({
	text: createStringType(1, RICH_EMBED_FOOTER_TEXT_MAX_LENGTH).describe(
		`Footer text (1-${RICH_EMBED_FOOTER_TEXT_MAX_LENGTH} characters)`,
	),
	icon_url: URLType.nullish().describe('URL of the footer icon'),
});
export const RichEmbedFooterRequest = z.preprocess(
	(value) => omitEmbedObjectWithoutRequiredField(value, 'text'),
	RichEmbedFooterRequestShape.nullish(),
);

export type RichEmbedFooterRequest = z.infer<typeof RichEmbedFooterRequest>;

const RichEmbedFieldRequest = z.object({
	name: createStringType(1, RICH_EMBED_FIELD_NAME_MAX_LENGTH).describe('Name of the field'),
	value: createStringType(0, RICH_EMBED_FIELD_VALUE_MAX_LENGTH).describe(
		`Value of the field (0-${RICH_EMBED_FIELD_VALUE_MAX_LENGTH} characters)`,
	),
	inline: z.boolean().default(false).describe('Whether the field should display inline'),
});

export const RichEmbedRequest = z.object({
	url: URLType.nullish().describe('URL of the embed'),
	title: createStringType(0, RICH_EMBED_TITLE_MAX_LENGTH)
		.nullish()
		.describe(`Title of the embed (0-${RICH_EMBED_TITLE_MAX_LENGTH} characters)`),
	color: ColorType.nullish().describe('Color code of the embed (hex integer)'),
	timestamp: DateTimeType.nullish().describe('ISO8601 timestamp for the embed'),
	description: z
		.preprocess(omitEmptyString, createStringType(1, RICH_EMBED_DESCRIPTION_MAX_LENGTH).nullish())
		.describe(`Description of the embed (1-${RICH_EMBED_DESCRIPTION_MAX_LENGTH} characters)`),
	author: RichEmbedAuthorRequest.nullish().describe('Author information'),
	image: RichEmbedMediaRequest.nullish().describe('Image to display in the embed'),
	thumbnail: RichEmbedMediaRequest.nullish().describe('Thumbnail image for the embed'),
	footer: RichEmbedFooterRequest.nullish().describe('Footer information'),
	fields: z
		.array(RichEmbedFieldRequest)
		.max(RICH_EMBED_FIELDS_MAX)
		.nullish()
		.describe(`Array of field objects (max ${RICH_EMBED_FIELDS_MAX})`),
});

export type RichEmbedRequest = z.infer<typeof RichEmbedRequest>;

const MessageAuthorType = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['user', 'user', 'A regular user account'],
			['bot', 'bot', 'An automated bot account'],
			['webhook', 'webhook', 'A webhook-generated message'],
		],
		'The type of author who sent the message',
	),
	'MessageAuthorType',
);

const MessageContentType = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['image', 'image', 'Message contains an uploaded image attachment; linked image previews are embeds'],
			['sound', 'sound', 'Message contains an uploaded audio attachment; linked audio previews are embeds'],
			['video', 'video', 'Message contains an uploaded video attachment; linked video previews are embeds'],
			['file', 'file', 'Message contains at least one uploaded attachment of any type'],
			['sticker', 'sticker', 'Message contains a sticker'],
			['embed', 'embed', 'Message contains a generated link preview or rich embed; uploads are attachments'],
			['link', 'link', 'Message text contains a typed URL link'],
			['poll', 'poll', 'Message contains a poll'],
			['snapshot', 'snapshot', 'Message contains a forwarded message snapshot'],
		],
		'The type of content contained in a message. Upload filters inspect attachments; embed filters inspect generated or supplied message embeds.',
	),
	'MessageContentType',
);

const MessageEmbedType = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[
				'image',
				'image',
				'An image preview from a linked URL or embed object; uploaded images use the image content flag',
			],
			[
				'video',
				'video',
				'A video preview from a linked URL or embed object; uploaded videos use the video content flag',
			],
			[
				'sound',
				'sound',
				'An audio preview from a linked URL or embed object; uploaded audio uses the sound content flag',
			],
			['article', 'article', 'An article or webpage preview with metadata'],
		],
		'The type of generated or supplied embed content, not uploaded attachments',
	),
	'MessageEmbedType',
);

const MessageSortField = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['timestamp', 'timestamp', 'Sort results by message timestamp'],
			['relevance', 'relevance', 'Sort results by search relevance score'],
		],
		'The field to sort search results by',
	),
	'MessageSortField',
);

const MessageSortOrder = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['asc', 'asc', 'Sort in ascending order (oldest/lowest first)'],
			['desc', 'desc', 'Sort in descending order (newest/highest first)'],
		],
		'The order to sort search results',
	),
	'MessageSortOrder',
);

const MessageSearchScope = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			['current', 'current', 'Search only in the current channel or community context'],
			['open_dms', 'open_dms', 'Search across all DMs you currently have open'],
			['all_dms', 'all_dms', "Search across all DMs you've ever been in"],
			['all_guilds', 'all_guilds', "Search across all Communities you're currently in"],
			['all', 'all', "Search across all DMs you've ever been in and all Communities you're currently in"],
			[
				'open_dms_and_all_guilds',
				'open_dms_and_all_guilds',
				"Search across all DMs you currently have open and all Communities you're currently in",
			],
		],
		'Search scope for message searches',
	),
	'MessageSearchScope',
);

export const MessageSearchRequest = z.object({
	hits_per_page: z.number().int().min(1).max(25).default(25).describe('Number of results per page (1-25)'),
	page: z
		.number()
		.int()
		.min(1)
		.max(Number.MAX_SAFE_INTEGER)
		.default(1)
		.describe('Page number for pagination (ignored when cursor is provided)'),
	cursor: z
		.array(z.string())
		.optional()
		.describe('Opaque cursor for search_after pagination. When provided, page is ignored.'),
	max_id: SnowflakeType.optional().describe('Maximum message ID to include in results'),
	min_id: SnowflakeType.optional().describe('Minimum message ID to include in results'),
	content: createStringType(1, 1024).optional().describe('Text content to search for'),
	contents: z.array(createStringType(1, 1024)).max(100).optional().describe('Multiple content queries to search for'),
	exact_phrases: z
		.array(createStringType(1, 1024))
		.max(10)
		.optional()
		.describe('Exact phrases that must appear contiguously in message content'),
	channel_id: z.array(SnowflakeType).max(500).optional().describe('Channel IDs to search in'),
	exclude_channel_id: z.array(SnowflakeType).max(500).optional().describe('Channel IDs to exclude from search'),
	author_type: z.array(MessageAuthorType).max(20).optional().describe('Author types to filter by'),
	exclude_author_type: z.array(MessageAuthorType).max(20).optional().describe('Author types to exclude'),
	author_id: z.array(SnowflakeType).max(100).optional().describe('Author user IDs to filter by'),
	exclude_author_id: z.array(SnowflakeType).max(100).optional().describe('Author user IDs to exclude'),
	mentions: z.array(SnowflakeType).max(100).optional().describe('User IDs that must be mentioned'),
	exclude_mentions: z.array(SnowflakeType).max(100).optional().describe('User IDs that must not be mentioned'),
	mention_everyone: z.boolean().optional().describe('Filter by whether message mentions everyone'),
	pinned: z.boolean().optional().describe('Filter by pinned status'),
	has: z
		.array(MessageContentType)
		.max(20)
		.optional()
		.describe(
			'Content flags the message must have. Use image, video, sound, or file for uploaded attachments and embed for link previews or rich embeds.',
		),
	exclude_has: z
		.array(MessageContentType)
		.max(20)
		.optional()
		.describe(
			'Content flags the message must not have. Use image, video, sound, or file for uploaded attachments and embed for link previews or rich embeds.',
		),
	embed_type: z
		.array(MessageEmbedType)
		.max(20)
		.optional()
		.describe('Generated or supplied embed types to filter by; does not match uploaded attachments'),
	exclude_embed_type: z
		.array(MessageEmbedType)
		.max(20)
		.optional()
		.describe('Generated or supplied embed types to exclude; does not match uploaded attachments'),
	embed_provider: z.array(createStringType(1, 256)).max(50).optional().describe('Embed providers to filter by'),
	exclude_embed_provider: z.array(createStringType(1, 256)).max(50).optional().describe('Embed providers to exclude'),
	link_hostname: z.array(createStringType(1, 255)).max(100).optional().describe('Link hostnames to filter by'),
	exclude_link_hostname: z.array(createStringType(1, 255)).max(100).optional().describe('Link hostnames to exclude'),
	attachment_filename: z
		.array(createStringType(1, 1024))
		.max(100)
		.optional()
		.describe('Attachment filenames to filter by'),
	exclude_attachment_filename: z
		.array(createStringType(1, 1024))
		.max(100)
		.optional()
		.describe('Attachment filenames to exclude'),
	attachment_extension: z.array(createStringType(1, 32)).max(50).optional().describe('File extensions to filter by'),
	exclude_attachment_extension: z
		.array(createStringType(1, 32))
		.max(50)
		.optional()
		.describe('File extensions to exclude'),
	sort_by: MessageSortField.default('timestamp').describe('Field to sort results by'),
	sort_order: MessageSortOrder.default('desc').describe('Sort order for results'),
	include_nsfw: z.boolean().default(false).describe('Whether to include NSFW channel results'),
	scope: MessageSearchScope.optional().describe('Scope to search within when querying messages'),
});

export type MessageSearchRequest = z.infer<typeof MessageSearchRequest>;

export const GlobalSearchMessagesRequest = MessageSearchRequest.extend({
	context_channel_id: SnowflakeType.optional().describe(
		'Channel ID for context when searching across multiple channels',
	),
	context_guild_id: SnowflakeType.optional().describe('Guild ID for context when searching across multiple guilds'),
	channel_ids: z.array(SnowflakeType).max(500).optional().describe('Specific channel IDs to search in'),
});

export type GlobalSearchMessagesRequest = z.infer<typeof GlobalSearchMessagesRequest>;

export const MessageNonceRequest = z
	.union([
		createStringType(1, 32),
		z
			.number()
			.int()
			.nonnegative()
			.safe()
			.transform((value) => value.toString()),
	])
	.pipe(createStringType(1, 32))
	.describe('Client-generated identifier for the message');

export type MessageNonceRequest = z.infer<typeof MessageNonceRequest>;

const MESSAGE_CONTENT_LIMIT_DESCRIPTION = `The message content. Non-premium users can send up to ${MAX_MESSAGE_LENGTH_NON_PREMIUM} characters; premium users, bots, and webhooks can send up to ${MAX_MESSAGE_LENGTH_PREMIUM} characters.`;

export const MessageContentRequest = createUnboundedStringType().describe(MESSAGE_CONTENT_LIMIT_DESCRIPTION);

export type MessageContentRequest = z.infer<typeof MessageContentRequest>;

export const MessageRequestSchema = z
	.object({
		content: MessageContentRequest.nullish(),
		embeds: z.array(RichEmbedRequest).describe('Array of embed objects to include in the message'),
		attachments: z
			.array(z.union([ClientUploadedAttachmentRequest, ClientAttachmentRequest]))
			.describe('Array of attachment objects'),
		message_reference: MessageReferenceRequest.nullish().describe(
			'Reference to another message (for replies or forwards)',
		),
		allowed_mentions: AllowedMentionsRequest.nullish().describe('Controls which mentions trigger notifications'),
		flags: createBitflagInt32Type(
			MessageFlags,
			MessageFlagsDescriptions,
			'Message flags bitfield',
			'MessageFlags',
		).default(0),
		nonce: MessageNonceRequest,
		favorite_meme_id: SnowflakeType.nullish().describe('ID of a favorite meme to attach'),
		sticker_ids: z.array(SnowflakeType).max(3).nullish().describe('Array of sticker IDs to include (max 3)'),
		tts: z.boolean().optional().describe('Whether this is a text-to-speech message'),
	})
	.partial();

export type MessageRequestSchemaType = z.infer<typeof MessageRequestSchema>;

const MessageSnapshotAttachmentEditRequest = z.object({
	id: z.union([Int32Type, SnowflakeType]).describe('The identifier of the snapshot attachment'),
	title: createStringType(1, 1024).nullish().describe('A title for the attachment (1-1024 characters)'),
	description: createStringType(1, 4096)
		.nullish()
		.describe('Alt text description for the attachment (1-4096 characters)'),
});
const MessageSnapshotEditRequest = z.object({
	attachments: z.array(MessageSnapshotAttachmentEditRequest).max(10).optional(),
});
export const MessageUpdateRequestSchema = MessageRequestSchema.pick({
	content: true,
	embeds: true,
	allowed_mentions: true,
}).extend({
	flags: createBitflagInt32Type(
		MessageFlags,
		MessageFlagsDescriptions,
		'Message flags bitfield',
		'MessageFlags',
	).optional(),
	attachments: z
		.array(z.union([ClientUploadedAttachmentRequest, ClientAttachmentReferenceRequest]))
		.optional()
		.describe('Array of attachment objects to keep or add'),
	message_snapshots: z
		.array(MessageSnapshotEditRequest)
		.max(10)
		.optional()
		.describe(
			'Per-snapshot edits aligned by index with the existing snapshots. Currently supports updating attachment metadata (title/description).',
		),
});

export type MessageUpdateRequestSchemaType = z.infer<typeof MessageUpdateRequestSchema>;

export const MessagesQuery = z.object({
	limit: createQueryIntegerType({defaultValue: 50, minValue: 1, maxValue: 100}).describe(
		'Number of messages to return (1-100, default 50)',
	),
	before: SnowflakeType.optional().describe('Get messages before this message ID'),
	after: SnowflakeType.optional().describe('Get messages after this message ID'),
	around: SnowflakeType.optional().describe('Get messages around this message ID'),
});

export type MessagesQuery = z.infer<typeof MessagesQuery>;

const BulkMessageFetchEntryRequest = z.object({
	channel_id: SnowflakeType.describe('The ID of the channel to fetch messages from'),
	limit: z.number().int().min(1).max(25).describe('Number of messages to return for this channel (1-25)'),
	before: SnowflakeType.optional().describe('Get messages before this message ID'),
	after: SnowflakeType.optional().describe('Get messages after this message ID'),
	around: SnowflakeType.optional().describe('Get messages around this message ID'),
});
export const BulkMessageFetchRequest = z
	.object({
		requests: z
			.array(BulkMessageFetchEntryRequest)
			.min(1)
			.max(25)
			.describe('Per-channel message windows to fetch in one request'),
	})
	.superRefine((value, ctx) => {
		const totalRequestedMessages = value.requests.reduce((total, request) => total + request.limit, 0);
		if (totalRequestedMessages > 250) {
			ctx.addIssue({
				code: 'custom',
				message: 'bulk message fetches may request at most 250 messages total',
				path: ['requests'],
			});
		}
	});

export type BulkMessageFetchRequest = z.infer<typeof BulkMessageFetchRequest>;

export const BulkDeleteMessagesRequest = z
	.object({
		message_ids: z.array(SnowflakeType).max(100).optional().describe('Array of message IDs to delete'),
		messages: z.array(SnowflakeType).max(100).optional().describe('Alias for message IDs'),
	})
	.transform((value, ctx) => {
		const messageIds = value.message_ids ?? value.messages;
		if (messageIds === undefined) {
			ctx.addIssue({
				code: 'custom',
				message: 'message_ids or messages is required',
				path: ['message_ids'],
			});
			return z.NEVER;
		}
		return {
			message_ids: messageIds,
		};
	});

export type BulkDeleteMessagesRequest = z.infer<typeof BulkDeleteMessagesRequest>;

export const MessageAckRequest = z.object({
	mention_count: Int32Type.optional().describe('Number of mentions to acknowledge'),
	manual: z.boolean().optional().describe('Whether this is a manual acknowledgement'),
});

export type MessageAckRequest = z.infer<typeof MessageAckRequest>;

export const ChannelPinsQuerySchema = z.object({
	limit: z.coerce
		.number()
		.int()
		.min(1)
		.max(50)
		.optional()
		.describe('Maximum number of pinned messages to return (1-50)'),
	before: z.coerce.date().optional().describe('Get pinned messages before this timestamp'),
});

export type ChannelPinsQuerySchema = z.infer<typeof ChannelPinsQuerySchema>;

export const ReactionUsersQuerySchema = z.object({
	limit: z.coerce.number().int().min(1).max(100).optional().describe('Maximum number of users to return (1-100)'),
	after: SnowflakeType.optional().describe('Get users after this user ID'),
});

export type ReactionUsersQuerySchema = z.infer<typeof ReactionUsersQuerySchema>;
