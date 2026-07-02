// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MessageAttachmentFlags,
	MessageAttachmentFlagsDescriptions,
	MessageFlags,
	MessageFlagsDescriptions,
} from '@fluxer/constants/src/ChannelConstants';
import {AVATAR_MAX_SIZE, MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {
	MessageContentRequest,
	MessageNonceRequest,
	RichEmbedRequest,
} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import {AllowedMentionsRequest, MessageReferenceRequest} from '@fluxer/schema/src/domains/message/SharedMessageSchemas';
import {createBase64StringType} from '@fluxer/schema/src/primitives/FileValidators';
import {QueryBooleanType} from '@fluxer/schema/src/primitives/QueryValidators';
import {
	coerceNumberFromString,
	createBitflagInt32Type,
	createStringType,
	createUnboundedStringType,
	Int32Type,
	NonNegativeSafeIntegerType,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {WebhookNameType} from '@fluxer/schema/src/primitives/UserValidators';
import {z} from 'zod';

export const WebhookCreateRequest = z.object({
	name: WebhookNameType.describe('The name of the webhook'),
	avatar: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
		.nullish()
		.describe('The avatar image as a base64-encoded data URI'),
});

export type WebhookCreateRequest = z.infer<typeof WebhookCreateRequest>;

export const WebhookUpdateRequest = z
	.object({
		name: WebhookNameType.describe('The new name of the webhook'),
		avatar: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
			.nullish()
			.describe('The new avatar image as a base64-encoded data URI'),
		channel_id: SnowflakeType.describe('The ID of the channel to move the webhook to'),
	})
	.partial();

export type WebhookUpdateRequest = z.infer<typeof WebhookUpdateRequest>;

export const WebhookTokenUpdateRequest = z
	.object({
		name: WebhookNameType.describe('The new name of the webhook'),
		avatar: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
			.nullish()
			.describe('The new avatar image as a base64-encoded data URI'),
	})
	.partial()
	.strict();

export type WebhookTokenUpdateRequest = z.infer<typeof WebhookTokenUpdateRequest>;

const WebhookAttachmentRequest = z.object({
	id: z
		.union([SnowflakeType, coerceNumberFromString(Int32Type)])
		.optional()
		.describe('Attachment ID for referencing uploaded files'),
	filename: createStringType(1, 1024).optional().describe('Name of the file (1-1024 characters)'),
	description: createStringType(1, 4096).optional().describe('Description for the attachment (max 4096 characters)'),
	content_type: createStringType(1, 256).optional().describe('MIME type of the file'),
	size: NonNegativeSafeIntegerType.optional().describe('Size of the file in bytes'),
	url: URLType.optional().describe('URL of the attachment'),
	proxy_url: URLType.optional().describe('Proxied URL of the attachment'),
	height: z.number().int().optional().describe('Height of the image/video in pixels'),
	width: z.number().int().optional().describe('Width of the image/video in pixels'),
	ephemeral: z.boolean().optional().describe('Whether this attachment is ephemeral'),
	duration: z.number().optional().describe('Duration of audio file in seconds'),
	waveform: createStringType().optional().describe('Base64-encoded bytearray of audio waveform'),
	flags: createBitflagInt32Type(
		MessageAttachmentFlags,
		MessageAttachmentFlagsDescriptions,
		'Attachment flags bitfield',
		'MessageAttachmentFlags',
	).optional(),
});

export const WebhookMessageRequest = z
	.object({
		content: MessageContentRequest.nullish(),
		embeds: z.array(RichEmbedRequest).optional().describe('Array of embed objects to include in the message'),
		attachments: z.array(WebhookAttachmentRequest).optional().describe('Array of attachment objects'),
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
		nonce: MessageNonceRequest.optional(),
		favorite_meme_id: SnowflakeType.nullish().describe('ID of a favorite meme to attach'),
		sticker_ids: z.array(SnowflakeType).max(3).nullish().describe('Array of sticker IDs to include (max 3)'),
		tts: z.boolean().optional().describe('Whether this is a text-to-speech message'),
		username: WebhookNameType.nullish().describe('Override the default username of the webhook for this message'),
		avatar_url: URLType.nullish().describe('Override the default avatar URL of the webhook for this message'),
	})
	.partial();

export type WebhookMessageRequest = z.infer<typeof WebhookMessageRequest>;

export const WebhookMessageEditRequest = z
	.object({
		content: MessageContentRequest.nullish().describe(
			`The new message content (up to ${MAX_MESSAGE_LENGTH_PREMIUM} characters)`,
		),
		embeds: z.array(RichEmbedRequest).optional().describe('Array of embed objects to include in the message'),
		flags: createBitflagInt32Type(
			MessageFlags,
			MessageFlagsDescriptions,
			'Message flags bitfield',
			'MessageFlags',
		).optional(),
		allowed_mentions: AllowedMentionsRequest.nullish().describe('Controls which mentions trigger notifications'),
	})
	.partial();

export type WebhookMessageEditRequest = z.infer<typeof WebhookMessageEditRequest>;

export const WebhookExecuteQueryRequest = z.object({
	wait: QueryBooleanType.optional().default(false).describe('Whether to wait for the webhook response'),
});

export type WebhookExecuteQueryRequest = z.infer<typeof WebhookExecuteQueryRequest>;

const SlackAttachmentFieldSchema = z.object({
	title: createUnboundedStringType().optional().describe('Title of the field'),
	value: createUnboundedStringType().optional().describe('Value of the field'),
	short: z.boolean().optional().describe('Whether the field should be displayed as a short column'),
});
const SlackUnixSecondsSchema = coerceNumberFromString(z.number().int().nonnegative());
const SlackAttachmentSchema = z.object({
	fallback: createUnboundedStringType().optional().describe('Fallback text for notifications'),
	pretext: createUnboundedStringType().optional().describe('Text that appears above the attachment block'),
	text: createUnboundedStringType().optional().describe('Main text content of the attachment'),
	color: createUnboundedStringType().optional().describe('Colour of the attachment sidebar (hex code or preset)'),
	title: createUnboundedStringType().optional().describe('Title of the attachment'),
	title_link: createUnboundedStringType().optional().describe('URL to link from the title'),
	fields: z.array(SlackAttachmentFieldSchema).optional().describe('Array of field objects'),
	footer: createUnboundedStringType().optional().describe('Footer text displayed at the bottom'),
	ts: SlackUnixSecondsSchema.optional().describe('Unix timestamp for the attachment footer'),
	author_name: createUnboundedStringType().optional().describe('Name of the author'),
	author_link: createUnboundedStringType().optional().describe('URL to link from the author name'),
	author_icon: createUnboundedStringType().optional().describe('URL for the author icon image'),
	image_url: createUnboundedStringType().optional().describe('URL of the main image to display'),
	thumb_url: createUnboundedStringType().optional().describe('URL of a thumbnail image'),
});
export const SlackWebhookRequest = z.object({
	text: createUnboundedStringType().optional().describe('Main text content of the message'),
	username: WebhookNameType.optional().describe('Override the default username of the webhook'),
	icon_url: createUnboundedStringType().optional().describe('Override the default icon of the webhook'),
	attachments: z.array(SlackAttachmentSchema).optional().describe('Array of attachment objects'),
});

export type SlackWebhookRequest = z.infer<typeof SlackWebhookRequest>;
