// SPDX-License-Identifier: AGPL-3.0-or-later

import {AllowedMentionParseTypes, MessageReferenceTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	AllowedMentionParseTypeSchema,
	MessageReferenceTypeSchema,
} from '@fluxer/schema/src/primitives/MessageValidators';
import {Int32Type, SnowflakeType, withFieldDescription} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const ALLOWED_MENTIONS_PARSE = [
	AllowedMentionParseTypes.USERS,
	AllowedMentionParseTypes.ROLES,
	AllowedMentionParseTypes.EVERYONE,
] as const;
export const AllowedMentionsRequest = z.object({
	parse: z.array(AllowedMentionParseTypeSchema).optional().describe('Types of mentions to parse from content'),
	users: z.array(SnowflakeType).max(100).optional().describe('Array of user IDs to mention (max 100)'),
	roles: z.array(SnowflakeType).max(100).optional().describe('Array of role IDs to mention (max 100)'),
	replied_user: z.boolean().optional().describe('Whether to mention the author of the replied message'),
});

export type AllowedMentionsRequest = z.infer<typeof AllowedMentionsRequest>;

export const MessageReferenceRequest = z
	.object({
		message_id: SnowflakeType.describe('ID of the message being referenced'),
		channel_id: SnowflakeType.optional().describe('ID of the channel containing the referenced message'),
		guild_id: SnowflakeType.optional().describe('ID of the guild containing the referenced message'),
		type: withFieldDescription(MessageReferenceTypeSchema, 'Type of reference (0 = default, 1 = forward)').optional(),
		attachment_ids: z
			.array(SnowflakeType)
			.max(10)
			.optional()
			.describe('Optional attachment IDs to include when forwarding only selected media'),
		embed_indices: z
			.array(Int32Type.nonnegative())
			.max(10)
			.optional()
			.describe('Optional embed indices to include when forwarding only selected media'),
	})
	.refine(
		(data) => {
			if (data.type === MessageReferenceTypes.FORWARD) {
				return data.channel_id !== undefined && data.message_id !== undefined;
			}
			return true;
		},
		{
			error: 'Forward message reference must include channel_id and message_id',
		},
	);

export type MessageReferenceRequest = z.infer<typeof MessageReferenceRequest>;
