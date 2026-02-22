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

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	AVATAR_MAX_SIZE,
	CHANNEL_RATE_LIMIT_PER_USER_MAX,
	CHANNEL_RATE_LIMIT_PER_USER_MIN,
	CHANNEL_TOPIC_MAX_LENGTH,
	CHANNEL_TOPIC_MIN_LENGTH,
	RTC_REGION_ID_MAX_LENGTH,
	RTC_REGION_ID_MIN_LENGTH,
	VOICE_CHANNEL_BITRATE_MAX,
	VOICE_CHANNEL_BITRATE_MIN,
	VOICE_CHANNEL_USER_LIMIT_MAX,
	VOICE_CHANNEL_USER_LIMIT_MIN,
} from '@fluxer/constants/src/LimitConstants';
import {ChannelNicknameOverrides} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {ChannelOverwriteTypeSchema, GeneralChannelNameType} from '@fluxer/schema/src/primitives/ChannelValidators';
import {createBase64StringType} from '@fluxer/schema/src/primitives/FileValidators';
import {QueryBooleanType} from '@fluxer/schema/src/primitives/QueryValidators';
import {
	createNamedLiteral,
	createNamedLiteralUnion,
	createStringType,
	SnowflakeType,
	UnsignedInt64Type,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {z} from 'zod';

export const ChannelOverwriteRequest = z.object({
	id: SnowflakeType.describe('The ID of the role or user to overwrite permissions for'),
	type: createNamedLiteralUnion(
		[
			[0, 'ROLE'],
			[1, 'MEMBER'],
		],
		'The type of overwrite (0 = role, 1 = member)',
	),
	allow: UnsignedInt64Type.optional().describe('fluxer:UnsignedInt64Type Bitwise value of allowed permissions'),
	deny: UnsignedInt64Type.optional().describe('fluxer:UnsignedInt64Type Bitwise value of denied permissions'),
});

export type ChannelOverwriteRequest = z.infer<typeof ChannelOverwriteRequest>;

const ChannelCommonBase = z.object({
	topic: createStringType(CHANNEL_TOPIC_MIN_LENGTH, CHANNEL_TOPIC_MAX_LENGTH)
		.nullish()
		.describe(`The channel topic (${CHANNEL_TOPIC_MIN_LENGTH}-${CHANNEL_TOPIC_MAX_LENGTH} characters)`),
	url: URLType.nullish().describe('External URL for link channels'),
	parent_id: SnowflakeType.nullish().describe('ID of the parent category for this channel'),
	bitrate: z
		.number()
		.int()
		.min(VOICE_CHANNEL_BITRATE_MIN)
		.max(VOICE_CHANNEL_BITRATE_MAX)
		.nullish()
		.describe(`Voice channel bitrate in bits per second (${VOICE_CHANNEL_BITRATE_MIN}-${VOICE_CHANNEL_BITRATE_MAX})`),
	user_limit: z
		.number()
		.int()
		.min(VOICE_CHANNEL_USER_LIMIT_MIN)
		.max(VOICE_CHANNEL_USER_LIMIT_MAX)
		.nullish()
		.describe(
			`Maximum users allowed in voice channel (${VOICE_CHANNEL_USER_LIMIT_MIN}-${VOICE_CHANNEL_USER_LIMIT_MAX}, ${VOICE_CHANNEL_USER_LIMIT_MIN} means unlimited)`,
		),
	permission_overwrites: z
		.array(ChannelOverwriteRequest)
		.optional()
		.describe('Permission overwrites for roles and members'),
});

const ChannelCreateCommon = ChannelCommonBase.extend({
	nsfw: z.boolean().default(false).describe('Whether the channel is marked as NSFW'),
});

const ChannelUpdateCommon = ChannelCommonBase.extend({
	nsfw: z.boolean().nullish().describe('Whether the channel is marked as NSFW'),
	rate_limit_per_user: z
		.number()
		.int()
		.min(CHANNEL_RATE_LIMIT_PER_USER_MIN)
		.max(CHANNEL_RATE_LIMIT_PER_USER_MAX)
		.nullish()
		.describe(`Slowmode delay in seconds (${CHANNEL_RATE_LIMIT_PER_USER_MIN}-${CHANNEL_RATE_LIMIT_PER_USER_MAX})`),
	icon: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
		.nullish()
		.describe('Base64-encoded icon image for group DM channels'),
	owner_id: SnowflakeType.nullish().describe('ID of the new owner for group DM channels'),
	nicks: ChannelNicknameOverrides.optional().describe('Custom nicknames for users in this channel'),
	rtc_region: createStringType(RTC_REGION_ID_MIN_LENGTH, RTC_REGION_ID_MAX_LENGTH)
		.nullish()
		.describe(
			`Voice region ID for the voice channel (${RTC_REGION_ID_MIN_LENGTH}-${RTC_REGION_ID_MAX_LENGTH} characters)`,
		),
});

export const ChannelCreateTextRequest = ChannelCreateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_TEXT, 'GUILD_TEXT', 'Channel type (text channel)'),
	name: GeneralChannelNameType.describe('The name of the channel'),
});

export type ChannelCreateTextRequest = z.infer<typeof ChannelCreateTextRequest>;

export const ChannelCreateVoiceRequest = ChannelCreateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_VOICE, 'GUILD_VOICE', 'Channel type (voice channel)'),
	name: GeneralChannelNameType.describe('The name of the channel'),
});

export type ChannelCreateVoiceRequest = z.infer<typeof ChannelCreateVoiceRequest>;

export const ChannelCreateCategoryRequest = ChannelCreateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_CATEGORY, 'GUILD_CATEGORY', 'Channel type (category)'),
	name: GeneralChannelNameType.describe('The name of the category'),
});

export type ChannelCreateCategoryRequest = z.infer<typeof ChannelCreateCategoryRequest>;

export const ChannelCreateLinkRequest = ChannelCreateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_LINK, 'GUILD_LINK', 'Channel type (link channel)'),
	name: GeneralChannelNameType.describe('The name of the channel'),
});

export type ChannelCreateLinkRequest = z.infer<typeof ChannelCreateLinkRequest>;

export const ChannelCreateRequest = z.discriminatedUnion('type', [
	ChannelCreateTextRequest,
	ChannelCreateVoiceRequest,
	ChannelCreateCategoryRequest,
	ChannelCreateLinkRequest,
]);

export type ChannelCreateRequest = z.infer<typeof ChannelCreateRequest>;

export const ChannelUpdateTextRequest = ChannelUpdateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_TEXT, 'GUILD_TEXT', 'Channel type (text channel)'),
	name: GeneralChannelNameType.nullish().describe('The name of the channel'),
});

export type ChannelUpdateTextRequest = z.infer<typeof ChannelUpdateTextRequest>;

export const ChannelUpdateVoiceRequest = ChannelUpdateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_VOICE, 'GUILD_VOICE', 'Channel type (voice channel)'),
	name: GeneralChannelNameType.nullish().describe('The name of the channel'),
});

export type ChannelUpdateVoiceRequest = z.infer<typeof ChannelUpdateVoiceRequest>;

export const ChannelUpdateCategoryRequest = ChannelUpdateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_CATEGORY, 'GUILD_CATEGORY', 'Channel type (category)'),
	name: GeneralChannelNameType.nullish().describe('The name of the category'),
});

export type ChannelUpdateCategoryRequest = z.infer<typeof ChannelUpdateCategoryRequest>;

export const ChannelUpdateLinkRequest = ChannelUpdateCommon.extend({
	type: createNamedLiteral(ChannelTypes.GUILD_LINK, 'GUILD_LINK', 'Channel type (link channel)'),
	name: GeneralChannelNameType.nullish().describe('The name of the channel'),
});

export type ChannelUpdateLinkRequest = z.infer<typeof ChannelUpdateLinkRequest>;

export const ChannelUpdateGroupDmRequest = z.object({
	type: createNamedLiteral(ChannelTypes.GROUP_DM, 'GROUP_DM', 'Channel type (group DM)'),
	name: GeneralChannelNameType.nullish().describe('The name of the group DM'),
	icon: createBase64StringType(1, Math.ceil(AVATAR_MAX_SIZE * (4 / 3)))
		.nullish()
		.describe('Base64-encoded icon image for the group DM'),
	owner_id: SnowflakeType.nullish().describe('ID of the new owner of the group DM'),
	nicks: ChannelNicknameOverrides.nullish().describe('Custom nicknames for users in this group DM'),
});

export type ChannelUpdateGroupDmRequest = z.infer<typeof ChannelUpdateGroupDmRequest>;

export const ChannelUpdateRequest = z.discriminatedUnion('type', [
	ChannelUpdateTextRequest,
	ChannelUpdateVoiceRequest,
	ChannelUpdateCategoryRequest,
	ChannelUpdateLinkRequest,
	ChannelUpdateGroupDmRequest,
]);

export type ChannelUpdateRequest = z.infer<typeof ChannelUpdateRequest>;

export const PermissionOverwriteCreateRequest = z.object({
	type: ChannelOverwriteTypeSchema.describe('The type of overwrite (0 = role, 1 = member)'),
	allow: UnsignedInt64Type.nullish().describe('fluxer:UnsignedInt64Type Bitwise value of allowed permissions'),
	deny: UnsignedInt64Type.nullish().describe('fluxer:UnsignedInt64Type Bitwise value of denied permissions'),
});

export type PermissionOverwriteCreateRequest = z.infer<typeof PermissionOverwriteCreateRequest>;

export const DeleteChannelQuery = z.object({
	silent: QueryBooleanType.describe('Whether to suppress the system message when leaving a group DM'),
});

export type DeleteChannelQuery = z.infer<typeof DeleteChannelQuery>;

export const ReadStateAckBulkRequest = z.object({
	read_states: z
		.array(
			z.object({
				channel_id: SnowflakeType.describe('The ID of the channel'),
				message_id: SnowflakeType.describe('The ID of the last read message'),
			}),
		)
		.min(1)
		.max(100)
		.describe('Array of channel/message pairs to acknowledge'),
});

export type ReadStateAckBulkRequest = z.infer<typeof ReadStateAckBulkRequest>;

export const ChannelPositionUpdateRequest = z.array(
	z.object({
		id: SnowflakeType.describe('The ID of the channel to reposition'),
		position: z.number().int().nonnegative().optional().describe('New position for the channel'),
		parent_id: SnowflakeType.nullish().describe('New parent category ID'),
		preceding_sibling_id: SnowflakeType.nullish().describe(
			'ID of the sibling channel that should directly precede this channel after reordering',
		),
		lock_permissions: z.boolean().optional().describe('Whether to sync permissions with the new parent'),
	}),
);

export type ChannelPositionUpdateRequest = z.infer<typeof ChannelPositionUpdateRequest>;

export const CallUpdateBodySchema = z.object({
	region: createStringType(RTC_REGION_ID_MIN_LENGTH, RTC_REGION_ID_MAX_LENGTH)
		.nullish()
		.describe(
			`The preferred voice region for the call (${RTC_REGION_ID_MIN_LENGTH}-${RTC_REGION_ID_MAX_LENGTH} characters). Omit or set to null for automatic region selection.`,
		),
});
export type CallUpdateBodySchema = z.infer<typeof CallUpdateBodySchema>;

export const CallRingBodySchema = z.object({
	recipients: z.array(SnowflakeType).optional().describe('User IDs to ring for the call'),
});
export type CallRingBodySchema = z.infer<typeof CallRingBodySchema>;

export const StreamUpdateBodySchema = z.object({
	region: createStringType(RTC_REGION_ID_MIN_LENGTH, RTC_REGION_ID_MAX_LENGTH)
		.optional()
		.describe(
			`The preferred voice region for the stream (${RTC_REGION_ID_MIN_LENGTH}-${RTC_REGION_ID_MAX_LENGTH} characters)`,
		),
});
export type StreamUpdateBodySchema = z.infer<typeof StreamUpdateBodySchema>;

export const StreamPreviewUploadBodySchema = z.object({
	channel_id: SnowflakeType.describe('The ID of the channel where the stream is active'),
	thumbnail: createStringType(1, 2_000_000).describe('Base64-encoded thumbnail image data'),
	content_type: createStringType(1, 64).optional().describe('MIME type of the thumbnail image'),
});
export type StreamPreviewUploadBodySchema = z.infer<typeof StreamPreviewUploadBodySchema>;
