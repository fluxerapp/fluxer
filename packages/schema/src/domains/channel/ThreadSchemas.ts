// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	FORUM_TAG_NAME_MAX_LENGTH,
	MAX_APPLIED_TAGS_PER_THREAD,
	MAX_FORUM_TAGS_PER_CHANNEL,
	THREAD_MEMBER_IDS_PREVIEW_SIZE,
} from '@fluxer/constants/src/ThreadConstants';
import {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {Int32Type, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

const ThreadMuteConfig = z
	.object({
		end_time: z.string().nullable().describe('ISO8601 timestamp of when the mute expires'),
		selected_time_window: Int32Type.describe('The selected mute duration in seconds'),
	})
	.nullable();

export const ThreadMetadataResponse = z.object({
	archived: z.boolean().describe('Whether the thread is archived'),
	auto_archive_duration: Int32Type.describe(
		'Minutes of inactivity before the thread stops showing in the channel list',
	),
	archive_timestamp: z.iso.datetime().describe('When the archive status of the thread last changed'),
	locked: z.boolean().describe('Whether only moderators can interact with the thread'),
	invitable: z.boolean().optional().describe('Whether non-moderators can add other non-moderators (private threads)'),
	create_timestamp: z.iso.datetime().describe('When the thread was created'),
});

export type ThreadMetadataResponse = z.infer<typeof ThreadMetadataResponse>;

export const ThreadMemberResponse = z.object({
	id: SnowflakeStringType.optional().describe('The ID of the thread'),
	user_id: SnowflakeStringType.optional().describe('The ID of the user'),
	join_timestamp: z.iso.datetime().describe('When the user last joined the thread'),
	flags: Int32Type.describe('Thread member flags'),
	muted: z.boolean().optional().describe('Whether the user has muted the thread (current user only)'),
	mute_config: ThreadMuteConfig.optional().describe('The mute configuration for the thread (current user only)'),
	member: GuildMemberResponse.optional().describe('The guild member object for the user'),
});

export type ThreadMemberResponse = z.infer<typeof ThreadMemberResponse>;

export const ForumTagResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the tag'),
	name: z.string().max(FORUM_TAG_NAME_MAX_LENGTH).describe('The name of the tag'),
	moderated: z.boolean().describe('Whether only moderators can add or remove this tag'),
	emoji_id: SnowflakeStringType.nullable().describe('The ID of a custom guild emoji'),
	emoji_name: z.string().nullable().describe('The unicode character of the emoji'),
});

export type ForumTagResponse = z.infer<typeof ForumTagResponse>;

export const DefaultReactionEmojiResponse = z.object({
	emoji_id: SnowflakeStringType.nullable().describe('The ID of a custom guild emoji'),
	emoji_name: z.string().nullable().describe('The unicode character of the emoji'),
});

export type DefaultReactionEmojiResponse = z.infer<typeof DefaultReactionEmojiResponse>;

export const ThreadChannelFields = {
	flags: Int32Type.optional().describe('Channel flags'),
	thread_metadata: ThreadMetadataResponse.optional().describe('Thread-specific fields'),
	applied_tags: z
		.array(SnowflakeStringType)
		.max(MAX_APPLIED_TAGS_PER_THREAD)
		.optional()
		.describe('IDs of the tags applied to a forum or media post'),
	message_count: Int32Type.optional().describe('Messages in the thread, excluding the starter and deleted messages'),
	total_message_sent: Int32Type.optional().describe('Messages ever sent in the thread, never decremented'),
	member_count: Int32Type.optional().describe('Approximate number of thread members, capped at 50'),
	member_ids_preview: z
		.array(SnowflakeStringType)
		.max(THREAD_MEMBER_IDS_PREVIEW_SIZE)
		.optional()
		.describe('IDs of the most recently joined members of a forum or media post, newest first'),
	member: ThreadMemberResponse.optional().describe('The thread member object for the current user'),
};

export const ThreadParentChannelFields = {
	flags: Int32Type.optional().describe('Channel flags'),
	default_auto_archive_duration: Int32Type.nullish().describe('Default auto archive duration for new threads'),
	default_thread_rate_limit_per_user: Int32Type.optional().describe('Slowmode copied onto new threads'),
	available_tags: z
		.array(ForumTagResponse)
		.max(MAX_FORUM_TAGS_PER_CHANNEL)
		.optional()
		.describe('Tags that can be applied to posts'),
	default_reaction_emoji: DefaultReactionEmojiResponse.nullish().describe('Default reaction for posts'),
	default_sort_order: Int32Type.nullish().describe('Default sort order for posts'),
	default_forum_layout: Int32Type.optional().describe('Default layout for forum posts'),
	default_tag_setting: z.enum(['match_some', 'match_all']).optional().describe('Default tag search setting'),
};

export const ThreadParentSettingsRpcResponse = z.object({
	channel_id: SnowflakeStringType.describe('The ID of the parent channel'),
	...ThreadParentChannelFields,
});

export type ThreadParentSettingsRpcResponse = z.infer<typeof ThreadParentSettingsRpcResponse>;

export const ThreadMemberRpcResponse = z.object({
	id: SnowflakeStringType.describe('The ID of the thread'),
	user_id: SnowflakeStringType.describe('The ID of the user'),
	join_timestamp: z.iso.datetime().describe('When the user last joined the thread'),
	flags: Int32Type.describe('Thread member flags'),
	muted: z.boolean().optional().describe('Whether the user has muted the thread'),
	mute_config: ThreadMuteConfig.optional().describe('The mute configuration for the thread'),
});

export type ThreadMemberRpcResponse = z.infer<typeof ThreadMemberRpcResponse>;
