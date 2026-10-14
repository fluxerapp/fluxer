// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ThreadParentConfigPatch} from '@app/api/channel/repositories/ThreadRepository';
import type {ForumTagUdt} from '@app/api/database/types/ThreadTypes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {CHANNEL_TOPIC_MAX_LENGTH} from '@fluxer/constants/src/LimitConstants';
import {
	ChannelFlags,
	FORUM_TAG_NAME_MAX_LENGTH,
	FORUM_TOPIC_MAX_LENGTH,
	ForumLayoutTypes,
	ForumSortOrderTypes,
	ForumTagSettings,
	MAX_FORUM_TAGS_PER_CHANNEL,
	settableChannelFlags,
	THREAD_AUTO_ARCHIVE_DURATIONS,
	THREAD_ONLY_CHANNEL_TYPES,
	THREAD_PARENT_CHANNEL_TYPES,
	THREAD_RATE_LIMIT_PER_USER_MAX,
} from '@fluxer/constants/src/ThreadConstants';
import type {TemplateChannel} from '@fluxer/schema/src/domains/guild/GuildTemplateSchemas';
import {isValidSingleUnicodeEmoji} from '@fluxer/schema/src/primitives/EmojiValidators';

const THE_OTHER_PLATFORM_GUILD_STAGE_VOICE_CHANNEL_TYPE = 13;

const IMPORTED_CHANNEL_TYPES = new Set<number>([
	ChannelTypes.GUILD_TEXT,
	ChannelTypes.GUILD_VOICE,
	ChannelTypes.GUILD_CATEGORY,
	ChannelTypes.GUILD_ANNOUNCEMENT,
	ChannelTypes.GUILD_FORUM,
	ChannelTypes.GUILD_MEDIA,
]);

const FORUM_SORT_ORDERS = new Set<number>(Object.values(ForumSortOrderTypes));
const FORUM_LAYOUTS = new Set<number>(Object.values(ForumLayoutTypes));
const FORUM_TAG_SETTINGS = new Set<string>(Object.values(ForumTagSettings));
const AUTO_ARCHIVE_DURATIONS = new Set<number>(THREAD_AUTO_ARCHIVE_DURATIONS);

export function mapOtherPlatformTemplateChannelType(channelType: number): number | null {
	if (IMPORTED_CHANNEL_TYPES.has(channelType)) return channelType;
	if (channelType === THE_OTHER_PLATFORM_GUILD_STAGE_VOICE_CHANNEL_TYPE) return ChannelTypes.GUILD_VOICE;
	return null;
}

function clipText(value: string, maxLength: number): string {
	if (value.length <= maxLength) return value;
	const last = value.charCodeAt(maxLength - 1);
	const end = last >= 0xd800 && last <= 0xdbff ? maxLength - 1 : maxLength;
	return value.slice(0, end);
}

export function clipTemplateTopic(topic: string | null | undefined, channelType: number): string | null {
	if (!topic) return null;
	const maxLength = THREAD_ONLY_CHANNEL_TYPES.has(channelType) ? FORUM_TOPIC_MAX_LENGTH : CHANNEL_TOPIC_MAX_LENGTH;
	return clipText(topic, maxLength);
}

function pickUnicodeEmoji(emojiName: string | null | undefined): string | null {
	return emojiName != null && isValidSingleUnicodeEmoji(emojiName) ? emojiName : null;
}

async function buildTemplateTags(
	tags: NonNullable<TemplateChannel['available_tags']>,
	generateId: () => Promise<bigint>,
): Promise<Array<ForumTagUdt>> {
	const names = new Set<string>();
	const result: Array<ForumTagUdt> = [];
	for (const tag of tags) {
		if (result.length >= MAX_FORUM_TAGS_PER_CHANNEL) break;
		const name = clipText(tag.name.trim(), FORUM_TAG_NAME_MAX_LENGTH);
		if (name.length === 0 || names.has(name)) continue;
		names.add(name);
		result.push({
			id: await generateId(),
			name,
			moderated: tag.moderated === true,
			emoji_id: null,
			emoji_name: pickUnicodeEmoji(tag.emoji_name),
		});
	}
	return result;
}

export async function buildTemplateThreadParentConfig(
	channel: TemplateChannel,
	channelType: number,
	generateId: () => Promise<bigint>,
): Promise<ThreadParentConfigPatch | null> {
	if (!THREAD_PARENT_CHANNEL_TYPES.has(channelType)) return null;
	const patch: ThreadParentConfigPatch = {};
	const archiveDuration = channel.default_auto_archive_duration;
	if (archiveDuration != null && AUTO_ARCHIVE_DURATIONS.has(archiveDuration)) {
		patch.default_auto_archive_duration = archiveDuration;
	}
	const threadSlowmode = channel.default_thread_rate_limit_per_user;
	if (threadSlowmode != null && threadSlowmode > 0) {
		patch.default_thread_rate_limit_per_user = Math.min(threadSlowmode, THREAD_RATE_LIMIT_PER_USER_MAX);
	}
	if (THREAD_ONLY_CHANNEL_TYPES.has(channelType)) {
		const tags = await buildTemplateTags(channel.available_tags ?? [], generateId);
		if (tags.length > 0) patch.available_tags = tags;
		const reactionEmoji = pickUnicodeEmoji(channel.default_reaction_emoji?.emoji_name);
		if (reactionEmoji) patch.default_reaction_emoji_name = reactionEmoji;
		const sortOrder = channel.default_sort_order;
		if (sortOrder != null && FORUM_SORT_ORDERS.has(sortOrder)) patch.default_sort_order = sortOrder;
		const layout = channel.default_forum_layout;
		if (channelType === ChannelTypes.GUILD_FORUM && layout != null && FORUM_LAYOUTS.has(layout)) {
			patch.default_forum_layout = layout;
		}
		const tagSetting = channel.default_tag_setting;
		if (tagSetting != null && FORUM_TAG_SETTINGS.has(tagSetting)) patch.default_tag_setting = tagSetting;
		let flags = (channel.flags ?? 0) & settableChannelFlags(channelType);
		if (!tags.some((tag) => !tag.moderated)) flags &= ~ChannelFlags.REQUIRE_TAG;
		if (flags !== 0) patch.flags = flags;
	}
	return Object.keys(patch).length > 0 ? patch : null;
}
