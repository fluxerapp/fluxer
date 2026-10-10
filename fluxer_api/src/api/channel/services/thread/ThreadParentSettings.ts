// SPDX-License-Identifier: AGPL-3.0-or-later

import {createEmojiID, type GuildID} from '@app/api/BrandedTypes';
import type {ThreadParentConfigPatch, ThreadRepository} from '@app/api/channel/repositories/ThreadRepository';
import type {ForumTagUdt} from '@app/api/database/types/ThreadTypes';
import {guildActive, type ThreadViewer, viewerActive} from '@app/api/experiment/ChannelThreadsGate';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {Channel} from '@app/api/models/Channel';
import type {ThreadParentConfig} from '@app/api/models/ThreadParentConfig';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	ChannelFlags,
	ForumLayoutTypes,
	ForumTagSettings,
	MAX_FORUM_TAGS_PER_CHANNEL,
	settableChannelFlags,
	THREAD_ONLY_CHANNEL_TYPES,
	THREAD_PARENT_CHANNEL_TYPES,
} from '@fluxer/constants/src/ThreadConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ForumTagNamesMustBeUniqueError} from '@fluxer/errors/src/domains/channel/ForumTagNamesMustBeUniqueError';
import {HideMediaDownloadOptionMediaOnlyError} from '@fluxer/errors/src/domains/channel/HideMediaDownloadOptionMediaOnlyError';
import {MaxForumTagsError} from '@fluxer/errors/src/domains/channel/MaxForumTagsError';
import {NoTagsAvailableToNonModeratorsError} from '@fluxer/errors/src/domains/channel/NoTagsAvailableToNonModeratorsError';
import {UnknownForumTagError} from '@fluxer/errors/src/domains/channel/UnknownForumTagError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownGuildEmojiError} from '@fluxer/errors/src/domains/guild/UnknownGuildEmojiError';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {
	DefaultReactionEmojiRequest,
	ForumTagUpdateRequest,
} from '@fluxer/schema/src/domains/channel/ForumRequestSchemas';
import {isValidSingleUnicodeEmoji} from '@fluxer/schema/src/primitives/EmojiValidators';

export interface ThreadParentSettingsInput {
	default_auto_archive_duration?: number | null;
	default_thread_rate_limit_per_user?: number | null;
	available_tags?: Array<ForumTagUpdateRequest>;
	default_reaction_emoji?: DefaultReactionEmojiRequest | null;
	default_sort_order?: number | null;
	default_forum_layout?: number | null;
	default_tag_setting?: string | null;
	flags?: number;
}

const TEXT_PARENT_KEYS = ['default_auto_archive_duration', 'default_thread_rate_limit_per_user'] as const;
const THREAD_ONLY_KEYS = [
	...TEXT_PARENT_KEYS,
	'available_tags',
	'default_reaction_emoji',
	'default_sort_order',
	'default_forum_layout',
	'default_tag_setting',
	'flags',
] as const;

export function pickThreadParentSettings(channelType: number, data: object): ThreadParentSettingsInput | null {
	if (!THREAD_PARENT_CHANNEL_TYPES.has(channelType)) return null;
	const keys = THREAD_ONLY_CHANNEL_TYPES.has(channelType) ? THREAD_ONLY_KEYS : TEXT_PARENT_KEYS;
	const source = data as Record<string, unknown>;
	const picked: Record<string, unknown> = {};
	for (const key of keys) {
		if (source[key] !== undefined) picked[key] = source[key];
	}
	if (channelType !== ChannelTypes.GUILD_FORUM) delete picked.default_forum_layout;
	return Object.keys(picked).length > 0 ? (picked as ThreadParentSettingsInput) : null;
}

export function assertSettableParentFlags(channelType: number, flags: number): void {
	const settable = settableChannelFlags(channelType);
	if ((flags & ~settable) === 0) return;
	if ((flags & ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS) !== 0 && channelType !== ChannelTypes.GUILD_MEDIA) {
		throw new HideMediaDownloadOptionMediaOnlyError();
	}
	throw InputValidationError.fromCode('flags', ValidationErrorCodes.INVALID_FORMAT);
}

function assertSingleEmoji(
	field: string,
	emoji: {emoji_id?: bigint | null; emoji_name?: string | null} | null | undefined,
): void {
	if (emoji?.emoji_id != null && emoji.emoji_name != null) {
		throw InputValidationError.fromCode(field, ValidationErrorCodes.INVALID_FORMAT);
	}
	if (emoji?.emoji_name != null && !isValidSingleUnicodeEmoji(emoji.emoji_name)) {
		throw InputValidationError.fromCode(`${field}.emoji_name`, ValidationErrorCodes.NOT_A_VALID_UNICODE_EMOJI);
	}
}

async function assertGuildEmoji(
	guildRepository: GuildRepository,
	guildId: GuildID,
	emojiId: bigint | null | undefined,
): Promise<void> {
	if (emojiId == null) return;
	const emoji = await guildRepository.getEmoji(createEmojiID(emojiId), guildId);
	if (!emoji) throw new UnknownGuildEmojiError();
}

async function buildTags(params: {
	input: Array<ForumTagUpdateRequest>;
	current: ReadonlyArray<ForumTagUdt>;
	guildId: GuildID;
	guildRepository: GuildRepository;
	generateId: () => Promise<bigint>;
}): Promise<Array<ForumTagUdt>> {
	if (params.input.length > MAX_FORUM_TAGS_PER_CHANNEL) throw new MaxForumTagsError(MAX_FORUM_TAGS_PER_CHANNEL);
	const existing = new Set(params.current.map((tag) => tag.id));
	const storedEmoji = new Map(params.current.map((tag) => [tag.id, tag.emoji_id ?? null]));
	const names = new Set<string>();
	const tags: Array<ForumTagUdt> = [];
	for (const [index, tag] of params.input.entries()) {
		if (names.has(tag.name)) throw new ForumTagNamesMustBeUniqueError();
		names.add(tag.name);
		assertSingleEmoji(`available_tags.${index}`, tag);
		if (tag.id === undefined || storedEmoji.get(tag.id) !== (tag.emoji_id ?? null)) {
			await assertGuildEmoji(params.guildRepository, params.guildId, tag.emoji_id);
		}
		if (tag.id !== undefined) {
			if (!existing.delete(tag.id)) throw new UnknownForumTagError();
		}
		tags.push({
			id: tag.id ?? (await params.generateId()),
			name: tag.name,
			moderated: tag.moderated ?? false,
			emoji_id: tag.emoji_id ?? null,
			emoji_name: tag.emoji_name ?? null,
		});
	}
	return tags;
}

export async function buildThreadParentPatch(params: {
	channelType: number;
	guildId: GuildID;
	input: ThreadParentSettingsInput;
	current: ThreadParentConfig | null;
	guildRepository: GuildRepository;
	generateId: () => Promise<bigint>;
}): Promise<ThreadParentConfigPatch> {
	const {input, current} = params;
	const patch: ThreadParentConfigPatch = {};
	if (input.default_auto_archive_duration !== undefined) {
		patch.default_auto_archive_duration = input.default_auto_archive_duration;
	}
	if (input.default_thread_rate_limit_per_user !== undefined) {
		patch.default_thread_rate_limit_per_user = input.default_thread_rate_limit_per_user;
	}
	if (input.flags !== undefined) {
		assertSettableParentFlags(params.channelType, input.flags);
		patch.flags = input.flags;
	}
	if (input.available_tags !== undefined) {
		patch.available_tags = await buildTags({
			input: input.available_tags,
			current: current?.availableTags.map((tag) => tag.toUdt()) ?? [],
			guildId: params.guildId,
			guildRepository: params.guildRepository,
			generateId: params.generateId,
		});
	}
	if (input.default_reaction_emoji !== undefined) {
		const emoji = input.default_reaction_emoji;
		assertSingleEmoji('default_reaction_emoji', emoji);
		await assertGuildEmoji(params.guildRepository, params.guildId, emoji?.emoji_id);
		patch.default_reaction_emoji_id = emoji?.emoji_id ?? null;
		patch.default_reaction_emoji_name = emoji?.emoji_name ?? null;
	}
	if (input.default_sort_order !== undefined) patch.default_sort_order = input.default_sort_order;
	if (input.default_forum_layout !== undefined) patch.default_forum_layout = input.default_forum_layout;
	if (input.default_tag_setting !== undefined) patch.default_tag_setting = input.default_tag_setting;
	const flags = patch.flags ?? current?.flags ?? 0;
	const tags = patch.available_tags ?? current?.availableTags.map((tag) => tag.toUdt()) ?? [];
	if ((flags & ChannelFlags.REQUIRE_TAG) !== 0 && !tags.some((tag) => !tag.moderated)) {
		throw new NoTagsAvailableToNonModeratorsError();
	}
	return patch;
}

export function mapThreadParentFields(
	channelType: number,
	config: ThreadParentConfig | null,
): Partial<ChannelResponse> {
	if (!THREAD_ONLY_CHANNEL_TYPES.has(channelType)) {
		const fields: Partial<ChannelResponse> = {};
		if (config?.defaultAutoArchiveDuration != null) {
			fields.default_auto_archive_duration = config.defaultAutoArchiveDuration;
		}
		if (config?.defaultThreadRateLimitPerUser != null) {
			fields.default_thread_rate_limit_per_user = config.defaultThreadRateLimitPerUser;
		}
		return fields;
	}
	const fields: Partial<ChannelResponse> = {
		flags: config?.flags ?? 0,
		default_auto_archive_duration: config?.defaultAutoArchiveDuration ?? null,
		default_thread_rate_limit_per_user: config?.defaultThreadRateLimitPerUser ?? 0,
		available_tags: (config?.availableTags ?? []).map((tag) => ({
			id: tag.id.toString(),
			name: tag.name,
			moderated: tag.moderated,
			emoji_id: tag.emojiId?.toString() ?? null,
			emoji_name: tag.emojiName,
		})),
		default_reaction_emoji:
			config && (config.defaultReactionEmojiId !== null || config.defaultReactionEmojiName !== null)
				? {
						emoji_id: config.defaultReactionEmojiId?.toString() ?? null,
						emoji_name: config.defaultReactionEmojiName,
					}
				: null,
		default_sort_order: config?.defaultSortOrder ?? null,
		default_tag_setting:
			config?.defaultTagSetting === ForumTagSettings.MATCH_ALL
				? ForumTagSettings.MATCH_ALL
				: ForumTagSettings.MATCH_SOME,
	};
	if (channelType === ChannelTypes.GUILD_FORUM) {
		fields.default_forum_layout = config?.defaultForumLayout ?? ForumLayoutTypes.DEFAULT;
	}
	return fields;
}

export function serializeThreadParentForAudit(
	channelType: number,
	config: ThreadParentConfig | null,
): Record<string, unknown> {
	if (!config) return {};
	const fields = mapThreadParentFields(channelType, config);
	return Object.fromEntries(Object.entries(fields).filter(([, value]) => value !== null && value !== undefined));
}

function isThreadParentInGuild(channel: Channel): channel is Channel & {guildId: GuildID} {
	return channel.guildId !== null && THREAD_PARENT_CHANNEL_TYPES.has(channel.type);
}

export async function loadThreadParentConfig(
	threads: ThreadRepository,
	channel: Channel,
): Promise<ThreadParentConfig | null> {
	if (!isThreadParentInGuild(channel) || !guildActive(channel.guildId)) return null;
	return threads.getParentConfig(channel.guildId, channel.id);
}

export async function withThreadParentFields(
	threads: ThreadRepository,
	channel: Channel,
	response: ChannelResponse,
	viewer?: ThreadViewer,
): Promise<ChannelResponse> {
	if (!isThreadParentInGuild(channel) || !guildActive(channel.guildId)) return response;
	if (viewer && !viewerActive(viewer, channel.guildId)) return response;
	const config = await threads.getParentConfig(channel.guildId, channel.id);
	return {...response, ...mapThreadParentFields(channel.type, config)};
}

export async function withThreadParentFieldsMany(
	threads: ThreadRepository,
	guildId: GuildID,
	channels: ReadonlyArray<Channel>,
	responses: Array<ChannelResponse>,
	viewer?: ThreadViewer,
): Promise<Array<ChannelResponse>> {
	if (!guildActive(guildId) || (viewer && !viewerActive(viewer, guildId))) return responses;
	if (!channels.some((channel) => THREAD_PARENT_CHANNEL_TYPES.has(channel.type))) return responses;
	const configs = new Map(
		(await threads.listParentConfigs(guildId)).map((config) => [config.channelId.toString(), config]),
	);
	const types = new Map(channels.map((channel) => [channel.id.toString(), channel.type]));
	return responses.map((response) => {
		const type = types.get(response.id);
		if (type === undefined || !THREAD_PARENT_CHANNEL_TYPES.has(type)) return response;
		return {...response, ...mapThreadParentFields(type, configs.get(response.id) ?? null)};
	});
}
