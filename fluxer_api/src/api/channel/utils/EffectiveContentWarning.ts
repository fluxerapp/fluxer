// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';

export interface ContentWarningChannelLike {
	readonly type: number;
	readonly nsfwOverride: boolean | null;
	readonly contentWarningLevel: number;
	readonly contentWarningText: string | null;
}

interface ContentWarningGuildLike {
	readonly nsfw: boolean;
	readonly contentWarningLevel: number;
	readonly contentWarningText: string | null;
}

interface EffectiveContentWarning {
	level: number;
	text: string | null;
}

export function computeEffectiveChannelNsfw(
	channel: ContentWarningChannelLike,
	parentCategory: ContentWarningChannelLike | null,
	guild: ContentWarningGuildLike,
): boolean {
	if (channel.nsfwOverride !== null) return channel.nsfwOverride;
	const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
	if (!isCategory && parentCategory && parentCategory.nsfwOverride !== null) {
		return parentCategory.nsfwOverride;
	}
	return guild.nsfw;
}

export function resolveEffectiveThreadNsfw(
	parent: ContentWarningChannelLike,
	parentCategory: ContentWarningChannelLike | null,
	guild: ContentWarningGuildLike,
): boolean {
	return computeEffectiveChannelNsfw(parent, parentCategory, guild);
}

export function computeEffectiveContentWarning(
	channel: ContentWarningChannelLike,
	parentCategory: ContentWarningChannelLike | null,
	guild: ContentWarningGuildLike,
): EffectiveContentWarning {
	if (channel.contentWarningLevel !== ContentWarningLevel.INHERIT) {
		return {level: channel.contentWarningLevel, text: channel.contentWarningText};
	}
	const isCategory = channel.type === ChannelTypes.GUILD_CATEGORY;
	if (!isCategory && parentCategory && parentCategory.contentWarningLevel !== ContentWarningLevel.INHERIT) {
		return {level: parentCategory.contentWarningLevel, text: parentCategory.contentWarningText};
	}
	if (guild.contentWarningLevel !== ContentWarningLevel.INHERIT) {
		return {level: guild.contentWarningLevel, text: guild.contentWarningText};
	}
	return {level: ContentWarningLevel.INHERIT, text: null};
}

export function channelToContentWarningView(channel: {
	type: number;
	nsfwOverride: boolean | null;
	contentWarningLevel: number;
	contentWarningText: string | null;
}): ContentWarningChannelLike {
	return {
		type: channel.type,
		nsfwOverride: channel.nsfwOverride,
		contentWarningLevel: channel.contentWarningLevel,
		contentWarningText: channel.contentWarningText,
	};
}

export function channelResponseToContentWarningView(channel: {
	type: number;
	nsfw_override?: boolean | null;
	content_warning_level?: number;
	content_warning_text?: string | null;
}): ContentWarningChannelLike {
	return {
		type: channel.type,
		nsfwOverride: channel.nsfw_override ?? null,
		contentWarningLevel: channel.content_warning_level ?? ContentWarningLevel.INHERIT,
		contentWarningText: channel.content_warning_text ?? null,
	};
}

export function guildToContentWarningView(guild: {
	nsfw: boolean;
	contentWarningLevel: number;
	contentWarningText: string | null;
}): ContentWarningGuildLike {
	return {
		nsfw: guild.nsfw,
		contentWarningLevel: guild.contentWarningLevel,
		contentWarningText: guild.contentWarningText,
	};
}

export function guildResponseToContentWarningView(guild: {
	nsfw_level?: number;
	nsfw?: boolean;
	content_warning_level?: number;
	content_warning_text?: string | null;
}): ContentWarningGuildLike {
	return {
		nsfw: guild.nsfw ?? guild.nsfw_level === 3,
		contentWarningLevel: guild.content_warning_level ?? 0,
		contentWarningText: guild.content_warning_text ?? null,
	};
}
