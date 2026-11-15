// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Guild} from '@app/features/guild/models/Guild';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const THIS_CONTAINS_SENSITIVE_CONTENT_DESCRIPTOR = msg({
	message: 'This contains sensitive content.',
	comment: 'Label in the content warning utils helper. Keep the tone plain and specific.',
});

type EffectiveSource = 'channel' | 'parent' | 'guild' | 'none';

export interface EffectiveContentWarning {
	level: number;
	text: string | null;
	source: EffectiveSource;
}

export interface EffectiveMatureContentResult {
	value: boolean;
	source: EffectiveSource;
}

function getParentCategory(channel: Channel): Channel | null {
	if (channel.type === ChannelTypes.GUILD_CATEGORY) return null;
	if (!channel.parentId) return null;
	const parent = Channels.getChannel(channel.parentId);
	if (!parent) return null;
	return parent.type === ChannelTypes.GUILD_CATEGORY ? parent : null;
}

export function getEffectiveChannelMatureContent(channel: Channel, guild: Guild | null | undefined): boolean {
	return resolveEffectiveChannelMatureContent(channel, guild).value;
}

export function resolveEffectiveChannelMatureContent(
	channel: Channel,
	guild: Guild | null | undefined,
): EffectiveMatureContentResult {
	if (channel.nsfwOverride !== null && channel.nsfwOverride !== undefined) {
		return {value: channel.nsfwOverride, source: 'channel'};
	}
	const parent = getParentCategory(channel);
	if (parent && parent.nsfwOverride !== null && parent.nsfwOverride !== undefined) {
		return {value: parent.nsfwOverride, source: 'parent'};
	}
	if (guild) {
		return {value: guild.nsfw, source: 'guild'};
	}
	return {value: false, source: 'none'};
}

export function getEffectiveChannelContentWarning(
	channel: Channel,
	guild: Guild | null | undefined,
): EffectiveContentWarning {
	if (channel.contentWarningLevel !== ContentWarningLevel.INHERIT) {
		return {level: channel.contentWarningLevel, text: channel.contentWarningText, source: 'channel'};
	}
	const parent = getParentCategory(channel);
	if (parent && parent.contentWarningLevel !== ContentWarningLevel.INHERIT) {
		return {level: parent.contentWarningLevel, text: parent.contentWarningText, source: 'parent'};
	}
	if (guild && guild.contentWarningLevel !== ContentWarningLevel.INHERIT) {
		return {level: guild.contentWarningLevel, text: guild.contentWarningText, source: 'guild'};
	}
	return {level: ContentWarningLevel.INHERIT, text: null, source: 'none'};
}

export function getDefaultContentWarningText(i18n: I18n): string {
	return i18n._(THIS_CONTAINS_SENSITIVE_CONTENT_DESCRIPTOR);
}
