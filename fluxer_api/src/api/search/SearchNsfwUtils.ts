// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	channelToContentWarningView,
	computeEffectiveChannelNsfw,
	resolveEffectiveThreadNsfw,
} from '@app/api/channel/utils/EffectiveContentWarning';
import type {Channel} from '@app/api/models/Channel';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {THREAD_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';

export function channelRequiresAgeVerification(
	channel: Channel,
	channelsById: ReadonlyMap<string, Channel>,
	guildNsfw: boolean,
): boolean {
	const guild = {nsfw: guildNsfw, contentWarningLevel: ContentWarningLevel.INHERIT, contentWarningText: null};
	const parent = channel.parentId != null ? (channelsById.get(channel.parentId.toString()) ?? null) : null;
	if (THREAD_CHANNEL_TYPES.has(channel.type)) {
		if (!parent) return guildNsfw;
		const category = parent.parentId != null ? (channelsById.get(parent.parentId.toString()) ?? null) : null;
		return resolveEffectiveThreadNsfw(
			channelToContentWarningView(parent),
			category ? channelToContentWarningView(category) : null,
			guild,
		);
	}
	return computeEffectiveChannelNsfw(
		channelToContentWarningView(channel),
		parent ? channelToContentWarningView(parent) : null,
		guild,
	);
}
