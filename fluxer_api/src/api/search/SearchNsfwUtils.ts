// SPDX-License-Identifier: AGPL-3.0-or-later

import {channelToContentWarningView, computeEffectiveChannelNsfw} from '@app/api/channel/utils/EffectiveContentWarning';
import type {Channel} from '@app/api/models/Channel';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';

export function channelRequiresAgeVerification(
	channel: Channel,
	channelsById: ReadonlyMap<string, Channel>,
	guildNsfw: boolean,
): boolean {
	const parentCategory = channel.parentId != null ? (channelsById.get(channel.parentId.toString()) ?? null) : null;
	return computeEffectiveChannelNsfw(
		channelToContentWarningView(channel),
		parentCategory ? channelToContentWarningView(parentCategory) : null,
		{nsfw: guildNsfw, contentWarningLevel: ContentWarningLevel.INHERIT, contentWarningText: null},
	);
}
