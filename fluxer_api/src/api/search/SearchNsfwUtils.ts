// SPDX-License-Identifier: AGPL-3.0-or-later

import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {channelToContentWarningView, computeEffectiveChannelNsfw} from '../channel/utils/EffectiveContentWarning';
import type {Channel} from '../models/Channel';

export function channelRequiresAgeVerification(
	channel: Channel,
	channelsById: ReadonlyMap<string, Channel>,
	guildNsfw: boolean,
): boolean {
	const parentCategory =
		channel.parentId != null ? (channelsById.get(channel.parentId.toString()) ?? null) : null;
	return computeEffectiveChannelNsfw(
		channelToContentWarningView(channel),
		parentCategory ? channelToContentWarningView(parentCategory) : null,
		{nsfw: guildNsfw, contentWarningLevel: ContentWarningLevel.INHERIT, contentWarningText: null},
	);
}
