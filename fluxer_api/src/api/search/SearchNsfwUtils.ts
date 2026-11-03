// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {
	channelToContentWarningView,
	computeEffectiveChannelNsfw,
	resolveEffectiveThreadNsfw,
} from '@app/api/channel/utils/EffectiveContentWarning';
import type {Channel} from '@app/api/models/Channel';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {canUserAccessNsfwContent} from '@app/api/utils/AgeUtils';
import {ContentWarningLevel} from '@fluxer/constants/src/GuildConstants';
import {THREAD_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';

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

export async function getCanUserAccessNsfw(userRepository: UserRepository, userId: UserID): Promise<boolean> {
	const user = await userRepository.findUnique(userId);
	if (!user) {
		throw new UnknownUserError();
	}
	return canUserAccessNsfwContent(user);
}
