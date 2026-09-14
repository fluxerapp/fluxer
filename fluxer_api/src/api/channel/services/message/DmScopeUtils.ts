// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {Channel} from '@app/api/models/Channel';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';

export type DmSearchScope = 'all_dms' | 'open_dms';

interface DmScopeOptions {
	scope: DmSearchScope;
	userId: UserID;
	userRepository: IUserRepository;
	includeChannel?: Channel | null;
}

export function isDmScopeChannelForUser(channel: Channel, userId: UserID): boolean {
	const isDm = channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM;
	return isDm && channel.recipientIds.has(userId);
}

export async function getDmChannelIdsForScope({
	scope,
	userId,
	userRepository,
	includeChannel,
}: DmScopeOptions): Promise<Array<string>> {
	const summaryResults = await userRepository.listPrivateChannelSummaries(userId);
	const channelIdStrings = new Set<string>();
	for (const summary of summaryResults) {
		const isDm =
			summary.channelType === ChannelTypes.DM || summary.channelType === ChannelTypes.GROUP_DM || summary.isGroupDm;
		if (!isDm) {
			continue;
		}
		if (scope === 'open_dms' && !summary.open) {
			continue;
		}
		channelIdStrings.add(summary.channelId.toString());
	}
	if (scope === 'all_dms') {
		const historicalIds = await userRepository.listHistoricalDmChannelIds(userId);
		for (const channelId of historicalIds) {
			channelIdStrings.add(channelId.toString());
		}
	}
	if (includeChannel && isDmScopeChannelForUser(includeChannel, userId)) {
		channelIdStrings.add(includeChannel.id.toString());
	}
	return Array.from(channelIdStrings);
}
