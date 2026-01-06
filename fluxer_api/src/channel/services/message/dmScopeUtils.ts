/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ChannelID, UserID} from '~/BrandedTypes';
import {ChannelTypes} from '~/Constants';
import type {IUserRepository} from '~/user/IUserRepository';

export type DmSearchScope = 'all_dms' | 'open_dms';

export interface DmScopeOptions {
	scope: DmSearchScope;
	userId: UserID;
	userRepository: IUserRepository;
	includeChannelId?: ChannelID | null;
}

export const getDmChannelIdsForScope = async ({
	scope,
	userId,
	userRepository,
	includeChannelId,
}: DmScopeOptions): Promise<Array<string>> => {
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

	if (includeChannelId) {
		channelIdStrings.add(includeChannelId.toString());
	}

	return Array.from(channelIdStrings);
};
