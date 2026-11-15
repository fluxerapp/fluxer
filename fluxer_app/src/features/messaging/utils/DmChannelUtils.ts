// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Relationships from '@app/features/relationship/state/Relationships';
import UserPinnedDM from '@app/features/user/state/UserPinnedDM';
import {compareStrings} from '@app/lib/list/CompareStrings';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {compare, fromTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';

const getChannelSortSnowflake = (channel: Channel): string => {
	const baseSnowflake = channel.lastMessageId ?? channel.id;
	if (channel.type !== ChannelTypes.DM) {
		return baseSnowflake;
	}
	const recipientId = channel.recipientIds[0];
	if (!recipientId) {
		return baseSnowflake;
	}
	const relationship = Relationships.getRelationship(recipientId);
	if (!relationship || relationship.type !== RelationshipTypes.FRIEND) {
		return baseSnowflake;
	}
	const sinceTimestamp = relationship.since.getTime();
	if (!Number.isFinite(sinceTimestamp)) {
		return baseSnowflake;
	}
	const friendshipSnowflake = fromTimestamp(sinceTimestamp);
	return compare(friendshipSnowflake, baseSnowflake) > 0 ? friendshipSnowflake : baseSnowflake;
};

export function getSortedDmChannels(dmChannels: ReadonlyArray<Channel>, currentUserId?: string | null): Array<Channel> {
	const pinnedOrder = new Map(UserPinnedDM.pinnedDMs.map((id, index) => [id, index]));
	return dmChannels
		.filter((channel) => !(channel.type === ChannelTypes.DM_PERSONAL_NOTES || channel.id === currentUserId))
		.sort((a, b) => {
			const aIndex = pinnedOrder.get(a.id);
			const bIndex = pinnedOrder.get(b.id);
			const aIsPinned = aIndex !== undefined;
			const bIsPinned = bIndex !== undefined;
			if (aIsPinned && bIsPinned) {
				const diff = aIndex - bIndex;
				if (diff !== 0) {
					return diff;
				}
				return compareStrings(a.id, b.id);
			}
			if (aIsPinned !== bIsPinned) {
				return aIsPinned ? -1 : 1;
			}
			const aSortSnowflake = getChannelSortSnowflake(a);
			const bSortSnowflake = getChannelSortSnowflake(b);
			const sortDiff = compare(bSortSnowflake, aSortSnowflake);
			if (sortDiff !== 0) {
				return sortDiff;
			}
			return compareStrings(a.id, b.id);
		});
}
