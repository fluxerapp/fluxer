// SPDX-License-Identifier: AGPL-3.0-or-later

import {Limits} from '@app/features/app/utils/UserLimits';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

const canonicalizeRecipientIds = (recipientIds: ReadonlyArray<string>): string => {
	const sortedRecipients = Array.from(new Set(recipientIds)).sort();
	return JSON.stringify(sortedRecipients);
};

function getMaxGroupDmRecipients(): number {
	return Limits.getMaxGroupDmRecipients();
}

export function getMaxGroupDmOtherRecipients(): number {
	return Math.max(getMaxGroupDmRecipients() - 1, 0);
}

export function getGroupDmRemainingSlots(channel?: Channel): number {
	if (!channel) {
		return getMaxGroupDmRecipients();
	}
	return Math.max(getMaxGroupDmRecipients() - (channel.recipientIds.length + 1), 0);
}

export function isGroupDmFull(channel?: Channel): boolean {
	if (!channel) {
		return false;
	}
	return channel.recipientIds.length + 1 >= getMaxGroupDmRecipients();
}

export function getDuplicateGroupDMChannels(
	recipientIds: ReadonlyArray<string>,
	excludeChannelId?: string,
): Array<Channel> {
	const key = canonicalizeRecipientIds(recipientIds);
	return Channels.getPrivateChannels()
		.filter((channel) => channel.type === ChannelTypes.GROUP_DM && channel.recipientIds.length > 0)
		.filter((channel) => !excludeChannelId || channel.id !== excludeChannelId)
		.filter((channel) => canonicalizeRecipientIds(channel.recipientIds) === key)
		.sort((a, b) => {
			const aSnowflake = a.lastMessageId ?? a.id;
			const bSnowflake = b.lastMessageId ?? b.id;
			return SnowflakeUtils.compare(bSnowflake, aSnowflake);
		});
}
