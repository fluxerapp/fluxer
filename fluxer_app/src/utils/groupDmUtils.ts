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

import {ChannelTypes} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import ChannelStore from '~/stores/ChannelStore';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';

export const MAX_GROUP_DM_RECIPIENTS = 10;

const canonicalizeRecipientIds = (recipientIds: ReadonlyArray<string>): string => {
	const sortedRecipients = Array.from(new Set(recipientIds)).sort();
	return JSON.stringify(sortedRecipients);
};

export const getDuplicateGroupDMChannels = (
	recipientIds: ReadonlyArray<string>,
	excludeChannelId?: string,
): Array<ChannelRecord> => {
	const key = canonicalizeRecipientIds(recipientIds);

	return ChannelStore.getPrivateChannels()
		.filter((channel) => channel.type === ChannelTypes.GROUP_DM && channel.recipientIds.length > 0)
		.filter((channel) => !excludeChannelId || channel.id !== excludeChannelId)
		.filter((channel) => canonicalizeRecipientIds(channel.recipientIds) === key)
		.sort((a, b) => {
			const aSnowflake = a.lastMessageId ?? a.id;
			const bSnowflake = b.lastMessageId ?? b.id;
			return SnowflakeUtils.compare(bSnowflake, aSnowflake);
		});
};
