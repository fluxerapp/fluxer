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

import type {ChannelID, MessageID, UserID} from '~/BrandedTypes';
import type {Channel} from '~/Models';

export interface PrivateChannelSummary {
	channelId: ChannelID;
	isGroupDm: boolean;
	channelType: number | null;
	lastMessageId: MessageID | null;
	open: boolean;
}

export interface IUserChannelRepository {
	listPrivateChannels(userId: UserID): Promise<Array<Channel>>;
	deleteAllPrivateChannels(userId: UserID): Promise<void>;
	listPrivateChannelSummaries(userId: UserID): Promise<Array<PrivateChannelSummary>>;
	listHistoricalDmChannelIds(userId: UserID): Promise<Array<ChannelID>>;
	recordHistoricalDmChannel(userId: UserID, channelId: ChannelID, isGroupDm: boolean): Promise<void>;

	findExistingDmState(user1Id: UserID, user2Id: UserID): Promise<Channel | null>;
	createDmChannelAndState(user1Id: UserID, user2Id: UserID, channelId: ChannelID): Promise<Channel>;
	isDmChannelOpen(userId: UserID, channelId: ChannelID): Promise<boolean>;
	openDmForUser(userId: UserID, channelId: ChannelID, isGroupDm?: boolean): Promise<void>;
	closeDmForUser(userId: UserID, channelId: ChannelID): Promise<void>;

	getPinnedDms(userId: UserID): Promise<Array<ChannelID>>;
	getPinnedDmsWithDetails(userId: UserID): Promise<Array<{channel_id: ChannelID; sort_order: number}>>;
	addPinnedDm(userId: UserID, channelId: ChannelID): Promise<Array<ChannelID>>;
	removePinnedDm(userId: UserID, channelId: ChannelID): Promise<Array<ChannelID>>;
	deletePinnedDmsByUserId(userId: UserID): Promise<void>;

	deleteAllReadStates(userId: UserID): Promise<void>;
}
