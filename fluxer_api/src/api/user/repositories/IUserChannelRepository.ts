// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {Channel} from '@app/api/models/Channel';

export interface PrivateChannelSummary {
	channelId: ChannelID;
	isGroupDm: boolean;
	channelType: number | null;
	lastMessageId: MessageID | null;
	open: boolean;
}

export interface ListHistoricalDmChannelOptions {
	limit: number;
	beforeChannelId?: ChannelID;
	afterChannelId?: ChannelID;
}

export interface HistoricalDmChannelSummary {
	channelId: ChannelID;
	channelType: number | null;
	recipientIds: Array<UserID>;
	lastMessageId: MessageID | null;
	open: boolean;
}

export interface IUserChannelRepository {
	listPrivateChannels(userId: UserID): Promise<Array<Channel>>;
	deleteAllPrivateChannels(userId: UserID): Promise<void>;
	listPrivateChannelSummaries(userId: UserID): Promise<Array<PrivateChannelSummary>>;
	listHistoricalDmChannelIds(userId: UserID): Promise<Array<ChannelID>>;
	listHistoricalDmChannelsPaginated(
		userId: UserID,
		options: ListHistoricalDmChannelOptions,
	): Promise<Array<HistoricalDmChannelSummary>>;
	recordHistoricalDmChannel(userId: UserID, channelId: ChannelID, isGroupDm: boolean): Promise<void>;
	findExistingDmState(user1Id: UserID, user2Id: UserID): Promise<Channel | null>;
	createDmChannelAndState(user1Id: UserID, user2Id: UserID, channelId: ChannelID): Promise<Channel>;
	createLocalOnlyDmChannel(ownerId: UserID, recipientId: UserID, channelId: ChannelID): Promise<Channel>;
	isDmChannelOpen(userId: UserID, channelId: ChannelID): Promise<boolean>;
	openDmForUser(userId: UserID, channelId: ChannelID, isGroupDm?: boolean): Promise<void>;
	openPrivateChannelForUser(userId: UserID, channel: Channel): Promise<void>;
	closeDmForUser(userId: UserID, channelId: ChannelID): Promise<void>;
	getPinnedDms(userId: UserID): Promise<Array<ChannelID>>;
	getPinnedDmsWithDetails(userId: UserID): Promise<
		Array<{
			channel_id: ChannelID;
			sort_order: number;
		}>
	>;
	addPinnedDm(userId: UserID, channelId: ChannelID): Promise<Array<ChannelID>>;
	removePinnedDm(userId: UserID, channelId: ChannelID): Promise<Array<ChannelID>>;
	deletePinnedDmsByUserId(userId: UserID): Promise<void>;
	deleteAllReadStates(userId: UserID): Promise<void>;
}
