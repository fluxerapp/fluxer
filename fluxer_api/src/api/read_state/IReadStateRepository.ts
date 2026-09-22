// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {ReadState} from '@app/api/models/ReadState';

export abstract class IReadStateRepository {
	abstract listReadStates(userId: UserID): Promise<Array<ReadState>>;

	abstract upsertReadState(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		mentionCount?: number,
		lastPinTimestamp?: Date,
		manual?: boolean,
	): Promise<ReadState>;

	abstract incrementReadStateMentions(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		incrementBy?: number,
	): Promise<ReadState | null>;

	abstract bulkIncrementMentionCounts(
		updates: Array<{
			userId: UserID;
			channelId: ChannelID;
			messageId: MessageID;
		}>,
	): Promise<
		Array<{
			userId: UserID;
			channelId: ChannelID;
		}>
	>;

	abstract deleteReadState(userId: UserID, channelId: ChannelID): Promise<void>;

	abstract bulkAckMessages(
		userId: UserID,
		readStates: Array<{
			channelId: ChannelID;
			messageId: MessageID;
		}>,
	): Promise<Array<ReadState>>;

	abstract upsertPinAck(userId: UserID, channelId: ChannelID, lastPinTimestamp: Date): Promise<void>;
}
