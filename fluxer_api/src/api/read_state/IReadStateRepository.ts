// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {ReadState} from '@app/api/models/ReadState';

export interface ReadStateUpsert {
	readState: ReadState;
	previous: ReadState | null;
}

export interface ReadStateMarker {
	flags: number;
	guildId: GuildID;
}

export interface ReadStateMentionUpdate {
	userId: UserID;
	channelId: ChannelID;
	messageId: MessageID;
	marker?: ReadStateMarker | null;
}

export abstract class IReadStateRepository {
	abstract listReadStates(userId: UserID): Promise<Array<ReadState>>;

	abstract getReadState(userId: UserID, channelId: ChannelID): Promise<ReadState | null>;

	abstract upsertReadState(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		mentionCount?: number,
		lastPinTimestamp?: Date,
		manual?: boolean,
		marker?: ReadStateMarker | null,
	): Promise<ReadStateUpsert>;

	abstract incrementReadStateMentions(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		incrementBy?: number,
		marker?: ReadStateMarker | null,
	): Promise<ReadState | null>;

	abstract bulkIncrementMentionCounts(updates: Array<ReadStateMentionUpdate>): Promise<
		Array<{
			userId: UserID;
			channelId: ChannelID;
		}>
	>;

	abstract bulkAckMessages(
		userId: UserID,
		readStates: Array<{
			channelId: ChannelID;
			messageId: MessageID;
			marker?: ReadStateMarker | null;
		}>,
	): Promise<Array<ReadState>>;

	abstract upsertPinAck(
		userId: UserID,
		channelId: ChannelID,
		lastPinTimestamp: Date,
		marker?: ReadStateMarker | null,
	): Promise<void>;
}
