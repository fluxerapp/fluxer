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
import type {ReadState} from '~/Models';

export abstract class IReadStateRepository {
	abstract listReadStates(userId: UserID): Promise<Array<ReadState>>;
	abstract upsertReadState(
		userId: UserID,
		channelId: ChannelID,
		messageId: MessageID,
		mentionCount?: number,
		lastPinTimestamp?: Date,
	): Promise<ReadState>;
	abstract incrementReadStateMentions(
		userId: UserID,
		channelId: ChannelID,
		incrementBy?: number,
	): Promise<ReadState | null>;
	abstract bulkIncrementMentionCounts(updates: Array<{userId: UserID; channelId: ChannelID}>): Promise<void>;
	abstract deleteReadState(userId: UserID, channelId: ChannelID): Promise<void>;
	abstract bulkAckMessages(
		userId: UserID,
		readStates: Array<{channelId: ChannelID; messageId: MessageID}>,
	): Promise<Array<ReadState>>;
	abstract upsertPinAck(userId: UserID, channelId: ChannelID, lastPinTimestamp: Date): Promise<void>;
}
