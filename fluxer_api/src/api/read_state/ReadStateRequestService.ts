// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {createChannelID, createMessageID} from '@app/api/BrandedTypes';
import {mapReadStateResponse} from '@app/api/read_state/ReadStateResponseMapper';
import type {ReadStateService} from '@app/api/read_state/ReadStateService';
import type {
	ReadStateAckBulkRequest,
	ReadStateAckRequest,
	ReadStateAckResponse,
} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';

interface ReadStateAckBulkParams {
	userId: UserID;
	data: ReadStateAckBulkRequest;
}

interface ReadStateAckParams {
	userId: UserID;
	data: ReadStateAckRequest;
}

export class ReadStateRequestService {
	constructor(private readStateService: ReadStateService) {}

	async bulkAckMessages({userId, data}: ReadStateAckBulkParams): Promise<void> {
		await this.readStateService.bulkAckMessages({
			userId,
			readStates: data.read_states.map((readState) => ({
				channelId: createChannelID(readState.channel_id),
				messageId: createMessageID(readState.message_id),
			})),
		});
	}

	async ackReadStates({userId, data}: ReadStateAckParams): Promise<ReadStateAckResponse> {
		const readStates = await this.readStateService.ackReadStates({
			userId,
			readStates: data.read_states.map((readState) => ({
				channelId: createChannelID(readState.channel_id),
				messageId: createMessageID(readState.message_id),
				mentionCount: readState.mention_count,
				manual: readState.manual,
			})),
		});
		return {
			read_states: readStates.map(mapReadStateResponse),
		};
	}
}
