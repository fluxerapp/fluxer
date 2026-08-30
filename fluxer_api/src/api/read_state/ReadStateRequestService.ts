// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	ReadStateAckBulkRequest,
	ReadStateAckRequest,
	ReadStateAckResponse,
} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import type {UserID} from '../BrandedTypes';
import {createChannelID, createMessageID} from '../BrandedTypes';
import {mapReadStateResponse} from './ReadStateResponseMapper';
import type {ReadStateService} from './ReadStateService';

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
