// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelDataRepository} from '@app/api/channel/repositories/ChannelDataRepository';
import {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import {MessageInteractionRepository} from '@app/api/channel/repositories/MessageInteractionRepository';
import {MessageRepository} from '@app/api/channel/repositories/MessageRepository';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';

export class ChannelRepository extends IChannelRepositoryAggregate {
	readonly channelData: ChannelDataRepository;
	readonly messages: MessageRepository;
	readonly messageInteractions: MessageInteractionRepository;

	constructor(requestCache?: RequestCache) {
		super();
		this.channelData = new ChannelDataRepository(requestCache);
		this.messages = new MessageRepository(this.channelData);
		this.messageInteractions = new MessageInteractionRepository(this.messages);
	}
}
