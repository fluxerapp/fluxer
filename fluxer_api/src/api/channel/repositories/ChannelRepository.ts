// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import {ChannelDataRepository} from './ChannelDataRepository';
import {IChannelRepositoryAggregate} from './IChannelRepositoryAggregate';
import {MessageInteractionRepository} from './MessageInteractionRepository';
import {MessageRepository} from './MessageRepository';

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
