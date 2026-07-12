// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelDataRepository} from './ChannelDataRepository';
import {IChannelRepositoryAggregate} from './IChannelRepositoryAggregate';
import {MessageInteractionRepository} from './MessageInteractionRepository';
import {MessageRepository} from './MessageRepository';
import {ThreadRepository} from './ThreadRepository';

export class ChannelRepository extends IChannelRepositoryAggregate {
	readonly channelData: ChannelDataRepository;
	readonly messages: MessageRepository;
	readonly messageInteractions: MessageInteractionRepository;
	readonly threads: ThreadRepository;

	constructor() {
		super();
		this.channelData = new ChannelDataRepository();
		this.messages = new MessageRepository(this.channelData);
		this.messageInteractions = new MessageInteractionRepository(this.messages);
		this.threads = new ThreadRepository();
	}
}
