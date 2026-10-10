// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelDataRepository} from '@app/api/channel/repositories/ChannelDataRepository';
import {CrosspostedMessageRepository} from '@app/api/channel/repositories/CrosspostedMessageRepository';
import {MessageInteractionRepository} from '@app/api/channel/repositories/MessageInteractionRepository';
import {MessageRepository} from '@app/api/channel/repositories/MessageRepository';
import {ThreadRepository} from '@app/api/channel/repositories/ThreadRepository';
import {enqueueRepairThreadIndexes} from '@app/api/channel/threads/ThreadJobs';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';

export class ChannelRepository {
	readonly channelData: ChannelDataRepository;
	readonly messages: MessageRepository;
	readonly messageInteractions: MessageInteractionRepository;
	readonly threads: ThreadRepository;
	readonly crossposts = new CrosspostedMessageRepository();

	constructor(requestCache?: RequestCache) {
		this.channelData = new ChannelDataRepository(requestCache);
		this.messages = new MessageRepository(this.channelData);
		this.messageInteractions = new MessageInteractionRepository(this.messages);
		this.threads = new ThreadRepository(this.channelData, this.messages, enqueueRepairThreadIndexes);
	}
}
