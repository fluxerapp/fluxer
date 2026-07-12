// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IChannelDataRepository} from './IChannelDataRepository';
import type {IMessageInteractionRepository} from './IMessageInteractionRepository';
import type {IMessageRepository} from './IMessageRepository';
import type {IThreadRepository} from './IThreadRepository';

export abstract class IChannelRepositoryAggregate {
	abstract readonly channelData: IChannelDataRepository;
	abstract readonly messages: IMessageRepository;
	abstract readonly messageInteractions: IMessageInteractionRepository;
	abstract readonly threads: IThreadRepository;
}
