// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IChannelDataRepository} from '@app/api/channel/repositories/IChannelDataRepository';
import type {IMessageInteractionRepository} from '@app/api/channel/repositories/IMessageInteractionRepository';
import type {IMessageRepository} from '@app/api/channel/repositories/IMessageRepository';

export abstract class IChannelRepositoryAggregate {
	abstract readonly channelData: IChannelDataRepository;
	abstract readonly messages: IMessageRepository;
	abstract readonly messageInteractions: IMessageInteractionRepository;
}
