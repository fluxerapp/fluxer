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

import {ChannelDataRepository} from './ChannelDataRepository';
import {IChannelRepositoryAggregate} from './IChannelRepositoryAggregate';
import {MessageInteractionRepository} from './MessageInteractionRepository';
import {MessageRepository} from './MessageRepository';

export class ChannelRepository extends IChannelRepositoryAggregate {
	readonly channelData: ChannelDataRepository;
	readonly messages: MessageRepository;
	readonly messageInteractions: MessageInteractionRepository;

	constructor() {
		super();
		this.channelData = new ChannelDataRepository();
		this.messages = new MessageRepository(this.channelData);
		this.messageInteractions = new MessageInteractionRepository(this.messages);
	}
}
