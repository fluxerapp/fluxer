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

import {Logger} from '~/lib/Logger';
import MessageStore from '~/stores/MessageStore';
import NotificationStore from '~/stores/NotificationStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import SelectedGuildStore from '~/stores/SelectedGuildStore';

const logger = new Logger('Navigation');

export const selectChannel = (guildId?: string, channelId?: string | null, messageId?: string): void => {
	logger.debug(`Selecting channel: guildId=${guildId}, channelId=${channelId}, messageId=${messageId}`);
	MessageStore.handleChannelSelect({guildId, channelId, messageId});
	NotificationStore.handleChannelSelect({channelId});
	SelectedChannelStore.selectChannel(guildId, channelId);
};

export const selectGuild = (guildId: string): void => {
	logger.debug(`Selecting guild: ${guildId}`);
	SelectedGuildStore.selectGuild(guildId);
};

export const deselectGuild = (): void => {
	logger.debug('Deselecting guild');
	SelectedGuildStore.deselectGuild();
};
