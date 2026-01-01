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
import DimensionStore from '~/stores/DimensionStore';

const logger = new Logger('DimensionActions');

type GuildId = string;

export const updateChannelListScroll = (guildId: GuildId, scrollTop: number): void => {
	logger.debug(`Updating channel list scroll: guildId=${guildId}, scrollTop=${scrollTop}`);
	DimensionStore.updateGuildDimensions(guildId, scrollTop, undefined);
};

export const clearChannelListScrollTo = (guildId: GuildId): void => {
	logger.debug(`Clearing channel list scroll target: guildId=${guildId}`);
	DimensionStore.updateGuildDimensions(guildId, undefined, null);
};

export const updateGuildListScroll = (scrollTop: number): void => {
	logger.debug(`Updating guild list scroll: scrollTop=${scrollTop}`);
	DimensionStore.updateGuildListDimensions(scrollTop);
};
