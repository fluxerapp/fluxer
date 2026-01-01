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

import {Endpoints} from '~/Endpoints';
import http from '~/lib/HttpClient';
import {Logger} from '~/lib/Logger';
import type {GuildStickerWithUser} from '~/records/GuildStickerRecord';

const logger = new Logger('Stickers');

export const sanitizeStickerName = (fileName: string): string => {
	const name =
		fileName
			.split('.')
			.shift()
			?.replace(/[^a-zA-Z0-9_]/g, '') ?? '';
	return name.padEnd(2, '_').slice(0, 30);
};

export const list = async (guildId: string): Promise<ReadonlyArray<GuildStickerWithUser>> => {
	try {
		const response = await http.get<ReadonlyArray<GuildStickerWithUser>>({url: Endpoints.GUILD_STICKERS(guildId)});
		const stickers = response.body;
		logger.debug(`Retrieved ${stickers.length} stickers for guild ${guildId}`);
		return stickers;
	} catch (error) {
		logger.error(`Failed to list stickers for guild ${guildId}:`, error);
		throw error;
	}
};

export const create = async (
	guildId: string,
	sticker: {name: string; description: string; tags: Array<string>; image: string},
): Promise<void> => {
	try {
		await http.post({url: Endpoints.GUILD_STICKERS(guildId), body: sticker});
		logger.debug(`Created sticker ${sticker.name} in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to create sticker ${sticker.name} in guild ${guildId}:`, error);
		throw error;
	}
};

export const update = async (
	guildId: string,
	stickerId: string,
	data: {name?: string; description?: string; tags?: Array<string>},
): Promise<void> => {
	try {
		await http.patch({url: Endpoints.GUILD_STICKER(guildId, stickerId), body: data});
		logger.debug(`Updated sticker ${stickerId} in guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to update sticker ${stickerId} in guild ${guildId}:`, error);
		throw error;
	}
};

export const remove = async (guildId: string, stickerId: string, purge = false): Promise<void> => {
	try {
		const query = purge ? '?purge=true' : '';
		await http.delete({url: `${Endpoints.GUILD_STICKER(guildId, stickerId)}${query}`});
		logger.debug(`Removed sticker ${stickerId} from guild ${guildId}`);
	} catch (error) {
		logger.error(`Failed to remove sticker ${stickerId} from guild ${guildId}:`, error);
		throw error;
	}
};
