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

import type {ChannelID, GuildID} from '~/BrandedTypes';

export class InMemoryVoiceRoomStore {
	async pinRoomServer(
		_guildId: GuildID | undefined,
		_channelId: ChannelID,
		_regionId: string,
		_serverId: string,
		_endpoint: string,
	): Promise<void> {}

	async getPinnedRoomServer(
		_guildId: GuildID | undefined,
		_channelId: ChannelID,
	): Promise<{regionId: string; serverId: string; endpoint: string} | null> {
		return null;
	}

	async deleteRoomServer(_guildId: GuildID | undefined, _channelId: ChannelID): Promise<void> {}

	async getRegionOccupancy(_regionId: string): Promise<Array<string>> {
		return [];
	}

	async getServerOccupancy(_regionId: string, _serverId: string): Promise<Array<string>> {
		return [];
	}
}
