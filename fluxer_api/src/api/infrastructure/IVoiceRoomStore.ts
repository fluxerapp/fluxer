// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID} from '@app/api/BrandedTypes';

export abstract class IVoiceRoomStore {
	abstract pinRoomServer(
		guildId: GuildID | undefined,
		channelId: ChannelID,
		regionId: string,
		serverId: string,
		endpoint: string,
	): Promise<void>;

	abstract getPinnedRoomServer(
		guildId: GuildID | undefined,
		channelId: ChannelID,
	): Promise<{
		regionId: string;
		serverId: string;
		endpoint: string;
	} | null>;

	abstract deleteRoomServer(guildId: GuildID | undefined, channelId: ChannelID): Promise<void>;

	abstract getRegionOccupancy(regionId: string): Promise<Array<string>>;

	abstract getServerOccupancy(regionId: string, serverId: string): Promise<Array<string>>;

	async listPinnedRooms(): Promise<Array<{guildId?: GuildID; channelId: ChannelID}>> {
		return [];
	}
}
