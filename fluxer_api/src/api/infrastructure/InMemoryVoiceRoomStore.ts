// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID} from '@app/api/BrandedTypes';

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
	): Promise<{
		regionId: string;
		serverId: string;
		endpoint: string;
	} | null> {
		return null;
	}

	async deleteRoomServer(_guildId: GuildID | undefined, _channelId: ChannelID): Promise<void> {}
}
