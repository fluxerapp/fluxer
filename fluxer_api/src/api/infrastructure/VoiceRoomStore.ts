// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID} from '@app/api/BrandedTypes';
import {parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export interface PinnedRoomServer {
	regionId: string;
	serverId: string;
	endpoint: string;
}

export class VoiceRoomStore {
	private kvClient: IKVProvider;
	private readonly keyPrefix = 'voice:room:server';

	constructor(kvClient: IKVProvider) {
		this.kvClient = kvClient;
	}

	private getRoomKey(guildId: GuildID | undefined, channelId: ChannelID): string {
		if (guildId === undefined) {
			return `${this.keyPrefix}:dm:${channelId}`;
		}
		return `${this.keyPrefix}:guild:${guildId}:${channelId}`;
	}

	async pinRoomServer(
		guildId: GuildID | undefined,
		channelId: ChannelID,
		regionId: string,
		serverId: string,
		endpoint: string,
	): Promise<void> {
		const key = this.getRoomKey(guildId, channelId);
		await this.kvClient.set(
			key,
			JSON.stringify({
				regionId,
				serverId,
				endpoint,
				updatedAt: new Date().toISOString(),
			}),
		);
	}

	async getPinnedRoomServer(guildId: GuildID | undefined, channelId: ChannelID): Promise<PinnedRoomServer | null> {
		const key = this.getRoomKey(guildId, channelId);
		const data = await this.kvClient.get(key);
		if (!data) return null;
		const parsed = parseJsonRecord(data);
		const regionId = typeof parsed?.regionId === 'string' ? parsed.regionId : null;
		const serverId = typeof parsed?.serverId === 'string' ? parsed.serverId : null;
		const endpoint = typeof parsed?.endpoint === 'string' ? parsed.endpoint : null;
		if (!regionId || !serverId || !endpoint) {
			return null;
		}
		return {
			regionId,
			serverId,
			endpoint,
		};
	}

	async deleteRoomServer(guildId: GuildID | undefined, channelId: ChannelID): Promise<void> {
		const key = this.getRoomKey(guildId, channelId);
		await this.kvClient.del(key);
	}
}
