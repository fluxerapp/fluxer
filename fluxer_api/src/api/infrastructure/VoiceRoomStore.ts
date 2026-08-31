// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {ChannelID, GuildID} from '../BrandedTypes';
import {createChannelID, createGuildID} from '../BrandedTypes';
import {parseJsonRecord} from '../utils/JsonBoundaryUtils';
import {VOICE_OCCUPANCY_REGION_KEY_PREFIX, VOICE_OCCUPANCY_SERVER_KEY_PREFIX} from '../voice/VoiceConstants';

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
		const previous = await this.getPinnedRoomServer(guildId, channelId);
		if (previous) {
			await this.removeOccupancy(previous.regionId, previous.serverId, guildId, channelId);
		}
		await this.kvClient.set(
			key,
			JSON.stringify({
				regionId,
				serverId,
				endpoint,
				updatedAt: new Date().toISOString(),
			}),
		);
		await this.addOccupancy(regionId, serverId, guildId, channelId);
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
		const previous = await this.getPinnedRoomServer(guildId, channelId);
		await this.kvClient.del(key);
		if (previous) {
			await this.removeOccupancy(previous.regionId, previous.serverId, guildId, channelId);
		}
	}

	async getRegionOccupancy(regionId: string): Promise<Array<string>> {
		const key = `${VOICE_OCCUPANCY_REGION_KEY_PREFIX}:${regionId}`;
		const members = await this.kvClient.smembers(key);
		return members;
	}

	async getServerOccupancy(regionId: string, serverId: string): Promise<Array<string>> {
		const key = `${VOICE_OCCUPANCY_SERVER_KEY_PREFIX}:${regionId}:${serverId}`;
		const members = await this.kvClient.smembers(key);
		return members;
	}

	async listPinnedRooms(): Promise<Array<{guildId?: GuildID; channelId: ChannelID}>> {
		const keys = await this.kvClient.scan(`${this.keyPrefix}:*`, 1000);
		return keys
			.map((key) => this.parseRoomKey(key))
			.filter((room): room is {guildId?: GuildID; channelId: ChannelID} => room !== null);
	}

	private async addOccupancy(
		regionId: string,
		serverId: string,
		guildId: bigint | undefined,
		channelId: bigint,
	): Promise<void> {
		const member = this.buildOccupancyMember(guildId, channelId);
		const regionKey = `${VOICE_OCCUPANCY_REGION_KEY_PREFIX}:${regionId}`;
		const serverKey = `${VOICE_OCCUPANCY_SERVER_KEY_PREFIX}:${regionId}:${serverId}`;
		await Promise.all([this.kvClient.sadd(regionKey, member), this.kvClient.sadd(serverKey, member)]);
	}

	private async removeOccupancy(
		regionId: string,
		serverId: string,
		guildId: bigint | undefined,
		channelId: bigint,
	): Promise<void> {
		const member = this.buildOccupancyMember(guildId, channelId);
		const regionKey = `${VOICE_OCCUPANCY_REGION_KEY_PREFIX}:${regionId}`;
		const serverKey = `${VOICE_OCCUPANCY_SERVER_KEY_PREFIX}:${regionId}:${serverId}`;
		await Promise.all([this.kvClient.srem(regionKey, member), this.kvClient.srem(serverKey, member)]);
	}

	private buildOccupancyMember(guildId: bigint | undefined, channelId: bigint): string {
		if (!guildId) {
			return `dm:${channelId.toString()}`;
		}
		return `guild:${guildId.toString()}:channel:${channelId.toString()}`;
	}

	private parseRoomKey(key: string): {guildId?: GuildID; channelId: ChannelID} | null {
		const suffix = key.slice(`${this.keyPrefix}:`.length);
		if (suffix.startsWith('guild:')) {
			const parts = suffix.split(':');
			if (parts.length !== 3) return null;
			return {guildId: createGuildID(BigInt(parts[1])), channelId: createChannelID(BigInt(parts[2]))};
		}
		if (suffix.startsWith('dm:')) {
			return {channelId: createChannelID(BigInt(suffix.slice(3)))};
		}
		return null;
	}
}
