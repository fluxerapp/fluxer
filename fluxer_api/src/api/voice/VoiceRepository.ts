// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildIDSet, createUserIDSet} from '../BrandedTypes';
import {BatchBuilder, deleteOneOrMany, fetchMany, fetchOne, upsertOne} from '../database/CassandraQueryExecution';
import {defineTable} from '../database/CassandraTableDsl';
import {
	VOICE_REGION_COLUMNS,
	VOICE_SERVER_COLUMNS,
	type VoiceRegionRow,
	type VoiceServerRow,
} from '../database/types/VoiceTypes';
import type {IVoiceRepository} from './IVoiceRepository';
import type {VoiceRegionRecord, VoiceRegionWithServers, VoiceServerRecord} from './VoiceModel';

function toIterable<T>(value: unknown): Array<T> {
	if (value === null || value === undefined) return [];
	if (value instanceof Set) return Array.from(value) as Array<T>;
	if (Array.isArray(value)) return value as Array<T>;
	return [value as T];
}

const VoiceRegions = defineTable<VoiceRegionRow, 'id'>({
	name: 'voice_regions',
	columns: VOICE_REGION_COLUMNS,
	primaryKey: ['id'],
});
const VoiceServers = defineTable<VoiceServerRow, 'region_id' | 'server_id'>({
	name: 'voice_servers',
	columns: VOICE_SERVER_COLUMNS,
	primaryKey: ['region_id', 'server_id'],
});
const LIST_REGIONS_CQL = VoiceRegions.selectCql();
const GET_REGION_CQL = VoiceRegions.selectCql({
	where: VoiceRegions.where.eq('id'),
});
const LIST_SERVERS_FOR_REGION_CQL = VoiceServers.selectCql({
	where: VoiceServers.where.eq('region_id'),
});
const GET_SERVER_CQL = VoiceServers.selectCql({
	where: [VoiceServers.where.eq('region_id'), VoiceServers.where.eq('server_id')],
});

export class VoiceRepository implements IVoiceRepository {
	async listRegions(): Promise<Array<VoiceRegionRecord>> {
		const rows = await fetchMany<VoiceRegionRow>(LIST_REGIONS_CQL, {});
		return rows.map((row) => this.mapRegionRow(row));
	}

	async listRegionsWithServers(): Promise<Array<VoiceRegionWithServers>> {
		const regions = await this.listRegions();
		const results: Array<VoiceRegionWithServers> = [];
		for (const region of regions) {
			const servers = await this.listServersForRegion(region.id);
			results.push({
				...region,
				servers,
			});
		}
		return results;
	}

	async getRegion(id: string): Promise<VoiceRegionRecord | null> {
		const row = await fetchOne<VoiceRegionRow>(GET_REGION_CQL, {id});
		return row ? this.mapRegionRow(row) : null;
	}

	async getRegionWithServers(id: string): Promise<VoiceRegionWithServers | null> {
		const region = await this.getRegion(id);
		if (!region) {
			return null;
		}
		const servers = await this.listServersForRegion(id);
		return {...region, servers};
	}

	async upsertRegion(region: VoiceRegionRecord): Promise<void> {
		const row: VoiceRegionRow = {
			id: region.id,
			name: region.name,
			emoji: region.emoji,
			latitude: region.latitude,
			longitude: region.longitude,
			is_default: region.isDefault,
			vip_only: region.restrictions.vipOnly,
			required_guild_features: new Set(region.restrictions.requiredGuildFeatures),
			allowed_guild_ids: new Set(Array.from(region.restrictions.allowedGuildIds).map((id) => BigInt(id))),
			allowed_user_ids: new Set(Array.from(region.restrictions.allowedUserIds).map((id) => BigInt(id))),
			created_at: region.createdAt ?? new Date(),
			updated_at: region.updatedAt ?? new Date(),
		};
		await upsertOne(VoiceRegions.upsertAll(row));
	}

	async deleteRegion(regionId: string): Promise<void> {
		const batch = new BatchBuilder();
		batch.addPrepared(VoiceRegions.deleteByPk({id: regionId}));
		const servers = await this.listServersForRegion(regionId);
		for (const server of servers) {
			batch.addPrepared(VoiceServers.deleteByPk({region_id: regionId, server_id: server.serverId}));
		}
		await batch.execute();
	}

	async createRegion(region: Omit<VoiceRegionRecord, 'createdAt' | 'updatedAt'>): Promise<VoiceRegionRecord> {
		const now = new Date();
		const fullRegion: VoiceRegionRecord = {
			...region,
			createdAt: now,
			updatedAt: now,
		};
		await this.upsertRegion(fullRegion);
		return fullRegion;
	}

	async listServersForRegion(regionId: string): Promise<Array<VoiceServerRecord>> {
		const rows = await fetchMany<VoiceServerRow>(LIST_SERVERS_FOR_REGION_CQL, {region_id: regionId});
		return rows.map((row) => this.mapServerRow(row));
	}

	async listServers(regionId: string): Promise<Array<VoiceServerRecord>> {
		return this.listServersForRegion(regionId);
	}

	async getServer(regionId: string, serverId: string): Promise<VoiceServerRecord | null> {
		const row = await fetchOne<VoiceServerRow>(GET_SERVER_CQL, {region_id: regionId, server_id: serverId});
		return row ? this.mapServerRow(row) : null;
	}

	async createServer(server: Omit<VoiceServerRecord, 'createdAt' | 'updatedAt'>): Promise<VoiceServerRecord> {
		const now = new Date();
		const fullServer: VoiceServerRecord = {
			...server,
			createdAt: now,
			updatedAt: now,
		};
		await this.upsertServer(fullServer);
		return fullServer;
	}

	async upsertServer(server: VoiceServerRecord): Promise<void> {
		const row: VoiceServerRow = {
			region_id: server.regionId,
			server_id: server.serverId,
			endpoint: server.endpoint,
			api_key: server.apiKey,
			api_secret: server.apiSecret,
			latitude: server.latitude ?? null,
			longitude: server.longitude ?? null,
			is_active: server.isActive,
			soft_connection_limit: server.softConnectionLimit ?? null,
			vip_only: server.restrictions.vipOnly,
			required_guild_features: new Set(server.restrictions.requiredGuildFeatures),
			allowed_guild_ids: new Set(Array.from(server.restrictions.allowedGuildIds).map((id) => BigInt(id))),
			allowed_user_ids: new Set(Array.from(server.restrictions.allowedUserIds).map((id) => BigInt(id))),
			created_at: server.createdAt ?? new Date(),
			updated_at: server.updatedAt ?? new Date(),
		};
		await upsertOne(VoiceServers.upsertAll(row));
	}

	async deleteServer(regionId: string, serverId: string): Promise<void> {
		await deleteOneOrMany(VoiceServers.deleteByPk({region_id: regionId, server_id: serverId}));
	}

	private mapRegionRow(row: VoiceRegionRow): VoiceRegionRecord {
		return {
			id: row.id,
			name: row.name,
			emoji: row.emoji,
			latitude: row.latitude,
			longitude: row.longitude,
			isDefault: row.is_default ?? false,
			restrictions: {
				vipOnly: row.vip_only ?? false,
				requiredGuildFeatures: new Set(toIterable<string>(row.required_guild_features)),
				allowedGuildIds: createGuildIDSet(new Set(toIterable<bigint>(row.allowed_guild_ids))),
				allowedUserIds: createUserIDSet(new Set(toIterable<bigint>(row.allowed_user_ids))),
			},
			createdAt: row.created_at,
			updatedAt: row.updated_at,
		};
	}

	private mapServerRow(row: VoiceServerRow): VoiceServerRecord {
		return {
			regionId: row.region_id,
			serverId: row.server_id,
			endpoint: row.endpoint,
			apiKey: row.api_key,
			apiSecret: row.api_secret,
			latitude: row.latitude ?? null,
			longitude: row.longitude ?? null,
			isActive: row.is_active ?? true,
			softConnectionLimit: row.soft_connection_limit ?? null,
			restrictions: {
				vipOnly: row.vip_only ?? false,
				requiredGuildFeatures: new Set(toIterable<string>(row.required_guild_features)),
				allowedGuildIds: createGuildIDSet(new Set(toIterable<bigint>(row.allowed_guild_ids))),
				allowedUserIds: createUserIDSet(new Set(toIterable<bigint>(row.allowed_user_ids))),
			},
			createdAt: row.created_at,
			updatedAt: row.updated_at,
		};
	}
}
