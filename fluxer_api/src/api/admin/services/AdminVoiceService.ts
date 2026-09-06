// SPDX-License-Identifier: AGPL-3.0-or-later

import {UnknownVoiceRegionError} from '@fluxer/errors/src/domains/voice/UnknownVoiceRegionError';
import {UnknownVoiceServerError} from '@fluxer/errors/src/domains/voice/UnknownVoiceServerError';
import type {
	CreateVoiceRegionRequest,
	CreateVoiceServerRequest,
	DeleteVoiceRegionRequest,
	DeleteVoiceServerRequest,
	GetVoiceRegionRequest,
	GetVoiceServerRequest,
	ListVoiceRegionsRequest,
	ListVoiceServersRequest,
	UpdateVoiceRegionRequest,
	UpdateVoiceServerRequest,
	VoiceRegionAdminResponse,
	VoiceServerAdminResponse,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import type {ApiContext} from '../../ApiContext';
import {createGuildIDSet, createUserIDSet, type UserID} from '../../BrandedTypes';
import {VOICE_CONFIGURATION_CHANNEL} from '../../voice/VoiceConstants';
import type {VoiceRegionRecord, VoiceRegionWithServers, VoiceServerRecord} from '../../voice/VoiceModel';
import type {VoiceRepository} from '../../voice/VoiceRepository';
import type {AdminAuditService} from './AdminAuditService';

interface AdminVoiceServiceDeps {
	apiContext: ApiContext;
	voiceRepository: VoiceRepository;
	auditService: AdminAuditService;
}

interface VoiceRestrictionData {
	vip_only?: boolean;
	required_guild_features?: Array<string>;
	allowed_guild_ids?: Array<bigint | string>;
	allowed_user_ids?: Array<bigint | string>;
}

interface VoiceConfigurationMessage {
	type: string;
	regionId: string;
	serverId?: string;
}

interface VoiceAuditParams {
	adminUserId: UserID;
	auditLogReason: string | null;
	targetType: string;
	action: string;
	metadata: Map<string, string>;
}

function buildVoiceRestrictions(data: VoiceRestrictionData): VoiceRegionRecord['restrictions'] {
	return {
		vipOnly: data.vip_only ?? false,
		requiredGuildFeatures: new Set(data.required_guild_features ?? []),
		allowedGuildIds: createGuildIDSet(new Set((data.allowed_guild_ids ?? []).map(BigInt))),
		allowedUserIds: createUserIDSet(new Set((data.allowed_user_ids ?? []).map(BigInt))),
	};
}

function patchVoiceRestrictions(
	restrictions: VoiceRegionRecord['restrictions'],
	data: VoiceRestrictionData,
): VoiceRegionRecord['restrictions'] {
	if (
		data.vip_only === undefined &&
		data.required_guild_features === undefined &&
		data.allowed_guild_ids === undefined &&
		data.allowed_user_ids === undefined
	) {
		return restrictions;
	}
	const updatedRestrictions = {...restrictions};
	if (data.vip_only !== undefined) updatedRestrictions.vipOnly = data.vip_only;
	if (data.required_guild_features !== undefined)
		updatedRestrictions.requiredGuildFeatures = new Set(data.required_guild_features);
	if (data.allowed_guild_ids !== undefined) {
		updatedRestrictions.allowedGuildIds = createGuildIDSet(new Set(data.allowed_guild_ids.map(BigInt)));
	}
	if (data.allowed_user_ids !== undefined) {
		updatedRestrictions.allowedUserIds = createUserIDSet(new Set(data.allowed_user_ids.map(BigInt)));
	}
	return updatedRestrictions;
}

export class AdminVoiceService {
	constructor(private readonly deps: AdminVoiceServiceDeps) {}

	async listVoiceRegions(data: ListVoiceRegionsRequest) {
		const {voiceRepository} = this.deps;
		const regions = data.include_servers
			? await voiceRepository.listRegionsWithServers()
			: await voiceRepository.listRegions();
		regions.sort((a, b) => a.name.localeCompare(b.name));
		if (data.include_servers) {
			const regionsWithServers = regions as Array<VoiceRegionWithServers>;
			return {
				regions: regionsWithServers.map((region) => ({
					...this.mapVoiceRegionToAdminResponse(region),
					servers: region.servers
						.sort((a, b) => a.serverId.localeCompare(b.serverId))
						.map((server) => this.mapVoiceServerToAdminResponse(server)),
				})),
			};
		}
		const voiceRegions = regions as Array<VoiceRegionRecord>;
		return {
			regions: voiceRegions.map((region) => this.mapVoiceRegionToAdminResponse(region)),
		};
	}

	async getVoiceRegion(data: GetVoiceRegionRequest) {
		const {voiceRepository} = this.deps;
		const region = data.include_servers
			? await voiceRepository.getRegionWithServers(data.id)
			: await voiceRepository.getRegion(data.id);
		if (!region) {
			return {region: null};
		}
		if (data.include_servers && 'servers' in region) {
			const regionWithServers = region as VoiceRegionWithServers;
			return {
				region: {
					...this.mapVoiceRegionToAdminResponse(regionWithServers),
					servers: regionWithServers.servers.map((server) => this.mapVoiceServerToAdminResponse(server)),
				},
			};
		}
		return {
			region: this.mapVoiceRegionToAdminResponse(region),
		};
	}

	async createVoiceRegion(data: CreateVoiceRegionRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {voiceRepository} = this.deps;
		const region = await voiceRepository.createRegion({
			id: data.id,
			name: data.name,
			emoji: data.emoji,
			latitude: data.latitude,
			longitude: data.longitude,
			isDefault: data.is_default ?? false,
			restrictions: buildVoiceRestrictions(data),
		});
		await this.publishVoiceConfiguration({type: 'region_created', regionId: region.id});
		await this.createVoiceAuditLog({
			adminUserId,
			targetType: 'voice_region',
			action: 'create_voice_region',
			auditLogReason,
			metadata: new Map([
				['region_id', region.id],
				['name', region.name],
			]),
		});
		return {
			region: this.mapVoiceRegionToAdminResponse(region),
		};
	}

	async updateVoiceRegion(data: UpdateVoiceRegionRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {voiceRepository} = this.deps;
		const existing = await voiceRepository.getRegion(data.id);
		if (!existing) {
			throw new UnknownVoiceRegionError();
		}
		const updates: VoiceRegionRecord = {...existing};
		if (data.name !== undefined) updates.name = data.name;
		if (data.emoji !== undefined) updates.emoji = data.emoji;
		if (data.latitude !== undefined) updates.latitude = data.latitude;
		if (data.longitude !== undefined) updates.longitude = data.longitude;
		if (data.is_default !== undefined) updates.isDefault = data.is_default;
		updates.restrictions = patchVoiceRestrictions(existing.restrictions, data);
		updates.updatedAt = new Date();
		await voiceRepository.upsertRegion(updates);
		await this.publishVoiceConfiguration({type: 'region_updated', regionId: data.id});
		await this.createVoiceAuditLog({
			adminUserId,
			targetType: 'voice_region',
			action: 'update_voice_region',
			auditLogReason,
			metadata: new Map([['region_id', data.id]]),
		});
		return {
			region: this.mapVoiceRegionToAdminResponse(updates),
		};
	}

	async deleteVoiceRegion(data: DeleteVoiceRegionRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {voiceRepository} = this.deps;
		const existing = await voiceRepository.getRegion(data.id);
		if (!existing) {
			throw new UnknownVoiceRegionError();
		}
		await voiceRepository.deleteRegion(data.id);
		await this.publishVoiceConfiguration({type: 'region_deleted', regionId: data.id});
		await this.createVoiceAuditLog({
			adminUserId,
			targetType: 'voice_region',
			action: 'delete_voice_region',
			auditLogReason,
			metadata: new Map([
				['region_id', data.id],
				['name', existing.name],
			]),
		});
		return {success: true};
	}

	async listVoiceServers(data: ListVoiceServersRequest) {
		const {voiceRepository} = this.deps;
		const servers = await voiceRepository.listServers(data.region_id);
		return {
			servers: servers.map((server) => this.mapVoiceServerToAdminResponse(server)),
		};
	}

	async getVoiceServer(data: GetVoiceServerRequest) {
		const {voiceRepository} = this.deps;
		const server = await voiceRepository.getServer(data.region_id, data.server_id);
		return {
			server: server ? this.mapVoiceServerToAdminResponse(server) : null,
		};
	}

	async createVoiceServer(data: CreateVoiceServerRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {voiceRepository} = this.deps;
		const region = await voiceRepository.getRegion(data.region_id);
		if (!region) {
			throw new UnknownVoiceRegionError();
		}
		const server = await voiceRepository.createServer({
			regionId: data.region_id,
			serverId: data.server_id,
			endpoint: data.endpoint,
			isActive: data.is_active ?? true,
			apiKey: data.api_key ?? null,
			apiSecret: data.api_secret ?? null,
			latitude: data.latitude ?? null,
			longitude: data.longitude ?? null,
			restrictions: buildVoiceRestrictions(data),
		});
		await this.publishVoiceConfiguration({type: 'server_created', regionId: data.region_id, serverId: data.server_id});
		await this.createVoiceAuditLog({
			adminUserId,
			targetType: 'voice_server',
			action: 'create_voice_server',
			auditLogReason,
			metadata: new Map([
				['region_id', server.regionId],
				['server_id', server.serverId],
				['endpoint', server.endpoint],
			]),
		});
		return {
			server: this.mapVoiceServerToAdminResponse(server),
		};
	}

	async updateVoiceServer(data: UpdateVoiceServerRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {voiceRepository} = this.deps;
		const existing = await voiceRepository.getServer(data.region_id, data.server_id);
		if (!existing) {
			throw new UnknownVoiceServerError();
		}
		const updates: VoiceServerRecord = {...existing};
		if (data.endpoint !== undefined) updates.endpoint = data.endpoint;
		if (data.api_key !== undefined && data.api_key !== '') updates.apiKey = data.api_key;
		if (data.api_secret !== undefined && data.api_secret !== '') updates.apiSecret = data.api_secret;
		if (data.latitude !== undefined) updates.latitude = data.latitude;
		if (data.longitude !== undefined) updates.longitude = data.longitude;
		if (data.is_active !== undefined) updates.isActive = data.is_active;
		updates.restrictions = patchVoiceRestrictions(existing.restrictions, data);
		updates.updatedAt = new Date();
		await voiceRepository.upsertServer(updates);
		await this.publishVoiceConfiguration({type: 'server_updated', regionId: data.region_id, serverId: data.server_id});
		await this.createVoiceAuditLog({
			adminUserId,
			targetType: 'voice_server',
			action: 'update_voice_server',
			auditLogReason,
			metadata: new Map([
				['region_id', data.region_id],
				['server_id', data.server_id],
			]),
		});
		return {
			server: this.mapVoiceServerToAdminResponse(updates),
		};
	}

	async deleteVoiceServer(data: DeleteVoiceServerRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {voiceRepository} = this.deps;
		const existing = await voiceRepository.getServer(data.region_id, data.server_id);
		if (!existing) {
			throw new UnknownVoiceServerError();
		}
		await voiceRepository.deleteServer(data.region_id, data.server_id);
		await this.publishVoiceConfiguration({type: 'server_deleted', regionId: data.region_id, serverId: data.server_id});
		await this.createVoiceAuditLog({
			adminUserId,
			targetType: 'voice_server',
			action: 'delete_voice_server',
			auditLogReason,
			metadata: new Map([
				['region_id', data.region_id],
				['server_id', data.server_id],
				['endpoint', existing.endpoint],
			]),
		});
		return {success: true};
	}

	private mapVoiceRegionToAdminResponse(region: VoiceRegionRecord): VoiceRegionAdminResponse {
		const allowedGuildIds = Array.from(region.restrictions.allowedGuildIds).map((id) => id.toString());
		return {
			id: region.id,
			name: region.name,
			emoji: region.emoji,
			latitude: region.latitude,
			longitude: region.longitude,
			is_default: region.isDefault,
			vip_only: region.restrictions.vipOnly,
			required_guild_features: Array.from(region.restrictions.requiredGuildFeatures),
			allowed_guild_ids: allowedGuildIds,
			allowed_user_ids: Array.from(region.restrictions.allowedUserIds).map((id) => id.toString()),
			created_at: region.createdAt?.toISOString() ?? null,
			updated_at: region.updatedAt?.toISOString() ?? null,
		};
	}

	private mapVoiceServerToAdminResponse(server: VoiceServerRecord): VoiceServerAdminResponse {
		const allowedGuildIds = Array.from(server.restrictions.allowedGuildIds).map((id) => id.toString());
		return {
			region_id: server.regionId,
			server_id: server.serverId,
			endpoint: server.endpoint,
			latitude: server.latitude ?? null,
			longitude: server.longitude ?? null,
			is_active: server.isActive,
			vip_only: server.restrictions.vipOnly,
			required_guild_features: Array.from(server.restrictions.requiredGuildFeatures),
			allowed_guild_ids: allowedGuildIds,
			allowed_user_ids: Array.from(server.restrictions.allowedUserIds).map((id) => id.toString()),
			created_at: server.createdAt?.toISOString() ?? null,
			updated_at: server.updatedAt?.toISOString() ?? null,
		};
	}

	private async publishVoiceConfiguration(message: VoiceConfigurationMessage): Promise<void> {
		await this.deps.apiContext.services.cache.publish(VOICE_CONFIGURATION_CHANNEL, JSON.stringify(message));
	}

	private async createVoiceAuditLog({
		adminUserId,
		auditLogReason,
		targetType,
		action,
		metadata,
	}: VoiceAuditParams): Promise<void> {
		await this.deps.auditService.createAuditLog({
			adminUserId,
			targetType,
			targetId: BigInt(0),
			action,
			auditLogReason,
			metadata,
		});
	}
}
