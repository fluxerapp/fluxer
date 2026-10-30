// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createGuildIDSet, createUserIDSet, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {VoiceJoinTokenRow} from '@app/api/database/types/VoiceTypes';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {REXCTL_TEST_ROOM_PREFIX} from '@app/api/infrastructure/LiveKitWebhookService';
import {getVoiceServerHealth} from '@app/api/middleware/ServiceRegistry';
import {VOICE_CONFIGURATION_CHANNEL} from '@app/api/voice/VoiceConstants';
import type {VoiceJoinTokenRepository} from '@app/api/voice/VoiceJoinTokenRepository';
import type {VoiceRegionRecord, VoiceRegionWithServers, VoiceServerRecord} from '@app/api/voice/VoiceModel';
import {
	hashJoinSecret,
	joinTokenStatus,
	VOICE_JOIN_TOKEN_PREFIX,
	voiceJoinCommand,
} from '@app/api/voice/VoiceNodeService';
import type {VoiceRepository} from '@app/api/voice/VoiceRepository';
import {bundledVoiceServerId} from '@app/api/voice/VoiceServerEndpoints';
import {healthOf, toHealthResponse} from '@app/api/voice/VoiceServerHealth';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownVoiceRegionError} from '@fluxer/errors/src/domains/voice/UnknownVoiceRegionError';
import {UnknownVoiceServerError} from '@fluxer/errors/src/domains/voice/UnknownVoiceServerError';
import {VoiceJoinTokenInvalidError} from '@fluxer/errors/src/domains/voice/VoiceJoinTokenInvalidError';
import {VoiceServerApiKeyInUseError} from '@fluxer/errors/src/domains/voice/VoiceServerApiKeyInUseError';
import {VoiceServerIdInUseError} from '@fluxer/errors/src/domains/voice/VoiceServerIdInUseError';
import type {
	CreateVoiceJoinTokenRequest,
	CreateVoiceJoinTokenResponse,
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
	VoiceJoinTokenAdminResponse,
	VoiceRegionAdminResponse,
	VoiceServerAdminResponse,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import {AccessToken, TrackSource} from 'livekit-server-sdk';

interface AdminVoiceServiceDeps {
	apiContext: ApiContext;
	voiceRepository: VoiceRepository;
	joinTokenRepository: VoiceJoinTokenRepository;
	snowflakeService: ISnowflakeService;
	auditService: AdminAuditService;
}

const TEST_TOKEN_TTL_SECONDS = 60;

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
		const duplicates = await this.duplicateApiKeys();
		if (data.include_servers) {
			const regionsWithServers = regions as Array<VoiceRegionWithServers>;
			return {
				regions: regionsWithServers.map((region) => ({
					...this.mapVoiceRegionToAdminResponse(region),
					servers: region.servers
						.sort((a, b) => a.serverId.localeCompare(b.serverId))
						.map((server) => this.mapVoiceServerToAdminResponse(server, duplicates)),
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
			const duplicates = await this.duplicateApiKeys();
			return {
				region: {
					...this.mapVoiceRegionToAdminResponse(regionWithServers),
					servers: regionWithServers.servers.map((server) => this.mapVoiceServerToAdminResponse(server, duplicates)),
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
		await this.deps.auditService.createAuditLog({
			targetId: 0n,
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
		await this.deps.auditService.createAuditLog({
			targetId: 0n,
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
		await this.deps.auditService.createAuditLog({
			targetId: 0n,
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
		const duplicates = await this.duplicateApiKeys();
		return {
			servers: servers.map((server) => this.mapVoiceServerToAdminResponse(server, duplicates)),
		};
	}

	async getVoiceServer(data: GetVoiceServerRequest) {
		const {voiceRepository} = this.deps;
		const server = await voiceRepository.getServer(data.region_id, data.server_id);
		return {
			server: server ? this.mapVoiceServerToAdminResponse(server, await this.duplicateApiKeys()) : null,
		};
	}

	async createVoiceServer(data: CreateVoiceServerRequest, adminUserId: UserID, auditLogReason: string | null) {
		const {voiceRepository} = this.deps;
		const region = await voiceRepository.getRegion(data.region_id);
		if (!region) {
			throw new UnknownVoiceRegionError();
		}
		const existing = await voiceRepository.listAllServers();
		if (existing.some((server) => server.regionId === data.region_id && server.serverId === data.server_id)) {
			throw new VoiceServerIdInUseError();
		}
		if (data.server_id === bundledVoiceServerId() && data.api_key !== Config.voice.apiKey) {
			throw new VoiceServerIdInUseError();
		}
		if (existing.some((server) => server.apiKey === data.api_key)) {
			throw new VoiceServerApiKeyInUseError();
		}
		const server = await voiceRepository.createServer({
			regionId: data.region_id,
			serverId: data.server_id,
			endpoint: data.endpoint,
			isActive: data.is_active ?? true,
			softConnectionLimit: data.soft_connection_limit ?? null,
			notes: data.notes ?? null,
			apiKey: data.api_key ?? null,
			apiSecret: data.api_secret ?? null,
			latitude: data.latitude ?? null,
			longitude: data.longitude ?? null,
			internalEndpoint: data.internal_endpoint ?? null,
			draining: data.draining ?? false,
			pinnedHostname: null,
			pinnedSoftLimit: false,
			restrictions: buildVoiceRestrictions(data),
		});
		await this.publishVoiceConfiguration({type: 'server_created', regionId: data.region_id, serverId: data.server_id});
		await this.deps.auditService.createAuditLog({
			targetId: 0n,
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
			server: this.mapVoiceServerToAdminResponse(server, await this.duplicateApiKeys()),
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
		if (data.api_key !== undefined && data.api_key !== '' && data.api_key !== existing.apiKey) {
			const servers = await voiceRepository.listAllServers();
			if (servers.some((server) => server.apiKey === data.api_key)) {
				throw new VoiceServerApiKeyInUseError();
			}
			updates.apiKey = data.api_key;
		}
		if (data.api_secret !== undefined && data.api_secret !== '') updates.apiSecret = data.api_secret;
		if (data.latitude !== undefined) updates.latitude = data.latitude;
		if (data.longitude !== undefined) updates.longitude = data.longitude;
		if (data.is_active !== undefined) updates.isActive = data.is_active;
		if (data.soft_connection_limit !== undefined) updates.softConnectionLimit = data.soft_connection_limit;
		if (data.notes !== undefined) updates.notes = data.notes;
		if (data.internal_endpoint !== undefined) updates.internalEndpoint = data.internal_endpoint;
		if (data.draining !== undefined) updates.draining = data.draining;
		updates.restrictions = patchVoiceRestrictions(existing.restrictions, data);
		updates.updatedAt = new Date();
		await voiceRepository.upsertServer(updates);
		await this.publishVoiceConfiguration({type: 'server_updated', regionId: data.region_id, serverId: data.server_id});
		await this.deps.auditService.createAuditLog({
			targetId: 0n,
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
			server: this.mapVoiceServerToAdminResponse(updates, await this.duplicateApiKeys()),
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
		await this.deps.auditService.createAuditLog({
			targetId: 0n,
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

	private async duplicateApiKeys(): Promise<Set<string>> {
		const seen = new Set<string>();
		const duplicates = new Set<string>();
		for (const server of await this.deps.voiceRepository.listAllServers()) {
			if (seen.has(server.apiKey)) duplicates.add(server.apiKey);
			seen.add(server.apiKey);
		}
		return duplicates;
	}

	private mapVoiceServerToAdminResponse(server: VoiceServerRecord, duplicates: Set<string>): VoiceServerAdminResponse {
		const allowedGuildIds = Array.from(server.restrictions.allowedGuildIds).map((id) => id.toString());
		return {
			region_id: server.regionId,
			server_id: server.serverId,
			endpoint: server.endpoint,
			latitude: server.latitude ?? null,
			longitude: server.longitude ?? null,
			is_active: server.isActive,
			soft_connection_limit: server.softConnectionLimit ?? null,
			notes: server.notes,
			vip_only: server.restrictions.vipOnly,
			required_guild_features: Array.from(server.restrictions.requiredGuildFeatures),
			allowed_guild_ids: allowedGuildIds,
			allowed_user_ids: Array.from(server.restrictions.allowedUserIds).map((id) => id.toString()),
			internal_endpoint: server.internalEndpoint,
			draining: server.draining,
			health: toHealthResponse(healthOf(getVoiceServerHealth(), server)),
			duplicate_api_key: duplicates.has(server.apiKey),
			created_at: server.createdAt?.toISOString() ?? null,
			updated_at: server.updatedAt?.toISOString() ?? null,
		};
	}

	async createJoinToken(
		data: CreateVoiceJoinTokenRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<CreateVoiceJoinTokenResponse> {
		if (data.region_name && !data.region_id) {
			throw new UnknownVoiceRegionError();
		}
		if ((data.region_latitude === null) !== (data.region_longitude === null)) {
			throw InputValidationError.create('region_latitude', 'Pass region_latitude and region_longitude together.');
		}
		if (data.region_latitude !== null && !data.region_name) {
			throw InputValidationError.create(
				'region_latitude',
				'Region coordinates only apply to a new region in region_name.',
			);
		}
		if (data.region_id && !data.region_name && !(await this.deps.voiceRepository.getRegion(data.region_id))) {
			throw new UnknownVoiceRegionError();
		}
		if (data.server_id && data.server_id === bundledVoiceServerId()) {
			throw new VoiceServerIdInUseError();
		}
		const tokenId = await this.deps.snowflakeService.generate();
		const secret = randomBytes(32).toString('base64url');
		const now = new Date();
		const row: VoiceJoinTokenRow = {
			token_id: tokenId,
			secret_hash: hashJoinSecret(secret),
			created_by: adminUserId,
			created_at: now,
			expires_at: new Date(now.getTime() + data.expires_in_seconds * 1000),
			claim_id: null,
			claimed_at: null,
			used_at: null,
			used_by_ip: null,
			used_server_id: null,
			revoked_at: null,
			region_id: data.region_id,
			region_name: data.region_name,
			region_emoji: data.region_emoji,
			region_latitude: data.region_latitude,
			region_longitude: data.region_longitude,
			server_id: data.server_id,
			hostname: data.hostname,
			internal_endpoint: data.internal_endpoint,
			soft_connection_limit: data.soft_connection_limit,
			notes: data.notes,
			vip_only: data.vip_only,
			required_guild_features: new Set(data.required_guild_features),
			allowed_guild_ids: new Set(data.allowed_guild_ids.map(BigInt)),
			allowed_user_ids: new Set(data.allowed_user_ids.map(BigInt)),
		};
		await this.deps.joinTokenRepository.create(row);
		await this.deps.auditService.createAuditLog({
			targetId: 0n,
			adminUserId,
			targetType: 'voice_join_token',
			action: 'create_voice_join_token',
			auditLogReason,
			metadata: new Map([
				['token_id', tokenId.toString()],
				['region_id', data.region_id ?? ''],
			]),
		});
		const token = `${VOICE_JOIN_TOKEN_PREFIX}${tokenId}_${secret}`;
		return {
			token_id: tokenId.toString(),
			token,
			expires_at: row.expires_at.toISOString(),
			command: voiceJoinCommand(Config.endpoints.webApp, Config.instance.baseDomain, token),
		};
	}

	async listJoinTokens(): Promise<{tokens: Array<VoiceJoinTokenAdminResponse>}> {
		const rows = await this.deps.joinTokenRepository.list();
		rows.sort((a, b) => b.created_at.getTime() - a.created_at.getTime());
		return {
			tokens: rows.map((row) => ({
				token_id: row.token_id.toString(),
				status: joinTokenStatus(row),
				created_at: row.created_at.toISOString(),
				expires_at: row.expires_at.toISOString(),
				used_at: row.used_at?.toISOString() ?? null,
				used_server_id: row.used_server_id,
				revoked_at: row.revoked_at?.toISOString() ?? null,
				region_id: row.region_id,
				region_name: row.region_name,
				server_id: row.server_id,
				hostname: row.hostname,
				notes: row.notes,
			})),
		};
	}

	async revokeJoinToken(tokenId: bigint, adminUserId: UserID, auditLogReason: string | null) {
		const row = await this.deps.joinTokenRepository.get(tokenId);
		if (!row || joinTokenStatus(row) !== 'pending' || !(await this.deps.joinTokenRepository.revoke(row, new Date()))) {
			throw new VoiceJoinTokenInvalidError();
		}
		await this.deps.auditService.createAuditLog({
			targetId: 0n,
			adminUserId,
			targetType: 'voice_join_token',
			action: 'revoke_voice_join_token',
			auditLogReason,
			metadata: new Map([['token_id', tokenId.toString()]]),
		});
		return {success: true};
	}

	async createServerTestToken(
		data: GetVoiceServerRequest,
		adminUserId: UserID,
		auditLogReason: string | null,
	): Promise<{token: string; endpoint: string; room: string}> {
		const server = await this.deps.voiceRepository.getServer(data.region_id, data.server_id);
		if (!server) {
			throw new UnknownVoiceServerError();
		}
		const room = `${REXCTL_TEST_ROOM_PREFIX}${randomBytes(6).toString('hex')}`;
		const token = new AccessToken(server.apiKey, server.apiSecret, {
			identity: `rexctl-test-${randomBytes(4).toString('hex')}`,
			ttl: TEST_TOKEN_TTL_SECONDS,
		});
		token.addGrant({
			room,
			roomJoin: true,
			hidden: true,
			canPublish: true,
			canSubscribe: true,
			canPublishData: false,
			canPublishSources: [TrackSource.MICROPHONE],
		});
		await this.deps.auditService.createAuditLog({
			targetId: 0n,
			adminUserId,
			targetType: 'voice_server',
			action: 'create_voice_server_test_token',
			auditLogReason,
			metadata: new Map([
				['region_id', server.regionId],
				['server_id', server.serverId],
			]),
		});
		return {token: await token.toJwt(), endpoint: server.endpoint, room};
	}

	private async publishVoiceConfiguration(message: VoiceConfigurationMessage): Promise<void> {
		await this.deps.apiContext.services.cache.publish(VOICE_CONFIGURATION_CHANNEL, JSON.stringify(message));
	}
}
