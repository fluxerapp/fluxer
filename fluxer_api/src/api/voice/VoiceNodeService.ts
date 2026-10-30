// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, createHmac, randomUUID, timingSafeEqual} from 'node:crypto';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import {createGuildIDSet, createUserIDSet, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {VoiceJoinTokenRow} from '@app/api/database/types/VoiceTypes';
import {Logger} from '@app/api/Logger';
import {calculateDistance} from '@app/api/utils/GeoUtils';
import type {VoiceJoinTokenRepository} from '@app/api/voice/VoiceJoinTokenRepository';
import type {VoiceRegionRecord, VoiceServerRecord} from '@app/api/voice/VoiceModel';
import type {VoiceRepository} from '@app/api/voice/VoiceRepository';
import {
	bundledVoiceServerId,
	checkLiveKitKeys,
	parseNodeEndpoint,
	probeLiveKit,
	VoiceEndpointError,
} from '@app/api/voice/VoiceServerEndpoints';
import {healthOf, toHealthResponse, type VoiceServerHealthSource} from '@app/api/voice/VoiceServerHealth';
import {API_CODE_VERSION} from '@fluxer/constants/src/AppConstants';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {UnknownVoiceRegionError} from '@fluxer/errors/src/domains/voice/UnknownVoiceRegionError';
import {VoiceJoinEndpointInvalidError} from '@fluxer/errors/src/domains/voice/VoiceJoinEndpointInvalidError';
import {VoiceJoinEndpointUnreachableError} from '@fluxer/errors/src/domains/voice/VoiceJoinEndpointUnreachableError';
import {VoiceJoinKeyMismatchError} from '@fluxer/errors/src/domains/voice/VoiceJoinKeyMismatchError';
import {VoiceJoinTokenBusyError} from '@fluxer/errors/src/domains/voice/VoiceJoinTokenBusyError';
import {VoiceJoinTokenInvalidError} from '@fluxer/errors/src/domains/voice/VoiceJoinTokenInvalidError';
import {VoiceNodeSignatureInvalidError} from '@fluxer/errors/src/domains/voice/VoiceNodeSignatureInvalidError';
import {VoiceNodeTokenExpiredError} from '@fluxer/errors/src/domains/voice/VoiceNodeTokenExpiredError';
import {VoiceNodeUnknownError} from '@fluxer/errors/src/domains/voice/VoiceNodeUnknownError';
import {VoiceServerApiKeyInUseError} from '@fluxer/errors/src/domains/voice/VoiceServerApiKeyInUseError';
import {VoiceServerIdInUseError} from '@fluxer/errors/src/domains/voice/VoiceServerIdInUseError';
import type {
	UpdateVoiceNodeRequest,
	VoiceJoinPreviewRequest,
	VoiceJoinPreviewResponse,
	VoiceJoinRequest,
	VoiceJoinResponse,
	VoiceNodeResponse,
} from '@fluxer/schema/src/domains/voice/VoiceNodeSchemas';
import {lookupGeoipByIp} from '@pkgs/geoip/src/GeoipLookup';

export const VOICE_JOIN_TOKEN_PREFIX = 'fvj_';

export function voiceJoinCommand(webApp: string, baseDomain: string, token: string): string {
	const instance = URL.parse(webApp)?.host || baseDomain;
	return `curl -fsSL https://fluxer.dev/install.sh | sh -s -- voice ${instance} ${token}`;
}

export const VOICE_NODE_AUDIENCE = 'fluxer-voice-node';
export const VOICE_NODE_PATH = '/voice/node';

const CLAIM_STALE_MS = 2 * 60 * 1000;
const NODE_TOKEN_MAX_AHEAD_SECONDS = 5 * 60;
const NODE_TOKEN_LEEWAY_SECONDS = 60;
const PROBE_TIMEOUT_MS = 10_000;

export function hashJoinSecret(secret: string): string {
	return createHash('sha256').update(secret).digest('hex');
}

function parseJoinToken(header: string | undefined): {tokenId: bigint; secret: string} | null {
	const value = header?.startsWith('VoiceJoin ') ? header.slice('VoiceJoin '.length) : null;
	const match = value ? /^fvj_(\d{1,20})_([A-Za-z0-9_-]{43})$/.exec(value) : null;
	if (!match) return null;
	return {tokenId: BigInt(match[1]!), secret: match[2]!};
}

function sameHash(a: string, b: string): boolean {
	const left = Buffer.from(a, 'hex');
	const right = Buffer.from(b, 'hex');
	return left.length === right.length && timingSafeEqual(left, right);
}

export type JoinTokenStatus = 'pending' | 'used' | 'expired' | 'revoked';

export function joinTokenStatus(row: VoiceJoinTokenRow, now = new Date()): JoinTokenStatus {
	if (row.used_at) return 'used';
	if (row.revoked_at) return 'revoked';
	if (row.expires_at.getTime() <= now.getTime()) return 'expired';
	return 'pending';
}

function round(value: number): number {
	return Math.round(value * 10) / 10;
}

function bodyHash(body: string): string {
	return body === '' ? '' : createHash('sha256').update(body).digest('base64');
}

function decodeSegment(segment: string): Record<string, unknown> | null {
	try {
		const value: unknown = JSON.parse(Buffer.from(segment, 'base64url').toString('utf8'));
		return value && typeof value === 'object' && !Array.isArray(value) ? (value as Record<string, unknown>) : null;
	} catch {
		return null;
	}
}

interface NodeTokenParts {
	issuer: string;
	signingInput: string;
	signature: Buffer;
	claims: Record<string, unknown>;
}

function splitNodeToken(header: string | undefined): NodeTokenParts | null {
	const token = header?.startsWith('Bearer ') ? header.slice('Bearer '.length) : null;
	const parts = token?.split('.');
	if (parts?.length !== 3) return null;
	const [head, payload, signature] = parts as [string, string, string];
	const headerJson = decodeSegment(head);
	const claims = decodeSegment(payload);
	if (headerJson?.['alg'] !== 'HS256' || !claims || typeof claims['iss'] !== 'string') return null;
	return {
		issuer: claims['iss'],
		signingInput: `${head}.${payload}`,
		signature: Buffer.from(signature, 'base64url'),
		claims,
	};
}

function signatureMatches(parts: NodeTokenParts, secret: string): boolean {
	const expected = createHmac('sha256', secret).update(parts.signingInput).digest();
	return expected.length === parts.signature.length && timingSafeEqual(expected, parts.signature);
}

export type EndpointVerifier = (
	endpoint: string,
	keys: {apiKey: string; apiSecret: string},
	internalEndpoint: string | null,
	probePublic: boolean,
) => Promise<void>;

const verifyNodeEndpoint: EndpointVerifier = async (endpoint, keys, internalEndpoint, probePublic) => {
	const base = internalEndpoint ?? endpoint;
	const options = {
		checkAddress: internalEndpoint === null,
		allowPrivate: Config.voice.joinAllowPrivateEndpoints,
		timeoutMs: PROBE_TIMEOUT_MS,
	};
	if (probePublic && internalEndpoint !== null) {
		await probeLiveKit(endpoint, {...options, checkAddress: true}).catch((error: unknown) => {
			Logger.warn(
				{endpoint, error: error instanceof Error ? error.message : String(error)},
				"The API can't connect to the new public endpoint of a voice server with an internal endpoint",
			);
		});
	}
	await probeLiveKit(base, options);
	await checkLiveKitKeys(base, keys, options);
};

let endpointVerifier: EndpointVerifier = verifyNodeEndpoint;

export function setVoiceNodeEndpointVerifierForTesting(verifier: EndpointVerifier | null): void {
	endpointVerifier = verifier ?? verifyNodeEndpoint;
}

function asApiError(error: unknown): Error {
	if (!(error instanceof VoiceEndpointError)) return error as Error;
	switch (error.reason) {
		case 'invalid':
		case 'private':
			return new VoiceJoinEndpointInvalidError(error.message);
		case 'key_mismatch':
			return new VoiceJoinKeyMismatchError();
		default:
			return new VoiceJoinEndpointUnreachableError(error.message);
	}
}

interface VoiceNodeServiceDeps {
	voiceRepository: VoiceRepository;
	joinTokenRepository: VoiceJoinTokenRepository;
	auditService: AdminAuditService;
	health: VoiceServerHealthSource | null;
	publish: (message: string) => Promise<void>;
}

type RegionSource = 'token' | 'requested' | 'nearest' | 'default';

interface ChosenRegion {
	id: string;
	name: string;
	isNew: boolean;
	distanceKm: number | null;
	emoji: string;
	source: RegionSource;
}

export class VoiceNodeService {
	constructor(private readonly deps: VoiceNodeServiceDeps) {}

	async authenticateJoin(header: string | undefined): Promise<VoiceJoinTokenRow> {
		const parsed = parseJoinToken(header);
		if (!parsed) throw new VoiceJoinTokenInvalidError();
		const row = await this.deps.joinTokenRepository.get(parsed.tokenId);
		if (!row || !sameHash(row.secret_hash, hashJoinSecret(parsed.secret)) || joinTokenStatus(row) !== 'pending') {
			throw new VoiceJoinTokenInvalidError();
		}
		return row;
	}

	async preview(row: VoiceJoinTokenRow, body: VoiceJoinPreviewRequest, clientIp: string | null) {
		const point = await this.locate(body.public_ipv4 ?? clientIp);
		const region = await this.chooseRegion(row, body.region_id, point);
		const servers = await this.deps.voiceRepository.listAllServers();
		const response: VoiceJoinPreviewResponse = {
			server_id: row.server_id ?? this.suggestServerId(region.id, servers),
			server_id_pinned: row.server_id !== null,
			region: {
				id: region.id,
				name: region.name,
				distance_km: region.distanceKm,
				new: region.isNew,
				source: region.source,
			},
			latitude: point ? round(point.latitude) : null,
			longitude: point ? round(point.longitude) : null,
			hostname: row.hostname,
			suggested_hostname: `voice-${region.id}.${Config.instance.baseDomain}`,
			soft_connection_limit: row.soft_connection_limit,
			webhook_url: `${Config.endpoints.apiPublic}/webhooks/livekit`,
			api_code_version: API_CODE_VERSION,
			release: process.env.BUILD_VERSION?.trim() ?? '',
			expires_at: row.expires_at.toISOString(),
		};
		return response;
	}

	async join(row: VoiceJoinTokenRow, body: VoiceJoinRequest, clientIp: string | null): Promise<VoiceJoinResponse> {
		const {joinTokenRepository, voiceRepository} = this.deps;
		const now = new Date();
		if (row.claim_id && row.claimed_at && now.getTime() - row.claimed_at.getTime() < CLAIM_STALE_MS) {
			throw new VoiceJoinTokenBusyError();
		}
		const claimId = randomUUID();
		if (!(await joinTokenRepository.claim(row, claimId, now))) {
			throw new VoiceJoinTokenBusyError();
		}
		try {
			const point =
				body.latitude !== undefined && body.longitude !== undefined
					? {latitude: body.latitude, longitude: body.longitude}
					: await this.locate(body.public_ipv4 ?? clientIp);
			const region = await this.chooseRegion(row, body.region_id, point);
			const servers = await voiceRepository.listAllServers();
			const serverId = row.server_id ?? body.server_id ?? this.suggestServerId(region.id, servers);
			if (serverId === bundledVoiceServerId() || servers.some((server) => server.serverId === serverId)) {
				throw new VoiceServerIdInUseError();
			}
			if (servers.some((server) => server.apiKey === body.api_key)) throw new VoiceServerApiKeyInUseError();
			const endpoint = this.checkEndpoint(body.endpoint, row.hostname);
			await endpointVerifier(
				endpoint,
				{apiKey: body.api_key, apiSecret: body.api_secret},
				row.internal_endpoint,
				false,
			).catch((error: unknown) => {
				throw asApiError(error);
			});
			if (region.isNew) {
				await voiceRepository.createRegion({
					id: region.id,
					name: region.name,
					emoji: region.emoji,
					latitude: row.region_latitude ?? point?.latitude ?? 0,
					longitude: row.region_longitude ?? point?.longitude ?? 0,
					isDefault: false,
					restrictions: emptyRestrictions(),
				});
			}
			const created = await voiceRepository.createServerIfAbsent({
				regionId: region.id,
				serverId,
				endpoint,
				apiKey: body.api_key,
				apiSecret: body.api_secret,
				latitude: point ? round(point.latitude) : null,
				longitude: point ? round(point.longitude) : null,
				isActive: true,
				draining: false,
				internalEndpoint: row.internal_endpoint,
				pinnedHostname: row.hostname ?? null,
				pinnedSoftLimit: row.soft_connection_limit != null,
				softConnectionLimit: row.soft_connection_limit ?? body.soft_connection_limit ?? null,
				notes: row.notes ?? `Joined with a join token on ${now.toISOString().slice(0, 10)}`,
				restrictions: {
					vipOnly: row.vip_only ?? false,
					requiredGuildFeatures: new Set(row.required_guild_features ?? []),
					allowedGuildIds: createGuildIDSet(new Set(row.allowed_guild_ids ?? [])),
					allowedUserIds: createUserIDSet(new Set(row.allowed_user_ids ?? [])),
				},
			});
			if (!created) throw new VoiceServerIdInUseError();
			await joinTokenRepository.markUsed(row, claimId, {at: new Date(), ip: clientIp, serverId});
			await this.deps.publish(JSON.stringify({type: 'server_created', regionId: region.id, serverId}));
			await this.deps.auditService.createAuditLog({
				targetId: 0n,
				adminUserId: row.created_by as UserID,
				targetType: 'voice_server',
				action: 'create_voice_server',
				auditLogReason: null,
				metadata: new Map([
					['region_id', region.id],
					['server_id', serverId],
					['endpoint', endpoint],
					['via', `voice_join:${row.token_id}`],
					['node_ip', clientIp ?? ''],
				]),
			});
			return {
				server_id: serverId,
				region_id: region.id,
				endpoint,
				webhook_url: `${Config.endpoints.apiPublic}/webhooks/livekit`,
				node_url: `${Config.endpoints.apiPublic}${VOICE_NODE_PATH}`,
			};
		} catch (error) {
			await joinTokenRepository.release(row, claimId).catch(() => false);
			throw error;
		}
	}

	async authenticateNode(header: string | undefined, method: string, body: string): Promise<VoiceServerRecord> {
		const parts = splitNodeToken(header);
		if (!parts) throw new VoiceNodeSignatureInvalidError();
		const candidates = (await this.deps.voiceRepository.listAllServers()).filter(
			(server) => server.apiKey === parts.issuer,
		);
		if (candidates.length === 0) throw new VoiceNodeUnknownError();
		const server = candidates.find((candidate) => signatureMatches(parts, candidate.apiSecret));
		const {claims} = parts;
		const valid =
			claims['aud'] === VOICE_NODE_AUDIENCE &&
			claims['method'] === method &&
			claims['path'] === VOICE_NODE_PATH &&
			claims['sha256'] === bodyHash(body) &&
			claims['video'] === undefined &&
			typeof claims['exp'] === 'number';
		if (!valid || !server) throw new VoiceNodeSignatureInvalidError();
		const now = Math.floor(Date.now() / 1000);
		const exp = claims['exp'] as number;
		const iat = typeof claims['iat'] === 'number' ? claims['iat'] : null;
		const current =
			exp > now - NODE_TOKEN_LEEWAY_SECONDS &&
			exp <= now + NODE_TOKEN_MAX_AHEAD_SECONDS + NODE_TOKEN_LEEWAY_SECONDS &&
			(iat === null || iat <= now + NODE_TOKEN_LEEWAY_SECONDS);
		if (!current) throw new VoiceNodeTokenExpiredError();
		return server;
	}

	describe(server: VoiceServerRecord): VoiceNodeResponse {
		return {
			region_id: server.regionId,
			server_id: server.serverId,
			endpoint: server.endpoint,
			latitude: server.latitude,
			longitude: server.longitude,
			is_active: server.isActive,
			draining: server.draining,
			soft_connection_limit: server.softConnectionLimit,
			health: toHealthResponse(healthOf(this.deps.health, server)),
		};
	}

	async update(server: VoiceServerRecord, body: UpdateVoiceNodeRequest): Promise<VoiceNodeResponse> {
		const updated: VoiceServerRecord = {...server, updatedAt: new Date()};
		if (body.endpoint !== undefined && body.endpoint !== server.endpoint) {
			updated.endpoint = this.checkEndpoint(body.endpoint, server.pinnedHostname);
			await endpointVerifier(updated.endpoint, server, server.internalEndpoint, true).catch((error: unknown) => {
				throw asApiError(error);
			});
		}
		if (
			server.pinnedSoftLimit &&
			body.soft_connection_limit !== undefined &&
			body.soft_connection_limit !== server.softConnectionLimit
		) {
			throw InputValidationError.create(
				'soft_connection_limit',
				'The join token set the soft limit of this server. Change it in the admin panel.',
			);
		}
		if (body.draining !== undefined) updated.draining = body.draining;
		if (body.latitude !== undefined) updated.latitude = body.latitude;
		if (body.longitude !== undefined) updated.longitude = body.longitude;
		if (body.soft_connection_limit !== undefined) updated.softConnectionLimit = body.soft_connection_limit;
		await this.deps.voiceRepository.upsertServer(updated);
		await this.deps.publish(
			JSON.stringify({type: 'server_updated', regionId: server.regionId, serverId: server.serverId}),
		);
		await this.audit(server, 'update_voice_server', Object.keys(body).sort().join(','));
		return this.describe(updated);
	}

	async remove(server: VoiceServerRecord): Promise<void> {
		await this.deps.voiceRepository.deleteServer(server.regionId, server.serverId);
		await this.deps.publish(
			JSON.stringify({type: 'server_deleted', regionId: server.regionId, serverId: server.serverId}),
		);
		await this.audit(server, 'delete_voice_server', '');
	}

	private async audit(server: VoiceServerRecord, action: string, fields: string): Promise<void> {
		const metadata = new Map([
			['region_id', server.regionId],
			['server_id', server.serverId],
			['via', `voice_node:${server.serverId}`],
		]);
		if (fields) metadata.set('fields', fields);
		await this.deps.auditService.createAuditLog({
			targetId: 0n,
			adminUserId: 0n as UserID,
			targetType: 'voice_server',
			action,
			auditLogReason: null,
			metadata,
		});
	}

	private checkEndpoint(endpoint: string, hostname: string | null): string {
		try {
			const url = parseNodeEndpoint(endpoint);
			if (hostname && url.hostname.toLowerCase() !== hostname.toLowerCase()) {
				throw new VoiceEndpointError('invalid', `The endpoint must use the hostname ${hostname}`);
			}
			return url.toString().replace(/\/$/, '');
		} catch (error) {
			throw asApiError(error);
		}
	}

	private async locate(ip: string | null): Promise<{latitude: number; longitude: number} | null> {
		if (!ip || !Config.geoip.maxmindDbPath) return null;
		const result = await lookupGeoipByIp(ip, Config.geoip.maxmindDbPath).catch(() => null);
		if (result?.latitude == null || result.longitude == null) return null;
		return {latitude: result.latitude, longitude: result.longitude};
	}

	private async chooseRegion(
		row: VoiceJoinTokenRow,
		requested: string | undefined,
		point: {latitude: number; longitude: number} | null,
	): Promise<ChosenRegion> {
		const regions = await this.deps.voiceRepository.listRegions();
		const distance = (region: VoiceRegionRecord) =>
			point ? Math.round(calculateDistance(point.latitude, point.longitude, region.latitude, region.longitude)) : null;
		const pick = (region: VoiceRegionRecord, source: RegionSource): ChosenRegion => ({
			id: region.id,
			name: region.name,
			emoji: region.emoji,
			isNew: false,
			distanceKm: distance(region),
			source,
		});
		const wanted = row.region_id ?? requested ?? null;
		if (wanted) {
			const source = row.region_id ? 'token' : 'requested';
			const existing = regions.find((region) => region.id === wanted);
			if (existing) return pick(existing, source);
			if (row.region_id && row.region_name) {
				return {
					id: row.region_id,
					name: row.region_name,
					emoji: row.region_emoji ?? '🌍',
					isNew: true,
					distanceKm: 0,
					source,
				};
			}
			throw new UnknownVoiceRegionError();
		}
		if (regions.length === 0) throw new UnknownVoiceRegionError();
		if (point) {
			const nearest = regions.reduce((best, region) =>
				(distance(region) ?? 0) < (distance(best) ?? 0) ? region : best,
			);
			return pick(nearest, 'nearest');
		}
		return pick(regions.find((region) => region.isDefault) ?? regions[0]!, 'default');
	}

	private suggestServerId(regionId: string, servers: Array<VoiceServerRecord>): string {
		const used = new Set(servers.map((server) => server.serverId));
		const bundled = bundledVoiceServerId();
		if (bundled) used.add(bundled);
		for (let n = 1; ; n++) {
			const id = `${regionId}-server-${n}`;
			if (!used.has(id)) return id;
		}
	}
}

function emptyRestrictions(): VoiceRegionRecord['restrictions'] {
	return {
		vipOnly: false,
		requiredGuildFeatures: new Set(),
		allowedGuildIds: createGuildIDSet(new Set()),
		allowedUserIds: createUserIDSet(new Set()),
	};
}
