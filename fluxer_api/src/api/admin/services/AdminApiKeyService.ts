// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomInt, timingSafeEqual} from 'node:crypto';
import type {AdminApiKeyRepository} from '@app/api/admin/repositories/AdminApiKeyRepository';
import type {UserID} from '@app/api/BrandedTypes';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {requireGrantedAdminACLs} from '@app/api/middleware/AdminMiddleware';
import type {AdminApiKey} from '@app/api/models/AdminApiKey';
import type {StoredSecretVerifier} from '@app/api/utils/StoredSecret';
import {AdminApiKeyNotFoundError} from '@fluxer/errors/src/domains/admin/AdminApiKeyNotFoundError';
import type {CreateAdminApiKeyRequest, UpdateAdminApiKeyRequest} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {ms} from 'itty-time';

const ADMIN_KEY_PREFIX = 'fa_';
const RANDOM_KEY_LENGTH = 32;
const CHARSET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
const VERIFIED_KEY_CACHE_TTL_MS = ms('1 minute');
const VERIFIED_KEY_CACHE_MAX_ENTRIES = 1024;

interface VerifiedKeyEntry {
	keyHash: string;
	digest: Buffer;
	expiresAtMs: number;
}

interface CreateApiKeyResult {
	key: string;
	apiKey: {
		keyId: string;
		name: string;
		createdAt: Date;
		expiresAt: Date | null;
		acls: Set<string>;
	};
}

export interface AdminApiKeyView {
	keyId: string;
	name: string;
	createdAt: Date;
	lastUsedAt: Date | null;
	expiresAt: Date | null;
	createdById: UserID;
	acls: Set<string>;
}

export class AdminApiKeyService {
	private readonly verifiedKeys = new Map<bigint, VerifiedKeyEntry>();

	constructor(
		private readonly adminApiKeyRepository: AdminApiKeyRepository,
		private readonly snowflakeService: ISnowflakeService,
		private readonly storedSecrets: StoredSecretVerifier,
	) {}

	private generateRawKey(keyId: bigint): string {
		const randomChars = Array.from({length: RANDOM_KEY_LENGTH}, () => CHARSET[randomInt(CHARSET.length)]).join('');
		return `${ADMIN_KEY_PREFIX}${keyId.toString()}_${randomChars}`;
	}

	private extractKeyId(rawKey: string): bigint | null {
		if (!rawKey.startsWith(ADMIN_KEY_PREFIX)) {
			return null;
		}
		const remainder = rawKey.slice(ADMIN_KEY_PREFIX.length);
		const underscoreIdx = remainder.indexOf('_');
		if (underscoreIdx <= 0) {
			return null;
		}
		const keyIdStr = remainder.slice(0, underscoreIdx);
		if (!/^\d+$/.test(keyIdStr)) {
			return null;
		}
		try {
			return BigInt(keyIdStr);
		} catch {
			return null;
		}
	}

	async createApiKey(
		request: CreateAdminApiKeyRequest,
		createdBy: UserID,
		creatorAcls?: Set<string>,
	): Promise<CreateApiKeyResult> {
		const keyId = await this.snowflakeService.generate();
		const rawKey = this.generateRawKey(keyId);
		const expiresAt = request.expires_in_days ? new Date(Date.now() + request.expires_in_days * ms('1 day')) : null;
		this.assertGrantableAcls(request.acls, creatorAcls);
		const aclsSet = new Set(request.acls);
		const apiKey = await this.adminApiKeyRepository.create(
			{
				name: request.name,
				expiresAt,
				acls: aclsSet,
			},
			createdBy,
			keyId,
			rawKey,
		);
		return {
			key: rawKey,
			apiKey: {
				keyId: apiKey.keyId.toString(),
				name: apiKey.name,
				createdAt: apiKey.createdAt,
				expiresAt: apiKey.expiresAt,
				acls: apiKey.acls,
			},
		};
	}

	async validateApiKey(rawKey: string): Promise<{
		keyId: bigint;
		createdById: UserID;
		acls: Set<string> | null;
	} | null> {
		const keyId = this.extractKeyId(rawKey);
		if (keyId === null) return null;
		const apiKey = await this.adminApiKeyRepository.findById(keyId);
		if (!apiKey) {
			return null;
		}
		if (apiKey.isExpired()) {
			return null;
		}
		const verification = await this.verifyRawKey(apiKey, rawKey);
		if (verification === 'invalid') {
			return null;
		}
		if (verification === 'verified') {
			await this.adminApiKeyRepository.updateLastUsed(apiKey.keyId, apiKey.expiresAt);
		}
		return {
			keyId: apiKey.keyId,
			createdById: apiKey.createdById,
			acls: apiKey.acls,
		};
	}

	private async verifyRawKey(apiKey: AdminApiKey, rawKey: string): Promise<'cached' | 'verified' | 'invalid'> {
		const digest = createHash('sha256').update(rawKey).digest();
		const now = Date.now();
		const cached = this.verifiedKeys.get(apiKey.keyId);
		if (
			cached &&
			cached.expiresAtMs > now &&
			cached.keyHash === apiKey.keyHash &&
			timingSafeEqual(cached.digest, digest)
		) {
			return 'cached';
		}
		const valid = await this.storedSecrets.matches(`admin_api_key:${apiKey.keyId}`, rawKey, apiKey.keyHash);
		if (!valid) {
			return 'invalid';
		}
		this.verifiedKeys.delete(apiKey.keyId);
		if (this.verifiedKeys.size >= VERIFIED_KEY_CACHE_MAX_ENTRIES) {
			const oldestKeyId = this.verifiedKeys.keys().next().value;
			if (oldestKeyId !== undefined) this.verifiedKeys.delete(oldestKeyId);
		}
		this.verifiedKeys.set(apiKey.keyId, {
			keyHash: apiKey.keyHash,
			digest,
			expiresAtMs: now + VERIFIED_KEY_CACHE_TTL_MS,
		});
		return 'verified';
	}

	async listKeys(createdBy: UserID): Promise<Array<AdminApiKeyView>> {
		const apiKeys = await this.adminApiKeyRepository.listByCreator(createdBy);
		return apiKeys.map((key) => this.toView(key));
	}

	async getKey(keyId: bigint, createdBy: UserID): Promise<AdminApiKeyView> {
		const apiKey = await this.findOwnedKey(keyId, createdBy);
		return this.toView(apiKey);
	}

	async updateKey(
		keyId: bigint,
		createdBy: UserID,
		request: UpdateAdminApiKeyRequest,
		creatorAcls?: Set<string>,
	): Promise<AdminApiKeyView> {
		const apiKey = await this.findOwnedKey(keyId, createdBy);
		if (request.acls) {
			this.assertGrantableAcls(request.acls, creatorAcls);
		}
		const updated = await this.adminApiKeyRepository.update(apiKey, {
			name: request.name,
			acls: request.acls ? new Set(request.acls) : undefined,
		});
		return this.toView(updated);
	}

	async revokeKey(keyId: bigint, createdBy: UserID): Promise<void> {
		const apiKey = await this.findOwnedKey(keyId, createdBy);
		await this.adminApiKeyRepository.revoke(apiKey.keyId, createdBy);
	}

	private async findOwnedKey(keyId: bigint, createdBy: UserID): Promise<AdminApiKey> {
		const apiKey = await this.adminApiKeyRepository.findById(keyId);
		if (!apiKey) {
			throw new AdminApiKeyNotFoundError();
		}
		if (apiKey.createdById !== createdBy) {
			throw new AdminApiKeyNotFoundError();
		}
		return apiKey;
	}

	private assertGrantableAcls(acls: ReadonlyArray<string>, creatorAcls?: Set<string>): void {
		if (creatorAcls) requireGrantedAdminACLs(creatorAcls, acls);
	}

	private toView(apiKey: AdminApiKey): AdminApiKeyView {
		return {
			keyId: apiKey.keyId.toString(),
			name: apiKey.name,
			createdAt: apiKey.createdAt,
			lastUsedAt: apiKey.lastUsedAt,
			expiresAt: apiKey.expiresAt,
			createdById: apiKey.createdById,
			acls: apiKey.acls ?? new Set(),
		};
	}
}
