// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {EntityAssetService, PreparedAssetUpload} from '@app/api/infrastructure/EntityAssetService';
import type {IAssetDeletionQueue} from '@app/api/infrastructure/IAssetDeletionQueue';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {
	assetReferenceHash,
	assetReferenceUrl,
	assetStorageKey,
	customBadgeAssetReferenceFromOwnUrl,
	customBadgeImageUrl,
} from '@app/api/instance/InstanceAssetReferences';
import type {CustomBadgeDefinition, InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {type CustomBadgeType, MAX_CUSTOM_BADGES_PER_TYPE} from '@fluxer/constants/src/BadgeConstants';
import {MaxBadgesError} from '@fluxer/errors/src/domains/badge/MaxBadgesError';
import {UnknownBadgeError} from '@fluxer/errors/src/domains/badge/UnknownBadgeError';
import type {
	AdminCustomBadgeListResponse,
	AdminCustomBadgeResponse,
	CreateCustomBadgeRequest,
	UpdateCustomBadgeRequest,
} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';
import type {UserMutationResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

interface AdminBadgeServiceDeps {
	instanceConfigRepository: InstanceConfigRepository;
	entityAssetService: EntityAssetService;
	assetDeletionQueue: IAssetDeletionQueue;
	snowflakeService: ISnowflakeService;
	userRepository: UserRepository;
	cacheService: ICacheService;
}

function mapCustomBadgeToAdminResponse(badge: CustomBadgeDefinition): AdminCustomBadgeResponse {
	return {
		id: badge.id,
		type: badge.type,
		name: badge.name,
		description: badge.description,
		asset_url: customBadgeImageUrl(badge.asset, Config.endpoints.media),
		href: badge.href,
	};
}

function findBadge(badges: ReadonlyArray<CustomBadgeDefinition>, badgeId: string): CustomBadgeDefinition {
	const badge = badges.find((candidate) => candidate.id === badgeId);
	if (!badge) {
		throw new UnknownBadgeError();
	}
	return badge;
}

function assertBelowTypeLimit(badges: ReadonlyArray<CustomBadgeDefinition>, type: CustomBadgeType): void {
	if (badges.filter((badge) => badge.type === type).length >= MAX_CUSTOM_BADGES_PER_TYPE) {
		throw new MaxBadgesError({maxBadges: MAX_CUSTOM_BADGES_PER_TYPE});
	}
}

export async function assertCustomBadgeIdsOfType(
	instanceConfigRepository: Pick<InstanceConfigRepository, 'getCustomBadges'>,
	type: CustomBadgeType,
	badgeIds: ReadonlyArray<bigint>,
): Promise<void> {
	const badges = await instanceConfigRepository.getCustomBadges();
	const known = new Set(badges.filter((badge) => badge.type === type).map((badge) => badge.id));
	if (badgeIds.some((badgeId) => !known.has(badgeId.toString()))) {
		throw new UnknownBadgeError();
	}
}

export class AdminBadgeService {
	constructor(private readonly deps: AdminBadgeServiceDeps) {}

	async listBadges(): Promise<AdminCustomBadgeListResponse> {
		const badges = await this.deps.instanceConfigRepository.getCustomBadges();
		return {badges: badges.map(mapCustomBadgeToAdminResponse)};
	}

	async createBadge(data: CreateCustomBadgeRequest): Promise<AdminCustomBadgeResponse> {
		const {instanceConfigRepository, snowflakeService} = this.deps;
		assertBelowTypeLimit(await instanceConfigRepository.getCustomBadges(), data.type);
		const badgeId = await snowflakeService.generate();
		const prepared = await this.uploadImage(badgeId, data.image);
		const badge: CustomBadgeDefinition = {
			id: badgeId.toString(),
			type: data.type,
			name: data.name,
			description: data.description ?? null,
			asset: this.storedReference(prepared),
			href: data.href ?? null,
		};
		await this.writeOrRollback(prepared, (current) => {
			assertBelowTypeLimit(current, data.type);
			return [...current, badge];
		});
		return mapCustomBadgeToAdminResponse(badge);
	}

	async updateBadge(badgeId: bigint, data: UpdateCustomBadgeRequest): Promise<AdminCustomBadgeResponse> {
		const id = badgeId.toString();
		const existing = findBadge(await this.deps.instanceConfigRepository.getCustomBadges(), id);
		const prepared =
			data.image === undefined ? null : await this.uploadImage(badgeId, data.image, assetReferenceHash(existing.asset));
		const replaced: {badge?: CustomBadgeDefinition} = {};
		const badges = await this.writeOrRollback(prepared, (current) => {
			const previous = findBadge(current, id);
			replaced.badge = previous;
			const next: CustomBadgeDefinition = {
				...previous,
				name: data.name,
				description: data.description,
				href: data.href,
				asset: prepared ? this.storedReference(prepared) : previous.asset,
			};
			return current.map((badge) => (badge.id === id ? next : badge));
		});
		const updated = findBadge(badges, id);
		if (replaced.badge && replaced.badge.asset !== updated.asset) {
			await this.releaseAsset(replaced.badge.asset);
		}
		return mapCustomBadgeToAdminResponse(updated);
	}

	async deleteBadge(badgeId: bigint): Promise<void> {
		const id = badgeId.toString();
		const removed: {badge?: CustomBadgeDefinition} = {};
		await this.deps.instanceConfigRepository.updateCustomBadges((current) => {
			removed.badge = findBadge(current, id);
			return current.filter((badge) => badge.id !== id);
		});
		if (removed.badge) {
			await this.releaseAsset(removed.badge.asset);
		}
	}

	async updateUserBadges(params: {
		userId: UserID;
		badgeIds: ReadonlyArray<bigint>;
		acls: ReadonlySet<string>;
	}): Promise<UserMutationResponse> {
		const {userRepository, cacheService, instanceConfigRepository} = this.deps;
		const user = await userRepository.findUniqueAssert(params.userId);
		await assertCustomBadgeIdsOfType(instanceConfigRepository, 'user', params.badgeIds);
		const badgeIds = new Set(params.badgeIds);
		const updatedUser = await userRepository.patchUpsert(
			params.userId,
			{badge_ids: badgeIds.size > 0 ? badgeIds : null},
			user.toRow(),
		);
		return {user: await mapUserToAdminResponse(updatedUser, cacheService, params.acls)};
	}

	private async uploadImage(
		badgeId: bigint,
		image: string,
		previousHash: string | null = null,
	): Promise<PreparedAssetUpload> {
		return this.deps.entityAssetService.prepareAssetUpload({
			assetType: 'custom_badge',
			entityType: 'instance',
			entityId: badgeId,
			previousHash,
			base64Image: image,
			errorPath: 'image',
		});
	}

	private storedReference(prepared: PreparedAssetUpload): string {
		if (!prepared.newCdnUrl) {
			throw new Error('Custom badge image upload returned no URL');
		}
		return customBadgeAssetReferenceFromOwnUrl(prepared.newCdnUrl, Config.endpoints.media);
	}

	private async writeOrRollback(
		prepared: PreparedAssetUpload | null,
		update: (current: ReadonlyArray<CustomBadgeDefinition>) => Array<CustomBadgeDefinition>,
	): Promise<ReadonlyArray<CustomBadgeDefinition>> {
		try {
			return await this.deps.instanceConfigRepository.updateCustomBadges(update);
		} catch (error) {
			if (prepared) await this.deps.entityAssetService.rollbackAssetUpload(prepared);
			throw error;
		}
	}

	private async releaseAsset(reference: string): Promise<void> {
		await this.deps.assetDeletionQueue.queueDeletion({
			s3Key: assetStorageKey(reference),
			cdnUrl: assetReferenceUrl(reference, Config.endpoints.media),
			reason: 'custom_badge_released',
		});
	}
}
