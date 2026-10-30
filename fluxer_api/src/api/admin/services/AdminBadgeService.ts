// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {EntityAssetService, PreparedAssetUpload} from '@app/api/infrastructure/EntityAssetService';
import type {IAssetDeletionQueue} from '@app/api/infrastructure/IAssetDeletionQueue';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {
	type CustomBadgeCatalog,
	CustomBadgeCatalogUnpublishedError,
	type CustomBadgeMove,
	type CustomBadgePage,
} from '@app/api/instance/custom_badges/CustomBadgeCatalog';
import type {CustomBadge} from '@app/api/instance/custom_badges/CustomBadgeRepository';
import {
	assetReferenceHash,
	assetReferenceUrl,
	assetStorageKey,
	customBadgeAssetReferenceFromOwnUrl,
	customBadgeImageUrl,
} from '@app/api/instance/InstanceAssetReferences';
import type {
	BuiltInBadgeImages,
	CustomBadgeCatalogState,
	InstanceConfigRepository,
} from '@app/api/instance/InstanceConfigRepository';
import {PROFILE_BADGE_ASSETS} from '@app/api/user/profile_badges/ProfileBadgeAssets';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {
	BUILT_IN_BADGE_KINDS,
	type BuiltInBadgeKind,
	CUSTOM_BADGE_PAGE_LIMIT_MAX,
	type CustomBadgeType,
} from '@fluxer/constants/src/BadgeConstants';
import {MaxBadgesError} from '@fluxer/errors/src/domains/badge/MaxBadgesError';
import {UnknownBadgeError} from '@fluxer/errors/src/domains/badge/UnknownBadgeError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {
	AdminBuiltInBadgeImageResponse,
	AdminCustomBadgeListQuery,
	AdminCustomBadgeListResponse,
	AdminCustomBadgeResponse,
	AdminCustomBadgeSettingsResponse,
	CreateCustomBadgeRequest,
	MoveCustomBadgeRequest,
	UpdateCustomBadgeRequest,
	UpdateCustomBadgeSettingsRequest,
} from '@fluxer/schema/src/domains/admin/AdminBadgeSchemas';
import type {UserMutationResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

interface AdminBadgeServiceDeps {
	instanceConfigRepository: Pick<InstanceConfigRepository, 'getBuiltInBadgeImages' | 'updateBuiltInBadgeImages'>;
	catalog: CustomBadgeCatalog;
	entityAssetService: EntityAssetService;
	assetDeletionQueue: IAssetDeletionQueue;
	snowflakeService: ISnowflakeService;
	userRepository: UserRepository;
	cacheService: ICacheService;
}

function mapCustomBadgeToAdminResponse(badge: CustomBadge): AdminCustomBadgeResponse {
	return {
		id: badge.id,
		type: badge.type,
		position: badge.position.toString(),
		name: badge.name,
		description: badge.description,
		asset_url: customBadgeImageUrl(badge.asset, Config.endpoints.media),
		href: badge.href,
	};
}

function builtInBadgeAsset(kind: BuiltInBadgeKind): string | null {
	return kind === 'staff' || kind === 'premium' ? PROFILE_BADGE_ASSETS[kind] : null;
}

function mapBuiltInBadgeImages(images: BuiltInBadgeImages): Array<AdminBuiltInBadgeImageResponse> {
	return BUILT_IN_BADGE_KINDS.map((kind) => {
		const asset = images[kind];
		return {
			kind,
			asset_url: asset ? customBadgeImageUrl(asset, Config.endpoints.media) : null,
			built_in_asset: builtInBadgeAsset(kind),
		};
	});
}

const INT64_MAX = (1n << 63n) - 1n;

function mapSettings(state: CustomBadgeCatalogState): AdminCustomBadgeSettingsResponse {
	return {max_per_user: state.max_per_user, max_per_guild: state.max_per_guild};
}

export async function assertCustomBadgeIdsOfType(
	catalog: Pick<CustomBadgeCatalog, 'getByIds'>,
	type: CustomBadgeType,
	badgeIds: ReadonlyArray<bigint>,
): Promise<void> {
	const known = new Set((await catalog.getByIds(badgeIds, type)).map((badge) => badge.id));
	if (badgeIds.some((badgeId) => !known.has(badgeId.toString()))) {
		throw new UnknownBadgeError();
	}
}

export function assertCustomBadgeAssignmentLimit(
	current: ReadonlySet<bigint>,
	next: ReadonlySet<bigint>,
	max: number,
): void {
	if (next.size > max && [...next].some((badgeId) => !current.has(badgeId))) {
		throw new MaxBadgesError({maxBadges: max});
	}
}

function toMove(request: MoveCustomBadgeRequest): CustomBadgeMove {
	const hasDirection = request.direction !== undefined;
	const hasAfter = request.after_id !== undefined;
	if (hasDirection === hasAfter) {
		throw InputValidationError.create('direction', 'Send either direction or after_id');
	}
	return request.direction !== undefined ? {direction: request.direction} : {afterId: request.after_id ?? null};
}

export class AdminBadgeService {
	constructor(private readonly deps: AdminBadgeServiceDeps) {}

	async listBadges(query: AdminCustomBadgeListQuery): Promise<AdminCustomBadgeListResponse> {
		const {catalog, instanceConfigRepository} = this.deps;
		if (query.ids !== undefined && (query.query !== undefined || query.after !== undefined)) {
			throw InputValidationError.create('ids', 'Send ids without query and after');
		}
		const [page, state, builtIn] = await Promise.all([
			this.readListPage(query),
			catalog.state(),
			instanceConfigRepository.getBuiltInBadgeImages(),
		]);
		return {
			badges: page.badges.map(mapCustomBadgeToAdminResponse),
			next: page.next,
			version: catalog.clientVersion(state.version),
			settings: mapSettings(state),
			built_in: mapBuiltInBadgeImages(builtIn),
		};
	}

	private async readListPage(query: AdminCustomBadgeListQuery): Promise<CustomBadgePage> {
		const {catalog} = this.deps;
		if (query.ids !== undefined) {
			const ids = [...new Set(query.ids.split(','))].filter((id) => BigInt(id) <= INT64_MAX);
			return {badges: await catalog.getByIds(ids, query.type), next: null};
		}
		if (query.query !== undefined) {
			return catalog.search({type: query.type, query: query.query, after: query.after, limit: query.limit});
		}
		return (await catalog.readPage({type: query.type, after: query.after, limit: query.limit})).value;
	}

	private firstPage(): Promise<AdminCustomBadgeListResponse> {
		return this.listBadges({limit: CUSTOM_BADGE_PAGE_LIMIT_MAX});
	}

	async replaceBuiltInBadgeImage(kind: BuiltInBadgeKind, image: string): Promise<AdminCustomBadgeListResponse> {
		const {instanceConfigRepository, snowflakeService} = this.deps;
		const previous = (await instanceConfigRepository.getBuiltInBadgeImages())[kind];
		const prepared = await this.uploadImage(
			await snowflakeService.generate(),
			image,
			previous ? assetReferenceHash(previous) : null,
		);
		const replaced: {asset?: string} = {};
		try {
			await instanceConfigRepository.updateBuiltInBadgeImages((current) => {
				replaced.asset = current[kind];
				return {...current, [kind]: this.storedReference(prepared)};
			});
		} catch (error) {
			await this.deps.entityAssetService.rollbackAssetUpload(prepared);
			throw error;
		}
		if (replaced.asset) {
			await this.releaseAsset(replaced.asset);
		}
		return this.firstPage();
	}

	async resetBuiltInBadgeImage(kind: BuiltInBadgeKind): Promise<AdminCustomBadgeListResponse> {
		const removed: {asset?: string} = {};
		await this.deps.instanceConfigRepository.updateBuiltInBadgeImages((current) => {
			removed.asset = current[kind];
			const {[kind]: _, ...rest} = current;
			return rest;
		});
		if (removed.asset) {
			await this.releaseAsset(removed.asset);
		}
		return this.firstPage();
	}

	async createBadge(data: CreateCustomBadgeRequest): Promise<AdminCustomBadgeResponse> {
		const {catalog, snowflakeService} = this.deps;
		const badgeId = await snowflakeService.generate();
		const prepared = await this.uploadImage(badgeId, data.image);
		const badge = await this.orRollback(prepared, () =>
			catalog.create({
				id: badgeId,
				type: data.type,
				name: data.name,
				description: data.description ?? null,
				asset: this.storedReference(prepared),
				href: data.href ?? null,
			}),
		);
		return mapCustomBadgeToAdminResponse(badge);
	}

	async updateBadge(badgeId: bigint, data: UpdateCustomBadgeRequest): Promise<AdminCustomBadgeResponse> {
		const {catalog} = this.deps;
		const existing = await catalog.getById(badgeId);
		if (!existing) throw new UnknownBadgeError();
		const prepared =
			data.image === undefined ? null : await this.uploadImage(badgeId, data.image, assetReferenceHash(existing.asset));
		const {after} = await this.orRollback(prepared, () =>
			catalog.update(
				badgeId,
				{
					name: data.name,
					description: data.description,
					href: data.href,
					...(prepared ? {asset: this.storedReference(prepared)} : {}),
				},
				async ({before, after}) => {
					if (before.asset !== after.asset) await this.releaseAsset(before.asset);
				},
			),
		);
		return mapCustomBadgeToAdminResponse(after);
	}

	async deleteBadge(badgeId: bigint): Promise<void> {
		await this.deps.catalog.delete(badgeId, (removed) => this.releaseAsset(removed.asset));
	}

	async moveBadge(badgeId: bigint, request: MoveCustomBadgeRequest): Promise<AdminCustomBadgeResponse> {
		return mapCustomBadgeToAdminResponse(await this.deps.catalog.move(badgeId, toMove(request)));
	}

	async reorderBadges(badgeIds: ReadonlyArray<bigint>): Promise<AdminCustomBadgeListResponse> {
		await this.deps.catalog.reorderAll(badgeIds);
		return this.firstPage();
	}

	async updateSettings(patch: UpdateCustomBadgeSettingsRequest): Promise<AdminCustomBadgeSettingsResponse> {
		return mapSettings(await this.deps.catalog.updateSettings(patch));
	}

	async updateUserBadges(params: {
		userId: UserID;
		badgeIds: ReadonlyArray<bigint>;
		acls: ReadonlySet<string>;
	}): Promise<UserMutationResponse> {
		const {userRepository, cacheService, catalog} = this.deps;
		const user = await userRepository.findUniqueAssert(params.userId);
		await assertCustomBadgeIdsOfType(catalog, 'user', params.badgeIds);
		const badgeIds = new Set(params.badgeIds);
		assertCustomBadgeAssignmentLimit(user.badgeIds, badgeIds, (await catalog.state()).max_per_user);
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

	private async orRollback<T>(prepared: PreparedAssetUpload | null, write: () => Promise<T>): Promise<T> {
		try {
			return await write();
		} catch (error) {
			if (prepared && !(error instanceof CustomBadgeCatalogUnpublishedError)) {
				await this.deps.entityAssetService.rollbackAssetUpload(prepared);
			}
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
