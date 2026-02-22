/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {EmojiID, GuildID, StickerID, UserID} from '@fluxer/api/src/BrandedTypes';
import {createEmojiID, createGuildID, createStickerID} from '@fluxer/api/src/BrandedTypes';
import {
	mapGuildEmojisWithUsersToResponse,
	mapGuildEmojiToResponse,
	mapGuildStickersWithUsersToResponse,
	mapGuildStickerToResponse,
} from '@fluxer/api/src/guild/GuildModel';
import type {IGuildRepositoryAggregate} from '@fluxer/api/src/guild/repositories/IGuildRepositoryAggregate';
import type {ExpressionAssetPurger} from '@fluxer/api/src/guild/services/content/ExpressionAssetPurger';
import type {AvatarService} from '@fluxer/api/src/infrastructure/AvatarService';
import {getMetricsService} from '@fluxer/api/src/infrastructure/MetricsService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import type {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import type {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import {resolveLimitSafe} from '@fluxer/api/src/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@fluxer/api/src/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import {ExpressionPack} from '@fluxer/api/src/models/ExpressionPack';
import type {User} from '@fluxer/api/src/models/User';
import type {
	PackExpressionAccessResolution,
	PackExpressionAccessResolver,
	PackExpressionAccessResolverParams,
} from '@fluxer/api/src/pack/PackExpressionAccessResolver';
import {mapPackToSummary} from '@fluxer/api/src/pack/PackModel';
import type {PackRepository, PackType} from '@fluxer/api/src/pack/PackRepository';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {
	MAX_CREATED_PACKS_NON_PREMIUM,
	MAX_INSTALLED_PACKS_NON_PREMIUM,
	MAX_PACK_EXPRESSIONS,
} from '@fluxer/constants/src/LimitConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {FeatureAccessError} from '@fluxer/errors/src/domains/core/FeatureAccessError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {UnknownGuildEmojiError} from '@fluxer/errors/src/domains/guild/UnknownGuildEmojiError';
import {UnknownGuildStickerError} from '@fluxer/errors/src/domains/guild/UnknownGuildStickerError';
import {InvalidPackTypeError} from '@fluxer/errors/src/domains/pack/InvalidPackTypeError';
import {MaxPackExpressionsError} from '@fluxer/errors/src/domains/pack/MaxPackExpressionsError';
import {MaxPackLimitError} from '@fluxer/errors/src/domains/pack/MaxPackLimitError';
import {PackAccessDeniedError} from '@fluxer/errors/src/domains/pack/PackAccessDeniedError';
import {UnknownPackError} from '@fluxer/errors/src/domains/pack/UnknownPackError';
import type {
	GuildEmojiResponse,
	GuildEmojiWithUserResponse,
	GuildStickerResponse,
	GuildStickerWithUserResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {PackDashboardResponse, PackDashboardSectionResponse} from '@fluxer/schema/src/domains/pack/PackSchemas';

export class PackService {
	constructor(
		private readonly packRepository: PackRepository,
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly avatarService: AvatarService,
		private readonly snowflakeService: SnowflakeService,
		private readonly assetPurger: ExpressionAssetPurger,
		private readonly userRepository: IUserRepository,
		private readonly userCacheService: UserCacheService,
		private readonly limitConfigService: LimitConfigService,
	) {}

	private async requireExpressionPackAccess(userId: UserID): Promise<void> {
		const user = await this.userRepository.findUnique(userId);
		if (!user || (user.flags & UserFlags.STAFF) === 0n) {
			throw new FeatureTemporarilyDisabledError();
		}
	}

	private async hasFeatureAccess(userId: UserID, limitKey: LimitKey): Promise<boolean> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			return false;
		}

		const ctx = createLimitMatchContext({user});
		const value = resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, limitKey, 0);
		return value > 0;
	}

	private async ensurePackOwner(userId: UserID, packId: GuildID): Promise<ExpressionPack> {
		const pack = await this.packRepository.getPack(packId);
		if (!pack) {
			throw new UnknownPackError();
		}
		if (pack.creatorId !== userId) {
			throw new PackAccessDeniedError();
		}
		return pack;
	}

	private async collectInstalledPacks(userId: UserID): Promise<Array<{pack: ExpressionPack; installedAt: Date}>> {
		const installations = await this.packRepository.listInstallations(userId);
		const results: Array<{pack: ExpressionPack; installedAt: Date}> = [];
		for (const row of installations) {
			const pack = await this.packRepository.getPack(row.pack_id);
			if (!pack) {
				await this.packRepository.removeInstallation(userId, row.pack_id);
				continue;
			}
			results.push({pack, installedAt: row.installed_at});
		}
		return results;
	}

	private async requireFeature(userId: UserID, limitKey: LimitKey): Promise<void> {
		if (!(await this.hasFeatureAccess(userId, limitKey))) {
			throw new FeatureAccessError();
		}
	}

	private async getInstalledPackIdsByType(userId: UserID, packType: PackType): Promise<Set<GuildID>> {
		const installations = await this.packRepository.listInstallations(userId);
		return new Set(installations.filter((row) => row.pack_type === packType).map((row) => row.pack_id));
	}

	private buildPackExpressionAccessResolution(
		userId: UserID,
		packType: PackType,
		pack: ExpressionPack | null,
	): PackExpressionAccessResolution {
		if (!pack) {
			return 'not-pack';
		}
		if (pack.type !== packType) {
			return 'not-pack';
		}
		return pack.creatorId === userId ? 'accessible' : 'not-accessible';
	}

	async createPackExpressionAccessResolver(
		params: PackExpressionAccessResolverParams,
	): Promise<PackExpressionAccessResolver> {
		const {userId, type} = params;

		if (!userId) {
			return {
				resolve: async () => 'not-pack',
			};
		}

		const installedPackIds = await this.getInstalledPackIdsByType(userId, type);
		const resolutionCache = new Map<GuildID, PackExpressionAccessResolution>();

		return {
			resolve: async (packId: GuildID) => {
				if (installedPackIds.has(packId)) {
					resolutionCache.set(packId, 'accessible');
					return 'accessible';
				}

				const cached = resolutionCache.get(packId);
				if (cached) {
					return cached;
				}

				const pack = await this.packRepository.getPack(packId);
				const resolution = this.buildPackExpressionAccessResolution(userId, type, pack);
				resolutionCache.set(packId, resolution);
				return resolution;
			},
		};
	}

	async listUserPacks(userId: UserID): Promise<PackDashboardResponse> {
		await this.requireExpressionPackAccess(userId);
		const user = await this.userRepository.findUnique(userId);
		const ctx = createLimitMatchContext({user});
		resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, 'feature_global_expressions', 0);
		const createdEmoji = await this.packRepository.listPacksByCreator(userId, 'emoji');
		const createdSticker = await this.packRepository.listPacksByCreator(userId, 'sticker');
		const installations = await this.collectInstalledPacks(userId);

		const installedEmoji = installations
			.filter((entry) => entry.pack.type === 'emoji')
			.map((entry) => mapPackToSummary(entry.pack, entry.installedAt));
		const installedSticker = installations
			.filter((entry) => entry.pack.type === 'sticker')
			.map((entry) => mapPackToSummary(entry.pack, entry.installedAt));

		const fallbackCreatedLimit = MAX_CREATED_PACKS_NON_PREMIUM;
		const fallbackInstalledLimit = MAX_INSTALLED_PACKS_NON_PREMIUM;
		const createdLimit = this.resolveLimitForUser(user ?? null, 'max_created_packs', fallbackCreatedLimit);
		const installedLimit = this.resolveLimitForUser(user ?? null, 'max_installed_packs', fallbackInstalledLimit);

		const emojiSection: PackDashboardSectionResponse = {
			installed_limit: installedLimit,
			created_limit: createdLimit,
			installed: installedEmoji,
			created: createdEmoji.map((pack) => mapPackToSummary(pack)),
		};

		const stickerSection: PackDashboardSectionResponse = {
			installed_limit: installedLimit,
			created_limit: createdLimit,
			installed: installedSticker,
			created: createdSticker.map((pack) => mapPackToSummary(pack)),
		};

		return {
			emoji: emojiSection,
			sticker: stickerSection,
		};
	}

	async createPack(params: {
		user: User;
		type: PackType;
		name: string;
		description?: string | null;
	}): Promise<ExpressionPack> {
		await this.requireExpressionPackAccess(params.user.id);
		await this.requireFeature(params.user.id, 'feature_global_expressions');
		const createdCount = await this.packRepository.countPacksByCreator(params.user.id, params.type);
		const fallbackLimit = MAX_CREATED_PACKS_NON_PREMIUM;
		const limit = this.resolveLimitForUser(params.user, 'max_created_packs', fallbackLimit);
		if (createdCount >= limit) {
			throw new MaxPackLimitError(params.type, limit, 'create');
		}

		const now = new Date();
		const packId = createGuildID(await this.snowflakeService.generate());
		return await this.packRepository.upsertPack({
			pack_id: packId,
			pack_type: params.type,
			creator_id: params.user.id,
			name: params.name,
			description: params.description ?? null,
			created_at: now,
			updated_at: now,
			version: 1,
		});
	}

	async updatePack(params: {
		userId: UserID;
		packId: GuildID;
		name?: string;
		description?: string | null;
	}): Promise<ExpressionPack> {
		await this.requireExpressionPackAccess(params.userId);
		const pack = await this.ensurePackOwner(params.userId, params.packId);
		const now = new Date();
		const updatedPack = new ExpressionPack({
			...pack.toRow(),
			name: params.name ?? pack.name,
			description: params.description === undefined ? pack.description : params.description,
			updated_at: now,
		});
		return await this.packRepository.upsertPack(updatedPack.toRow());
	}

	async deletePack(userId: UserID, packId: GuildID): Promise<void> {
		await this.requireExpressionPackAccess(userId);
		await this.ensurePackOwner(userId, packId);
		await this.packRepository.deletePack(packId);
	}

	async installPack(userId: UserID, packId: GuildID): Promise<void> {
		await this.requireExpressionPackAccess(userId);
		const pack = await this.packRepository.getPack(packId);
		if (!pack) {
			throw new UnknownPackError();
		}

		const alreadyInstalled = await this.packRepository.hasInstallation(userId, packId);
		if (alreadyInstalled) {
			return;
		}

		await this.requireFeature(userId, 'feature_global_expressions');
		const user = await this.userRepository.findUnique(userId);
		const fallbackLimit = MAX_INSTALLED_PACKS_NON_PREMIUM;
		const limit = this.resolveLimitForUser(user ?? null, 'max_installed_packs', fallbackLimit);
		const installations = await this.collectInstalledPacks(userId);
		const typeCount = installations.filter((entry) => entry.pack.type === pack.type).length;
		if (typeCount >= limit) {
			throw new MaxPackLimitError(pack.type, limit, 'install');
		}

		await this.packRepository.addInstallation({
			user_id: userId,
			pack_id: packId,
			pack_type: pack.type,
			installed_at: new Date(),
		});
	}

	async uninstallPack(userId: UserID, packId: GuildID): Promise<void> {
		await this.requireExpressionPackAccess(userId);
		await this.packRepository.removeInstallation(userId, packId);
	}

	async getInstalledPackIds(userId: UserID): Promise<Set<GuildID>> {
		await this.requireExpressionPackAccess(userId);
		const installations = await this.collectInstalledPacks(userId);
		return new Set(installations.map((entry) => entry.pack.id));
	}

	async getPackEmojis(params: {
		userId: UserID;
		packId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildEmojiWithUserResponse>> {
		await this.requireExpressionPackAccess(params.userId);
		const pack = await this.ensurePackOwner(params.userId, params.packId);
		if (pack.type !== 'emoji') {
			throw new InvalidPackTypeError('emoji');
		}

		const emojis = await this.guildRepository.listEmojis(pack.id);
		return await mapGuildEmojisWithUsersToResponse(emojis, this.userCacheService, params.requestCache);
	}

	async getPackStickers(params: {
		userId: UserID;
		packId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildStickerWithUserResponse>> {
		await this.requireExpressionPackAccess(params.userId);
		const pack = await this.ensurePackOwner(params.userId, params.packId);
		if (pack.type !== 'sticker') {
			throw new InvalidPackTypeError('sticker');
		}

		const stickers = await this.guildRepository.listStickers(pack.id);
		return await mapGuildStickersWithUsersToResponse(stickers, this.userCacheService, params.requestCache);
	}

	async createPackEmoji(params: {
		user: User;
		packId: GuildID;
		name: string;
		image: string;
	}): Promise<GuildEmojiResponse> {
		await this.requireExpressionPackAccess(params.user.id);
		await this.requireFeature(params.user.id, 'feature_global_expressions');
		const pack = await this.ensurePackOwner(params.user.id, params.packId);
		if (pack.type !== 'emoji') {
			throw new InvalidPackTypeError('emoji');
		}

		const emojiCount = await this.guildRepository.countEmojis(pack.id);
		const expressionLimit = this.resolveLimitForUser(params.user, 'max_pack_expressions', MAX_PACK_EXPRESSIONS);
		if (emojiCount >= expressionLimit) {
			throw new MaxPackExpressionsError(expressionLimit);
		}

		const {animated, imageBuffer, contentType} = await this.avatarService.processEmoji({
			errorPath: 'image',
			base64Image: params.image,
		});

		const emojiId = createEmojiID(await this.snowflakeService.generate());
		await this.avatarService.uploadEmoji({
			prefix: 'emojis',
			emojiId,
			imageBuffer,
			contentType,
		});

		const emoji = await this.guildRepository.upsertEmoji({
			guild_id: pack.id,
			emoji_id: emojiId,
			name: params.name,
			creator_id: params.user.id,
			animated,
			version: 1,
		});

		return mapGuildEmojiToResponse(emoji);
	}

	async bulkCreatePackEmojis(params: {
		user: User;
		packId: GuildID;
		emojis: Array<{name: string; image: string}>;
	}): Promise<{success: Array<GuildEmojiResponse>; failed: Array<{name: string; error: string}>}> {
		await this.requireExpressionPackAccess(params.user.id);
		await this.requireFeature(params.user.id, 'feature_global_expressions');
		const pack = await this.ensurePackOwner(params.user.id, params.packId);
		if (pack.type !== 'emoji') {
			throw new InvalidPackTypeError('emoji');
		}

		let emojiCount = await this.guildRepository.countEmojis(pack.id);
		const expressionLimit = this.resolveLimitForUser(params.user, 'max_pack_expressions', MAX_PACK_EXPRESSIONS);
		const success: Array<GuildEmojiResponse> = [];
		const failed: Array<{name: string; error: string}> = [];

		for (const emojiData of params.emojis) {
			if (emojiCount >= expressionLimit) {
				failed.push({name: emojiData.name, error: 'Pack expression limit reached'});
				continue;
			}

			try {
				const {animated, imageBuffer, contentType} = await this.avatarService.processEmoji({
					errorPath: `emojis[${success.length + failed.length}].image`,
					base64Image: emojiData.image,
				});

				const emojiId = createEmojiID(await this.snowflakeService.generate());
				await this.avatarService.uploadEmoji({
					prefix: 'emojis',
					emojiId,
					imageBuffer,
					contentType,
				});

				const emoji = await this.guildRepository.upsertEmoji({
					guild_id: pack.id,
					emoji_id: emojiId,
					name: emojiData.name,
					creator_id: params.user.id,
					animated,
					version: 1,
				});

				success.push(mapGuildEmojiToResponse(emoji));
				emojiCount += 1;
			} catch (error) {
				const message = error instanceof Error ? error.message : 'Unknown error';
				failed.push({name: emojiData.name, error: message});
			}
		}

		return {success, failed};
	}

	async updatePackEmoji(params: {
		userId: UserID;
		packId: GuildID;
		emojiId: EmojiID;
		name: string;
	}): Promise<GuildEmojiResponse> {
		await this.requireExpressionPackAccess(params.userId);
		const pack = await this.ensurePackOwner(params.userId, params.packId);
		if (pack.type !== 'emoji') {
			throw new InvalidPackTypeError('emoji');
		}

		const emoji = await this.guildRepository.getEmoji(params.emojiId, pack.id);
		if (!emoji) {
			throw new UnknownGuildEmojiError();
		}

		const updatedEmoji = await this.guildRepository.upsertEmoji({
			...emoji.toRow(),
			name: params.name,
		});

		return mapGuildEmojiToResponse(updatedEmoji);
	}

	async deletePackEmoji(params: {userId: UserID; packId: GuildID; emojiId: EmojiID; purge?: boolean}): Promise<void> {
		await this.requireExpressionPackAccess(params.userId);
		const pack = await this.ensurePackOwner(params.userId, params.packId);
		if (pack.type !== 'emoji') {
			throw new InvalidPackTypeError('emoji');
		}

		const emoji = await this.guildRepository.getEmoji(params.emojiId, pack.id);
		if (!emoji) {
			throw new UnknownGuildEmojiError();
		}

		await this.guildRepository.deleteEmoji(pack.id, params.emojiId);

		if (params.purge) {
			await this.assetPurger.purgeEmoji(emoji.id.toString());
		}
	}

	async createPackSticker(params: {
		user: User;
		packId: GuildID;
		name: string;
		description?: string | null;
		tags: Array<string>;
		image: string;
	}): Promise<GuildStickerResponse> {
		await this.requireExpressionPackAccess(params.user.id);
		await this.requireFeature(params.user.id, 'feature_global_expressions');
		const pack = await this.ensurePackOwner(params.user.id, params.packId);
		if (pack.type !== 'sticker') {
			throw new InvalidPackTypeError('sticker');
		}

		const stickerCount = await this.guildRepository.countStickers(pack.id);
		const expressionLimit = this.resolveLimitForUser(params.user, 'max_pack_expressions', MAX_PACK_EXPRESSIONS);
		if (stickerCount >= expressionLimit) {
			throw new MaxPackExpressionsError(expressionLimit);
		}

		const {animated, imageBuffer} = await this.avatarService.processSticker({
			errorPath: 'image',
			base64Image: params.image,
		});

		const stickerId = createStickerID(await this.snowflakeService.generate());
		await this.avatarService.uploadSticker({prefix: 'stickers', stickerId, imageBuffer});

		const sticker = await this.guildRepository.upsertSticker({
			guild_id: pack.id,
			sticker_id: stickerId,
			name: params.name,
			description: params.description ?? null,
			tags: params.tags,
			animated,
			creator_id: params.user.id,
			version: 1,
		});

		const response = mapGuildStickerToResponse(sticker);
		getMetricsService().counter({name: 'fluxer.stickers.created', value: 1});
		return response;
	}

	async bulkCreatePackStickers(params: {
		user: User;
		packId: GuildID;
		stickers: Array<{name: string; description?: string | null; tags: Array<string>; image: string}>;
	}): Promise<{success: Array<GuildStickerResponse>; failed: Array<{name: string; error: string}>}> {
		await this.requireExpressionPackAccess(params.user.id);
		await this.requireFeature(params.user.id, 'feature_global_expressions');
		const pack = await this.ensurePackOwner(params.user.id, params.packId);
		if (pack.type !== 'sticker') {
			throw new InvalidPackTypeError('sticker');
		}

		let stickerCount = await this.guildRepository.countStickers(pack.id);
		const expressionLimit = this.resolveLimitForUser(params.user, 'max_pack_expressions', MAX_PACK_EXPRESSIONS);

		const success: Array<GuildStickerResponse> = [];
		const failed: Array<{name: string; error: string}> = [];

		for (const stickerData of params.stickers) {
			if (stickerCount >= expressionLimit) {
				failed.push({name: stickerData.name, error: 'Pack expression limit reached'});
				continue;
			}

			try {
				const {animated, imageBuffer} = await this.avatarService.processSticker({
					errorPath: `stickers[${success.length + failed.length}].image`,
					base64Image: stickerData.image,
				});

				const stickerId = createStickerID(await this.snowflakeService.generate());
				await this.avatarService.uploadSticker({prefix: 'stickers', stickerId, imageBuffer});

				const sticker = await this.guildRepository.upsertSticker({
					guild_id: pack.id,
					sticker_id: stickerId,
					name: stickerData.name,
					description: stickerData.description ?? null,
					tags: stickerData.tags,
					animated,
					creator_id: params.user.id,
					version: 1,
				});

				success.push(mapGuildStickerToResponse(sticker));
				stickerCount += 1;
			} catch (error) {
				const message = error instanceof Error ? error.message : 'Unknown error';
				failed.push({name: stickerData.name, error: message});
			}
		}

		for (const _ of success) {
			getMetricsService().counter({name: 'fluxer.stickers.created', value: 1});
		}

		return {success, failed};
	}

	async updatePackSticker(params: {
		userId: UserID;
		packId: GuildID;
		stickerId: StickerID;
		name: string;
		description?: string | null;
		tags: Array<string>;
	}): Promise<GuildStickerResponse> {
		await this.requireExpressionPackAccess(params.userId);
		const pack = await this.ensurePackOwner(params.userId, params.packId);
		if (pack.type !== 'sticker') {
			throw new InvalidPackTypeError('sticker');
		}

		const sticker = await this.guildRepository.getSticker(params.stickerId, pack.id);
		if (!sticker) {
			throw new UnknownGuildStickerError();
		}

		const updatedSticker = await this.guildRepository.upsertSticker({
			...sticker.toRow(),
			name: params.name,
			description: params.description ?? null,
			tags: params.tags,
		});

		return mapGuildStickerToResponse(updatedSticker);
	}

	async deletePackSticker(params: {
		userId: UserID;
		packId: GuildID;
		stickerId: StickerID;
		purge?: boolean;
	}): Promise<void> {
		await this.requireExpressionPackAccess(params.userId);
		const pack = await this.ensurePackOwner(params.userId, params.packId);
		if (pack.type !== 'sticker') {
			throw new InvalidPackTypeError('sticker');
		}

		const sticker = await this.guildRepository.getSticker(params.stickerId, pack.id);
		if (!sticker) {
			throw new UnknownGuildStickerError();
		}

		await this.guildRepository.deleteSticker(pack.id, params.stickerId);

		if (params.purge) {
			await this.assetPurger.purgeSticker(sticker.id.toString());
		}

		getMetricsService().counter({name: 'fluxer.stickers.deleted', value: 1});
	}

	private resolveLimitForUser(user: User | null, key: LimitKey, fallback: number): number {
		const ctx = createLimitMatchContext({user});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback);
	}
}
