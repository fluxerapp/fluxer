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

import type {EmojiID, GuildID, StickerID, UserID} from '~/BrandedTypes';
import {createEmojiID, createGuildID, createStickerID} from '~/BrandedTypes';
import {
	MAX_CREATED_PACKS_NON_PREMIUM,
	MAX_CREATED_PACKS_PREMIUM,
	MAX_INSTALLED_PACKS_NON_PREMIUM,
	MAX_INSTALLED_PACKS_PREMIUM,
	MAX_PACK_EXPRESSIONS,
} from '~/Constants';
import {FeatureFlags} from '~/constants/FeatureFlags';
import {FeatureTemporarilyDisabledError} from '~/errors/FeatureTemporarilyDisabledError';
import {InvalidPackTypeError} from '~/errors/InvalidPackTypeError';
import {MaxPackExpressionsError} from '~/errors/MaxPackExpressionsError';
import {MaxPackLimitError} from '~/errors/MaxPackLimitError';
import {PackAccessDeniedError} from '~/errors/PackAccessDeniedError';
import {PremiumRequiredError} from '~/errors/PremiumRequiredError';
import {UnknownGuildEmojiError} from '~/errors/UnknownGuildEmojiError';
import {UnknownGuildStickerError} from '~/errors/UnknownGuildStickerError';
import {UnknownPackError} from '~/errors/UnknownPackError';
import type {FeatureFlagService} from '~/feature_flag/FeatureFlagService';
import {
	type GuildEmojiResponse,
	type GuildEmojiWithUserResponse,
	type GuildStickerResponse,
	type GuildStickerWithUserResponse,
	mapGuildEmojisWithUsersToResponse,
	mapGuildEmojiToResponse,
	mapGuildStickersWithUsersToResponse,
	mapGuildStickerToResponse,
} from '~/guild/GuildModel';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {ExpressionAssetPurger} from '~/guild/services/content/ExpressionAssetPurger';
import type {AvatarService} from '~/infrastructure/AvatarService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {User} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {ExpressionPack} from '~/models/ExpressionPack';
import type {IUserRepository} from '~/user/IUserRepository';
import type {
	PackExpressionAccessResolution,
	PackExpressionAccessResolver,
	PackExpressionAccessResolverParams,
} from './PackExpressionAccessResolver';
import {mapPackToSummary, type PackDashboardResponse, type PackDashboardSection} from './PackModel';
import type {PackRepository, PackType} from './PackRepository';

export class PackService {
	constructor(
		private readonly packRepository: PackRepository,
		private readonly guildRepository: IGuildRepository,
		private readonly avatarService: AvatarService,
		private readonly snowflakeService: SnowflakeService,
		private readonly assetPurger: ExpressionAssetPurger,
		private readonly userRepository: IUserRepository,
		private readonly userCacheService: UserCacheService,
		private readonly featureFlagService?: FeatureFlagService,
	) {}

	private async requireExpressionPackAccess(userId: UserID): Promise<void> {
		if (!this.featureFlagService) {
			return;
		}

		const hasAccess = await this.featureFlagService.isFeatureEnabledForUser(FeatureFlags.EXPRESSION_PACKS, userId, () =>
			this.userRepository.getUserGuildIds(userId),
		);

		if (!hasAccess) {
			throw new FeatureTemporarilyDisabledError();
		}
	}

	private async isPremium(userId: UserID): Promise<boolean> {
		const user = await this.userRepository.findUnique(userId);
		return user?.canUseGlobalExpressions() ?? false;
	}

	private getCreatedLimit(isPremium: boolean): number {
		return isPremium ? MAX_CREATED_PACKS_PREMIUM : MAX_CREATED_PACKS_NON_PREMIUM;
	}

	private getInstalledLimit(isPremium: boolean): number {
		return isPremium ? MAX_INSTALLED_PACKS_PREMIUM : MAX_INSTALLED_PACKS_NON_PREMIUM;
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

	private async requirePremium(userId: UserID, message: string): Promise<void> {
		if (!(await this.isPremium(userId))) {
			throw new PremiumRequiredError(message);
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
		const isPremium = await this.isPremium(userId);
		const createdEmoji = await this.packRepository.listPacksByCreator(userId, 'emoji');
		const createdSticker = await this.packRepository.listPacksByCreator(userId, 'sticker');
		const installations = await this.collectInstalledPacks(userId);

		const installedEmoji = installations
			.filter((entry) => entry.pack.type === 'emoji')
			.map((entry) => mapPackToSummary(entry.pack, entry.installedAt));
		const installedSticker = installations
			.filter((entry) => entry.pack.type === 'sticker')
			.map((entry) => mapPackToSummary(entry.pack, entry.installedAt));

		const emojiSection: PackDashboardSection = {
			installed_limit: this.getInstalledLimit(isPremium),
			created_limit: this.getCreatedLimit(isPremium),
			installed: installedEmoji,
			created: createdEmoji.map((pack) => mapPackToSummary(pack)),
		};

		const stickerSection: PackDashboardSection = {
			installed_limit: this.getInstalledLimit(isPremium),
			created_limit: this.getCreatedLimit(isPremium),
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
		await this.requirePremium(params.user.id, 'Premium required to create emoji or sticker packs');
		const createdCount = await this.packRepository.countPacksByCreator(params.user.id, params.type);
		const limit = this.getCreatedLimit(true);
		if (createdCount >= limit) {
			throw new MaxPackLimitError(params.type, limit, 'create');
		}

		const now = new Date();
		const packId = createGuildID(this.snowflakeService.generate());
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

		await this.requirePremium(userId, 'Premium required to install emoji or sticker packs');
		const limit = this.getInstalledLimit(true);
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
		await this.requirePremium(params.user.id, 'Premium required to add emojis to packs');
		const pack = await this.ensurePackOwner(params.user.id, params.packId);
		if (pack.type !== 'emoji') {
			throw new InvalidPackTypeError('emoji');
		}

		const emojiCount = await this.guildRepository.countEmojis(pack.id);
		if (emojiCount >= MAX_PACK_EXPRESSIONS) {
			throw new MaxPackExpressionsError(MAX_PACK_EXPRESSIONS);
		}

		const {animated, imageBuffer} = await this.avatarService.processEmoji({
			errorPath: 'image',
			base64Image: params.image,
		});

		const emojiId = createEmojiID(this.snowflakeService.generate());
		await this.avatarService.uploadEmoji({prefix: 'emojis', emojiId, imageBuffer});

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
		await this.requirePremium(params.user.id, 'Premium required to add emojis to packs');
		const pack = await this.ensurePackOwner(params.user.id, params.packId);
		if (pack.type !== 'emoji') {
			throw new InvalidPackTypeError('emoji');
		}

		let emojiCount = await this.guildRepository.countEmojis(pack.id);
		const success: Array<GuildEmojiResponse> = [];
		const failed: Array<{name: string; error: string}> = [];

		for (const emojiData of params.emojis) {
			if (emojiCount >= MAX_PACK_EXPRESSIONS) {
				failed.push({name: emojiData.name, error: 'Pack expression limit reached'});
				continue;
			}

			try {
				const {animated, imageBuffer} = await this.avatarService.processEmoji({
					errorPath: `emojis[${success.length + failed.length}].image`,
					base64Image: emojiData.image,
				});

				const emojiId = createEmojiID(this.snowflakeService.generate());
				await this.avatarService.uploadEmoji({prefix: 'emojis', emojiId, imageBuffer});

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

		const emojis = await this.guildRepository.listEmojis(pack.id);
		const emoji = emojis.find((entry) => entry.id === params.emojiId);
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

		const emojis = await this.guildRepository.listEmojis(pack.id);
		const emoji = emojis.find((entry) => entry.id === params.emojiId);
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
		await this.requirePremium(params.user.id, 'Premium required to add stickers to packs');
		const pack = await this.ensurePackOwner(params.user.id, params.packId);
		if (pack.type !== 'sticker') {
			throw new InvalidPackTypeError('sticker');
		}

		const stickerCount = await this.guildRepository.countStickers(pack.id);
		if (stickerCount >= MAX_PACK_EXPRESSIONS) {
			throw new MaxPackExpressionsError(MAX_PACK_EXPRESSIONS);
		}

		const {formatType, imageBuffer} = await this.avatarService.processSticker({
			errorPath: 'image',
			base64Image: params.image,
		});

		const stickerId = createStickerID(this.snowflakeService.generate());
		await this.avatarService.uploadSticker({prefix: 'stickers', stickerId, imageBuffer});

		const sticker = await this.guildRepository.upsertSticker({
			guild_id: pack.id,
			sticker_id: stickerId,
			name: params.name,
			description: params.description ?? null,
			tags: params.tags,
			format_type: formatType,
			creator_id: params.user.id,
			version: 1,
		});

		return mapGuildStickerToResponse(sticker);
	}

	async bulkCreatePackStickers(params: {
		user: User;
		packId: GuildID;
		stickers: Array<{name: string; description?: string | null; tags: Array<string>; image: string}>;
	}): Promise<{success: Array<GuildStickerResponse>; failed: Array<{name: string; error: string}>}> {
		await this.requireExpressionPackAccess(params.user.id);
		await this.requirePremium(params.user.id, 'Premium required to add stickers to packs');
		const pack = await this.ensurePackOwner(params.user.id, params.packId);
		if (pack.type !== 'sticker') {
			throw new InvalidPackTypeError('sticker');
		}

		let stickerCount = await this.guildRepository.countStickers(pack.id);

		const success: Array<GuildStickerResponse> = [];
		const failed: Array<{name: string; error: string}> = [];

		for (const stickerData of params.stickers) {
			if (stickerCount >= MAX_PACK_EXPRESSIONS) {
				failed.push({name: stickerData.name, error: 'Pack expression limit reached'});
				continue;
			}

			try {
				const {formatType, imageBuffer} = await this.avatarService.processSticker({
					errorPath: `stickers[${success.length + failed.length}].image`,
					base64Image: stickerData.image,
				});

				const stickerId = createStickerID(this.snowflakeService.generate());
				await this.avatarService.uploadSticker({prefix: 'stickers', stickerId, imageBuffer});

				const sticker = await this.guildRepository.upsertSticker({
					guild_id: pack.id,
					sticker_id: stickerId,
					name: stickerData.name,
					description: stickerData.description ?? null,
					tags: stickerData.tags,
					format_type: formatType,
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

		const stickers = await this.guildRepository.listStickers(pack.id);
		const sticker = stickers.find((entry) => entry.id === params.stickerId);
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

		const stickers = await this.guildRepository.listStickers(pack.id);
		const sticker = stickers.find((entry) => entry.id === params.stickerId);
		if (!sticker) {
			throw new UnknownGuildStickerError();
		}

		await this.guildRepository.deleteSticker(pack.id, params.stickerId);

		if (params.purge) {
			await this.assetPurger.purgeSticker(sticker.id.toString());
		}
	}
}
