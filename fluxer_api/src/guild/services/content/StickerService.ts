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

import {createStickerID, type GuildID, type StickerID, type UserID} from '~/BrandedTypes';
import {GuildFeatures, MAX_GUILD_STICKERS, MAX_GUILD_STICKERS_MORE_STICKERS} from '~/Constants';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {MaxGuildStickersStaticError, MissingAccessError, UnknownGuildStickerError} from '~/Errors';
import {
	type GuildStickerResponse,
	type GuildStickerWithUserResponse,
	mapGuildStickersWithUsersToResponse,
	mapGuildStickerToResponse,
} from '~/guild/GuildModel';
import type {AvatarService} from '~/infrastructure/AvatarService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {GuildSticker, User} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {getCachedUserPartialResponse} from '~/user/UserCacheHelpers';
import type {UserPartialResponse} from '~/user/UserModel';
import type {IGuildRepository} from '../../IGuildRepository';
import type {ContentHelpers} from './ContentHelpers';
import type {ExpressionAssetPurger} from './ExpressionAssetPurger';

export class StickerService {
	constructor(
		private readonly guildRepository: IGuildRepository,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		private readonly avatarService: AvatarService,
		private readonly snowflakeService: SnowflakeService,
		private readonly contentHelpers: ContentHelpers,
		private readonly assetPurger: ExpressionAssetPurger,
	) {}

	async getStickers(params: {
		userId: UserID;
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<Array<GuildStickerWithUserResponse>> {
		const {userId, guildId, requestCache} = params;
		await this.contentHelpers.getGuildData({userId, guildId});

		const stickers = await this.guildRepository.listStickers(guildId);
		return await mapGuildStickersWithUsersToResponse(stickers, this.userCacheService, requestCache);
	}

	async getStickerUser(params: {
		userId: UserID;
		guildId: GuildID;
		stickerId: StickerID;
		requestCache: RequestCache;
	}): Promise<UserPartialResponse> {
		const {userId, guildId, stickerId, requestCache} = params;
		await this.contentHelpers.getGuildData({userId, guildId});

		const sticker = await this.guildRepository.getSticker(stickerId, guildId);
		if (!sticker) throw new UnknownGuildStickerError();

		const userPartial = await getCachedUserPartialResponse({
			userId: sticker.creatorId,
			userCacheService: this.userCacheService,
			requestCache,
		});
		return userPartial;
	}

	async createSticker(
		params: {
			user: User;
			guildId: GuildID;
			name: string;
			description?: string | null;
			tags: Array<string>;
			image: string;
		},
		auditLogReason?: string | null,
	): Promise<GuildStickerResponse> {
		const {user, guildId, name, description, tags, image} = params;
		const guildData = await this.contentHelpers.getGuildData({userId: user.id, guildId});
		await this.contentHelpers.checkCreateExpressionsPermission({userId: user.id, guildId});

		const allStickers = await this.guildRepository.listStickers(guildId);
		const stickerCount = allStickers.length;

		const {formatType, imageBuffer} = await this.avatarService.processSticker({errorPath: 'image', base64Image: image});

		const hasUnlimitedStickers = guildData.features.includes(GuildFeatures.UNLIMITED_STICKERS);

		if (!hasUnlimitedStickers) {
			const hasMoreStickers = guildData.features.includes(GuildFeatures.MORE_STICKERS);
			const maxStickers = hasMoreStickers ? MAX_GUILD_STICKERS_MORE_STICKERS : MAX_GUILD_STICKERS;

			if (stickerCount >= maxStickers) {
				throw new MaxGuildStickersStaticError(maxStickers);
			}
		}

		const stickerId = createStickerID(this.snowflakeService.generate());
		await this.avatarService.uploadSticker({prefix: 'stickers', stickerId, imageBuffer});

		const sticker = await this.guildRepository.upsertSticker({
			guild_id: guildId,
			sticker_id: stickerId,
			name,
			description: description ?? null,
			format_type: formatType,
			tags,
			creator_id: user.id,
			version: 1,
		});

		const updatedStickers = [...allStickers, sticker];
		await this.dispatchGuildStickersUpdate({guildId, stickers: updatedStickers});

		await this.contentHelpers.recordAuditLog({
			guildId,
			userId: user.id,
			action: AuditLogActionType.STICKER_CREATE,
			targetId: sticker.id,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(
				null,
				this.contentHelpers.serializeStickerForAudit(sticker),
			),
		});

		return mapGuildStickerToResponse(sticker);
	}

	async bulkCreateStickers(
		params: {
			user: User;
			guildId: GuildID;
			stickers: Array<{name: string; description?: string | null; tags: Array<string>; image: string}>;
		},
		auditLogReason?: string | null,
	): Promise<{
		success: Array<GuildStickerResponse>;
		failed: Array<{name: string; error: string}>;
	}> {
		const {user, guildId, stickers} = params;
		const guildData = await this.contentHelpers.getGuildData({userId: user.id, guildId});
		await this.contentHelpers.checkCreateExpressionsPermission({userId: user.id, guildId});

		const allStickers = await this.guildRepository.listStickers(guildId);

		const hasUnlimitedStickers = guildData.features.includes(GuildFeatures.UNLIMITED_STICKERS);
		const hasMoreStickers = guildData.features.includes(GuildFeatures.MORE_STICKERS);
		const maxStickers = hasUnlimitedStickers
			? Infinity
			: hasMoreStickers
				? MAX_GUILD_STICKERS_MORE_STICKERS
				: MAX_GUILD_STICKERS;

		let stickerCount = allStickers.length;

		const success: Array<GuildStickerResponse> = [];
		const failed: Array<{name: string; error: string}> = [];
		const newStickers: Array<GuildSticker> = [];

		for (const stickerData of stickers) {
			try {
				const {formatType, imageBuffer} = await this.avatarService.processSticker({
					errorPath: `stickers[${success.length + failed.length}].image`,
					base64Image: stickerData.image,
				});

				if (stickerCount >= maxStickers) {
					failed.push({name: stickerData.name, error: 'Maximum stickers reached'});
					continue;
				}

				const stickerId = createStickerID(this.snowflakeService.generate());
				await this.avatarService.uploadSticker({prefix: 'stickers', stickerId, imageBuffer});

				const sticker = await this.guildRepository.upsertSticker({
					guild_id: guildId,
					sticker_id: stickerId,
					name: stickerData.name,
					description: stickerData.description ?? null,
					tags: stickerData.tags,
					format_type: formatType,
					creator_id: user.id,
					version: 1,
				});

				stickerCount++;

				newStickers.push(sticker);
				success.push(mapGuildStickerToResponse(sticker));
			} catch (error) {
				const errorMessage = error instanceof Error ? error.message : 'Unknown error';
				failed.push({name: stickerData.name, error: errorMessage});
			}
		}

		if (newStickers.length > 0) {
			const updatedStickers = [...allStickers, ...newStickers];
			await this.dispatchGuildStickersUpdate({guildId, stickers: updatedStickers});

			await Promise.all(
				newStickers.map((sticker) =>
					this.contentHelpers.recordAuditLog({
						guildId,
						userId: user.id,
						action: AuditLogActionType.STICKER_CREATE,
						targetId: sticker.id,
						auditLogReason: auditLogReason ?? null,
						changes: this.contentHelpers.guildAuditLogService.computeChanges(
							null,
							this.contentHelpers.serializeStickerForAudit(sticker),
						),
					}),
				),
			);
		}

		return {success, failed};
	}

	async updateSticker(
		params: {
			userId: UserID;
			guildId: GuildID;
			stickerId: StickerID;
			name: string;
			description?: string | null;
			tags: Array<string>;
		},
		auditLogReason?: string | null,
	): Promise<GuildStickerResponse> {
		const {userId, guildId, stickerId, name, description, tags} = params;

		const allStickers = await this.guildRepository.listStickers(guildId);
		const sticker = allStickers.find((e) => e.id === stickerId);
		if (!sticker) throw new UnknownGuildStickerError();

		await this.contentHelpers.checkModifyExpressionPermission({userId, guildId, creatorId: sticker.creatorId});
		const previousSnapshot = this.contentHelpers.serializeStickerForAudit(sticker);

		const updatedSticker = await this.guildRepository.upsertSticker({
			...sticker.toRow(),
			name,
			description: description ?? null,
			tags,
		});
		const updatedStickers = allStickers.map((e) => (e.id === stickerId ? updatedSticker : e));
		await this.dispatchGuildStickersUpdate({guildId, stickers: updatedStickers});

		await this.contentHelpers.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.STICKER_UPDATE,
			targetId: stickerId,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(
				previousSnapshot,
				this.contentHelpers.serializeStickerForAudit(updatedSticker),
			),
		});

		return mapGuildStickerToResponse(updatedSticker);
	}

	async deleteSticker(
		params: {userId: UserID; guildId: GuildID; stickerId: StickerID; purge?: boolean},
		auditLogReason?: string | null,
	): Promise<void> {
		const {userId, guildId, stickerId, purge = false} = params;
		const guildData = await this.contentHelpers.getGuildData({userId, guildId});

		if (purge && !guildData.features.includes(GuildFeatures.EXPRESSION_PURGE_ALLOWED)) {
			throw new MissingAccessError();
		}

		const allStickers = await this.guildRepository.listStickers(guildId);
		const sticker = allStickers.find((e) => e.id === stickerId);
		if (!sticker) throw new UnknownGuildStickerError();

		await this.contentHelpers.checkModifyExpressionPermission({userId, guildId, creatorId: sticker.creatorId});
		const previousSnapshot = this.contentHelpers.serializeStickerForAudit(sticker);

		await this.guildRepository.deleteSticker(guildId, stickerId);
		const updatedStickers = allStickers.filter((e) => e.id !== stickerId);
		await this.dispatchGuildStickersUpdate({guildId, stickers: updatedStickers});

		if (purge) {
			await this.assetPurger.purgeSticker(sticker.id.toString());
		}

		await this.contentHelpers.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.STICKER_DELETE,
			targetId: stickerId,
			auditLogReason: auditLogReason ?? null,
			changes: this.contentHelpers.guildAuditLogService.computeChanges(previousSnapshot, null),
		});
	}

	private async dispatchGuildStickersUpdate(params: {guildId: GuildID; stickers: Array<GuildSticker>}): Promise<void> {
		const {guildId, stickers} = params;
		await this.gatewayService.dispatchGuild({
			guildId,
			event: 'GUILD_STICKERS_UPDATE',
			data: {stickers: stickers.map(mapGuildStickerToResponse)},
		});
	}
}
