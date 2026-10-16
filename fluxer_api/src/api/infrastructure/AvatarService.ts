// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {Config} from '@app/api/Config';
import {stripAnimationPrefix} from '@app/api/infrastructure/AssetEntitlementUtils';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {IMediaService} from '@app/api/infrastructure/IMediaService';
import {decodeValidatedImage, resolveAssetSizeLimit} from '@app/api/infrastructure/ImageUploadValidation';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {stripNonJpegImageMetadata} from '@app/api/infrastructure/StorageObjectHelpers';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {bannedAvatarHashCache} from '@app/api/middleware/BannedAvatarHashCache';
import type {AssetKind} from '@fluxer/constants/src/AssetFormatPolicy';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {AVATAR_MAX_SIZE, EMOJI_MAX_SIZE, STICKER_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import sharp from 'sharp';

type ResourceType = 'attachment' | 'avatar' | 'emoji' | 'sticker' | 'banner' | 'other';
type LimitConfigSnapshotProvider = Pick<LimitConfigService, 'getConfigSnapshot'>;

export class AvatarService {
	constructor(
		private storageService: IStorageService,
		private mediaService: IMediaService,
		private limitConfigService: LimitConfigSnapshotProvider,
	) {}

	private prefixToKind(prefix: 'avatars' | 'icons' | 'banners' | 'splashes' | 'emojis' | 'stickers'): AssetKind {
		switch (prefix) {
			case 'avatars':
				return 'avatar';
			case 'icons':
				return 'guild_icon';
			case 'banners':
				return 'banner';
			case 'splashes':
				return 'splash';
			case 'emojis':
				return 'emoji';
			case 'stickers':
				return 'sticker';
		}
	}

	private async stripImageMetadata(buffer: Uint8Array, format: string): Promise<Uint8Array> {
		try {
			const isJpeg = format === 'jpg' || format === 'jpeg';
			if (isJpeg) {
				return await sharp(buffer).jpeg({quality: 100}).toBuffer();
			}
			return await stripNonJpegImageMetadata(buffer);
		} catch (error) {
			Logger.error({error, format}, 'Failed to strip image metadata, using original buffer');
			return buffer;
		}
	}

	async uploadAvatar(params: {
		prefix: 'avatars' | 'icons' | 'banners' | 'splashes';
		entityId?: bigint;
		keyPath?: string;
		errorPath: string;
		previousKey?: string | null;
		base64Image?: string | null;
	}): Promise<string | null> {
		const {prefix, entityId, keyPath, errorPath, previousKey, base64Image} = params;
		const fullKeyPath = keyPath ?? (entityId ? entityId.toString() : '');
		if (!base64Image) {
			if (previousKey) {
				await this.storageService.deleteAvatar({
					prefix,
					key: `${fullKeyPath}/${stripAnimationPrefix(previousKey)}`,
				});
			}
			return null;
		}
		const {imageBuffer, metadata} = await decodeValidatedImage(this.mediaService, {
			base64Image,
			errorPath,
			kind: this.prefixToKind(prefix),
			maxSize: resolveAssetSizeLimit(this.limitConfigService, 'avatar_max_size', AVATAR_MAX_SIZE),
		});
		const imageHash = crypto.createHash('md5').update(Buffer.from(imageBuffer)).digest('hex');
		const imageHashShort = imageHash.slice(0, 8);
		const isAnimatedAvatar = metadata.animated ?? false;
		const storedHash = isAnimatedAvatar ? `a_${imageHashShort}` : imageHashShort;
		if (bannedAvatarHashCache.isBanned(imageHashShort)) {
			throw new ContentBlockedError();
		}
		await this.scanAndBlockBannedSha({
			imageBuffer,
			resourceType: this.getResourceTypeForPrefix(prefix),
		});
		const uploadBuffer = await this.stripImageMetadata(imageBuffer, metadata.format);
		await this.storageService.uploadAvatar({prefix, key: `${fullKeyPath}/${imageHashShort}`, body: uploadBuffer});
		if (previousKey && stripAnimationPrefix(previousKey) !== imageHashShort) {
			await this.storageService.deleteAvatar({
				prefix,
				key: `${fullKeyPath}/${stripAnimationPrefix(previousKey)}`,
			});
		}
		return storedHash;
	}

	async processEmoji(params: {errorPath: string; base64Image: string; guildFeatures: Iterable<string>}): Promise<{
		imageBuffer: Uint8Array;
		animated: boolean;
		format: string;
		contentType: string;
	}> {
		return this.processExpression('emoji', 'emoji_max_size', EMOJI_MAX_SIZE, params);
	}

	private async processExpression(
		kind: 'emoji' | 'sticker',
		limitKey: LimitKey,
		fallback: number,
		params: {errorPath: string; base64Image: string; guildFeatures: Iterable<string>},
	): Promise<{
		imageBuffer: Uint8Array;
		animated: boolean;
		format: string;
		contentType: string;
	}> {
		const {errorPath, base64Image, guildFeatures} = params;
		const {imageBuffer, metadata} = await decodeValidatedImage(this.mediaService, {
			base64Image,
			errorPath,
			kind,
			maxSize: resolveAssetSizeLimit(this.limitConfigService, limitKey, fallback, guildFeatures),
		});
		const animated = metadata.animated ?? false;
		return {
			imageBuffer,
			animated,
			format: metadata.format,
			contentType: metadata.content_type,
		};
	}

	async uploadEmoji(params: {
		prefix: 'emojis';
		emojiId: bigint;
		imageBuffer: Uint8Array;
		contentType?: string | null;
		animated?: boolean;
	}): Promise<void> {
		const {prefix, emojiId, imageBuffer, contentType} = params;
		await this.scanAndBlockBannedSha({
			imageBuffer,
			resourceType: 'emoji',
		});
		const ct = contentType ?? '';
		const format = ct.includes('jpeg') || ct.includes('jpg') ? 'jpeg' : ct.replace('image/', '') || 'png';
		const uploadBuffer = await this.stripImageMetadata(imageBuffer, format);
		await this.storageService.uploadAvatar({prefix, key: emojiId.toString(), body: uploadBuffer});
	}

	async cloneEmojiImage(params: {sourceEmojiId: bigint; emojiId: bigint}): Promise<void> {
		const {sourceEmojiId, emojiId} = params;
		await this.storageService.copyObject({
			sourceBucket: Config.s3.buckets.cdn,
			sourceKey: `emojis/${sourceEmojiId}`,
			destinationBucket: Config.s3.buckets.cdn,
			destinationKey: `emojis/${emojiId}`,
		});
	}

	async copyGuildIconToWebhookAvatar(params: {
		guildId: bigint;
		iconHash: string | null;
		webhookId: bigint;
	}): Promise<string | null> {
		const {guildId, iconHash, webhookId} = params;
		if (!iconHash) return null;
		const key = stripAnimationPrefix(iconHash);
		try {
			await this.storageService.copyObject({
				sourceBucket: Config.s3.buckets.cdn,
				sourceKey: `icons/${guildId}/${key}`,
				destinationBucket: Config.s3.buckets.cdn,
				destinationKey: `avatars/${webhookId}/${key}`,
			});
		} catch (error) {
			if (error instanceof Error && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				return null;
			}
			throw error;
		}
		return iconHash;
	}

	async ensureWebhookAvatarFromGuildIcon(params: {
		guildId: bigint;
		iconHash: string | null;
		webhookId: bigint;
	}): Promise<string | null> {
		const {iconHash, webhookId} = params;
		if (!iconHash) return null;
		const existing = await this.storageService.getObjectMetadata(
			Config.s3.buckets.cdn,
			`avatars/${webhookId}/${stripAnimationPrefix(iconHash)}`,
		);
		if (existing) return iconHash;
		return this.copyGuildIconToWebhookAvatar(params);
	}

	async processSticker(params: {errorPath: string; base64Image: string; guildFeatures: Iterable<string>}): Promise<{
		imageBuffer: Uint8Array;
		animated: boolean;
		format: string;
		contentType: string;
	}> {
		return this.processExpression('sticker', 'sticker_max_size', STICKER_MAX_SIZE, params);
	}

	async uploadSticker(params: {
		prefix: 'stickers';
		stickerId: bigint;
		imageBuffer: Uint8Array;
		contentType?: string | null;
		animated?: boolean;
	}): Promise<void> {
		const {prefix, stickerId, imageBuffer, contentType} = params;
		await this.scanAndBlockBannedSha({
			imageBuffer,
			resourceType: 'sticker',
		});
		const ct = contentType ?? '';
		const format = ct.includes('jpeg') || ct.includes('jpg') ? 'jpeg' : ct.replace('image/', '') || 'png';
		const uploadBuffer = await this.stripImageMetadata(imageBuffer, format);
		await this.storageService.uploadAvatar({prefix, key: stickerId.toString(), body: uploadBuffer});
	}

	async cloneStickerImage(params: {sourceStickerId: bigint; stickerId: bigint}): Promise<void> {
		const {sourceStickerId, stickerId} = params;
		await this.storageService.copyObject({
			sourceBucket: Config.s3.buckets.cdn,
			sourceKey: `stickers/${sourceStickerId}`,
			destinationBucket: Config.s3.buckets.cdn,
			destinationKey: `stickers/${stickerId}`,
		});
	}

	async checkStickerAnimated(stickerId: bigint): Promise<boolean | null> {
		try {
			const metadata = await this.mediaService.getMetadata({
				type: 's3',
				bucket: Config.s3.buckets.cdn,
				key: `stickers/${stickerId}`,
				version: 2,
				nsfw: 'allow',
			});
			return metadata?.animated ?? null;
		} catch (_error) {
			Logger.warn({stickerId}, 'Failed to check sticker animation status');
			return null;
		}
	}

	private async scanAndBlockBannedSha(params: {imageBuffer: Uint8Array; resourceType: ResourceType}): Promise<void> {
		const modCtx = {
			userId: null,
			guildId: null,
			channelId: null,
			messageId: null,
			surface: this.getResourceTypeForPrefix(params.resourceType) as
				| 'avatar'
				| 'banner'
				| 'emoji'
				| 'sticker'
				| 'guild_icon'
				| 'guild_splash'
				| 'guild_banner'
				| 'app_asset',
		};
		contentModerationService.scanFileBuffer(Buffer.from(params.imageBuffer), modCtx);
	}

	private getResourceTypeForPrefix(prefix: string): ResourceType {
		switch (prefix) {
			case 'avatars':
			case 'icons':
				return 'avatar';
			case 'banners':
			case 'splashes':
			case 'embed-splashes':
				return 'banner';
			case 'emojis':
				return 'emoji';
			case 'stickers':
				return 'sticker';
			default:
				return 'other';
		}
	}
}
