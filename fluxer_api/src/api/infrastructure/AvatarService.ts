// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {
	type AssetKind,
	formatAssetUploadExtensions,
	getPolicy,
	isExtensionAllowed,
} from '@fluxer/constants/src/AssetFormatPolicy';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {AVATAR_MAX_SIZE, EMOJI_MAX_SIZE, STICKER_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ContentBlockedError} from '@fluxer/errors/src/domains/content/ContentBlockedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {resolveLimit} from '@fluxer/limits/src/LimitResolver';
import sharp from 'sharp';
import {Config} from '../Config';
import {Logger} from '../Logger';
import type {LimitConfigService} from '../limits/LimitConfigService';
import {createLimitMatchContext} from '../limits/LimitMatchContextBuilder';
import {bannedAvatarHashCache} from '../middleware/BannedAvatarHashCache';
import {contentModerationService} from './ContentModerationService';
import type {IMediaService, MediaProxyMetadataResponse} from './IMediaService';
import type {IStorageService} from './IStorageService';
import {stripNonJpegImageMetadata} from './StorageObjectHelpers';

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

	private resolveSizeLimit(key: LimitKey, fallback: number, guildFeatures: Iterable<string> | null = null): number {
		const ctx = createLimitMatchContext({user: null, guildFeatures});
		const resolved = resolveLimit(this.limitConfigService.getConfigSnapshot(), ctx, key, {
			evaluationContext: guildFeatures ? 'guild' : 'user',
		});
		if (!Number.isFinite(resolved) || resolved < 0) {
			return fallback;
		}
		return Math.floor(resolved);
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
					key: `${fullKeyPath}/${this.stripAnimationPrefix(previousKey)}`,
				});
			}
			return null;
		}
		const base64Data = base64Image.includes(',') ? base64Image.split(',')[1] : base64Image;
		const imageBuffer = new Uint8Array(Buffer.from(base64Data, 'base64'));
		const maxAvatarSize = this.resolveSizeLimit('avatar_max_size', AVATAR_MAX_SIZE);
		if (imageBuffer.length > maxAvatarSize) {
			throw InputValidationError.fromCode(errorPath, ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT, {
				maxSize: maxAvatarSize,
			});
		}
		const kind = this.prefixToKind(prefix);
		const metadata = this.requireAllowedMetadata({
			metadata: await this.mediaService.getMetadata({
				type: 'base64',
				base64: base64Data,
				version: 2,
				nsfw: 'allow',
			}),
			kind,
			errorPath,
		});
		const imageHash = crypto.createHash('md5').update(Buffer.from(imageBuffer)).digest('hex');
		const imageHashShort = imageHash.slice(0, 8);
		const isAnimatedAvatar = metadata.animated ?? false;
		const storedHash = isAnimatedAvatar ? `a_${imageHashShort}` : imageHashShort;
		if (bannedAvatarHashCache.contains(imageHashShort)) {
			throw new ContentBlockedError();
		}
		await this.scanAndBlockBannedSha({
			imageBuffer,
			resourceType: this.getResourceTypeForPrefix(prefix),
		});
		const uploadBuffer = await this.stripImageMetadata(imageBuffer, metadata.format);
		await this.storageService.uploadAvatar({prefix, key: `${fullKeyPath}/${imageHashShort}`, body: uploadBuffer});
		if (previousKey && this.stripAnimationPrefix(previousKey) !== imageHashShort) {
			await this.storageService.deleteAvatar({
				prefix,
				key: `${fullKeyPath}/${this.stripAnimationPrefix(previousKey)}`,
			});
		}
		return storedHash;
	}

	async uploadAvatarToPath(params: {
		bucket: string;
		keyPath: string;
		errorPath: string;
		previousKey?: string | null;
		base64Image?: string | null;
	}): Promise<string | null> {
		const {bucket, keyPath, errorPath, previousKey, base64Image} = params;
		const stripAnimationPrefix = (key: string) => (key.startsWith('a_') ? key.substring(2) : key);
		if (!base64Image) {
			if (previousKey) {
				await this.storageService.deleteObject(bucket, `${keyPath}/${stripAnimationPrefix(previousKey)}`);
			}
			return null;
		}
		const base64Data = base64Image.includes(',') ? base64Image.split(',')[1] : base64Image;
		const imageBuffer = new Uint8Array(Buffer.from(base64Data, 'base64'));
		const maxAvatarSize = this.resolveSizeLimit('avatar_max_size', AVATAR_MAX_SIZE);
		if (imageBuffer.length > maxAvatarSize) {
			throw InputValidationError.fromCode(errorPath, ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT, {
				maxSize: maxAvatarSize,
			});
		}
		const metadata = this.requireAllowedMetadata({
			metadata: await this.mediaService.getMetadata({
				type: 'base64',
				base64: base64Data,
				version: 2,
				nsfw: 'allow',
			}),
			kind: 'avatar',
			errorPath,
		});
		const imageHash = crypto.createHash('md5').update(Buffer.from(imageBuffer)).digest('hex');
		const imageHashShort = imageHash.slice(0, 8);
		const isAnimatedAvatar = metadata.animated ?? false;
		const storedHash = isAnimatedAvatar ? `a_${imageHashShort}` : imageHashShort;
		if (bannedAvatarHashCache.contains(imageHashShort)) {
			throw new ContentBlockedError();
		}
		await this.scanAndBlockBannedSha({
			imageBuffer,
			resourceType: 'other',
		});
		const uploadBuffer = await this.stripImageMetadata(imageBuffer, metadata.format);
		await this.storageService.uploadObject({
			bucket,
			key: `${keyPath}/${imageHashShort}`,
			body: uploadBuffer,
		});
		if (previousKey && stripAnimationPrefix(previousKey) !== imageHashShort) {
			await this.storageService.deleteObject(bucket, `${keyPath}/${stripAnimationPrefix(previousKey)}`);
		}
		return storedHash;
	}

	async processEmoji(params: {errorPath: string; base64Image: string; guildFeatures: Iterable<string>}): Promise<{
		imageBuffer: Uint8Array;
		animated: boolean;
		format: string;
		contentType: string;
	}> {
		const {errorPath, base64Image, guildFeatures} = params;
		const base64Data = base64Image.includes(',') ? base64Image.split(',')[1] : base64Image;
		const imageBuffer = new Uint8Array(Buffer.from(base64Data, 'base64'));
		const maxEmojiSize = this.resolveSizeLimit('emoji_max_size', EMOJI_MAX_SIZE, guildFeatures);
		if (imageBuffer.length > maxEmojiSize) {
			throw InputValidationError.fromCode(errorPath, ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT, {
				maxSize: maxEmojiSize,
			});
		}
		const metadata = this.requireAllowedMetadata({
			metadata: await this.mediaService.getMetadata({
				type: 'base64',
				base64: base64Data,
				version: 2,
				nsfw: 'allow',
			}),
			kind: 'emoji',
			errorPath,
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

	async processSticker(params: {errorPath: string; base64Image: string; guildFeatures: Iterable<string>}): Promise<{
		imageBuffer: Uint8Array;
		animated: boolean;
		format: string;
		contentType: string;
	}> {
		const {errorPath, base64Image, guildFeatures} = params;
		const base64Data = base64Image.includes(',') ? base64Image.split(',')[1] : base64Image;
		const imageBuffer = new Uint8Array(Buffer.from(base64Data, 'base64'));
		const maxStickerSize = this.resolveSizeLimit('sticker_max_size', STICKER_MAX_SIZE, guildFeatures);
		if (imageBuffer.length > maxStickerSize) {
			throw InputValidationError.fromCode(errorPath, ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT, {
				maxSize: maxStickerSize,
			});
		}
		const metadata = this.requireAllowedMetadata({
			metadata: await this.mediaService.getMetadata({
				type: 'base64',
				base64: base64Data,
				version: 2,
				nsfw: 'allow',
			}),
			kind: 'sticker',
			errorPath,
		});
		const animated = metadata.animated ?? false;
		return {
			imageBuffer,
			animated,
			format: metadata.format,
			contentType: metadata.content_type,
		};
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

	private stripAnimationPrefix(hash: string): string {
		return hash.startsWith('a_') ? hash.substring(2) : hash;
	}

	private requireAllowedMetadata(params: {
		metadata: MediaProxyMetadataResponse | null;
		kind: AssetKind;
		errorPath: string;
	}): MediaProxyMetadataResponse {
		const {metadata, kind, errorPath} = params;
		if (metadata == null || !isExtensionAllowed(kind, metadata.format)) {
			this.throwInvalidImageFormat(errorPath, kind);
		}
		const animated = metadata.animated ?? false;
		if (!animated) return metadata;
		if (getPolicy(kind).animated === 'never' || this.isAnimatedAvif(metadata)) {
			this.throwInvalidImageFormat(errorPath, kind);
		}
		return metadata;
	}

	private isAnimatedAvif(metadata: MediaProxyMetadataResponse): boolean {
		const format = metadata.format.toLowerCase();
		const contentType = metadata.content_type.toLowerCase();
		return format === 'avif' || contentType === 'image/avif' || contentType === 'image/avif-sequence';
	}

	private throwInvalidImageFormat(errorPath: string, kind: AssetKind): never {
		throw InputValidationError.fromCode(errorPath, ValidationErrorCodes.INVALID_IMAGE_FORMAT, {
			supportedExtensions: this.formatSupportedExtensions(kind),
		});
	}

	private formatSupportedExtensions(kind: AssetKind): string {
		return formatAssetUploadExtensions(kind, {labelStyle: 'extension'});
	}
}
