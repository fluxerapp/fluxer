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

import type {ChannelID, MemeID, MessageID, UserID} from '@fluxer/api/src/BrandedTypes';
import {createAttachmentID, createMemeID, userIdToChannelId} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import type {ChannelService} from '@fluxer/api/src/channel/services/ChannelService';
import {makeAttachmentCdnKey, makeAttachmentCdnUrl} from '@fluxer/api/src/channel/services/message/MessageHelpers';
import {mapFavoriteMemeToResponse} from '@fluxer/api/src/favorite_meme/FavoriteMemeModel';
import type {IFavoriteMemeRepository} from '@fluxer/api/src/favorite_meme/IFavoriteMemeRepository';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import type {IMediaService} from '@fluxer/api/src/infrastructure/IMediaService';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import type {IUnfurlerService} from '@fluxer/api/src/infrastructure/IUnfurlerService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import {Logger} from '@fluxer/api/src/Logger';
import type {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import {resolveLimitSafe} from '@fluxer/api/src/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@fluxer/api/src/limits/LimitMatchContextBuilder';
import type {FavoriteMeme} from '@fluxer/api/src/models/FavoriteMeme';
import type {Message} from '@fluxer/api/src/models/Message';
import type {User} from '@fluxer/api/src/models/User';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_FAVORITE_MEME_TAGS, MAX_FAVORITE_MEMES_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MaxFavoriteMemesError} from '@fluxer/errors/src/domains/core/MaxFavoriteMemesError';
import {MediaMetadataError} from '@fluxer/errors/src/domains/core/MediaMetadataError';
import {UnknownFavoriteMemeError} from '@fluxer/errors/src/domains/core/UnknownFavoriteMemeError';
import {normalizeFilename} from '@fluxer/schema/src/primitives/FileValidators';
import mime from 'mime';

export class FavoriteMemeService {
	constructor(
		private readonly favoriteMemeRepository: IFavoriteMemeRepository,
		private readonly channelService: ChannelService,
		private readonly storageService: IStorageService,
		private readonly mediaService: IMediaService,
		private readonly snowflakeService: SnowflakeService,
		private readonly gatewayService: IGatewayService,
		private readonly unfurlerService: IUnfurlerService,
		private readonly limitConfigService: LimitConfigService,
	) {}

	async createFromMessage({
		user,
		channelId,
		messageId,
		attachmentId,
		embedIndex,
		name,
		altText,
		tags,
	}: {
		user: User;
		channelId: ChannelID;
		messageId: MessageID;
		attachmentId?: string;
		embedIndex?: number;
		name: string;
		altText?: string;
		tags?: Array<string>;
	}): Promise<FavoriteMeme> {
		const count = await this.favoriteMemeRepository.count(user.id);
		const fallbackLimit = MAX_FAVORITE_MEMES_NON_PREMIUM;
		const maxMemes = this.resolveUserLimit(user, 'max_favorite_memes', fallbackLimit);

		if (count >= maxMemes) {
			throw new MaxFavoriteMemesError(maxMemes);
		}

		await this.channelService.getChannelAuthenticated({userId: user.id, channelId});
		const message = await this.channelService.getMessage({userId: user.id, channelId, messageId});
		if (!message) {
			throw new UnknownMessageError();
		}

		const media = this.findMediaInMessage(message, attachmentId, embedIndex);
		if (!media) {
			throw InputValidationError.fromCode('media', ValidationErrorCodes.NO_VALID_MEDIA_IN_MESSAGE);
		}

		const todoTags = tags ?? [];
		this.ensureFavoriteMemeTagLimit(user, todoTags);

		const existingMemes = await this.favoriteMemeRepository.findByUserId(user.id);
		if (media.contentHash) {
			const duplicate = existingMemes.find((meme) => meme.contentHash === media.contentHash);
			Logger.debug(
				{
					userId: user.id.toString(),
					contentHash: media.contentHash,
					source: 'pre-metadata',
					duplicate: Boolean(duplicate),
					channelId: channelId.toString(),
					messageId: messageId.toString(),
				},
				'Favorite meme duplicate check (pre-metadata)',
			);
			if (duplicate) {
				throw InputValidationError.fromCode('media', ValidationErrorCodes.MEDIA_ALREADY_IN_FAVORITE_MEMES);
			}
		}

		const metadata = await this.mediaService.getMetadata(
			media.isExternal
				? {
						type: 'external',
						url: media.url,
						with_base64: true,
						isNSFWAllowed: true,
					}
				: {
						type: 's3',
						bucket: Config.s3.buckets.cdn,
						key: media.sourceKey,
						with_base64: true,
						isNSFWAllowed: true,
					},
		);

		if (!metadata) {
			throw new MediaMetadataError(media.isExternal ? 'external URL' : 'CDN');
		}

		const contentHash = media.contentHash ?? metadata.content_hash;
		Logger.debug(
			{
				userId: user.id.toString(),
				contentHash,
				url: media.url,
				source: 'post-metadata',
				duplicate: existingMemes.some((meme) => meme.contentHash === contentHash),
				channelId: channelId.toString(),
				messageId: messageId.toString(),
			},
			'Favorite meme duplicate check (post-metadata)',
		);
		const fileData = Buffer.from(metadata.base64 ?? '', 'base64');
		const updatedMetadata = media.isExternal
			? {
					contentType: metadata.content_type,
					size: metadata.size,
					width: metadata.width,
					height: metadata.height,
					duration: metadata.duration && metadata.duration > 0 ? metadata.duration : null,
				}
			: null;

		const duplicate = existingMemes.find((meme) => meme.contentHash === contentHash);
		if (duplicate) {
			throw InputValidationError.fromCode('media', ValidationErrorCodes.MEDIA_ALREADY_IN_FAVORITE_MEMES);
		}

		const memeId = createMemeID(await this.snowflakeService.generate());
		const userChannelId = userIdToChannelId(user.id);
		const newAttachmentId = createAttachmentID(await this.snowflakeService.generate());
		const storageKey = makeAttachmentCdnKey(userChannelId, newAttachmentId, media.filename);

		await this.storageService.uploadObject({
			bucket: Config.s3.buckets.cdn,
			key: storageKey,
			body: fileData,
			contentType: media.contentType,
		});

		const favoriteMeme = await this.favoriteMemeRepository.create({
			user_id: user.id,
			meme_id: memeId,
			name: name.trim(),
			alt_text: altText?.trim() || media.altText || null,
			tags: todoTags,
			attachment_id: newAttachmentId,
			filename: media.filename,
			content_type: updatedMetadata?.contentType ?? media.contentType,
			content_hash: contentHash,
			size: updatedMetadata ? BigInt(updatedMetadata.size) : media.size,
			width: updatedMetadata?.width ?? media.width,
			height: updatedMetadata?.height ?? media.height,
			duration: updatedMetadata?.duration ?? media.duration,
			is_gifv: media.isGifv,
			klipy_slug: media.klipySlug,
		});

		const responseData = mapFavoriteMemeToResponse(favoriteMeme);
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'FAVORITE_MEME_CREATE',
			data: responseData,
		});

		Logger.debug({userId: user.id, memeId}, 'Created favorite meme');

		return favoriteMeme;
	}

	async createFromUrl({
		user,
		url,
		name,
		altText,
		tags,
		isGifv = false,
		klipySlug,
		tenorSlugId,
	}: {
		user: User;
		url: string;
		name?: string | null;
		altText?: string;
		tags?: Array<string>;
		isGifv?: boolean;
		klipySlug?: string;
		tenorSlugId?: string;
	}): Promise<FavoriteMeme> {
		const count = await this.favoriteMemeRepository.count(user.id);
		const fallbackLimit = MAX_FAVORITE_MEMES_NON_PREMIUM;
		const maxMemes = this.resolveUserLimit(user, 'max_favorite_memes', fallbackLimit);

		if (count >= maxMemes) {
			throw new MaxFavoriteMemesError(maxMemes);
		}

		const urlTags = tags ?? [];
		this.ensureFavoriteMemeTagLimit(user, urlTags);

		const metadata = await this.mediaService.getMetadata({
			type: 'external',
			url,
			with_base64: true,
			isNSFWAllowed: true,
		});

		if (!metadata) {
			throw new MediaMetadataError('URL');
		}

		let contentHash = metadata.content_hash;
		const fileData = Buffer.from(metadata.base64 ?? '', 'base64');
		const normalizedKlipySlug = this.normalizeKlipySlug(klipySlug) ?? this.extractKlipySlugFromUrl(url) ?? undefined;
		const normalizedTenorSlugId =
			this.normalizeTenorSlugId(tenorSlugId) ?? this.extractTenorSlugIdFromUrl(url) ?? undefined;

		if (normalizedKlipySlug) {
			try {
				const klipyUrl = this.buildKlipyGifUrl(normalizedKlipySlug);
				const unfurled = await this.unfurlerService.unfurl(klipyUrl, true);
				if (unfurled.length > 0 && unfurled[0].video?.content_hash) {
					contentHash = unfurled[0].video.content_hash;
					Logger.debug(
						{klipySlug: normalizedKlipySlug, contentHash},
						'Using unfurled video content_hash for KLIPY GIF',
					);
				}
			} catch (error) {
				Logger.warn({error, klipySlug: normalizedKlipySlug}, 'Failed to unfurl KLIPY URL, using original content_hash');
			}
		} else if (normalizedTenorSlugId) {
			try {
				const tenorUrl = this.buildTenorGifUrl(normalizedTenorSlugId);
				const unfurled = await this.unfurlerService.unfurl(tenorUrl, true);
				if (unfurled.length > 0 && unfurled[0].video?.content_hash) {
					contentHash = unfurled[0].video.content_hash;
					Logger.debug(
						{tenorSlugId: normalizedTenorSlugId, contentHash},
						'Using unfurled video content_hash for Tenor GIF',
					);
				}
			} catch (error) {
				Logger.warn(
					{error, tenorSlugId: normalizedTenorSlugId},
					'Failed to unfurl Tenor URL, using original content_hash',
				);
			}
		}

		const existingMemes = await this.favoriteMemeRepository.findByUserId(user.id);
		const duplicate = existingMemes.find((meme) => meme.contentHash === contentHash);
		if (duplicate) {
			throw InputValidationError.fromCode('media', ValidationErrorCodes.MEDIA_ALREADY_IN_FAVORITE_MEMES);
		}

		const filename = this.buildFilenameFromUrl(url, metadata.content_type);
		const finalName = this.resolveFavoriteMemeName(name, filename);

		const memeId = createMemeID(await this.snowflakeService.generate());
		const userChannelId = userIdToChannelId(user.id);
		const newAttachmentId = createAttachmentID(await this.snowflakeService.generate());
		const storageKey = makeAttachmentCdnKey(userChannelId, newAttachmentId, filename);

		await this.storageService.uploadObject({
			bucket: Config.s3.buckets.cdn,
			key: storageKey,
			body: fileData,
			contentType: metadata.content_type,
		});

		const favoriteMeme = await this.favoriteMemeRepository.create({
			user_id: user.id,
			meme_id: memeId,
			name: finalName,
			alt_text: altText?.trim() || null,
			tags: urlTags,
			attachment_id: newAttachmentId,
			filename,
			content_type: metadata.content_type,
			content_hash: contentHash,
			size: BigInt(metadata.size),
			width: metadata.width || null,
			height: metadata.height || null,
			duration: metadata.duration && metadata.duration > 0 ? metadata.duration : null,
			is_gifv: isGifv,
			klipy_slug: normalizedKlipySlug ?? null,
			tenor_slug_id: normalizedTenorSlugId ?? null,
		});

		const responseData = mapFavoriteMemeToResponse(favoriteMeme);
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'FAVORITE_MEME_CREATE',
			data: responseData,
		});

		Logger.debug({userId: user.id, memeId, url}, 'Created favorite meme from URL');

		return favoriteMeme;
	}

	async update({
		user,
		memeId,
		name,
		altText,
		tags,
	}: {
		user: User;
		memeId: MemeID;
		name?: string;
		altText?: string | null;
		tags?: Array<string>;
	}): Promise<FavoriteMeme> {
		const existingMeme = await this.favoriteMemeRepository.findById(user.id, memeId);
		if (!existingMeme) {
			throw new UnknownFavoriteMemeError();
		}

		const updatedTags = tags ?? existingMeme.tags;
		this.ensureFavoriteMemeTagLimit(user, updatedTags);

		const updatedRow = {
			user_id: user.id,
			meme_id: memeId,
			name: name ?? existingMeme.name,
			alt_text: altText !== undefined ? altText : existingMeme.altText,
			tags: updatedTags,
			attachment_id: existingMeme.attachmentId,
			filename: existingMeme.filename,
			content_type: existingMeme.contentType,
			content_hash: existingMeme.contentHash,
			size: existingMeme.size,
			width: existingMeme.width,
			height: existingMeme.height,
			duration: existingMeme.duration,
			is_gifv: existingMeme.isGifv,
			klipy_slug: existingMeme.klipySlug,
			tenor_slug_id: existingMeme.tenorSlugId,
			version: existingMeme.version,
		};

		const updatedMeme = await this.favoriteMemeRepository.update(user.id, memeId, updatedRow);
		const responseData = mapFavoriteMemeToResponse(updatedMeme);
		await this.gatewayService.dispatchPresence({
			userId: user.id,
			event: 'FAVORITE_MEME_UPDATE',
			data: responseData,
		});

		Logger.debug({userId: user.id, memeId}, 'Updated favorite meme');

		return updatedMeme;
	}

	async delete(userId: UserID, memeId: MemeID): Promise<void> {
		const meme = await this.favoriteMemeRepository.findById(userId, memeId);
		if (!meme) {
			return;
		}

		try {
			await this.storageService.deleteObject(Config.s3.buckets.cdn, meme.storageKey);
		} catch (error) {
			Logger.error({error, userId, memeId}, 'Failed to delete meme from storage');
		}

		await this.favoriteMemeRepository.delete(userId, memeId);

		await this.gatewayService.dispatchPresence({
			userId,
			event: 'FAVORITE_MEME_DELETE',
			data: {meme_id: memeId.toString()},
		});

		Logger.debug({userId, memeId}, 'Deleted favorite meme');
	}

	async getFavoriteMeme(userId: UserID, memeId: MemeID): Promise<FavoriteMeme | null> {
		return this.favoriteMemeRepository.findById(userId, memeId);
	}

	async listFavoriteMemes(userId: UserID): Promise<Array<FavoriteMeme>> {
		return this.favoriteMemeRepository.findByUserId(userId);
	}

	private buildFilenameFromUrl(url: string, contentType: string): string {
		const extension = mime.getExtension(contentType) || 'bin';
		try {
			const urlPath = new URL(url).pathname;
			const rawSegment = urlPath.split('/').pop() || 'media';
			const decoded = decodeURIComponent(rawSegment);
			const base = decoded.includes('.') ? decoded : `${decoded}.${extension}`;
			return normalizeFilename(base) || `media.${extension}`;
		} catch {
			return `media.${extension}`;
		}
	}

	private normalizeKlipySlug(klipySlug?: string): string | undefined {
		if (!klipySlug) {
			return undefined;
		}
		const trimmed = klipySlug.trim();
		if (!trimmed) {
			return undefined;
		}
		return this.extractKlipySlugFromUrl(trimmed) ?? trimmed;
	}

	private extractKlipySlugFromUrl(url: string): string | null {
		try {
			const parsedUrl = new URL(url);
			const hostname = parsedUrl.hostname.toLowerCase();
			if (hostname !== 'klipy.com' && hostname !== 'www.klipy.com') {
				return null;
			}
			const pathMatch = parsedUrl.pathname.match(/^\/(gif|gifs|clip|clips)\/([^/]+)/i);
			if (!pathMatch?.[2]) {
				return null;
			}
			const slug = decodeURIComponent(pathMatch[2]).trim();
			return slug.length > 0 ? slug : null;
		} catch {
			return null;
		}
	}

	private buildKlipyGifUrl(klipySlug: string): string {
		return `https://klipy.com/gifs/${encodeURIComponent(klipySlug)}`;
	}

	private normalizeTenorSlugId(tenorSlugId?: string): string | undefined {
		if (!tenorSlugId) {
			return undefined;
		}

		const trimmed = tenorSlugId.trim();
		if (!trimmed) {
			return undefined;
		}

		const extracted = this.extractTenorSlugIdFromUrl(trimmed);
		if (extracted) {
			return extracted;
		}

		const withoutLeadingSlash = trimmed.startsWith('/') ? trimmed.slice(1) : trimmed;
		if (withoutLeadingSlash.toLowerCase().startsWith('view/')) {
			return withoutLeadingSlash.replace(/\/+$/, '');
		}

		// Allow callers to pass just the last path segment and normalise it.
		if (!withoutLeadingSlash.includes('/')) {
			return `view/${withoutLeadingSlash.replace(/\/+$/, '')}`;
		}

		return undefined;
	}

	private extractTenorSlugIdFromUrl(url: string): string | null {
		try {
			const parsedUrl = new URL(url);
			const hostname = parsedUrl.hostname.toLowerCase();
			if (hostname !== 'tenor.com' && hostname !== 'www.tenor.com') {
				return null;
			}

			const pathMatch = parsedUrl.pathname.match(/^\/view\/([^/]+)/i);
			if (!pathMatch?.[1]) {
				return null;
			}

			const slugId = decodeURIComponent(pathMatch[1]).trim();
			if (!slugId) {
				return null;
			}

			return `view/${slugId}`;
		} catch {
			return null;
		}
	}

	private buildTenorGifUrl(tenorSlugId: string): string {
		const normalized = this.normalizeTenorSlugId(tenorSlugId) ?? tenorSlugId;
		const withoutLeadingSlash = normalized.startsWith('/') ? normalized.slice(1) : normalized;
		return `https://tenor.com/${withoutLeadingSlash}`;
	}

	private resolveFavoriteMemeName(name: string | undefined | null, fallbackFilename: string): string {
		const normalizedInput = typeof name === 'string' ? name.trim() : '';
		const fallbackName = fallbackFilename.trim() || 'favorite meme';
		const candidate = normalizedInput.length > 0 ? normalizedInput : fallbackName;
		const finalName = candidate.slice(0, 100);

		if (finalName.length === 0) {
			throw InputValidationError.fromCode('name', ValidationErrorCodes.FAVORITE_MEME_NAME_REQUIRED);
		}

		return finalName;
	}

	private findMediaInMessage(
		message: Message,
		preferredAttachmentId?: string,
		preferredEmbedIndex?: number,
	): {
		isExternal: boolean;
		url: string;
		sourceKey: string;
		filename: string;
		contentType: string;
		size: bigint;
		width: number | null;
		height: number | null;
		duration: number | null;
		altText: string | null;
		isGifv: boolean;
		contentHash: string | null;
		klipySlug: string | null;
	} | null {
		if (preferredEmbedIndex !== undefined) {
			if (preferredEmbedIndex < 0 || preferredEmbedIndex >= message.embeds.length) {
				throw InputValidationError.fromCode('embed_index', ValidationErrorCodes.EMBED_INDEX_OUT_OF_BOUNDS, {
					embedIndex: preferredEmbedIndex,
					embedCount: message.embeds.length,
				});
			}
			const embed = message.embeds[preferredEmbedIndex];
			const media = embed.image || embed.video || embed.thumbnail;
			if (media?.url) {
				const filename = this.extractFilenameFromUrl(media.url) || `embed_${preferredEmbedIndex}`;
				const contentType = media.contentType ?? mime.getType(filename) ?? 'application/octet-stream';

				if (this.isValidMediaType(contentType)) {
					const isExternal = !this.isInternalCDNUrl(media.url);
					const isGifv = embed.type === 'gifv';
					return {
						isExternal,
						url: media.url,
						sourceKey: isExternal ? '' : this.extractStorageKeyFromUrl(media.url) || '',
						filename,
						contentType,
						size: BigInt(0),
						width: media.width ?? null,
						height: media.height ?? null,
						duration: null,
						altText: null,
						isGifv,
						contentHash: media.contentHash ?? null,
						klipySlug: isGifv ? this.extractKlipySlugFromUrl(media.url) : null,
					};
				}
			}
			return null;
		}

		if (message.attachments.length > 0) {
			let attachment: (typeof message.attachments)[0] | undefined;

			if (preferredAttachmentId) {
				attachment = message.attachments.find((a) => a.id.toString() === preferredAttachmentId);
				if (!attachment) {
					throw InputValidationError.fromCode(
						'preferred_attachment_id',
						ValidationErrorCodes.ATTACHMENT_ID_NOT_FOUND_IN_MESSAGE,
						{attachmentId: preferredAttachmentId},
					);
				}
			} else {
				attachment = message.attachments[0];
			}

			if (attachment && this.isValidMediaType(attachment.contentType)) {
				const isGifv = attachment.contentType === 'image/gif';
				return {
					isExternal: false,
					url: makeAttachmentCdnUrl(message.channelId, attachment.id, attachment.filename),
					sourceKey: makeAttachmentCdnKey(message.channelId, attachment.id, attachment.filename),
					filename: attachment.filename,
					contentType: attachment.contentType,
					size: attachment.size,
					width: attachment.width ?? null,
					height: attachment.height ?? null,
					duration: attachment.duration ?? null,
					altText: attachment.description ?? null,
					isGifv,
					contentHash: attachment.contentHash ?? null,
					klipySlug: null,
				};
			}
		}

		for (const embed of message.embeds) {
			const media = embed.image || embed.video || embed.thumbnail;
			if (media?.url) {
				const filename = this.extractFilenameFromUrl(media.url) || 'media';
				const contentType = media.contentType ?? mime.getType(filename) ?? 'application/octet-stream';

				if (this.isValidMediaType(contentType)) {
					const isExternal = !this.isInternalCDNUrl(media.url);
					const isGifv = embed.type === 'gifv';
					return {
						isExternal,
						url: media.url,
						sourceKey: isExternal ? '' : this.extractStorageKeyFromUrl(media.url) || '',
						filename,
						contentType,
						size: BigInt(0),
						width: media.width ?? null,
						height: media.height ?? null,
						duration: null,
						altText: null,
						isGifv,
						contentHash: media.contentHash ?? null,
						klipySlug: isGifv ? this.extractKlipySlugFromUrl(media.url) : null,
					};
				}
			}
		}

		return null;
	}

	private isInternalCDNUrl(url: string): boolean {
		return url.startsWith(`${Config.endpoints.media}/`);
	}

	private isValidMediaType(contentType: string): boolean {
		return contentType.startsWith('image/') || contentType.startsWith('video/') || contentType.startsWith('audio/');
	}

	private extractFilenameFromUrl(url: string): string | null {
		try {
			const urlObj = new URL(url);
			const rawSegment = urlObj.pathname.split('/').pop();
			if (!rawSegment) {
				return null;
			}
			const decoded = decodeURIComponent(rawSegment);
			return normalizeFilename(decoded) || null;
		} catch {
			return null;
		}
	}

	private extractStorageKeyFromUrl(url: string): string | null {
		try {
			const urlObj = new URL(url);
			return urlObj.pathname.substring(1);
		} catch {
			return null;
		}
	}

	private ensureFavoriteMemeTagLimit(user: User, tags?: Array<string>): void {
		const limit = this.resolveUserLimit(user, 'max_favorite_meme_tags', MAX_FAVORITE_MEME_TAGS);
		if ((tags?.length ?? 0) > limit) {
			throw InputValidationError.create('tags', `Maximum ${limit} tags allowed`);
		}
	}

	private resolveUserLimit(user: User, key: LimitKey, fallback: number): number {
		const ctx = createLimitMatchContext({user});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback);
	}
}
