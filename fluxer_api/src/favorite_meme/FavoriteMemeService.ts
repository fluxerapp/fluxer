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

import mime from 'mime';
import {
	type ChannelID,
	createAttachmentID,
	createMemeID,
	type MemeID,
	type MessageID,
	type UserID,
	userIdToChannelId,
} from '~/BrandedTypes';
import {Config} from '~/Config';
import {MAX_FAVORITE_MEMES_NON_PREMIUM, MAX_FAVORITE_MEMES_PREMIUM} from '~/Constants';
import type {ChannelService} from '~/channel/services/ChannelService';
import {makeAttachmentCdnKey, makeAttachmentCdnUrl} from '~/channel/services/message/MessageHelpers';
import {
	InputValidationError,
	MaxFavoriteMemesError,
	MediaMetadataError,
	UnknownFavoriteMemeError,
	UnknownMessageError,
} from '~/Errors';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {IUnfurlerService} from '~/infrastructure/IUnfurlerService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {Logger} from '~/Logger';
import {FavoriteMeme, type Message, type User} from '~/Models';
import {mapFavoriteMemeToResponse} from './FavoriteMemeModel';
import type {IFavoriteMemeRepository} from './IFavoriteMemeRepository';

export class FavoriteMemeService {
	constructor(
		private readonly favoriteMemeRepository: IFavoriteMemeRepository,
		private readonly channelService: ChannelService,
		private readonly storageService: IStorageService,
		private readonly mediaService: IMediaService,
		private readonly snowflakeService: SnowflakeService,
		private readonly gatewayService: IGatewayService,
		private readonly unfurlerService: IUnfurlerService,
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
		const maxMemes = user.isPremium() ? MAX_FAVORITE_MEMES_PREMIUM : MAX_FAVORITE_MEMES_NON_PREMIUM;

		if (count >= maxMemes) {
			throw new MaxFavoriteMemesError(user.isPremium());
		}

		await this.channelService.getChannelAuthenticated({userId: user.id, channelId});
		const message = await this.channelService.getMessage({userId: user.id, channelId, messageId});
		if (!message) {
			throw new UnknownMessageError();
		}

		const media = this.findMediaInMessage(message, attachmentId, embedIndex);
		if (!media) {
			throw InputValidationError.create('media', 'No valid media found in message');
		}

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
				throw InputValidationError.create('media', 'This media is already in your favorite memes');
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
			throw InputValidationError.create('media', 'This media is already in your favorite memes');
		}

		const memeId = createMemeID(this.snowflakeService.generate());
		const userChannelId = userIdToChannelId(user.id);
		const newAttachmentId = createAttachmentID(this.snowflakeService.generate());
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
			tags: tags || [],
			attachment_id: newAttachmentId,
			filename: media.filename,
			content_type: updatedMetadata?.contentType ?? media.contentType,
			content_hash: contentHash,
			size: updatedMetadata ? BigInt(updatedMetadata.size) : media.size,
			width: updatedMetadata?.width ?? media.width,
			height: updatedMetadata?.height ?? media.height,
			duration: updatedMetadata?.duration ?? media.duration,
			is_gifv: media.isGifv,
			tenor_id: null,
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
		tenorId,
	}: {
		user: User;
		url: string;
		name?: string | null;
		altText?: string;
		tags?: Array<string>;
		isGifv?: boolean;
		tenorId?: string;
	}): Promise<FavoriteMeme> {
		const count = await this.favoriteMemeRepository.count(user.id);
		const maxMemes = user.isPremium() ? MAX_FAVORITE_MEMES_PREMIUM : MAX_FAVORITE_MEMES_NON_PREMIUM;

		if (count >= maxMemes) {
			throw new MaxFavoriteMemesError(user.isPremium());
		}

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

		if (tenorId) {
			try {
				const tenorUrl = `https://tenor.com/view/${tenorId}`;
				const unfurled = await this.unfurlerService.unfurl(tenorUrl, true);
				if (unfurled.length > 0 && unfurled[0].video?.content_hash) {
					contentHash = unfurled[0].video.content_hash;
					Logger.debug({tenorId, contentHash}, 'Using unfurled video content_hash for Tenor GIF');
				}
			} catch (error) {
				Logger.warn({error, tenorId}, 'Failed to unfurl Tenor URL, using original content_hash');
			}
		}

		const existingMemes = await this.favoriteMemeRepository.findByUserId(user.id);
		const duplicate = existingMemes.find((meme) => meme.contentHash === contentHash);
		if (duplicate) {
			throw InputValidationError.create('media', 'This media is already in your favorite memes');
		}

		const filename = this.buildFilenameFromUrl(url, metadata.content_type);
		const finalName = this.resolveFavoriteMemeName(name, filename);

		const memeId = createMemeID(this.snowflakeService.generate());
		const userChannelId = userIdToChannelId(user.id);
		const newAttachmentId = createAttachmentID(this.snowflakeService.generate());
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
			tags: tags || [],
			attachment_id: newAttachmentId,
			filename,
			content_type: metadata.content_type,
			content_hash: contentHash,
			size: BigInt(metadata.size),
			width: metadata.width || null,
			height: metadata.height || null,
			duration: metadata.duration && metadata.duration > 0 ? metadata.duration : null,
			is_gifv: isGifv,
			tenor_id: tenorId ?? null,
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

		const updatedRow = {
			user_id: user.id,
			meme_id: memeId,
			name: name ?? existingMeme.name,
			alt_text: altText !== undefined ? altText : existingMeme.altText,
			tags: tags ?? existingMeme.tags,
			attachment_id: existingMeme.attachmentId,
			filename: existingMeme.filename,
			content_type: existingMeme.contentType,
			content_hash: existingMeme.contentHash,
			size: existingMeme.size,
			width: existingMeme.width,
			height: existingMeme.height,
			duration: existingMeme.duration,
			is_gifv: existingMeme.isGifv,
			tenor_id_str: existingMeme.tenorId,
			version: existingMeme.version,
		};

		await this.favoriteMemeRepository.update(user.id, memeId, updatedRow);

		const updatedMeme = new FavoriteMeme(updatedRow);
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
			const urlFilename = urlPath.split('/').pop() || 'media';
			return urlFilename.includes('.') ? urlFilename : `${urlFilename}.${extension}`;
		} catch {
			return `media.${extension}`;
		}
	}

	private resolveFavoriteMemeName(name: string | undefined | null, fallbackFilename: string): string {
		const normalizedInput = typeof name === 'string' ? name.trim() : '';
		const fallbackName = fallbackFilename.trim() || 'favorite meme';
		const candidate = normalizedInput.length > 0 ? normalizedInput : fallbackName;
		const finalName = candidate.slice(0, 100);

		if (finalName.length === 0) {
			throw InputValidationError.create('name', 'Favorite meme name is required');
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
	} | null {
		if (preferredEmbedIndex !== undefined) {
			if (preferredEmbedIndex < 0 || preferredEmbedIndex >= message.embeds.length) {
				throw InputValidationError.create(
					'embed_index',
					`Embed index ${preferredEmbedIndex} is out of bounds (message has ${message.embeds.length} embed(s))`,
				);
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
					throw InputValidationError.create(
						'preferred_attachment_id',
						`Attachment with ID ${preferredAttachmentId} not found in message`,
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
			const parts = urlObj.pathname.split('/');
			return parts[parts.length - 1] || null;
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
}
