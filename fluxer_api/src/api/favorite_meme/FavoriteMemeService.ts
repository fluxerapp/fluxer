// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmbedMediaFlags, MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_FAVORITE_MEME_TAGS, MAX_FAVORITE_MEMES_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MaxFavoriteMemesError} from '@fluxer/errors/src/domains/core/MaxFavoriteMemesError';
import {MediaMetadataError} from '@fluxer/errors/src/domains/core/MediaMetadataError';
import {UnknownFavoriteMemeError} from '@fluxer/errors/src/domains/core/UnknownFavoriteMemeError';
import type {GifMediaFormat} from '@fluxer/schema/src/domains/gif/GifSchemas';
import {normalizeFilename} from '@fluxer/schema/src/primitives/FileValidators';
import mime from 'mime';
import type {ApiContext} from '../ApiContext';
import type {ChannelID, MemeID, MessageID, UserID} from '../BrandedTypes';
import {createAttachmentID, createMemeID, userIdToChannelId} from '../BrandedTypes';
import {Config} from '../Config';
import type {ChannelService} from '../channel/services/ChannelService';
import {makeAttachmentCdnKey, makeAttachmentCdnUrl} from '../channel/services/message/MessageHelpers';
import {
	isOptionalGifProviderError,
	type ResolvedGifProviderSlug,
	tryExtractGifProviderSlug,
} from '../gif/GifProviderUtils';
import type {GifService} from '../gif/GifService';
import type {MediaProxyMetadataResponse} from '../infrastructure/IMediaService';
import type {IStorageService} from '../infrastructure/IStorageService';
import type {IUnfurlerService} from '../infrastructure/IUnfurlerService';
import {Logger} from '../Logger';
import type {LimitConfigService} from '../limits/LimitConfigService';
import {resolveLimitSafe} from '../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../limits/LimitMatchContextBuilder';
import type {FavoriteMeme} from '../models/FavoriteMeme';
import type {Message} from '../models/Message';
import type {User} from '../models/User';
import {mapFavoriteMemeToResponse} from './FavoriteMemeModel';
import type {IFavoriteMemeRepository} from './IFavoriteMemeRepository';

type MessageAttachmentCandidate = Message['attachments'][number];
type MessageEmbedCandidate = Message['embeds'][number];
type FavoriteMemeMedia = {
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
	placeholder: string | null;
	gifSlug: string | null;
	gifProvider: string | null;
};

type ResolvedFavoriteMemeMetadata = {
	contentHash: string;
	contentType: string;
	size: bigint;
	width: number | null;
	height: number | null;
	duration: number | null;
	isGifv: boolean;
	placeholder: string | null;
};

function isAnimatedAttachment(contentType: string, flags: number | undefined): boolean {
	if (contentType === 'image/gif' || contentType === 'image/apng') return true;
	return ((flags ?? 0) & MessageAttachmentFlags.IS_ANIMATED) !== 0;
}

function isAnimatedEmbedMedia(contentType: string | null | undefined, flags: number | undefined): boolean {
	if (contentType === 'image/gif' || contentType === 'image/apng') return true;
	return ((flags ?? 0) & EmbedMediaFlags.IS_ANIMATED) !== 0;
}

function resolveFavoriteMemeAnimationFlag(
	media: Pick<FavoriteMemeMedia, 'isGifv'>,
	metadata: Pick<MediaProxyMetadataResponse, 'animated'> | null | undefined,
): boolean {
	return media.isGifv || metadata?.animated === true;
}

export class FavoriteMemeService {
	constructor(
		private readonly apiContext: ApiContext,
		private readonly favoriteMemeRepository: IFavoriteMemeRepository,
		private readonly channelService: ChannelService,
		private readonly storageService: IStorageService,
		private readonly unfurlerService: IUnfurlerService,
		private readonly limitConfigService: LimitConfigService,
		private readonly gifService: GifService,
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
		await this.channelService.channelData.auth.getChannelAuthenticated({userId: user.id, channelId});
		const message = await this.channelService.messages.retrieval.getMessage({userId: user.id, channelId, messageId});
		if (!message) {
			throw new UnknownMessageError();
		}
		const media = await this.findMediaInMessage(message, attachmentId, embedIndex);
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
		const memeId = createMemeID(await this.apiContext.services.snowflake.generate());
		const userChannelId = userIdToChannelId(user.id);
		const newAttachmentId = createAttachmentID(await this.apiContext.services.snowflake.generate());
		const storageKey = makeAttachmentCdnKey(userChannelId, newAttachmentId, media.filename);
		const resolved = media.isExternal
			? await this.copyExternalMediaToFavoriteMeme({media, storageKey})
			: await this.copyCdnMediaToFavoriteMeme({media, storageKey});
		Logger.debug(
			{
				userId: user.id.toString(),
				contentHash: resolved.contentHash,
				url: media.url,
				source: 'post-metadata',
				duplicate: existingMemes.some((meme) => meme.contentHash === resolved.contentHash),
				channelId: channelId.toString(),
				messageId: messageId.toString(),
			},
			'Favorite meme duplicate check (post-metadata)',
		);
		const duplicate = existingMemes.find((meme) => meme.contentHash === resolved.contentHash);
		if (duplicate) {
			await this.storageService.deleteObject(Config.s3.buckets.cdn, storageKey).catch((error) => {
				Logger.warn({error, storageKey}, 'Failed to clean up favorite meme copy after duplicate detection');
			});
			throw InputValidationError.fromCode('media', ValidationErrorCodes.MEDIA_ALREADY_IN_FAVORITE_MEMES);
		}
		const favoriteMeme = await this.favoriteMemeRepository.create({
			user_id: user.id,
			meme_id: memeId,
			name: name.trim(),
			alt_text: altText?.trim() || media.altText || null,
			tags: todoTags,
			attachment_id: newAttachmentId,
			filename: media.filename,
			content_type: resolved.contentType,
			content_hash: resolved.contentHash,
			size: resolved.size,
			width: resolved.width,
			height: resolved.height,
			duration: resolved.duration,
			is_gifv: resolved.isGifv,
			gif_slug: media.gifSlug,
			gif_provider: media.gifProvider,
			placeholder: resolved.placeholder,
		});
		const responseData = mapFavoriteMemeToResponse(favoriteMeme);
		await this.apiContext.services.gateway.dispatchPresence({
			userId: user.id,
			event: 'FAVORITE_MEME_CREATE',
			data: responseData,
		});
		Logger.debug({userId: user.id, memeId}, 'Created favorite meme');
		return favoriteMeme;
	}

	private async copyCdnMediaToFavoriteMeme({
		media,
		storageKey,
	}: {
		media: FavoriteMemeMedia;
		storageKey: string;
	}): Promise<ResolvedFavoriteMemeMetadata> {
		const metadata = await this.getCdnMediaMetadata(media);
		await this.storageService.copyObject({
			sourceBucket: Config.s3.buckets.cdn,
			sourceKey: media.sourceKey,
			destinationBucket: Config.s3.buckets.cdn,
			destinationKey: storageKey,
		});
		const contentHash =
			media.contentHash ??
			metadata?.content_hash ??
			(await this.storageService.computeObjectSha256(Config.s3.buckets.cdn, media.sourceKey));
		return {
			contentHash,
			contentType: metadata?.content_type ?? media.contentType,
			size: metadata?.size !== undefined ? BigInt(metadata.size) : media.size,
			width: metadata?.width ?? media.width,
			height: metadata?.height ?? media.height,
			duration: metadata?.duration && metadata.duration > 0 ? metadata.duration : media.duration,
			isGifv: resolveFavoriteMemeAnimationFlag(media, metadata),
			placeholder: metadata?.placeholder ?? media.placeholder,
		};
	}

	private async getCdnMediaMetadata(media: FavoriteMemeMedia): Promise<MediaProxyMetadataResponse | null> {
		try {
			return await this.apiContext.services.media.getMetadata({
				type: 's3',
				bucket: Config.s3.buckets.cdn,
				key: media.sourceKey,
				nsfw: 'allow',
			});
		} catch (error) {
			Logger.warn({error, sourceKey: media.sourceKey}, 'Favorite meme media metadata probe failed');
			return null;
		}
	}

	private async copyExternalMediaToFavoriteMeme({
		media,
		storageKey,
	}: {
		media: FavoriteMemeMedia;
		storageKey: string;
	}): Promise<ResolvedFavoriteMemeMetadata> {
		const metadata = await this.apiContext.services.media.getMetadata({
			type: 'external',
			url: media.url,
			with_base64: true,
			nsfw: 'allow',
		});
		if (!metadata) {
			throw new MediaMetadataError('external URL');
		}
		const fileData = Buffer.from(metadata.base64 ?? '', 'base64');
		await this.storageService.uploadObject({
			bucket: Config.s3.buckets.cdn,
			key: storageKey,
			body: fileData,
			contentType: metadata.content_type,
		});
		return {
			contentHash: media.contentHash ?? metadata.content_hash,
			contentType: metadata.content_type,
			size: BigInt(metadata.size),
			width: metadata.width ?? null,
			height: metadata.height ?? null,
			duration: metadata.duration && metadata.duration > 0 ? metadata.duration : null,
			isGifv: resolveFavoriteMemeAnimationFlag(media, metadata),
			placeholder: metadata.placeholder ?? null,
		};
	}

	async createFromUrl({
		user,
		url,
		name,
		altText,
		tags,
		isGifv = false,
		gifSlug,
		gifProvider,
		media,
	}: {
		user: User;
		url: string;
		name?: string | null;
		altText?: string;
		tags?: Array<string>;
		isGifv?: boolean;
		gifSlug?: string;
		gifProvider?: string;
		media?: Record<string, GifMediaFormat> | null;
	}): Promise<FavoriteMeme> {
		const count = await this.favoriteMemeRepository.count(user.id);
		const fallbackLimit = MAX_FAVORITE_MEMES_NON_PREMIUM;
		const maxMemes = this.resolveUserLimit(user, 'max_favorite_memes', fallbackLimit);
		if (count >= maxMemes) {
			throw new MaxFavoriteMemesError(maxMemes);
		}
		const urlTags = tags ?? [];
		this.ensureFavoriteMemeTagLimit(user, urlTags);
		const metadata = await this.apiContext.services.media.getMetadata({
			type: 'external',
			url,
			with_base64: true,
			nsfw: 'allow',
		});
		if (!metadata) {
			throw new MediaMetadataError('URL');
		}
		let contentHash = metadata.content_hash;
		const resolvedGif = await this.resolveGifFromInputs({slug: gifSlug, providerName: gifProvider, url});
		if (resolvedGif) {
			contentHash = await this.resolveProviderGifContentHash(resolvedGif, contentHash);
		}
		const existingMemes = await this.favoriteMemeRepository.findByUserId(user.id);
		const duplicate = existingMemes.find((meme) => meme.contentHash === contentHash);
		if (duplicate) {
			throw InputValidationError.fromCode('media', ValidationErrorCodes.MEDIA_ALREADY_IN_FAVORITE_MEMES);
		}
		const filename = this.buildFilenameFromUrl(url, metadata.content_type);
		const finalName = this.resolveFavoriteMemeName(name, filename);
		const memeId = createMemeID(await this.apiContext.services.snowflake.generate());
		const userChannelId = userIdToChannelId(user.id);
		const newAttachmentId = createAttachmentID(await this.apiContext.services.snowflake.generate());
		const storageKey = makeAttachmentCdnKey(userChannelId, newAttachmentId, filename);
		const fileData = Buffer.from(metadata.base64 ?? '', 'base64');
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
			is_gifv: isGifv || metadata.animated === true,
			gif_slug: resolvedGif?.slug ?? null,
			gif_provider: resolvedGif?.provider.meta.name ?? null,
			media_formats: resolvedGif && media && Object.keys(media).length > 0 ? media : null,
			placeholder: metadata.placeholder ?? null,
		});
		const responseData = mapFavoriteMemeToResponse(favoriteMeme);
		await this.apiContext.services.gateway.dispatchPresence({
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
			gif_slug: existingMeme.gifSlug,
			gif_provider: existingMeme.gifProvider,
			media_formats: existingMeme.mediaFormats,
			placeholder: existingMeme.placeholder,
			version: existingMeme.version,
		};
		const updatedMeme = await this.favoriteMemeRepository.update(user.id, memeId, updatedRow);
		const responseData = mapFavoriteMemeToResponse(updatedMeme);
		await this.apiContext.services.gateway.dispatchPresence({
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
		await this.apiContext.services.gateway.dispatchPresence({
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

	private async resolveGifFromInputs({
		slug,
		providerName,
		url,
	}: {
		slug?: string | null;
		providerName?: string | null;
		url: string;
	}): Promise<ResolvedGifProviderSlug | null> {
		if (slug && providerName) {
			const provider = this.gifService.getByName(providerName);
			const trimmed = slug.trim();
			if (provider && trimmed) {
				const normalized = (await tryExtractGifProviderSlug(provider, trimmed)) ?? trimmed;
				return {provider, slug: normalized};
			}
		}
		const provider = this.gifService.getProvider();
		const extracted = await tryExtractGifProviderSlug(provider, url);
		return extracted ? {provider, slug: extracted} : null;
	}

	private async detectGifFromUrl(url: string): Promise<ResolvedGifProviderSlug | null> {
		return this.resolveGifFromInputs({url});
	}

	private async resolveProviderGifContentHash(
		resolvedGif: ResolvedGifProviderSlug,
		fallbackContentHash: string,
	): Promise<string> {
		try {
			const canonicalUrl = resolvedGif.provider.buildShareUrl(resolvedGif.slug);
			const unfurled = await this.unfurlerService.unfurl(canonicalUrl, 'allow');
			if (unfurled.length > 0 && unfurled[0].video?.content_hash) {
				Logger.debug(
					{
						provider: resolvedGif.provider.meta.name,
						gifSlug: resolvedGif.slug,
						contentHash: unfurled[0].video.content_hash,
					},
					'Using unfurled video content_hash for provider GIF',
				);
				return unfurled[0].video.content_hash;
			}
		} catch (error) {
			if (!isOptionalGifProviderError(error)) {
				throw error;
			}
			Logger.debug({error, provider: resolvedGif.provider.meta.name}, 'Skipping unavailable GIF provider enrichment');
		}
		return fallbackContentHash;
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

	private async findMediaInMessage(
		message: Message,
		preferredAttachmentId?: string,
		preferredEmbedIndex?: number,
	): Promise<FavoriteMemeMedia | null> {
		const attachments = this.getMessageAttachmentCandidates(message);
		const embeds = this.getMessageEmbedCandidates(message);
		if (preferredEmbedIndex !== undefined) {
			if (preferredEmbedIndex < 0 || preferredEmbedIndex >= embeds.length) {
				throw InputValidationError.fromCode('embed_index', ValidationErrorCodes.EMBED_INDEX_OUT_OF_BOUNDS, {
					embedIndex: preferredEmbedIndex,
					embedCount: embeds.length,
				});
			}
			return this.mediaFromEmbed(embeds[preferredEmbedIndex], `embed_${preferredEmbedIndex}`);
		}
		if (attachments.length > 0) {
			let attachment: MessageAttachmentCandidate | undefined;
			if (preferredAttachmentId) {
				attachment = attachments.find((a) => a.id.toString() === preferredAttachmentId);
				if (!attachment) {
					throw InputValidationError.fromCode(
						'attachment_id',
						ValidationErrorCodes.ATTACHMENT_ID_NOT_FOUND_IN_MESSAGE,
						{attachmentId: preferredAttachmentId},
					);
				}
			} else {
				attachment = attachments[0];
			}
			const media = attachment ? this.mediaFromAttachment(message, attachment) : null;
			if (media) {
				return media;
			}
		}
		for (const embed of embeds) {
			const media = await this.mediaFromEmbed(embed, 'media');
			if (media) return media;
		}
		return null;
	}

	private getMessageAttachmentCandidates(message: Message): Array<MessageAttachmentCandidate> {
		return [...message.attachments, ...message.messageSnapshots.flatMap((snapshot) => snapshot.attachments)];
	}

	private getMessageEmbedCandidates(message: Message): Array<MessageEmbedCandidate> {
		return [...message.embeds, ...message.messageSnapshots.flatMap((snapshot) => snapshot.embeds)];
	}

	private mediaFromAttachment(message: Message, attachment: MessageAttachmentCandidate): FavoriteMemeMedia | null {
		if (!this.isValidMediaType(attachment.contentType)) {
			return null;
		}
		const isGifv = isAnimatedAttachment(attachment.contentType, attachment.flags);
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
			placeholder: attachment.placeholder ?? null,
			gifSlug: null,
			gifProvider: null,
		};
	}

	private async mediaFromEmbed(
		embed: MessageEmbedCandidate,
		fallbackFilename: string,
	): Promise<FavoriteMemeMedia | null> {
		const media = embed.image || embed.video || embed.thumbnail;
		if (!media?.url) {
			return null;
		}
		const filename = this.extractFilenameFromUrl(media.url) || fallbackFilename;
		const contentType = media.contentType ?? mime.getType(filename) ?? 'application/octet-stream';
		if (!this.isValidMediaType(contentType)) {
			return null;
		}
		const isExternal = !this.isInternalCDNUrl(media.url);
		const isGifv = embed.type === 'gifv' || isAnimatedEmbedMedia(media.contentType, media.flags);
		const detectedGif = embed.type === 'gifv' ? await this.detectGifFromUrl(media.url) : null;
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
			placeholder: media.placeholder ?? null,
			gifSlug: detectedGif?.slug ?? null,
			gifProvider: detectedGif?.provider.meta.name ?? null,
		};
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
			throw InputValidationError.fromCode('tags', ValidationErrorCodes.MAX_FAVORITE_MEME_TAGS_EXCEEDED, {limit});
		}
	}

	private resolveUserLimit(user: User, key: LimitKey, fallback: number): number {
		const ctx = createLimitMatchContext({user});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback);
	}
}
