// SPDX-License-Identifier: AGPL-3.0-or-later

import {S3ServiceException} from '@aws-sdk/client-s3';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {ChannelID, UserID} from '../../../BrandedTypes';
import {createAttachmentID, createChannelID, createMemeID, createMessageID} from '../../../BrandedTypes';
import {Config} from '../../../Config';
import type {MessageAttachment} from '../../../database/types/MessageTypes';
import type {IFavoriteMemeRepository} from '../../../favorite_meme/IFavoriteMemeRepository';
import type {IMediaService} from '../../../infrastructure/IMediaService';
import type {ISnowflakeService} from '../../../infrastructure/ISnowflakeService';
import type {IStorageService} from '../../../infrastructure/IStorageService';
import {Logger} from '../../../Logger';
import type {FavoriteMeme} from '../../../models/FavoriteMeme';
import type {Message} from '../../../models/Message';
import type {User} from '../../../models/User';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import {makeAttachmentCdnKey} from './MessageHelpers';

interface MessageOperationsHelpersDeps {
	channelRepository: IChannelRepositoryAggregate;
	cacheService: ICacheService;
	storageService: IStorageService;
	snowflakeService: ISnowflakeService;
	favoriteMemeRepository: IFavoriteMemeRepository;
	mediaService: IMediaService;
}

const ANIMATION_PROBE_CONTENT_TYPES = new Set(['image/webp', 'image/apng', 'image/avif', 'image/png']);

export class MessageOperationsHelpers {
	constructor(private readonly deps: MessageOperationsHelpersDeps) {}

	async findExistingMessage({
		userId,
		nonce,
		expectedChannelId,
	}: {
		userId: UserID;
		nonce?: string;
		expectedChannelId: ChannelID;
	}): Promise<Message | null> {
		if (!nonce) return null;
		const existingNonce = await this.deps.cacheService.get<{
			channel_id: string;
			message_id: string;
		}>(`message-nonce:${userId}:${nonce}`);
		if (!existingNonce) return null;
		const cachedChannelId = createChannelID(BigInt(existingNonce.channel_id));
		if (cachedChannelId !== expectedChannelId) {
			throw new UnknownMessageError();
		}
		return this.deps.channelRepository.messages.getMessage(
			cachedChannelId,
			createMessageID(BigInt(existingNonce.message_id)),
		);
	}

	async processFavoriteMeme({
		user,
		channelId,
		favoriteMemeId,
	}: {
		user: User;
		channelId: ChannelID;
		favoriteMemeId: bigint;
	}): Promise<MessageAttachment> {
		const memeId = createMemeID(favoriteMemeId);
		const favoriteMeme = await this.deps.favoriteMemeRepository.findById(user.id, memeId);
		if (!favoriteMeme) {
			throw InputValidationError.fromCode('favorite_meme_id', ValidationErrorCodes.FAVORITE_MEME_NOT_FOUND);
		}
		const memeAttachmentId = createAttachmentID(await this.deps.snowflakeService.generate());
		const sourceKey = favoriteMeme.storageKey;
		const destKey = makeAttachmentCdnKey(channelId, memeAttachmentId, favoriteMeme.filename);
		try {
			await this.deps.storageService.copyObject({
				sourceBucket: Config.s3.buckets.cdn,
				sourceKey,
				destinationBucket: Config.s3.buckets.cdn,
				destinationKey: destKey,
				newContentType: favoriteMeme.contentType,
			});
		} catch (error) {
			if (error instanceof S3ServiceException && (error.name === 'NoSuchKey' || error.name === 'NotFound')) {
				throw InputValidationError.fromCode('favorite_meme_id', ValidationErrorCodes.FAVORITE_MEME_NOT_FOUND);
			}
			throw error;
		}
		const needsAnimationProbe =
			!favoriteMeme.isGifv &&
			favoriteMeme.contentType !== 'image/gif' &&
			favoriteMeme.contentType !== 'image/apng' &&
			ANIMATION_PROBE_CONTENT_TYPES.has(favoriteMeme.contentType);
		const metadata =
			needsAnimationProbe || favoriteMeme.placeholder == null
				? await this.probeFavoriteMemeMetadata(favoriteMeme)
				: null;
		const placeholder = favoriteMeme.placeholder ?? metadata?.placeholder ?? null;
		if (placeholder != null && favoriteMeme.placeholder == null) {
			await this.repairFavoriteMemePlaceholder(favoriteMeme, placeholder);
		}
		let flags = 0;
		if (this.isFavoriteMemeAnimated(favoriteMeme, metadata)) {
			flags |= MessageAttachmentFlags.IS_ANIMATED;
		}
		return {
			attachment_id: memeAttachmentId,
			filename: favoriteMeme.filename,
			size: favoriteMeme.size,
			title: null,
			description: favoriteMeme.altText,
			width: favoriteMeme.width,
			height: favoriteMeme.height,
			content_type: favoriteMeme.contentType,
			content_hash: favoriteMeme.contentHash,
			placeholder,
			flags,
			duration: favoriteMeme.duration,
			nsfw: null,
			waveform: null,
		};
	}

	private async probeFavoriteMemeMetadata(favoriteMeme: FavoriteMeme) {
		try {
			return await this.deps.mediaService.getMetadata({
				type: 's3',
				bucket: Config.s3.buckets.cdn,
				key: favoriteMeme.storageKey,
				nsfw: 'allow',
			});
		} catch {
			return null;
		}
	}

	private async repairFavoriteMemePlaceholder(favoriteMeme: FavoriteMeme, placeholder: string): Promise<void> {
		try {
			await this.deps.favoriteMemeRepository.updatePlaceholder(favoriteMeme.userId, favoriteMeme.id, placeholder);
		} catch (error) {
			Logger.warn({error, memeId: favoriteMeme.id.toString()}, 'Failed to backfill favorite meme placeholder');
		}
	}

	private isFavoriteMemeAnimated(favoriteMeme: FavoriteMeme, metadata: {animated?: boolean | null} | null): boolean {
		if (favoriteMeme.isGifv) return true;
		if (favoriteMeme.contentType === 'image/gif' || favoriteMeme.contentType === 'image/apng') return true;
		if (!ANIMATION_PROBE_CONTENT_TYPES.has(favoriteMeme.contentType)) return false;
		return metadata?.animated === true;
	}
}
