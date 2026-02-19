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

import {S3ServiceException} from '@aws-sdk/client-s3';
import type {ChannelID, UserID} from '@fluxer/api/src/BrandedTypes';
import {createAttachmentID, createChannelID, createMemeID, createMessageID} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import type {IChannelRepositoryAggregate} from '@fluxer/api/src/channel/repositories/IChannelRepositoryAggregate';
import {makeAttachmentCdnKey} from '@fluxer/api/src/channel/services/message/MessageHelpers';
import type {MessageAttachment} from '@fluxer/api/src/database/types/MessageTypes';
import type {IFavoriteMemeRepository} from '@fluxer/api/src/favorite_meme/IFavoriteMemeRepository';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import type {Message} from '@fluxer/api/src/models/Message';
import type {User} from '@fluxer/api/src/models/User';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';

interface MessageOperationsHelpersDeps {
	channelRepository: IChannelRepositoryAggregate;
	cacheService: ICacheService;
	storageService: IStorageService;
	snowflakeService: SnowflakeService;
	favoriteMemeRepository: IFavoriteMemeRepository;
}

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

		const existingNonce = await this.deps.cacheService.get<{channel_id: string; message_id: string}>(
			`message-nonce:${userId}:${nonce}`,
		);

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

		let flags = 0;
		if (favoriteMeme.isGifv) {
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
			placeholder: null,
			flags,
			duration: favoriteMeme.duration,
			nsfw: null,
			waveform: null,
		};
	}
}
