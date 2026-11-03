// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import {createAttachmentID, createChannelID, createMemeID, createMessageID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {MESSAGE_NONCE_TTL, makeAttachmentCdnKey} from '@app/api/channel/services/message/MessageHelpers';
import type {MessageAttachment} from '@app/api/database/types/MessageTypes';
import type {FavoriteMemeRepository} from '@app/api/favorite_meme/FavoriteMemeRepository';
import type {IMediaService} from '@app/api/infrastructure/IMediaService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {FavoriteMeme} from '@app/api/models/FavoriteMeme';
import type {Message} from '@app/api/models/Message';
import type {User} from '@app/api/models/User';
import {S3ServiceException} from '@aws-sdk/client-s3';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {ThrottledError} from '@fluxer/errors/src/domains/core/ThrottledError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

interface MessageOperationsHelpersDeps {
	channelRepository: ChannelRepository;
	cacheService: ICacheService;
	storageService: IStorageService;
	snowflakeService: ISnowflakeService;
	favoriteMemeRepository: FavoriteMemeRepository;
	mediaService: IMediaService;
}

const ANIMATION_PROBE_CONTENT_TYPES = new Set(['image/webp', 'image/apng', 'image/avif', 'image/png']);
const MESSAGE_NONCE_CLAIM_TTL_SECONDS = 10;
const MESSAGE_NONCE_WAIT_MS = 30_000;
const MESSAGE_NONCE_POLL_MS = 100;

function messageNonceKey(userId: UserID, nonce: string): string {
	return `message-nonce:${userId}:${nonce}`;
}

function messageNonceClaimKey(userId: UserID, nonce: string): string {
	return `message-nonce-claim:${userId}:${createHash('sha256').update(nonce).digest('hex')}`;
}

type RecordMessageNonce = (messageId: MessageID) => Promise<void>;

export class MessageOperationsHelpers {
	constructor(private readonly deps: MessageOperationsHelpersDeps) {}

	private async findExistingMessage({
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
		}>(messageNonceKey(userId, nonce));
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

	async sendOncePerNonce<T>({
		userId,
		nonce,
		expectedChannelId,
		existing,
		send,
	}: {
		userId: UserID;
		nonce?: string;
		expectedChannelId: ChannelID;
		existing: (message: Message) => T;
		send: (recordNonce: RecordMessageNonce) => Promise<T>;
	}): Promise<T> {
		if (!nonce) return send(async () => {});
		const claimKey = messageNonceClaimKey(userId, nonce);
		const deadline = Date.now() + MESSAGE_NONCE_WAIT_MS;
		while (true) {
			const existingMessage = await this.findExistingMessage({userId, nonce, expectedChannelId});
			if (existingMessage) return existing(existingMessage);
			const token = await this.deps.cacheService.acquireLock(claimKey, MESSAGE_NONCE_CLAIM_TTL_SECONDS);
			if (token) {
				return this.sendWithNonceClaim({userId, nonce, expectedChannelId, claimKey, token, existing, send});
			}
			if (Date.now() >= deadline) {
				throw new ThrottledError({
					code: APIErrorCodes.RESOURCE_LOCKED,
					retryAfterSeconds: 1,
					data: {retry_after: 1},
				});
			}
			await new Promise((resolve) => setTimeout(resolve, MESSAGE_NONCE_POLL_MS));
		}
	}

	private async sendWithNonceClaim<T>({
		userId,
		nonce,
		expectedChannelId,
		claimKey,
		token,
		existing,
		send,
	}: {
		userId: UserID;
		nonce: string;
		expectedChannelId: ChannelID;
		claimKey: string;
		token: string;
		existing: (message: Message) => T;
		send: (recordNonce: RecordMessageNonce) => Promise<T>;
	}): Promise<T> {
		const keepAlive = setInterval(
			() => {
				this.deps.cacheService.extendLock(claimKey, token, MESSAGE_NONCE_CLAIM_TTL_SECONDS).catch(() => {});
			},
			(MESSAGE_NONCE_CLAIM_TTL_SECONDS * 1000) / 3,
		);
		let keepClaim = false;
		try {
			const existingMessage = await this.findExistingMessage({userId, nonce, expectedChannelId});
			if (existingMessage) return existing(existingMessage);
			return await send(async (messageId) => {
				try {
					await this.deps.cacheService.set(
						messageNonceKey(userId, nonce),
						{channel_id: expectedChannelId.toString(), message_id: messageId.toString()},
						MESSAGE_NONCE_TTL,
					);
				} catch (error) {
					keepClaim = true;
					Logger.warn(
						{
							userId: userId.toString(),
							channelId: expectedChannelId.toString(),
							messageId: messageId.toString(),
							error,
						},
						'Message was persisted but nonce cache failed',
					);
				}
			});
		} finally {
			clearInterval(keepAlive);
			if (!keepClaim) await this.deps.cacheService.releaseLock(claimKey, token).catch(() => {});
		}
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
