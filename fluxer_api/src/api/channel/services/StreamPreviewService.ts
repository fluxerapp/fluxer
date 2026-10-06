// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {ChannelID, UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {applyUploadRelayDecision, resolveUploadRelayDecision} from '@app/api/channel/services/UploadRelay';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import {S3ServiceException} from '@aws-sdk/client-s3';
import {STREAM_PREVIEW_CONTENT_TYPE_JPEG, STREAM_PREVIEW_MAX_BYTES} from '@fluxer/constants/src/StreamConstants';
import {FileSizeTooLargeError} from '@fluxer/errors/src/domains/core/FileSizeTooLargeError';
import {PreviewMustBeJpegError} from '@fluxer/errors/src/domains/core/PreviewMustBeJpegError';
import type {StreamPreviewUploadUrlResponseSchema} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {ms, seconds} from 'itty-time';

const PREVIEW_TTL_SECONDS = seconds('1 day');
const PREVIEW_UPLOAD_URL_TTL_SECONDS = seconds('1 day');
const RECOVERABLE_S3_ERROR_NAMES = new Set([
	'InternalError',
	'OperationAborted',
	'RequestTimeout',
	'ServiceUnavailable',
	'SlowDown',
]);
const RECOVERABLE_STORAGE_ERROR_CODES = new Set(['EAI_AGAIN', 'ECONNREFUSED', 'ECONNRESET', 'ENOTFOUND', 'ETIMEDOUT']);
const MISSING_OBJECT_ERROR_NAMES = new Set(['NoSuchKey', 'NotFound']);

interface StreamPreviewMeta {
	bucket: string;
	key: string;
	updatedAt: number;
	ownerId: string;
	channelId: string;
	contentType: string;
}

function getErrorName(error: unknown): string | undefined {
	if (!error || typeof error !== 'object') return undefined;
	const name = (error as {name?: unknown}).name;
	return typeof name === 'string' ? name : undefined;
}

function getErrorCode(error: unknown): string | undefined {
	if (!error || typeof error !== 'object') return undefined;
	const code = (error as {code?: unknown}).code;
	return typeof code === 'string' ? code : undefined;
}

function isMissingObjectError(error: unknown): boolean {
	return error instanceof S3ServiceException && MISSING_OBJECT_ERROR_NAMES.has(error.name);
}

function isRecoverableStorageError(error: unknown): boolean {
	if (error instanceof S3ServiceException) {
		return error.$fault === 'server' || RECOVERABLE_S3_ERROR_NAMES.has(error.name);
	}
	const code = getErrorCode(error);
	if (code && RECOVERABLE_STORAGE_ERROR_CODES.has(code)) return true;
	const name = getErrorName(error);
	return name === 'TimeoutError';
}

function isObjectTooLargeError(error: unknown): boolean {
	return error instanceof Error && error.message.startsWith('Stream exceeds maximum buffer size');
}

export class StreamPreviewService {
	constructor(
		private readonly storageService: IStorageService,
		private readonly cacheService: ICacheService,
	) {}

	private getCacheKey(streamKey: string): string {
		return `stream_preview:${streamKey}`;
	}

	private getObjectKey(streamKey: string): string {
		const safeSegment = streamKey.replace(/[^A-Za-z0-9._~-]/g, '-');
		const digest = createHash('sha256').update(streamKey).digest('hex').slice(0, 16);
		return `stream_previews/${safeSegment}-${digest}.jpg`;
	}

	private assertJpeg(buffer: Uint8Array, contentType?: string) {
		const ct = (contentType || '').toLowerCase();
		const isJpeg = ct.includes('jpeg') || ct.includes('jpg') || this.looksLikeJpeg(buffer);
		if (!isJpeg) {
			throw new PreviewMustBeJpegError();
		}
		if (buffer.byteLength > STREAM_PREVIEW_MAX_BYTES) {
			throw new FileSizeTooLargeError();
		}
	}

	private looksLikeJpeg(buffer: Uint8Array): boolean {
		return (
			buffer.length > 3 &&
			buffer[0] === 0xff &&
			buffer[1] === 0xd8 &&
			buffer[buffer.length - 2] === 0xff &&
			buffer[buffer.length - 1] === 0xd9
		);
	}

	private normalizeUploadUrlContentType(contentType?: string): string {
		if (!contentType) return STREAM_PREVIEW_CONTENT_TYPE_JPEG;
		const ct = contentType.toLowerCase();
		if (!ct.includes('jpeg') && !ct.includes('jpg')) {
			throw new PreviewMustBeJpegError();
		}
		return contentType;
	}

	async uploadPreview(params: {
		streamKey: string;
		channelId: ChannelID;
		userId: UserID;
		body: Uint8Array;
		contentType?: string;
	}): Promise<void> {
		this.assertJpeg(params.body, params.contentType);
		const bucket = Config.s3.buckets.uploads;
		const key = this.getObjectKey(params.streamKey);
		const contentType = params.contentType ?? STREAM_PREVIEW_CONTENT_TYPE_JPEG;
		const expiresAt = new Date(Date.now() + ms('1 day'));
		try {
			await this.storageService.uploadObject({
				bucket,
				key,
				body: params.body,
				contentType,
				expiresAt,
			});
		} catch (error) {
			if (isRecoverableStorageError(error)) {
				Logger.warn(
					{error, streamKey: params.streamKey},
					'Stream preview upload failed due to storage error, skipping',
				);
				return;
			}
			throw error;
		}
		const meta: StreamPreviewMeta = {
			bucket,
			key,
			updatedAt: Date.now(),
			ownerId: params.userId.toString(),
			channelId: params.channelId.toString(),
			contentType,
		};
		try {
			await this.cacheService.set(this.getCacheKey(params.streamKey), meta, PREVIEW_TTL_SECONDS);
		} catch (error) {
			if (!isRecoverableStorageError(error)) {
				throw error;
			}
			Logger.warn({error, streamKey: params.streamKey}, 'Stream preview metadata cache write failed, skipping');
		}
	}

	async createUploadUrl(params: {
		streamKey: string;
		channelId: ChannelID;
		userId: UserID;
		contentType?: string;
		clientIp?: string | null;
	}): Promise<StreamPreviewUploadUrlResponseSchema> {
		const bucket = Config.s3.buckets.uploads;
		const key = this.getObjectKey(params.streamKey);
		const contentType = this.normalizeUploadUrlContentType(params.contentType);
		const presignedUrl = await this.storageService.getPresignedUploadURL({
			bucket,
			key,
			contentType,
			expiresIn: PREVIEW_UPLOAD_URL_TTL_SECONDS,
		});
		const relayDecision = await resolveUploadRelayDecision(params.clientIp);
		const uploadUrl = applyUploadRelayDecision({
			presignedUrl,
			bucket,
			key,
			relayDecision,
			contentType,
			maxBytes: STREAM_PREVIEW_MAX_BYTES,
		});
		const expiresIn = relayDecision ? relayDecision.tokenTtlSecs : PREVIEW_UPLOAD_URL_TTL_SECONDS;
		const expiresAt = new Date(Date.now() + expiresIn * 1000).toISOString();
		const meta: StreamPreviewMeta = {
			bucket,
			key,
			updatedAt: Date.now(),
			ownerId: params.userId.toString(),
			channelId: params.channelId.toString(),
			contentType,
		};
		await this.cacheService.set(this.getCacheKey(params.streamKey), meta, PREVIEW_TTL_SECONDS);
		return {
			upload_url: uploadUrl,
			method: 'PUT',
			content_type: contentType,
			expires_at: expiresAt,
			expires_in: expiresIn,
			max_bytes: STREAM_PREVIEW_MAX_BYTES,
		};
	}

	async getPreview(streamKey: string): Promise<{
		buffer: Uint8Array;
		contentType: string;
	} | null> {
		const cacheKey = this.getCacheKey(streamKey);
		const meta = await this.cacheService.get<StreamPreviewMeta>(cacheKey);
		if (!meta) return null;
		let buffer: Uint8Array;
		try {
			buffer = await this.storageService.readObject(meta.bucket, meta.key, STREAM_PREVIEW_MAX_BYTES);
		} catch (error) {
			if (isMissingObjectError(error)) {
				return null;
			}
			if (isObjectTooLargeError(error)) {
				Logger.warn({error, streamKey}, 'Stream preview object exceeded maximum size, returning no preview');
				return null;
			}
			if (isRecoverableStorageError(error)) {
				Logger.warn({error, streamKey}, 'Stream preview read failed due to storage error, returning no preview');
				return null;
			}
			throw error;
		}
		return {buffer, contentType: meta.contentType || STREAM_PREVIEW_CONTENT_TYPE_JPEG};
	}

	async deletePreview(streamKey: string): Promise<void> {
		const cacheKey = this.getCacheKey(streamKey);
		const meta = await this.cacheService.get<StreamPreviewMeta>(cacheKey);
		await this.cacheService.delete(cacheKey);
		if (!meta) return;
		try {
			await this.storageService.deleteObject(meta.bucket, meta.key);
		} catch (error) {
			if (isMissingObjectError(error)) {
				return;
			}
			if (isRecoverableStorageError(error)) {
				Logger.warn({error, streamKey}, 'Stream preview delete failed due to storage error after cache removal');
				return;
			}
			throw error;
		}
	}
}
