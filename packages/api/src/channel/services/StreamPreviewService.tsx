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
import {Config} from '@fluxer/api/src/Config';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import {Logger} from '@fluxer/api/src/Logger';
import type {ICacheService} from '@fluxer/cache/src/ICacheService';
import {STREAM_PREVIEW_CONTENT_TYPE_JPEG, STREAM_PREVIEW_MAX_BYTES} from '@fluxer/constants/src/StreamConstants';
import {FileSizeTooLargeError} from '@fluxer/errors/src/domains/core/FileSizeTooLargeError';
import {PreviewMustBeJpegError} from '@fluxer/errors/src/domains/core/PreviewMustBeJpegError';
import {ms, seconds} from 'itty-time';

const PREVIEW_TTL_SECONDS = seconds('1 day');

interface StreamPreviewMeta {
	bucket: string;
	key: string;
	updatedAt: number;
	ownerId: string;
	channelId: string;
	contentType: string;
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
		return `stream_previews/${streamKey}.jpg`;
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
		const expiresAt = new Date(Date.now() + ms('1 day'));

		try {
			await this.storageService.uploadObject({
				bucket,
				key,
				body: params.body,
				contentType: params.contentType ?? STREAM_PREVIEW_CONTENT_TYPE_JPEG,
				expiresAt,
			});
		} catch (error) {
			if (error instanceof S3ServiceException && error.name === 'OperationAborted') {
				Logger.warn({streamKey: params.streamKey}, 'Stream preview upload aborted due to S3 conflict, skipping');
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
			contentType: params.contentType ?? STREAM_PREVIEW_CONTENT_TYPE_JPEG,
		};

		await this.cacheService.set(this.getCacheKey(params.streamKey), meta, PREVIEW_TTL_SECONDS);
	}

	async getPreview(streamKey: string): Promise<{buffer: Uint8Array; contentType: string} | null> {
		const meta = await this.cacheService.get<StreamPreviewMeta>(this.getCacheKey(streamKey));
		if (!meta) return null;
		const buffer = await this.storageService.readObject(meta.bucket, meta.key);
		return {buffer, contentType: meta.contentType || STREAM_PREVIEW_CONTENT_TYPE_JPEG};
	}
}
