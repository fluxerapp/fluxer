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

import type {ChannelID, UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {APIErrorCodes} from '~/constants/API';
import {BadRequestError} from '~/Errors';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IStorageService} from '~/infrastructure/IStorageService';

const MAX_PREVIEW_BYTES = 1_000_000;
const PREVIEW_TTL_SECONDS = 60 * 60 * 24;

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
			throw new BadRequestError({
				code: APIErrorCodes.INVALID_REQUEST,
				message: 'Preview must be JPEG',
			});
		}
		if (buffer.byteLength > MAX_PREVIEW_BYTES) {
			throw new BadRequestError({
				code: APIErrorCodes.FILE_SIZE_TOO_LARGE,
				message: 'Preview too large (max 1MB)',
			});
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
		const expiresAt = new Date(Date.now() + PREVIEW_TTL_SECONDS * 1000);

		await this.storageService.uploadObject({
			bucket,
			key,
			body: params.body,
			contentType: params.contentType ?? 'image/jpeg',
			expiresAt,
		});

		const meta: StreamPreviewMeta = {
			bucket,
			key,
			updatedAt: Date.now(),
			ownerId: params.userId.toString(),
			channelId: params.channelId.toString(),
			contentType: params.contentType ?? 'image/jpeg',
		};

		await this.cacheService.set(this.getCacheKey(params.streamKey), meta, PREVIEW_TTL_SECONDS);
	}

	async getPreview(streamKey: string): Promise<{buffer: Uint8Array; contentType: string} | null> {
		const meta = await this.cacheService.get<StreamPreviewMeta>(this.getCacheKey(streamKey));
		if (!meta) return null;
		const buffer = await this.storageService.readObject(meta.bucket, meta.key);
		return {buffer, contentType: meta.contentType || 'image/jpeg'};
	}
}
