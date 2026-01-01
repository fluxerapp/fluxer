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

import assert from 'node:assert/strict';
import crypto from 'node:crypto';
import fs from 'node:fs/promises';
import type {Context} from 'hono';
import {HTTPException} from 'hono/http-exception';
import sharp from 'sharp';
import {temporaryFile} from 'tempy';
import {Config} from '~/Config';
import {Logger} from '~/Logger';
import {createThumbnail} from '~/lib/FFmpegUtils';
import type {InMemoryCoalescer} from '~/lib/InMemoryCoalescer';
import type {HonoEnv} from '~/lib/MediaTypes';
import {processMetadata, validateMedia} from '~/lib/MediaValidation';
import {generateFilename, getMimeType} from '~/lib/MimeTypeUtils';
import type {NSFWDetectionService} from '~/lib/NSFWDetectionService';
import {readS3Object, streamToBuffer} from '~/lib/S3Utils';
import * as FetchUtils from '~/utils/FetchUtils';

type MediaProxyMetadataRequest =
	| {type: 'external'; url: string; with_base64?: boolean; isNSFWAllowed: boolean}
	| {type: 'upload'; upload_filename: string; isNSFWAllowed: boolean}
	| {type: 'base64'; base64: string; isNSFWAllowed: boolean}
	| {type: 's3'; bucket: string; key: string; with_base64?: boolean; isNSFWAllowed: boolean};

export const handleMetadataRequest = (coalescer: InMemoryCoalescer, nsfwDetectionService: NSFWDetectionService) => {
	return async (ctx: Context<HonoEnv>) => {
		const request = await ctx.req.json<MediaProxyMetadataRequest>();
		const cacheKey = (() => {
			switch (request.type) {
				case 'base64':
					return `base64_${request.base64}`;
				case 'upload':
					return `upload_${request.upload_filename}`;
				case 'external':
					return `external_${request.url}_${request.with_base64}`;
				case 's3':
					return `s3_${request.bucket}_${request.key}_${request.with_base64}`;
			}
		})();

		return coalescer.coalesce(cacheKey, async () => {
			const {buffer, filename} = await (async () => {
				switch (request.type) {
					case 'base64':
						return {buffer: Buffer.from(request.base64!, 'base64'), filename: undefined};

					case 'upload': {
						const {data} = await readS3Object(Config.AWS_S3_BUCKET_UPLOADS, request.upload_filename!);
						assert(data instanceof Buffer);
						return {buffer: data, filename: request.upload_filename};
					}

					case 's3': {
						const {data} = await readS3Object(request.bucket!, request.key!);
						assert(data instanceof Buffer);
						const filename = request.key!.substring(request.key!.lastIndexOf('/') + 1);
						return {buffer: data, filename: filename || undefined};
					}

					case 'external': {
						try {
							const response = await FetchUtils.sendRequest({url: request.url!});
							if (response.status !== 200) throw new HTTPException(400, {message: 'Failed to fetch media'});

							const url = new URL(request.url!);
							const filename = url.pathname.substring(url.pathname.lastIndexOf('/') + 1);
							return {buffer: await streamToBuffer(response.stream), filename: filename || undefined};
						} catch (error) {
							if (error instanceof Error && 'isExpected' in error && error.isExpected) {
								throw new HTTPException(400, {message: `Unable to fetch media: ${error.message}`});
							}
							throw error;
						}
					}

					default:
						throw new HTTPException(400, {message: 'Invalid request type'});
				}
			})();

			let effectiveFilename = filename;
			if (!effectiveFilename && request.type === 'base64') {
				try {
					const detectedMime = getMimeType(buffer);
					if (detectedMime) {
						effectiveFilename = generateFilename(detectedMime);
					} else {
						const metadata = await sharp(buffer).metadata();
						if (metadata.format) effectiveFilename = `image.${metadata.format}`;
					}
				} catch (error) {
					Logger.error({error}, 'Failed to detect format of base64 data');
					throw new HTTPException(400, {message: 'Invalid or corrupt media data'});
				}
			}

			if (!effectiveFilename) {
				throw new HTTPException(400, {message: 'Cannot determine file type'});
			}

			const mimeType = await validateMedia(buffer, effectiveFilename, ctx);
			const metadata = await processMetadata(ctx, mimeType, buffer);
			const contentHash = crypto.createHash('sha256').update(buffer).digest('hex');

			let nsfw = false;
			let nsfwProbability = 0;
			let nsfwPredictions: Record<string, number> = {};
			if (!request.isNSFWAllowed) {
				const isImageOrVideo = mimeType.startsWith('image/') || mimeType.startsWith('video/');
				if (isImageOrVideo) {
					try {
						let checkBuffer = buffer;
						if (mimeType.startsWith('video/')) {
							const videoExtension = mimeType.split('/')[1] ?? 'tmp';
							const tempPath = temporaryFile({extension: videoExtension});
							ctx.get('tempFiles').push(tempPath);
							await fs.writeFile(tempPath, buffer);

							const thumbnailPath = await createThumbnail(tempPath);
							ctx.get('tempFiles').push(thumbnailPath);

							checkBuffer = await fs.readFile(thumbnailPath);
						}
						const nsfwResult = await nsfwDetectionService.checkNSFWBuffer(checkBuffer);
						nsfw = nsfwResult.isNSFW;
						nsfwProbability = nsfwResult.probability;
						nsfwPredictions = nsfwResult.predictions ?? {};
					} catch (error) {
						Logger.error({error}, 'Failed to perform NSFW detection');
					}
				}
			}

			return ctx.json({
				...metadata,
				content_type: mimeType,
				content_hash: contentHash,
				nsfw,
				nsfw_probability: nsfwProbability,
				nsfw_predictions: nsfwPredictions,
				base64:
					(request.type === 'external' || request.type === 's3') && request.with_base64
						? buffer.toString('base64')
						: undefined,
			});
		});
	};
};
