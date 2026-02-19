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
import {Stream} from 'node:stream';
import {HeadObjectCommand, type S3Client} from '@aws-sdk/client-s3';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {toBodyData, toWebReadableStream} from '@fluxer/media_proxy/src/lib/BinaryUtils';
import {parseRange, setHeaders} from '@fluxer/media_proxy/src/lib/HttpUtils';
import {ImageProcessingError} from '@fluxer/media_proxy/src/lib/ImageProcessing';
import type {InMemoryCoalescer} from '@fluxer/media_proxy/src/lib/InMemoryCoalescer';
import type {MediaTransformService} from '@fluxer/media_proxy/src/lib/MediaTransformService';
import {SUPPORTED_MIME_TYPES} from '@fluxer/media_proxy/src/lib/MediaTypes';
import type {MediaValidator} from '@fluxer/media_proxy/src/lib/MediaValidation';
import type {MimeTypeUtils} from '@fluxer/media_proxy/src/lib/MimeTypeUtils';
import type {S3Utils} from '@fluxer/media_proxy/src/lib/S3Utils';
import {ExternalQuerySchema} from '@fluxer/media_proxy/src/schemas/ValidationSchemas';
import type {HonoEnv} from '@fluxer/media_proxy/src/types/HonoEnv';
import type {Context} from 'hono';
import {HTTPException} from 'hono/http-exception';
import * as v from 'valibot';

interface AttachmentsControllerDeps {
	coalescer: InMemoryCoalescer;
	s3Client: S3Client;
	s3Utils: S3Utils;
	mimeTypeUtils: MimeTypeUtils;
	mediaValidator: MediaValidator;
	mediaTransformService: MediaTransformService;
	logger: LoggerInterface;
	bucketCdn: string;
}

export function createAttachmentsHandler(deps: AttachmentsControllerDeps) {
	const {coalescer, s3Client, s3Utils, mimeTypeUtils, mediaValidator, mediaTransformService, logger, bucketCdn} = deps;
	const {readS3Object} = s3Utils;
	const {getMimeType, getMediaCategory, getContentType} = mimeTypeUtils;
	const {validateMedia} = mediaValidator;
	const {transformImage, transformVideoThumbnail} = mediaTransformService;

	return async (ctx: Context<HonoEnv>): Promise<Response> => {
		const {channel_id, attachment_id, filename} = ctx.req.param();
		if (!filename) throw new HTTPException(400);
		const {width, height, format, quality, animated} = v.parse(ExternalQuerySchema, ctx.req.query());
		const key = `attachments/${channel_id}/${attachment_id}/${filename}`;

		const isStreamableMedia = /\.(mp3|wav|ogg|flac|m4a|aac|opus|wma|mp4|webm|mov|avi|mkv|m4v)$/i.test(filename);
		const hasTransformations = width || height || format || quality !== 'lossless' || animated;

		if (
			(isStreamableMedia && !hasTransformations) ||
			(!width && !height && !format && quality === 'lossless' && !animated)
		) {
			try {
				const headCommand = new HeadObjectCommand({
					Bucket: bucketCdn,
					Key: key,
				});
				const headResponse = await s3Client.send(headCommand);
				const totalSize = headResponse.ContentLength || 0;

				const range = parseRange(ctx.req.header('Range') ?? '', totalSize);

				let streamData: Stream;
				let lastModified: Date | undefined;

				if (range) {
					const result = await readS3Object(bucketCdn, key, range);
					assert(result.data instanceof Stream, 'Expected range request to return a stream');
					streamData = result.data;
					lastModified = result.lastModified;
				} else {
					const result = await s3Utils.streamS3Object(bucketCdn, key);
					streamData = result.stream;
					lastModified = result.lastModified;
				}

				const contentType = getContentType(filename);
				setHeaders(ctx, totalSize, contentType, range, lastModified);
				ctx.header('Content-Disposition', `attachment; filename="${encodeURIComponent(filename)}"`);

				return new Response(toWebReadableStream(streamData), {
					status: range ? 206 : 200,
					headers: Object.fromEntries(ctx.res.headers),
				});
			} catch (error) {
				logger.error({error}, 'Failed to process attachment media');
				throw new HTTPException(400);
			}
		}

		const normalizedFormat = format ? format.toLowerCase() : '';
		const cacheKey = `${key}_${width}_${height}_${normalizedFormat}_${quality}_${animated}`;

		const result = await coalescer.coalesce(cacheKey, async () => {
			try {
				const {data} = await readS3Object(bucketCdn, key);
				assert(data instanceof Buffer);

				const mimeType = getMimeType(data, filename);
				const contentType = getContentType(filename);

				if (mimeType && SUPPORTED_MIME_TYPES.has(mimeType)) {
					await validateMedia(data, filename);
				}

				const mediaType = mimeType ? getMediaCategory(mimeType) : null;

				if (!mediaType) throw new HTTPException(400, {message: 'Invalid media type'});

				if (mediaType === 'image') {
					try {
						return await transformImage(data, {
							width,
							height,
							format: normalizedFormat || undefined,
							quality,
							animated,
							fallbackContentType: contentType,
						});
					} catch (error) {
						if (error instanceof ImageProcessingError) {
							logger.warn({error}, 'Image transformation failed, serving original');
							return {data, contentType};
						}
						throw error;
					}
				}

				if (mediaType === 'video' && format && mimeType) {
					return transformVideoThumbnail(data, mimeType, {
						width,
						height,
						format,
						quality,
					});
				}

				throw new HTTPException(400, {message: 'Only images can be transformed via this endpoint'});
			} catch (error) {
				if (error instanceof HTTPException) throw error;
				if (error instanceof Error && 'isExpected' in error) {
					logger.warn({error}, 'Failed to process attachment media');
				} else {
					logger.error({error}, 'Failed to process attachment media');
				}
				throw new HTTPException(400);
			}
		});

		const range = parseRange(ctx.req.header('Range') ?? '', result.data.length);
		setHeaders(ctx, result.data.length, result.contentType, range);

		const downloadFilename = format && filename ? filename.replace(/\.[^.]+$/, `.${format}`) : (filename ?? 'file');
		ctx.header('Content-Disposition', `attachment; filename="${encodeURIComponent(downloadFilename)}"`);

		const fileData = range ? result.data.subarray(range.start, range.end + 1) : result.data;
		return ctx.body(toBodyData(fileData));
	};
}
