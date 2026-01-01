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
import fs from 'node:fs/promises';
import {Stream} from 'node:stream';
import {HeadObjectCommand} from '@aws-sdk/client-s3';
import type {Context} from 'hono';
import {HTTPException} from 'hono/http-exception';
import sharp from 'sharp';
import {temporaryFile} from 'tempy';
import * as v from 'valibot';
import {Config} from '~/Config';
import {Logger} from '~/Logger';
import {toBodyData, toWebReadableStream} from '~/lib/BinaryUtils';
import {createThumbnail} from '~/lib/FFmpegUtils';
import {parseRange, setHeaders} from '~/lib/HttpUtils';
import {processImage} from '~/lib/ImageProcessing';
import type {InMemoryCoalescer} from '~/lib/InMemoryCoalescer';
import type {HonoEnv} from '~/lib/MediaTypes';
import {SUPPORTED_MIME_TYPES} from '~/lib/MediaTypes';
import {validateMedia} from '~/lib/MediaValidation';
import {getMediaCategory, getMimeType} from '~/lib/MimeTypeUtils';
import {readS3Object, s3Client, streamS3Object} from '~/lib/S3Utils';
import {ExternalQuerySchema} from '~/schemas/ValidationSchemas';

export const createAttachmentsHandler = (coalescer: InMemoryCoalescer) => {
	return async (ctx: Context<HonoEnv>): Promise<Response> => {
		const {channel_id, attachment_id, filename} = ctx.req.param();
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
					Bucket: Config.AWS_S3_BUCKET_CDN,
					Key: key,
				});
				const headResponse = await s3Client.send(headCommand);
				const totalSize = headResponse.ContentLength || 0;

				const range = parseRange(ctx.req.header('Range') ?? '', totalSize);

				let streamData: Stream;
				let contentType: string;
				let lastModified: Date | undefined;

				if (range) {
					const result = await readS3Object(Config.AWS_S3_BUCKET_CDN, key, range);
					assert(result.data instanceof Stream, 'Expected range request to return a stream');
					streamData = result.data;
					contentType = result.contentType;
					lastModified = result.lastModified;
				} else {
					const result = await streamS3Object(Config.AWS_S3_BUCKET_CDN, key);
					streamData = result.stream;
					contentType = result.contentType;
					lastModified = result.lastModified;
				}

				setHeaders(ctx, totalSize, contentType, range, lastModified);
				ctx.header('Content-Disposition', `attachment; filename="${encodeURIComponent(filename)}"`);

				return new Response(toWebReadableStream(streamData), {
					status: range ? 206 : 200,
					headers: Object.fromEntries(ctx.res.headers),
				});
			} catch (error) {
				Logger.error({error}, 'Failed to process attachment media');
				throw new HTTPException(400);
			}
		}

		const cacheKey = `${key}_${width}_${height}_${format}_${quality}_${animated}`;

		const result = await coalescer.coalesce(cacheKey, async () => {
			try {
				const {data, contentType: originalContentType} = await readS3Object(Config.AWS_S3_BUCKET_CDN, key);
				assert(data instanceof Buffer);

				const mimeType = getMimeType(data, filename) || originalContentType;

				if (mimeType && SUPPORTED_MIME_TYPES.has(mimeType)) {
					await validateMedia(data, filename, ctx);
				}

				const mediaType = getMediaCategory(mimeType);

				if (!mediaType) throw new HTTPException(400, {message: 'Invalid media type'});

				if (mediaType === 'image') {
					const metadata = await sharp(data).metadata();
					const targetWidth = width ? Math.min(width, metadata.width || 0) : metadata.width || 0;
					const targetHeight = height ? Math.min(height, metadata.height || 0) : metadata.height || 0;

					const image = await processImage({
						buffer: data,
						width: targetWidth,
						height: targetHeight,
						format: format || metadata.format || '',
						quality,
						animated: (mimeType.endsWith('gif') || mimeType.endsWith('webp')) && animated,
					});

					const finalContentType = format ? getMimeType(Buffer.from(''), `image.${format}`) : originalContentType;
					return {data: image, contentType: finalContentType || 'application/octet-stream'};
				}

				if (mediaType === 'video' && format) {
					const ext = mimeType.split('/')[1];
					const tempPath = temporaryFile({extension: ext});
					ctx.get('tempFiles').push(tempPath);
					await fs.writeFile(tempPath, data);

					const thumbnailPath = await createThumbnail(tempPath);
					ctx.get('tempFiles').push(thumbnailPath);

					const thumbnailData = await fs.readFile(thumbnailPath);
					const thumbMeta = await sharp(thumbnailData).metadata();

					const targetWidth = width ? Math.min(width, thumbMeta.width || 0) : thumbMeta.width || 0;
					const targetHeight = height ? Math.min(height, thumbMeta.height || 0) : thumbMeta.height || 0;

					const processedThumbnail = await processImage({
						buffer: thumbnailData,
						width: targetWidth,
						height: targetHeight,
						format,
						quality,
						animated: false,
					});

					const contentType = getMimeType(Buffer.from(''), `image.${format}`);
					if (!contentType) throw new HTTPException(400, {message: 'Unsupported image format'});

					return {data: processedThumbnail, contentType};
				}

				throw new HTTPException(400, {message: 'Only images can be transformed via this endpoint'});
			} catch (error) {
				Logger.error({error}, 'Failed to process attachment media');
				throw new HTTPException(400);
			}
		});

		const range = parseRange(ctx.req.header('Range') ?? '', result.data.length);
		setHeaders(ctx, result.data.length, result.contentType, range);

		const downloadFilename = format ? filename.replace(/\.[^.]+$/, `.${format}`) : filename;
		ctx.header('Content-Disposition', `attachment; filename="${encodeURIComponent(downloadFilename)}"`);

		const fileData = range ? result.data.subarray(range.start, range.end + 1) : result.data;
		return ctx.body(toBodyData(fileData));
	};
};
