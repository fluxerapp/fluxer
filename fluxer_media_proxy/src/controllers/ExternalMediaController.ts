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

import fs from 'node:fs/promises';
import type {Context} from 'hono';
import {HTTPException} from 'hono/http-exception';
import sharp from 'sharp';
import {temporaryFile} from 'tempy';
import * as v from 'valibot';
import {Config} from '~/Config';
import {Logger} from '~/Logger';
import {toBodyData} from '~/lib/BinaryUtils';
import {createThumbnail} from '~/lib/FFmpegUtils';
import {parseRange, setHeaders} from '~/lib/HttpUtils';
import {processImage} from '~/lib/ImageProcessing';
import type {InMemoryCoalescer} from '~/lib/InMemoryCoalescer';
import type {ErrorType, HonoEnv} from '~/lib/MediaTypes';
import {validateMedia} from '~/lib/MediaValidation';
import * as metrics from '~/lib/MetricsClient';
import {generateFilename, getMediaCategory, getMimeType} from '~/lib/MimeTypeUtils';
import {streamToBuffer} from '~/lib/S3Utils';
import {ExternalQuerySchema} from '~/schemas/ValidationSchemas';
import * as FetchUtils from '~/utils/FetchUtils';
import * as MediaProxyUtils from '~/utils/MediaProxyUtils';

const getErrorTypeFromUpstreamStatus = (status: number): ErrorType => {
	if (status >= 500) return 'upstream_5xx';
	if (status === 404) return 'not_found';
	if (status === 403) return 'forbidden';
	if (status === 401) return 'unauthorized';
	return 'other';
};

const fetchAndValidate = async (
	url: string,
	ctx: Context<HonoEnv>,
): Promise<{buffer: Buffer; mimeType: string; filename: string}> => {
	try {
		const response = await FetchUtils.sendRequest({url});
		if (response.status !== 200) {
			const errorType = getErrorTypeFromUpstreamStatus(response.status);
			metrics.counter({
				name: 'media_proxy.external.upstream_error',
				dimensions: {status: String(response.status), error_type: errorType},
			});
			ctx.set('metricsErrorContext', {errorType, errorSource: 'upstream'});
			throw new Error(`Failed to fetch media: ${response.status}`);
		}

		const buffer = await streamToBuffer(response.stream);
		const urlObj = new URL(url);
		const filename = urlObj.pathname.substring(urlObj.pathname.lastIndexOf('/') + 1);

		const mimeType = getMimeType(buffer, filename);
		if (!mimeType) throw new HTTPException(400, {message: 'Unsupported file format'});

		const effectiveFilename = filename?.includes('.') ? filename : generateFilename(mimeType, filename);
		await validateMedia(buffer, effectiveFilename, ctx);

		return {buffer, mimeType, filename: effectiveFilename};
	} catch (error) {
		if (error instanceof HTTPException) throw error;
		if (error instanceof Error && 'isExpected' in error && error.isExpected) {
			const httpError = error as Error & {errorType?: ErrorType};
			if (httpError.errorType) {
				ctx.set('metricsErrorContext', {errorType: httpError.errorType, errorSource: 'network'});
			}
			throw new HTTPException(400, {message: `Unable to fetch media: ${error.message}`});
		}
		throw error;
	}
};

export const createExternalMediaHandler = (coalescer: InMemoryCoalescer) => {
	return async (ctx: Context<HonoEnv>, path: string): Promise<Response> => {
		const {width, height, format, quality, animated} = v.parse(ExternalQuerySchema, ctx.req.query());
		const parts = path.split('/');
		const signature = parts[0];
		const proxyUrlPath = parts.slice(1).join('/');

		if (!signature || !proxyUrlPath) throw new HTTPException(400);
		if (!MediaProxyUtils.verifySignature(proxyUrlPath, signature, Config.SECRET_KEY)) {
			throw new HTTPException(401);
		}

		const cacheKey = `${proxyUrlPath}_${signature}_${width}_${height}_${format}_${quality}_${animated}`;

		const result = await coalescer.coalesce(cacheKey, async () => {
			try {
				const actualUrl = MediaProxyUtils.reconstructOriginalURL(proxyUrlPath);
				const {buffer, mimeType} = await fetchAndValidate(actualUrl, ctx);
				const mediaType = getMediaCategory(mimeType);

				if (!mediaType) throw new HTTPException(400, {message: 'Invalid media type'});

				if (mediaType === 'image') {
					const metadata = await sharp(buffer).metadata();
					const targetWidth = width ? Math.min(width, metadata.width || 0) : metadata.width || 0;
					const targetHeight = height ? Math.min(height, metadata.height || 0) : metadata.height || 0;

					const image = await processImage({
						buffer,
						width: targetWidth,
						height: targetHeight,
						format: format || metadata.format || '',
						quality,
						animated: (mimeType.endsWith('gif') || mimeType.endsWith('webp')) && animated,
					});

					const contentType = format ? getMimeType(Buffer.from(''), `image.${format}`) : mimeType;

					return {data: image, contentType: contentType || 'application/octet-stream'};
				}

				if (mediaType === 'video' && format) {
					const ext = mimeType.split('/')[1];
					const tempPath = temporaryFile({extension: ext});
					ctx.get('tempFiles').push(tempPath);
					await fs.writeFile(tempPath, buffer);

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

				return {data: buffer, contentType: mimeType};
			} catch (error) {
				if (error instanceof HTTPException) throw error;
				Logger.error({error}, 'Failed to process external media');
				throw new HTTPException(400, {message: 'Failed to process media'});
			}
		});

		const range = parseRange(ctx.req.header('Range') ?? '', result.data.length);
		setHeaders(ctx, result.data.length, result.contentType, range);

		const fileData = range ? result.data.subarray(range.start, range.end + 1) : result.data;
		return ctx.body(toBodyData(fileData));
	};
};
