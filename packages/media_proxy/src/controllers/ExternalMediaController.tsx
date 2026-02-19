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

import type {HttpClient} from '@fluxer/http_client/src/HttpClientTypes';
import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import {toBodyData} from '@fluxer/media_proxy/src/lib/BinaryUtils';
import {parseRange, setHeaders} from '@fluxer/media_proxy/src/lib/HttpUtils';
import {ImageProcessingError} from '@fluxer/media_proxy/src/lib/ImageProcessing';
import type {InMemoryCoalescer} from '@fluxer/media_proxy/src/lib/InMemoryCoalescer';
import type {MediaTransformService} from '@fluxer/media_proxy/src/lib/MediaTransformService';
import type {MediaValidator} from '@fluxer/media_proxy/src/lib/MediaValidation';
import type {MimeTypeUtils} from '@fluxer/media_proxy/src/lib/MimeTypeUtils';
import {streamToBuffer} from '@fluxer/media_proxy/src/lib/S3Utils';
import {ExternalQuerySchema} from '@fluxer/media_proxy/src/schemas/ValidationSchemas';
import type {ErrorType, HonoEnv} from '@fluxer/media_proxy/src/types/HonoEnv';
import type {MetricsInterface} from '@fluxer/media_proxy/src/types/Metrics';
import type {TracingInterface} from '@fluxer/media_proxy/src/types/Tracing';
import {reconstructOriginalUrl} from '@fluxer/media_proxy_utils/src/ExternalMediaProxyPathCodec';
import {verifySignature} from '@fluxer/media_proxy_utils/src/MediaProxyUtils';
import type {Context} from 'hono';
import {HTTPException} from 'hono/http-exception';
import * as v from 'valibot';

interface ExternalMediaControllerDeps {
	coalescer: InMemoryCoalescer;
	httpClient: HttpClient;
	mimeTypeUtils: MimeTypeUtils;
	mediaValidator: MediaValidator;
	mediaTransformService: MediaTransformService;
	logger: LoggerInterface;
	secretKey: string;
	metrics?: MetricsInterface | undefined;
	tracing?: TracingInterface | undefined;
}

function getErrorTypeFromUpstreamStatus(status: number): ErrorType {
	if (status >= 500) return 'upstream_5xx';
	if (status === 404) return 'not_found';
	if (status === 403) return 'forbidden';
	if (status === 401) return 'unauthorized';
	return 'other';
}

export function createExternalMediaHandler(deps: ExternalMediaControllerDeps) {
	const {
		coalescer,
		httpClient,
		mimeTypeUtils,
		mediaValidator,
		mediaTransformService,
		logger,
		secretKey,
		metrics,
		tracing,
	} = deps;
	const {getMimeType, generateFilename, getMediaCategory} = mimeTypeUtils;
	const {validateMedia} = mediaValidator;
	const {transformImage, transformVideoThumbnail} = mediaTransformService;

	const fetchAndValidate = async (
		url: string,
		ctx: Context<HonoEnv>,
	): Promise<{buffer: Buffer; mimeType: string; filename: string}> => {
		try {
			const response = await httpClient.sendRequest({url});
			if (response.status !== 200) {
				const errorType = getErrorTypeFromUpstreamStatus(response.status);
				metrics?.counter({
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
			await validateMedia(buffer, effectiveFilename);

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

	return async (ctx: Context<HonoEnv>, path: string): Promise<Response> => {
		const {width, height, format, quality, animated} = v.parse(ExternalQuerySchema, ctx.req.query());
		const parts = path.split('/');
		const signature = parts[0];
		const proxyUrlPath = parts.slice(1).join('/');
		const hasTransformations = Boolean(width || height || format || quality !== 'lossless' || animated);

		if (!signature || !proxyUrlPath) throw new HTTPException(400);
		if (!verifySignature(proxyUrlPath, signature, secretKey)) {
			throw new HTTPException(401);
		}

		const normalizedFormat = format ? format.toLowerCase() : '';
		const cacheKey = `${proxyUrlPath}_${signature}_${width}_${height}_${normalizedFormat}_${quality}_${animated}`;

		const result = await coalescer.coalesce(cacheKey, async () => {
			const fn = async () => {
				try {
					const actualUrl = reconstructOriginalUrl(proxyUrlPath);
					const {buffer, mimeType} = await fetchAndValidate(actualUrl, ctx);
					const mediaType = getMediaCategory(mimeType);

					if (!mediaType) throw new HTTPException(400, {message: 'Invalid media type'});
					if (!hasTransformations) {
						return {data: buffer, contentType: mimeType};
					}

					if (mediaType === 'image') {
						tracing?.addSpanEvent('image.process.start', {mimeType});
						try {
							return await transformImage(buffer, {
								width,
								height,
								format: normalizedFormat || undefined,
								quality,
								animated,
								fallbackContentType: mimeType,
							});
						} catch (error) {
							if (error instanceof ImageProcessingError) {
								logger.warn({error}, 'Image transformation failed, serving original');
								return {data: buffer, contentType: mimeType};
							}
							throw error;
						}
					}

					if (mediaType === 'video' && format) {
						tracing?.addSpanEvent('video.thumb.start', {mimeType});
						return transformVideoThumbnail(buffer, mimeType, {
							width,
							height,
							format,
							quality,
						});
					}

					return {data: buffer, contentType: mimeType};
				} catch (error) {
					if (error instanceof HTTPException) throw error;
					if (error instanceof Error && 'isExpected' in error) {
						logger.warn({error}, 'Failed to process external media');
					} else {
						logger.error({error}, 'Failed to process external media');
					}
					throw new HTTPException(400, {message: 'Failed to process media'});
				}
			};

			if (tracing) {
				return tracing.withSpan(
					{
						name: 'media_proxy.external.process',
						attributes: {
							'media.proxy.path': proxyUrlPath,
							'media.proxy.cache_key': cacheKey,
							'media.request.format': normalizedFormat || 'original',
						},
					},
					fn,
				);
			}

			return fn();
		});

		const range = parseRange(ctx.req.header('Range') ?? '', result.data.length);
		setHeaders(ctx, result.data.length, result.contentType, range);

		const fileData = range ? result.data.subarray(range.start, range.end + 1) : result.data;
		return ctx.body(toBodyData(fileData));
	};
}
