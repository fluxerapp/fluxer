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

import type {MetricsInterface} from '@fluxer/media_proxy/src/types/Metrics';
import type {TracingInterface} from '@fluxer/media_proxy/src/types/Tracing';
import sharp from 'sharp';
import {rgbaToThumbHash} from 'thumbhash';

export class ImageProcessingError extends Error {
	readonly isExpected = true;

	constructor(message: string, cause?: unknown) {
		super(message);
		this.name = 'ImageProcessingError';
		this.cause = cause;
	}
}

const VIPS_ERROR_PATTERN = /^Vips|^Input |unexpected end of|load_buffer: |save_buffer: /i;

function isCorruptImageError(error: unknown): error is Error {
	return error instanceof Error && VIPS_ERROR_PATTERN.test(error.message);
}

export async function generatePlaceholder(imageBuffer: Buffer): Promise<string> {
	try {
		const metadata = await sharp(imageBuffer).metadata();
		const width = metadata.width ?? 1;
		const height = metadata.height ?? 1;

		if (width < 3 || height < 3) {
			return '';
		}

		let pipeline = sharp(imageBuffer);

		if (width > 10 && height > 10) {
			const blurSigma = Math.min(10, Math.floor(Math.min(width, height) / 10));
			if (blurSigma >= 0.3) {
				pipeline = pipeline.blur(blurSigma);
			}
		}

		const {data, info} = await pipeline
			.resize(100, 100, {fit: 'inside', withoutEnlargement: true})
			.ensureAlpha()
			.raw()
			.toBuffer({resolveWithObject: true});

		if (data.length !== info.width * info.height * 4) {
			return '';
		}

		const placeholder = rgbaToThumbHash(info.width, info.height, data);
		return Buffer.from(placeholder).toString('base64');
	} catch {
		return '';
	}
}

const ANIMATED_OUTPUT_FORMATS = new Set(['gif', 'webp', 'avif', 'png', 'apng']);

export function createImageProcessor(options?: {
	metrics?: MetricsInterface | undefined;
	tracing?: TracingInterface | undefined;
}) {
	const {metrics, tracing} = options ?? {};

	const processImage = async (opts: {
		buffer: Buffer;
		width: number;
		height: number;
		format: string;
		quality: string;
		animated: boolean;
	}): Promise<Buffer> => {
		const fn = async () => {
			try {
				const startTime = Date.now();
				const metadata = await sharp(opts.buffer).metadata();

				const resizeWidth = Math.min(opts.width, metadata.width || 0);
				const resizeHeight = Math.min(opts.height, metadata.height || 0);
				const targetFormat = opts.format.toLowerCase();
				if (!targetFormat) {
					throw new Error('Target image format is required');
				}

				const supportsAnimation = ANIMATED_OUTPUT_FORMATS.has(targetFormat);
				const shouldAnimate = opts.animated && supportsAnimation;

				const buildPipeline = (animated: boolean) =>
					sharp(opts.buffer, {animated})
						.resize(resizeWidth, resizeHeight, {
							fit: 'cover',
							withoutEnlargement: true,
						})
						.toFormat(targetFormat as keyof sharp.FormatEnum, {
							quality: opts.quality === 'high' ? 80 : opts.quality === 'low' ? 20 : 100,
						})
						.toBuffer();

				let result: Buffer;
				try {
					result = await buildPipeline(shouldAnimate);
				} catch (error) {
					if (!shouldAnimate || !isCorruptImageError(error)) {
						throw error;
					}
					result = await buildPipeline(false);
				}

				const duration = Date.now() - startTime;
				metrics?.histogram({
					name: 'media_proxy.transform.latency',
					dimensions: {format: targetFormat, quality: opts.quality},
					valueMs: duration,
				});

				metrics?.counter({
					name: 'media_proxy.transform.bytes',
					dimensions: {format: targetFormat, quality: opts.quality},
					value: result.length,
				});

				return result;
			} catch (error) {
				if (isCorruptImageError(error)) {
					throw new ImageProcessingError(error.message, error);
				}
				throw error;
			}
		};

		if (tracing) {
			return tracing.withSpan(
				{
					name: 'image.transform',
					attributes: {
						'image.width': opts.width,
						'image.height': opts.height,
						'image.format': opts.format,
						'image.quality': opts.quality,
						'image.animated': String(opts.animated),
					},
				},
				fn,
			);
		}

		return fn();
	};

	return {processImage};
}

export type ImageProcessor = ReturnType<typeof createImageProcessor>;
