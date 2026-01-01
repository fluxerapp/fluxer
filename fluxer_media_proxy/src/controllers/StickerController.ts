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
import type {Context} from 'hono';
import {HTTPException} from 'hono/http-exception';
import sharp from 'sharp';
import * as v from 'valibot';
import {Config} from '~/Config';
import {toBodyData} from '~/lib/BinaryUtils';
import {parseRange, setHeaders} from '~/lib/HttpUtils';
import type {InMemoryCoalescer} from '~/lib/InMemoryCoalescer';
import type {HonoEnv} from '~/lib/MediaTypes';
import {getMimeType} from '~/lib/MimeTypeUtils';
import {readS3Object} from '~/lib/S3Utils';
import {ImageQuerySchema} from '~/schemas/ValidationSchemas';

const STICKER_EXTENSIONS = ['gif', 'webp'];

const processStickerRequest = async (params: {
	coalescer: InMemoryCoalescer;
	ctx: Context<HonoEnv>;
	cacheKey: string;
	s3Key: string;
	ext: string;
	size: string;
	quality: string;
	animated: boolean;
}): Promise<Response> => {
	const {coalescer, ctx, cacheKey, s3Key, ext, size, quality, animated} = params;

	const result = await coalescer.coalesce(cacheKey, async () => {
		const {data} = await readS3Object(Config.AWS_S3_BUCKET_CDN, s3Key);
		assert(data instanceof Buffer);

		const metadata = await sharp(data).metadata();
		const requestedSize = Number(size);

		const width = Math.min(requestedSize, metadata.width || 0);
		const height = Math.min(requestedSize, metadata.height || 0);

		const shouldAnimate = ext === 'gif' ? true : ext === 'webp' && animated;
		const image = await sharp(data, {animated: shouldAnimate})
			.resize(width, height, {
				fit: 'contain',
				background: {r: 255, g: 255, b: 255, alpha: 0},
				withoutEnlargement: true,
			})
			.toFormat(ext as keyof sharp.FormatEnum, {
				quality: quality === 'high' ? 80 : quality === 'low' ? 20 : 100,
			})
			.toBuffer();

		const mimeType = getMimeType(Buffer.from(''), `image.${ext}`) || 'application/octet-stream';
		return {data: image, contentType: mimeType};
	});

	const range = parseRange(ctx.req.header('Range') ?? '', result.data.length);
	setHeaders(ctx, result.data.length, result.contentType, range);

	const fileData = range ? result.data.subarray(range.start, range.end + 1) : result.data;
	return ctx.body(toBodyData(fileData));
};

export const createStickerRouteHandler = (coalescer: InMemoryCoalescer) => {
	return async (ctx: Context<HonoEnv>): Promise<Response> => {
		const {id} = ctx.req.param();
		const {size, quality, animated} = v.parse(ImageQuerySchema, ctx.req.query());

		const parts = id.split('.');
		if (parts.length !== 2 || !STICKER_EXTENSIONS.includes(parts[1])) {
			throw new HTTPException(400);
		}

		const [filename, ext] = parts;
		const cacheKey = `stickers_${filename}_${ext}_${size}_${quality}_${animated}`;
		const s3Key = `stickers/${filename}`;

		return processStickerRequest({coalescer, ctx, cacheKey, s3Key, ext, size, quality, animated});
	};
};
