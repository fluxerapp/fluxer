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
import type {Context} from 'hono';
import {HTTPException} from 'hono/http-exception';
import sharp from 'sharp';
import {temporaryFile} from 'tempy';
import * as v from 'valibot';
import {Config} from '~/Config';
import {Logger} from '~/Logger';
import {toBodyData} from '~/lib/BinaryUtils';
import {createThumbnail} from '~/lib/FFmpegUtils';
import type {HonoEnv} from '~/lib/MediaTypes';
import {getMediaCategory, getMimeType} from '~/lib/MimeTypeUtils';
import {readS3Object} from '~/lib/S3Utils';

const ThumbnailRequestSchema = v.object({
	type: v.literal('upload'),
	upload_filename: v.string(),
});

export const handleThumbnailRequest = async (ctx: Context<HonoEnv>): Promise<Response> => {
	try {
		const body = await ctx.req.json();
		const {upload_filename} = v.parse(ThumbnailRequestSchema, body);

		const {data} = await readS3Object(Config.AWS_S3_BUCKET_UPLOADS, upload_filename);

		assert(data instanceof Buffer);

		const mimeType = getMimeType(data, upload_filename);
		if (!mimeType) {
			throw new HTTPException(400, {message: 'Unable to determine file type'});
		}

		const mediaType = getMediaCategory(mimeType);
		if (mediaType !== 'video') {
			throw new HTTPException(400, {message: 'Not a video file'});
		}

		const ext = mimeType.split('/')[1] || 'mp4';
		const tempVideoPath = temporaryFile({extension: ext});
		ctx.get('tempFiles').push(tempVideoPath);
		await fs.writeFile(tempVideoPath, data);

		const thumbnailPath = await createThumbnail(tempVideoPath);
		ctx.get('tempFiles').push(thumbnailPath);

		const thumbnailData = await fs.readFile(thumbnailPath);
		const processedThumbnail = await sharp(thumbnailData).jpeg({quality: 80}).toBuffer();

		return ctx.body(toBodyData(processedThumbnail), {
			headers: {
				'Content-Type': 'image/jpeg',
			},
		});
	} catch (error) {
		Logger.error({error}, 'Failed to generate thumbnail');
		throw new HTTPException(404);
	}
};
