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
import {validateCodecs} from '~/lib/CodecValidation';
import {createThumbnail, ffprobe} from '~/lib/FFmpegUtils';
import {generatePlaceholder} from '~/lib/ImageProcessing';
import type {HonoEnv} from '~/lib/MediaTypes';
import {generateFilename, getMediaCategory, getMimeType} from '~/lib/MimeTypeUtils';

interface ImageMetadata {
	format: string;
	size: number;
	width: number;
	height: number;
	placeholder: string;
	animated: boolean;
}

interface VideoMetadata {
	format: string;
	size: number;
	width: number;
	height: number;
	duration: number;
	placeholder: string;
}

interface AudioMetadata {
	format: string;
	size: number;
	duration: number;
}

type MediaMetadata = ImageMetadata | VideoMetadata | AudioMetadata;

export const validateMedia = async (buffer: Buffer, filename: string, ctx: Context<HonoEnv>): Promise<string> => {
	const mimeType = getMimeType(buffer, filename);

	if (!mimeType) {
		throw new HTTPException(400, {message: 'Unsupported file format'});
	}

	const mediaType = getMediaCategory(mimeType);

	if (!mediaType) {
		throw new HTTPException(400, {message: 'Invalid media type'});
	}

	if (mediaType !== 'image') {
		const validationFilename = filename.includes('.') ? filename : generateFilename(mimeType, filename);
		const isValid = await validateCodecs(buffer, validationFilename, ctx);
		if (!isValid) {
			throw new HTTPException(400, {message: 'File contains unsupported or non-web-compatible codecs'});
		}
	}

	return mimeType;
};

const toNumericField = (value: string | number | undefined): number => {
	const parsed = Number(value);
	return Number.isFinite(parsed) ? parsed : 0;
};

export const processMetadata = async (
	ctx: Context<HonoEnv>,
	mimeType: string,
	buffer: Buffer,
): Promise<MediaMetadata> => {
	const mediaType = getMediaCategory(mimeType);

	if (mediaType === 'image') {
		const {format = '', size = 0, width = 0, height = 0, pages} = await sharp(buffer).metadata();

		if (width > 9500 || height > 9500) throw new HTTPException(400);

		return {
			format,
			size,
			width,
			height,
			placeholder: await generatePlaceholder(buffer),
			animated: pages ? pages > 1 : false,
		} satisfies ImageMetadata;
	}

	const ext = mimeType.split('/')[1];
	const tempPath = temporaryFile({extension: ext});
	ctx.get('tempFiles').push(tempPath);
	await fs.writeFile(tempPath, buffer);

	if (mediaType === 'video') {
		const thumbnailPath = await createThumbnail(tempPath);
		ctx.get('tempFiles').push(thumbnailPath);

		const thumbnailData = await fs.readFile(thumbnailPath);
		const {width = 0, height = 0} = await sharp(thumbnailData).metadata();
		const probeData = await ffprobe(tempPath);

		return {
			format: probeData.format?.format_name || '',
			size: toNumericField(probeData.format?.size),
			width,
			height,
			duration: Math.ceil(Number(probeData.format?.duration || 0)),
			placeholder: await generatePlaceholder(thumbnailData),
		} satisfies VideoMetadata;
	}

	if (mediaType === 'audio') {
		const probeData = await ffprobe(tempPath);
		return {
			format: probeData.format?.format_name || '',
			size: toNumericField(probeData.format?.size),
			duration: Math.ceil(Number(probeData.format?.duration || 0)),
		} satisfies AudioMetadata;
	}

	throw new HTTPException(400, {message: 'Unsupported media type'});
};
