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

import sharp from 'sharp';
import {rgbaToThumbHash} from 'thumbhash';

export const generatePlaceholder = async (imageBuffer: Buffer): Promise<string> => {
	const {data, info} = await sharp(imageBuffer)
		.blur(10)
		.resize(100, 100, {fit: 'inside', withoutEnlargement: true})
		.ensureAlpha()
		.raw()
		.toBuffer({resolveWithObject: true});

	if (data.length !== info.width * info.height * 4) {
		throw new Error('Unexpected data length');
	}

	const placeholder = rgbaToThumbHash(info.width, info.height, data);
	return Buffer.from(placeholder).toString('base64');
};

export const processImage = async (opts: {
	buffer: Buffer;
	width: number;
	height: number;
	format: string;
	quality: string;
	animated: boolean;
}): Promise<Buffer> => {
	const metadata = await sharp(opts.buffer).metadata();

	const resizeWidth = Math.min(opts.width, metadata.width || 0);
	const resizeHeight = Math.min(opts.height, metadata.height || 0);

	return sharp(opts.buffer, {
		animated: opts.format === 'gif' || (opts.format === 'webp' && opts.animated),
	})
		.resize(resizeWidth, resizeHeight, {
			fit: 'cover',
			withoutEnlargement: true,
		})
		.toFormat(opts.format as keyof sharp.FormatEnum, {
			quality: opts.quality === 'high' ? 80 : opts.quality === 'low' ? 20 : 100,
		})
		.toBuffer();
};
