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

import crypto from 'node:crypto';
import {
	AVATAR_EXTENSIONS,
	AVATAR_MAX_SIZE,
	EMOJI_EXTENSIONS,
	EMOJI_MAX_SIZE,
	STICKER_EXTENSIONS,
	STICKER_MAX_SIZE,
	StickerFormatTypes,
} from '~/Constants';
import {InputValidationError} from '~/Errors';
import type {IMediaService} from './IMediaService';
import type {IStorageService} from './IStorageService';

export class AvatarService {
	constructor(
		private storageService: IStorageService,
		private mediaService: IMediaService,
	) {}

	async uploadAvatar(params: {
		prefix: 'avatars' | 'icons' | 'banners' | 'splashes';
		entityId?: bigint;
		keyPath?: string;
		errorPath: string;
		previousKey?: string | null;
		base64Image?: string | null;
	}): Promise<string | null> {
		const {prefix, entityId, keyPath, errorPath, previousKey, base64Image} = params;

		const stripAnimationPrefix = (key: string) => (key.startsWith('a_') ? key.substring(2) : key);

		const fullKeyPath = keyPath ?? (entityId ? entityId.toString() : '');

		if (!base64Image) {
			if (previousKey) {
				await this.storageService.deleteAvatar({prefix, key: `${fullKeyPath}/${stripAnimationPrefix(previousKey)}`});
			}
			return null;
		}

		const base64Data = base64Image.includes(',') ? base64Image.split(',')[1] : base64Image;

		let imageBuffer: Uint8Array;
		try {
			imageBuffer = new Uint8Array(Buffer.from(base64Data, 'base64'));
		} catch {
			throw InputValidationError.create(errorPath, 'Invalid image data');
		}

		if (imageBuffer.length > AVATAR_MAX_SIZE) {
			throw InputValidationError.create(errorPath, `Image size exceeds ${AVATAR_MAX_SIZE} bytes`);
		}

		const metadata = await this.mediaService.getMetadata({
			type: 'base64',
			base64: base64Data,
			isNSFWAllowed: false,
		});

		if (metadata == null || !AVATAR_EXTENSIONS.has(metadata.format)) {
			throw InputValidationError.create(
				errorPath,
				`Invalid image format. Supported extensions: ${[...AVATAR_EXTENSIONS].join(', ')}`,
			);
		}

		const imageHash = crypto.createHash('md5').update(Buffer.from(imageBuffer)).digest('hex');
		const imageHashShort = imageHash.slice(0, 8);

		await this.storageService.uploadAvatar({prefix, key: `${fullKeyPath}/${imageHashShort}`, body: imageBuffer});

		if (previousKey) {
			await this.storageService.deleteAvatar({prefix, key: `${fullKeyPath}/${stripAnimationPrefix(previousKey)}`});
		}

		return metadata.format === 'gif' ? `a_${imageHashShort}` : imageHashShort;
	}

	async uploadAvatarToPath(params: {
		bucket: string;
		keyPath: string;
		errorPath: string;
		previousKey?: string | null;
		base64Image?: string | null;
	}): Promise<string | null> {
		const {bucket, keyPath, errorPath, previousKey, base64Image} = params;

		const stripAnimationPrefix = (key: string) => (key.startsWith('a_') ? key.substring(2) : key);

		if (!base64Image) {
			if (previousKey) {
				await this.storageService.deleteObject(bucket, `${keyPath}/${stripAnimationPrefix(previousKey)}`);
			}
			return null;
		}

		const base64Data = base64Image.includes(',') ? base64Image.split(',')[1] : base64Image;

		let imageBuffer: Uint8Array;
		try {
			imageBuffer = new Uint8Array(Buffer.from(base64Data, 'base64'));
		} catch {
			throw InputValidationError.create(errorPath, 'Invalid image data');
		}

		if (imageBuffer.length > AVATAR_MAX_SIZE) {
			throw InputValidationError.create(errorPath, `Image size exceeds ${AVATAR_MAX_SIZE} bytes`);
		}

		const metadata = await this.mediaService.getMetadata({
			type: 'base64',
			base64: base64Data,
			isNSFWAllowed: false,
		});

		if (metadata == null || !AVATAR_EXTENSIONS.has(metadata.format)) {
			throw InputValidationError.create(
				errorPath,
				`Invalid image format. Supported extensions: ${[...AVATAR_EXTENSIONS].join(', ')}`,
			);
		}

		const imageHash = crypto.createHash('md5').update(Buffer.from(imageBuffer)).digest('hex');
		const imageHashShort = imageHash.slice(0, 8);

		await this.storageService.uploadObject({
			bucket,
			key: `${keyPath}/${imageHashShort}`,
			body: imageBuffer,
		});

		if (previousKey) {
			await this.storageService.deleteObject(bucket, `${keyPath}/${stripAnimationPrefix(previousKey)}`);
		}

		return metadata.format === 'gif' ? `a_${imageHashShort}` : imageHashShort;
	}

	async processEmoji(params: {errorPath: string; base64Image: string}): Promise<{
		imageBuffer: Uint8Array;
		animated: boolean;
	}> {
		const {errorPath, base64Image} = params;

		const base64Data = base64Image.includes(',') ? base64Image.split(',')[1] : base64Image;

		let imageBuffer: Uint8Array;
		try {
			imageBuffer = new Uint8Array(Buffer.from(base64Data, 'base64'));
		} catch {
			throw InputValidationError.create(errorPath, 'Invalid image data');
		}

		if (imageBuffer.length > EMOJI_MAX_SIZE) {
			throw InputValidationError.create(errorPath, `Image size exceeds ${EMOJI_MAX_SIZE} bytes`);
		}

		const metadata = await this.mediaService.getMetadata({
			type: 'base64',
			base64: base64Data,
			isNSFWAllowed: false,
		});

		if (metadata == null || !EMOJI_EXTENSIONS.has(metadata.format)) {
			throw InputValidationError.create(
				errorPath,
				`Invalid image format. Supported extensions: ${[...EMOJI_EXTENSIONS].join(', ')}`,
			);
		}

		return {imageBuffer, animated: metadata.format === 'gif'};
	}

	async uploadEmoji(params: {prefix: 'emojis'; emojiId: bigint; imageBuffer: Uint8Array}): Promise<void> {
		const {prefix, emojiId, imageBuffer} = params;
		await this.storageService.uploadAvatar({prefix, key: emojiId.toString(), body: imageBuffer});
	}

	async processSticker(params: {errorPath: string; base64Image: string}): Promise<{
		imageBuffer: Uint8Array;
		formatType: number;
	}> {
		const {errorPath, base64Image} = params;

		const base64Data = base64Image.includes(',') ? base64Image.split(',')[1] : base64Image;

		let imageBuffer: Uint8Array;
		try {
			imageBuffer = new Uint8Array(Buffer.from(base64Data, 'base64'));
		} catch {
			throw InputValidationError.create(errorPath, 'Invalid image data');
		}

		if (imageBuffer.length > STICKER_MAX_SIZE) {
			throw InputValidationError.create(errorPath, `Image size exceeds ${STICKER_MAX_SIZE} bytes`);
		}

		const metadata = await this.mediaService.getMetadata({
			type: 'base64',
			base64: base64Data,
			isNSFWAllowed: false,
		});

		if (metadata == null || !EMOJI_EXTENSIONS.has(metadata.format)) {
			throw InputValidationError.create(
				errorPath,
				`Invalid image format. Supported extensions: ${[...STICKER_EXTENSIONS].join(', ')}`,
			);
		}

		let computedFormat: (typeof StickerFormatTypes)[keyof typeof StickerFormatTypes];

		switch (metadata.format) {
			case 'png': {
				computedFormat = StickerFormatTypes.PNG;
				break;
			}
			case 'gif': {
				computedFormat = StickerFormatTypes.GIF;
				break;
			}
			default: {
				throw InputValidationError.create(errorPath, 'Unknown image format.');
			}
		}

		return {imageBuffer, formatType: computedFormat};
	}

	async uploadSticker(params: {prefix: 'stickers'; stickerId: bigint; imageBuffer: Uint8Array}): Promise<void> {
		const {prefix, stickerId, imageBuffer} = params;
		await this.storageService.uploadAvatar({prefix, key: stickerId.toString(), body: imageBuffer});
	}
}
