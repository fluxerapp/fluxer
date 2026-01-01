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

import {S3ServiceException} from '@aws-sdk/client-s3';
import type archiver from 'archiver';

import {Config} from '~/Config';
import {StickerFormatTypes} from '~/constants/Guild';
import type {StorageService} from '~/infrastructure/StorageService';
import {Logger} from '~/Logger';

const CDN_BUCKET = Config.s3.buckets.cdn;

const stripAnimationPrefix = (hash: string): string => (hash.startsWith('a_') ? hash.slice(2) : hash);

export const buildHashedAssetKey = (prefix: string, entityId: string, hash: string): string =>
	`${prefix}/${entityId}/${stripAnimationPrefix(hash)}`;

export const buildSimpleAssetKey = (prefix: string, key: string): string => `${prefix}/${key}`;

export const getAnimatedAssetExtension = (hash: string): 'gif' | 'png' => (hash.startsWith('a_') ? 'gif' : 'png');

export const getEmojiExtension = (animated: boolean): 'gif' | 'webp' => (animated ? 'gif' : 'webp');

export const getStickerExtension = (formatType: number): 'gif' | 'webp' =>
	formatType === StickerFormatTypes.GIF ? 'gif' : 'webp';

const readCdnAssetIfExists = async (storageService: StorageService, key: string): Promise<Buffer | null> => {
	try {
		const data = await storageService.readObject(CDN_BUCKET, key);
		return Buffer.from(data);
	} catch (error) {
		if (error instanceof S3ServiceException && error.name === 'NoSuchKey') {
			return null;
		}
		throw error;
	}
};

export interface AppendAssetToArchiveParams {
	archive: archiver.Archiver;
	storageService: StorageService;
	storageKey: string;
	archiveName: string;
	label: string;
	subjectId: string;
}

export const appendAssetToArchive = async ({
	archive,
	storageService,
	storageKey,
	archiveName,
	label,
	subjectId,
}: AppendAssetToArchiveParams): Promise<void> => {
	const buffer = await readCdnAssetIfExists(storageService, storageKey);
	if (!buffer) {
		Logger.warn({subjectId, storageKey}, `Skipping missing ${label}`);
		return;
	}

	archive.append(buffer, {name: archiveName});
};
