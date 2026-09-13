// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Readable} from 'node:stream';
import {Config} from '@app/api/Config';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {ArchiveEntryWriter} from '@app/api/worker/utils/ArchiveFile';
import {S3ServiceException} from '@aws-sdk/client-s3';

let _cdnBucket: string | null = null;

function getCdnBucket(): string {
	if (!_cdnBucket) {
		_cdnBucket = Config.s3.buckets.cdn;
	}
	return _cdnBucket;
}

function stripAnimationPrefix(hash: string): string {
	return hash.startsWith('a_') ? hash.slice(2) : hash;
}

export function buildHashedAssetKey(prefix: string, entityId: string, hash: string): string {
	return `${prefix}/${entityId}/${stripAnimationPrefix(hash)}`;
}

export function buildSimpleAssetKey(prefix: string, key: string): string {
	return `${prefix}/${key}`;
}

export function getAnimatedAssetExtension(hash: string): 'gif' | 'png' {
	return hash.startsWith('a_') ? 'gif' : 'png';
}

export function getEmojiExtension(animated: boolean): 'gif' | 'webp' {
	return animated ? 'gif' : 'webp';
}

async function readCdnAssetIfExists(storageService: IStorageService, key: string): Promise<Buffer | null> {
	try {
		const data = await storageService.readObject(getCdnBucket(), key);
		return Buffer.from(data);
	} catch (error) {
		if (
			(error instanceof S3ServiceException && error.name === 'NoSuchKey') ||
			(error instanceof Error && error.name === 'NoSuchKey')
		) {
			return null;
		}
		throw error;
	}
}

export async function streamCdnAssetIfExists(
	storageService: IStorageService,
	key: string,
): Promise<{
	body: Readable;
	contentLength: number;
} | null> {
	try {
		const result = await storageService.streamObject({bucket: getCdnBucket(), key});
		if (!result) return null;
		return {body: result.body, contentLength: result.contentLength};
	} catch (error) {
		if (
			(error instanceof S3ServiceException && error.name === 'NoSuchKey') ||
			(error instanceof Error && error.name === 'NoSuchKey')
		) {
			return null;
		}
		throw error;
	}
}

interface AppendAssetToArchiveParams {
	archive: ArchiveEntryWriter;
	storageService: IStorageService;
	storageKey: string;
	archiveName: string;
	label: string;
	subjectId: string;
}

export async function appendAssetToArchive({
	archive,
	storageService,
	storageKey,
	archiveName,
	label,
	subjectId,
}: AppendAssetToArchiveParams): Promise<void> {
	const buffer = await readCdnAssetIfExists(storageService, storageKey);
	if (!buffer) {
		Logger.warn({subjectId, storageKey}, `Skipping missing ${label}`);
		return;
	}
	await archive.append(buffer, {name: archiveName});
}
