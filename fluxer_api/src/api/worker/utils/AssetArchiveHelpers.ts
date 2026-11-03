// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Readable} from 'node:stream';
import {Config} from '@app/api/Config';
import {stripAnimationPrefix} from '@app/api/infrastructure/AssetEntitlementUtils';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {ArchiveEntryWriter} from '@app/api/worker/utils/ArchiveFile';

export function buildHashedAssetKey(prefix: string, entityId: string, hash: string): string {
	return `${prefix}/${entityId}/${stripAnimationPrefix(hash)}`;
}

export function getAnimatedAssetExtension(hash: string): 'gif' | 'png' {
	return hash.startsWith('a_') ? 'gif' : 'png';
}

export async function streamCdnAssetIfExists(
	storageService: IStorageService,
	key: string,
): Promise<{
	body: Readable;
	contentLength: number;
} | null> {
	return storageService.streamObject({bucket: Config.s3.buckets.cdn, key});
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
	const stream = await streamCdnAssetIfExists(storageService, storageKey);
	if (!stream) {
		Logger.warn({subjectId, storageKey}, `Skipping missing ${label}`);
		return;
	}
	await archive.append(stream.body, {name: archiveName});
}
