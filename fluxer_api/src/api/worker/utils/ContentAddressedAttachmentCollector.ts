// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {AttachmentID, ChannelID} from '@app/api/BrandedTypes';
import {makeAttachmentCdnKey} from '@app/api/channel/services/message/MessageHelpers';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import type {ArchiveEntryWriter} from '@app/api/worker/utils/ArchiveFile';
import {streamCdnAssetIfExists} from '@app/api/worker/utils/AssetArchiveHelpers';

interface CollectedAttachment {
	archivePath: string;
	filename: string;
	size: number;
}

interface CollectResult {
	hash: string;
	archivePath: string;
}

interface AttachmentManifestEntry {
	hash: string;
	archivePath: string;
	filename: string;
	size: number;
}

interface CollectParams {
	storageService: IStorageService;
	archive: ArchiveEntryWriter;
	channelId: ChannelID;
	attachmentId: AttachmentID | bigint;
	filename: string;
}

export class ContentAddressedAttachmentCollector {
	private hashIndex = new Map<string, CollectedAttachment>();

	async collect(params: CollectParams): Promise<CollectResult | null> {
		const {storageService, archive, channelId, attachmentId, filename} = params;
		const storageKey = makeAttachmentCdnKey(channelId, attachmentId, filename);
		const STREAM_THRESHOLD = 10 * 1024 * 1024;
		const keyHash = crypto.createHash('sha256').update(storageKey).digest('hex');
		const existingStream = this.hashIndex.get(keyHash);
		if (existingStream) return {hash: keyHash, archivePath: existingStream.archivePath};
		const streamed = await streamCdnAssetIfExists(storageService, storageKey);
		if (!streamed) {
			Logger.warn(
				{channelId: channelId.toString(), attachmentId: attachmentId.toString(), filename},
				'Attachment not found in S3 during archive collection',
			);
			return null;
		}
		if (streamed.contentLength > STREAM_THRESHOLD) {
			const archivePath = `attachments/${keyHash.slice(0, 16)}/${filename}`;
			await archive.append(streamed.body, {name: archivePath});
			this.hashIndex.set(keyHash, {archivePath, filename, size: streamed.contentLength});
			return {hash: keyHash, archivePath};
		}
		const chunks: Array<Buffer> = [];
		for await (const chunk of streamed.body) {
			chunks.push(Buffer.from(chunk));
		}
		const buffer = Buffer.concat(chunks);
		const hash = crypto.createHash('sha256').update(buffer).digest('hex');
		const existing = this.hashIndex.get(hash);
		if (existing) {
			return {hash, archivePath: existing.archivePath};
		}
		const archivePath = `attachments/${hash.slice(0, 16)}/${filename}`;
		await archive.append(buffer, {name: archivePath});
		this.hashIndex.set(hash, {archivePath, filename, size: buffer.length});
		return {hash, archivePath};
	}

	getManifest(): Array<AttachmentManifestEntry> {
		const entries: Array<AttachmentManifestEntry> = [];
		for (const [hash, entry] of this.hashIndex.entries()) {
			entries.push({
				hash,
				archivePath: entry.archivePath,
				filename: entry.filename,
				size: entry.size,
			});
		}
		return entries;
	}
}
