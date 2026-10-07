// SPDX-License-Identifier: AGPL-3.0-or-later

export type QueuedAssetEntityType = 'user' | 'guild' | 'guild_member' | 'instance' | 'persona';
export type QueuedAssetType = 'avatar' | 'banner' | 'icon' | 'splash' | 'embed_splash' | 'branding';

export interface QueuedAssetReference {
	entityType: QueuedAssetEntityType;
	assetType: QueuedAssetType;
	entityId: string;
	guildId?: string;
	hash: string;
}

export interface QueuedAssetDeletion {
	s3Key: string;
	cdnUrl: string | null;
	reason: string;
	staleReference?: QueuedAssetReference;
	queuedAt?: number;
	retryCount?: number;
}

export interface IAssetDeletionQueue {
	queueDeletion(item: Omit<QueuedAssetDeletion, 'queuedAt' | 'retryCount'>): Promise<void>;
	queueCdnPurge(cdnUrl: string): Promise<void>;
	getBatch(count: number): Promise<Array<QueuedAssetDeletion>>;
	requeueItem(item: QueuedAssetDeletion): Promise<void>;
	getQueueSize(): Promise<number>;
	clear(): Promise<void>;
}
