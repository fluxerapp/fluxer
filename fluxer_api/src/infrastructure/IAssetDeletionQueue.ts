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

export interface QueuedAssetDeletion {
	s3Key: string;
	cdnUrl: string | null;
	reason: string;
	queuedAt?: number;
	retryCount?: number;
}

export interface DeletionQueueProcessResult {
	deleted: number;
	requeued: number;
	failed: number;
	remaining: number;
}

export interface IAssetDeletionQueue {
	queueDeletion(item: Omit<QueuedAssetDeletion, 'queuedAt' | 'retryCount'>): Promise<void>;

	queueCloudflarePurge(cdnUrl: string): Promise<void>;

	getBatch(count: number): Promise<Array<QueuedAssetDeletion>>;

	requeueItem(item: QueuedAssetDeletion): Promise<void>;

	getQueueSize(): Promise<number>;

	clear(): Promise<void>;
}
