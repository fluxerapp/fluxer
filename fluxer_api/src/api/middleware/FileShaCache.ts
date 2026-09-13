// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {BANNED_FILE_SHAS_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import {Logger} from '@app/api/Logger';
import {RefreshSubscription} from '@app/api/utils/RefreshSubscription';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

class FileShaCache {
	private banned: Set<string> = new Set();
	private adminRepository = new AdminRepository();
	private kvClient: IKVProvider | null = null;
	private consecutiveFailures = 0;
	private readonly maxConsecutiveFailures = 5;
	private readonly refreshSubscription = new RefreshSubscription({
		name: 'file-SHA blocklist cache',
		channels: [BANNED_FILE_SHAS_REFRESH_CHANNEL],
		refresh: () => this.refresh(),
		onRefreshError: (err) => {
			this.consecutiveFailures++;
			const message = err instanceof Error ? err.message : String(err);
			if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
				Logger.error({error: message}, 'Failed to refresh file-SHA blocklist cache after notification');
			} else {
				Logger.warn({error: message}, 'Failed to refresh file-SHA blocklist cache after notification');
			}
		},
	});

	setRefreshSubscriber(kvClient: IKVProvider | null): void {
		this.kvClient = kvClient;
	}

	initialize(): Promise<void> {
		return this.refreshSubscription.start(this.kvClient);
	}

	async refresh(): Promise<void> {
		const rows = await this.adminRepository.loadAllBannedFileShas();
		const next = new Set<string>();
		for (const row of rows) {
			if (row.sha256_hex) next.add(row.sha256_hex.toLowerCase());
		}
		this.banned = next;
		this.consecutiveFailures = 0;
		Logger.debug({count: next.size}, 'File-SHA blocklist cache refreshed');
	}

	isBanned(sha256Hex: string): boolean {
		return this.banned.has(sha256Hex.toLowerCase());
	}

	add(sha256Hex: string): void {
		this.banned.add(sha256Hex.toLowerCase());
	}

	remove(sha256Hex: string): void {
		this.banned.delete(sha256Hex.toLowerCase());
	}

	get size(): number {
		return this.banned.size;
	}

	resetForTesting(): void {
		void this.shutdown().catch((error) => {
			Logger.error({error}, 'Failed to shut down file-SHA blocklist cache');
		});
		this.banned = new Set();
		this.kvClient = null;
		this.consecutiveFailures = 0;
	}

	shutdown(): Promise<void> {
		return this.refreshSubscription.stop();
	}
}

export const fileShaCache = new FileShaCache();
