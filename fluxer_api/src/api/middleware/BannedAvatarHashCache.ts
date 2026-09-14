// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {BANNED_AVATAR_HASHES_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import {Logger} from '@app/api/Logger';
import {RefreshSubscription} from '@app/api/utils/RefreshSubscription';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

function stripAnimationPrefix(hash: string): string {
	return hash.startsWith('a_') ? hash.substring(2) : hash;
}

class BannedAvatarHashCache {
	private banned: Set<string> = new Set();
	private adminRepository = new AdminRepository();
	private kvClient: IKVProvider | null = null;
	private consecutiveFailures = 0;
	private readonly maxConsecutiveFailures = 5;
	private readonly refreshSubscription = new RefreshSubscription({
		name: 'avatar-hash blocklist cache',
		channels: [BANNED_AVATAR_HASHES_REFRESH_CHANNEL],
		refresh: () => this.refresh(),
		onRefreshError: (err) => {
			this.consecutiveFailures++;
			const message = err instanceof Error ? err.message : String(err);
			if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
				Logger.error({error: message}, 'Failed to refresh avatar-hash blocklist cache after notification');
			} else {
				Logger.warn({error: message}, 'Failed to refresh avatar-hash blocklist cache after notification');
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
		const rows = await this.adminRepository.loadAllBannedAvatarHashes();
		const next = new Set<string>();
		for (const row of rows) {
			if (row.hash_short) next.add(stripAnimationPrefix(row.hash_short.toLowerCase()));
		}
		this.banned = next;
		this.consecutiveFailures = 0;
		Logger.debug({count: next.size}, 'Avatar-hash blocklist cache refreshed');
	}

	contains(hashShort: string): boolean {
		return this.banned.has(stripAnimationPrefix(hashShort.toLowerCase()));
	}

	add(hashShort: string): void {
		this.banned.add(stripAnimationPrefix(hashShort.toLowerCase()));
	}

	remove(hashShort: string): void {
		this.banned.delete(stripAnimationPrefix(hashShort.toLowerCase()));
	}

	get size(): number {
		return this.banned.size;
	}

	resetForTesting(): void {
		void this.shutdown().catch((error) => {
			Logger.error({error}, 'Failed to shut down avatar-hash blocklist cache');
		});
		this.banned = new Set();
		this.kvClient = null;
		this.consecutiveFailures = 0;
	}

	shutdown(): Promise<void> {
		return this.refreshSubscription.stop();
	}
}

export const bannedAvatarHashCache = new BannedAvatarHashCache();
