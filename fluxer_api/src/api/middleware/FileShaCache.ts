// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider, IKVSubscription} from '@pkgs/kv_client/src/IKVProvider';
import {AdminRepository} from '../admin/AdminRepository';
import {BANNED_FILE_SHAS_REFRESH_CHANNEL} from '../constants/ContentModeration';
import {Logger} from '../Logger';

class FileShaCache {
	private banned: Set<string> = new Set();
	private isInitialized = false;
	private adminRepository = new AdminRepository();
	private kvClient: IKVProvider | null = null;
	private kvSubscription: IKVSubscription | null = null;
	private subscriberInitialized = false;
	private messageHandler: ((channel: string) => void) | null = null;
	private consecutiveFailures = 0;
	private readonly maxConsecutiveFailures = 5;

	setRefreshSubscriber(kvClient: IKVProvider | null): void {
		this.kvClient = kvClient;
	}

	async initialize(): Promise<void> {
		if (this.isInitialized) return;
		await this.refresh();
		this.isInitialized = true;
		this.setupSubscriber();
	}

	private setupSubscriber(): void {
		if (this.subscriberInitialized || !this.kvClient) return;
		const subscription = this.kvClient.duplicate();
		this.kvSubscription = subscription;
		this.messageHandler = (channel: string) => {
			if (channel === BANNED_FILE_SHAS_REFRESH_CHANNEL) {
				this.refresh().catch((err) => {
					this.consecutiveFailures++;
					const message = err instanceof Error ? err.message : String(err);
					if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
						Logger.error({error: message}, 'Failed to refresh file-SHA blocklist cache after notification');
					} else {
						Logger.warn({error: message}, 'Failed to refresh file-SHA blocklist cache after notification');
					}
				});
			}
		};
		subscription
			.connect()
			.then(() => subscription.subscribe(BANNED_FILE_SHAS_REFRESH_CHANNEL))
			.then(() => {
				if (this.messageHandler) {
					subscription.on('message', this.messageHandler);
				}
			})
			.catch((error) => {
				Logger.error({error}, 'Failed to subscribe to file-SHA blocklist refresh channel');
			});
		this.subscriberInitialized = true;
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
		this.shutdown();
		this.banned = new Set();
		this.kvClient = null;
		this.subscriberInitialized = false;
		this.consecutiveFailures = 0;
		this.isInitialized = false;
	}

	shutdown(): void {
		if (this.kvSubscription && this.messageHandler) {
			this.kvSubscription.off('message', this.messageHandler);
		}
		if (this.kvSubscription) {
			this.kvSubscription.disconnect();
			this.kvSubscription = null;
		}
		this.messageHandler = null;
	}
}

export const fileShaCache = new FileShaCache();
