// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider, IKVSubscription} from '@pkgs/kv_client/src/IKVProvider';
import {AdminRepository} from '../admin/AdminRepository';
import {BANNED_URL_DOMAINS_REFRESH_CHANNEL, BANNED_URLS_REFRESH_CHANNEL} from '../constants/ContentModeration';
import type {IStorageService} from '../infrastructure/IStorageService';
import {Logger} from '../Logger';
import {RISK_S3_KEYS, readLinesFromS3} from '../risk/RiskBlocklistS3';
import {canonicalizeUrl} from '../utils/UrlNormalizer';

class UrlBlocklistCache {
	private exactUrls: Set<string> = new Set();
	private blockedDomains: Set<string> = new Set();
	private isInitialized = false;
	private adminRepository = new AdminRepository();
	private kvClient: IKVProvider | null = null;
	private storageService: IStorageService | null = null;
	private kvSubscription: IKVSubscription | null = null;
	private subscriberInitialized = false;
	private messageHandler: ((channel: string) => void) | null = null;
	private consecutiveFailures = 0;
	private readonly maxConsecutiveFailures = 5;

	setRefreshSubscriber(kvClient: IKVProvider | null): void {
		this.kvClient = kvClient;
	}

	setStorageService(storageService: IStorageService | null): void {
		this.storageService = storageService;
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
			if (channel === BANNED_URLS_REFRESH_CHANNEL || channel === BANNED_URL_DOMAINS_REFRESH_CHANNEL) {
				this.refresh().catch((err) => {
					this.consecutiveFailures++;
					const message = err instanceof Error ? err.message : String(err);
					if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
						Logger.error({error: message}, 'Failed to refresh URL blocklist cache after notification');
					} else {
						Logger.warn({error: message}, 'Failed to refresh URL blocklist cache after notification');
					}
				});
			}
		};
		subscription
			.connect()
			.then(() => subscription.subscribe(BANNED_URLS_REFRESH_CHANNEL))
			.then(() => subscription.subscribe(BANNED_URL_DOMAINS_REFRESH_CHANNEL))
			.then(() => {
				if (this.messageHandler) {
					subscription.on('message', this.messageHandler);
				}
			})
			.catch((error) => {
				Logger.error({error}, 'Failed to subscribe to URL blocklist refresh channels');
			});
		this.subscriberInitialized = true;
	}

	async refresh(): Promise<void> {
		const [manualUrls, domains, feedUrls] = await Promise.all([
			this.adminRepository.loadAllBannedUrls(),
			this.adminRepository.loadAllBannedUrlDomains(),
			this.loadFeedUrls(),
		]);
		const nextUrls = feedUrls;
		for (const row of manualUrls) {
			if (row.url_canonical) nextUrls.add(row.url_canonical.toLowerCase());
		}
		const nextDomains = new Set<string>();
		for (const row of domains) {
			nextDomains.add(row.domain.toLowerCase());
		}
		this.exactUrls = nextUrls;
		this.blockedDomains = nextDomains;
		this.consecutiveFailures = 0;
		Logger.debug(
			{urls: nextUrls.size, domains: nextDomains.size, feedUrls: feedUrls.size},
			'URL blocklist cache refreshed',
		);
	}

	private async loadFeedUrls(): Promise<Set<string>> {
		if (!this.storageService) return new Set();
		const lines = await readLinesFromS3(this.storageService, RISK_S3_KEYS.feedUrls);
		return new Set(lines);
	}

	isUrlBanned(rawUrl: string): boolean {
		const canonical = canonicalizeUrl(rawUrl);
		if (!canonical) return false;
		return this.exactUrls.has(canonical);
	}

	isUrlOrDomainBanned(rawUrl: string): boolean {
		const canonical = canonicalizeUrl(rawUrl);
		if (!canonical) return false;
		if (this.exactUrls.has(canonical)) return true;
		let host: string;
		try {
			host = new URL(canonical).hostname;
		} catch {
			return false;
		}
		return this.isHostnameBanned(host);
	}

	isHostnameBanned(host: string): boolean {
		return this.blockedDomains.has(host.toLowerCase());
	}

	addExactUrl(canonical: string): void {
		this.exactUrls.add(canonical.toLowerCase());
	}

	removeExactUrl(canonical: string): void {
		this.exactUrls.delete(canonical.toLowerCase());
	}

	addDomain(domain: string): void {
		this.blockedDomains.add(domain.toLowerCase());
	}

	removeDomain(domain: string): void {
		this.blockedDomains.delete(domain.toLowerCase());
	}

	get size(): {
		urls: number;
		domains: number;
	} {
		return {
			urls: this.exactUrls.size,
			domains: this.blockedDomains.size,
		};
	}

	resetForTesting(): void {
		this.shutdown();
		this.exactUrls = new Set();
		this.blockedDomains = new Set();
		this.kvClient = null;
		this.storageService = null;
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

export const urlBlocklistCache = new UrlBlocklistCache();
