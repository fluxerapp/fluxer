// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {Config} from '@app/api/Config';
import {BANNED_URL_DOMAINS_REFRESH_CHANNEL, BANNED_URLS_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import {RISK_S3_KEYS, readLinesFromS3} from '@app/api/risk/RiskBlocklistS3';
import {RefreshSubscription} from '@app/api/utils/RefreshSubscription';
import {canonicalizeUrl} from '@app/api/utils/UrlNormalizer';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

class UrlBlocklistCache {
	private exactUrls: Set<string> = new Set();
	private blockedDomains: Set<string> = new Set();
	private adminRepository = new AdminRepository();
	private kvClient: IKVProvider | null = null;
	private storageService: IStorageService | null = null;
	private consecutiveFailures = 0;
	private readonly maxConsecutiveFailures = 5;
	private readonly refreshSubscription = new RefreshSubscription({
		name: 'URL blocklist cache',
		channels: [BANNED_URLS_REFRESH_CHANNEL, BANNED_URL_DOMAINS_REFRESH_CHANNEL],
		refresh: () => this.refresh(),
		onRefreshError: (err) => {
			this.consecutiveFailures++;
			const message = err instanceof Error ? err.message : String(err);
			if (this.consecutiveFailures >= this.maxConsecutiveFailures) {
				Logger.error({error: message}, 'Failed to refresh URL blocklist cache after notification');
			} else {
				Logger.warn({error: message}, 'Failed to refresh URL blocklist cache after notification');
			}
		},
	});

	setRefreshSubscriber(kvClient: IKVProvider | null): void {
		this.kvClient = kvClient;
	}

	setStorageService(storageService: IStorageService | null): void {
		this.storageService = storageService;
	}

	initialize(): Promise<void> {
		return this.refreshSubscription.start(this.kvClient);
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
		if (!this.storageService || !Config.blocklistFeeds.enabled) return new Set();
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
		void this.shutdown().catch((error) => {
			Logger.error({error}, 'Failed to shut down URL blocklist cache');
		});
		this.exactUrls = new Set();
		this.blockedDomains = new Set();
		this.kvClient = null;
		this.storageService = null;
		this.consecutiveFailures = 0;
	}

	shutdown(): Promise<void> {
		return this.refreshSubscription.stop();
	}
}

export const urlBlocklistCache = new UrlBlocklistCache();
