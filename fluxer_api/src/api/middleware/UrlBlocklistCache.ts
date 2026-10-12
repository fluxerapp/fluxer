// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {BLOCKLIST_S3_KEYS, readLinesFromS3} from '@app/api/blocklist/BlocklistS3';
import {Config} from '@app/api/Config';
import {BANNED_URL_DOMAINS_REFRESH_CHANNEL, BANNED_URLS_REFRESH_CHANNEL} from '@app/api/constants/ContentModeration';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import {Logger} from '@app/api/Logger';
import {RefreshedCache} from '@app/api/utils/RefreshedCache';
import {UrlHostRuleSet} from '@app/api/utils/UrlHostRules';
import {canonicalizeUrl, extractLinkHosts, extractUrlCandidates} from '@app/api/utils/UrlNormalizer';

class UrlBlocklistCache extends RefreshedCache {
	constructor() {
		super({name: 'URL blocklist cache', channels: [BANNED_URLS_REFRESH_CHANNEL, BANNED_URL_DOMAINS_REFRESH_CHANNEL]});
	}

	private exactUrls: Set<string> = new Set();
	private hostRules = new UrlHostRuleSet();
	private adminRepository = new AdminRepository();
	private storageService: IStorageService | null = null;
	setStorageService(storageService: IStorageService | null): void {
		this.storageService = storageService;
	}

	protected async load(): Promise<void> {
		const [manualUrls, domains, feedUrls] = await Promise.all([
			this.adminRepository.loadAllBannedUrls(),
			this.adminRepository.loadAllBannedUrlDomains(),
			this.loadFeedUrls(),
		]);
		const nextUrls = feedUrls;
		for (const row of manualUrls) {
			if (row.url_canonical) nextUrls.add(row.url_canonical.toLowerCase());
		}
		const nextHostRules = new UrlHostRuleSet();
		for (const row of domains) {
			nextHostRules.add(row.domain, row.match_subdomains ?? true);
		}
		this.exactUrls = nextUrls;
		this.hostRules = nextHostRules;

		Logger.debug(
			{urls: nextUrls.size, ...nextHostRules.size, feedUrls: feedUrls.size},
			'URL blocklist cache refreshed',
		);
	}

	private async loadFeedUrls(): Promise<Set<string>> {
		if (!this.storageService || !Config.blocklistFeeds.enabled) return new Set();
		const lines = await readLinesFromS3(this.storageService, BLOCKLIST_S3_KEYS.feedUrls);
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
		return this.hostRules.matches(host);
	}

	containsBannedLink(text: string): boolean {
		for (const url of extractUrlCandidates(text)) {
			if (this.isUrlOrDomainBanned(url)) return true;
		}
		for (const host of extractLinkHosts(text)) {
			if (this.isHostnameBanned(host)) return true;
		}
		return false;
	}

	addExactUrl(canonical: string): void {
		this.exactUrls.add(canonical.toLowerCase());
	}

	removeExactUrl(canonical: string): void {
		this.exactUrls.delete(canonical.toLowerCase());
	}

	addDomain(domain: string, matchSubdomains = true): void {
		this.hostRules.add(domain, matchSubdomains);
	}

	removeDomain(domain: string): void {
		this.hostRules.remove(domain);
	}

	get size(): {
		urls: number;
		domains: number;
		patterns: number;
	} {
		return {
			urls: this.exactUrls.size,
			...this.hostRules.size,
		};
	}

	resetForTesting(): void {
		this.resetRefreshState();
		this.exactUrls = new Set();
		this.hostRules = new UrlHostRuleSet();

		this.storageService = null;
	}
}

export const urlBlocklistCache = new UrlBlocklistCache();
