// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import UserSettings from '@app/features/user/state/UserSettings';
import {makeAutoObservable} from 'mobx';

const BUILT_IN_TRUST_PATTERNS = [
	'fluxer.app',
	'*.fluxer.app',
	'fluxer.com',
	'*.fluxer.com',
	'fluxerstatus.com',
	'*.fluxerstatus.com',
	'fluxer.gg',
	'fluxer.gift',
] as const;
const TRUST_EVERYTHING_PATTERN = '*';
const LEGACY_TRUSTED_DOMAINS_KEY = 'TrustedDomain';

type DomainPattern = (typeof BUILT_IN_TRUST_PATTERNS)[number] | string;

function toComparableHost(hostname: string): string {
	return hostname.trim().toLowerCase().replace(/\.$/, '');
}

function coversHost(pattern: DomainPattern, hostname: string): boolean {
	const normalizedPattern = toComparableHost(pattern);
	if (normalizedPattern === TRUST_EVERYTHING_PATTERN) return true;
	if (!normalizedPattern.startsWith('*.')) return hostname === normalizedPattern;
	const baseDomain = normalizedPattern.slice(2);
	return hostname === baseDomain || hostname.endsWith(`.${baseDomain}`);
}

function uniqueDomains(domains: ReadonlyArray<string>): Array<string> {
	return Array.from(new Set(domains));
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function configuredTrustedDomainPatterns(): Array<string> {
	return uniqueDomains([RuntimeConfig.mediaEndpoint, RuntimeConfig.staticCdnEndpoint].flatMap(domainPatternFromUrl));
}

function domainPatternFromUrl(endpoint: string): Array<string> {
	if (!endpoint) return [];
	try {
		const hostname = new URL(endpoint).hostname;
		return hostname ? [hostname] : [];
	} catch {
		return [];
	}
}

function readLegacyTrustedDomains(): Array<string> | null {
	const raw = AppStorage.getItem(LEGACY_TRUSTED_DOMAINS_KEY);
	if (raw == null) return null;
	try {
		const decoded: unknown = JSON.parse(raw);
		if (!isRecord(decoded)) return [];
		if (!Array.isArray(decoded.trustedDomains)) return [];
		return decoded.trustedDomains.filter((domain): domain is string => typeof domain === 'string');
	} catch {
		return [];
	}
}

function addDomain(current: ReadonlyArray<string>, domain: string): Array<string> {
	if (current.includes(domain)) return [...current];
	return [...current, domain];
}

function removeDomain(current: ReadonlyArray<string>, domain: string): Array<string> {
	return current.filter((storedDomain) => storedDomain !== domain);
}

class TrustedDomainState {
	private legacyMigrationDone = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get trustedDomains(): ReadonlyArray<string> {
		return UserSettings.getTrustedDomains();
	}

	get trustAllDomains(): boolean {
		return UserSettings.trustAllDomains();
	}

	async checkAndMigrateLegacyData(): Promise<void> {
		if (this.legacyMigrationDone) return;
		this.legacyMigrationDone = true;
		const legacyDomains = readLegacyTrustedDomains();
		if (legacyDomains == null || legacyDomains.length === 0 || this.trustedDomains.length > 0) return;
		await UserSettings.saveSettings({trustedDomains: uniqueDomains(legacyDomains)});
		AppStorage.removeItem(LEGACY_TRUSTED_DOMAINS_KEY);
	}

	async addTrustedDomain(domain: string): Promise<void> {
		if (this.trustAllDomains) return;
		const nextDomains = addDomain(this.trustedDomains, domain);
		if (nextDomains.length === this.trustedDomains.length) return;
		await UserSettings.saveSettings({trustedDomains: nextDomains});
	}

	async removeTrustedDomain(domain: string): Promise<void> {
		const nextDomains = removeDomain(this.trustedDomains, domain);
		if (nextDomains.length === this.trustedDomains.length) return;
		await UserSettings.saveSettings({trustedDomains: nextDomains});
	}

	async clearAllTrustedDomains(): Promise<void> {
		await UserSettings.saveSettings({trustedDomains: []});
	}

	async setTrustAllDomains(trustAll: boolean): Promise<void> {
		await UserSettings.saveSettings({trustedDomains: trustAll ? [TRUST_EVERYTHING_PATTERN] : []});
	}

	isTrustedDomain(hostname: string): boolean {
		if (this.trustAllDomains) return true;
		const normalizedHostname = toComparableHost(hostname);
		const currentHostname = toComparableHost(globalThis.location?.hostname ?? '');
		if (currentHostname !== '' && normalizedHostname === currentHostname) return true;
		const patterns = [...BUILT_IN_TRUST_PATTERNS, ...configuredTrustedDomainPatterns(), ...this.trustedDomains];
		return patterns.some((pattern) => coversHost(pattern, normalizedHostname));
	}

	getTrustedDomains(): ReadonlyArray<string> {
		return this.trustedDomains;
	}

	getTrustedDomainsCount(): number {
		if (this.trustAllDomains) return 0;
		return this.trustedDomains.length;
	}
}

export default new TrustedDomainState();
