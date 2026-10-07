// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
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

function addDomain(current: ReadonlyArray<string>, domain: string): Array<string> {
	if (current.includes(domain)) return [...current];
	return [...current, domain];
}

class TrustedDomainState {
	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get trustedDomains(): ReadonlyArray<string> {
		return UserSettings.getTrustedDomains();
	}

	get trustAllDomains(): boolean {
		return UserSettings.trustAllDomains();
	}

	async addTrustedDomain(domain: string): Promise<void> {
		if (this.trustAllDomains) return;
		const nextDomains = addDomain(this.trustedDomains, domain);
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

	getTrustedDomainsCount(): number {
		if (this.trustAllDomains) return 0;
		return this.trustedDomains.length;
	}
}

export default new TrustedDomainState();
