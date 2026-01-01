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

import {makeAutoObservable} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';

const IMPLICITLY_TRUSTED_DOMAINS = [
	'fluxer.app',
	'*.fluxer.app',
	'fluxer.gg',
	'fluxer.gift',
	'fluxerusercontent.com',
	'fluxerstatic.com',
] as const;

const getCurrentHostname = (): string | undefined => {
	if (typeof location === 'undefined') {
		return undefined;
	}
	return location.hostname;
};

class TrustedDomainStore {
	trustedDomains: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'TrustedDomainStore', ['trustedDomains']);
	}

	addTrustedDomain(domain: string): void {
		if (this.trustedDomains.includes(domain)) {
			return;
		}

		this.trustedDomains = [...this.trustedDomains, domain];
	}

	removeTrustedDomain(domain: string): void {
		if (!this.trustedDomains.includes(domain)) {
			return;
		}

		this.trustedDomains = this.trustedDomains.filter((d) => d !== domain);
	}

	isTrustedDomain(hostname: string): boolean {
		const currentHostname = getCurrentHostname();
		if (currentHostname && hostname === currentHostname) {
			return true;
		}

		for (const pattern of IMPLICITLY_TRUSTED_DOMAINS) {
			if (this.matchesDomainPattern(hostname, pattern)) {
				return true;
			}
		}

		return this.trustedDomains.some((pattern) => this.matchesDomainPattern(hostname, pattern));
	}

	private matchesDomainPattern(hostname: string, pattern: string): boolean {
		if (pattern.startsWith('*.')) {
			const baseDomain = pattern.slice(2);
			return hostname === baseDomain || hostname.endsWith(`.${baseDomain}`);
		}
		return hostname === pattern;
	}

	getTrustedDomains(): ReadonlyArray<string> {
		return this.trustedDomains;
	}
}

export default new TrustedDomainStore();
