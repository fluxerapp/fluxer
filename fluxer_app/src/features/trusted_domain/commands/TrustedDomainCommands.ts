// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import TrustedDomain from '@app/features/trusted_domain/state/TrustedDomain';

const logger = new Logger('TrustedDomain');

export async function addTrustedDomain(domain: string): Promise<void> {
	logger.debug(`Adding trusted domain: ${domain}`);
	await TrustedDomain.addTrustedDomain(domain);
}

export async function clearAllTrustedDomains(): Promise<void> {
	logger.debug('Clearing all trusted domains');
	await TrustedDomain.clearAllTrustedDomains();
}

export async function setTrustAllDomains(trustAll: boolean): Promise<void> {
	logger.debug(`Setting trust all domains: ${trustAll}`);
	await TrustedDomain.setTrustAllDomains(trustAll);
}
