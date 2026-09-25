// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveDomainMigrationSide} from '@app/features/app/domain_migration/DomainMigrationCore';

export function isPasskeyMigrationOrigin(): boolean {
	return resolveDomainMigrationSide(window.location.origin)?.role === 'target';
}

export function rpIdMatchesPage(rpId: string, hostname: string = window.location.hostname): boolean {
	const host = hostname.toLowerCase();
	const normalizedRpId = rpId.toLowerCase();
	return host === normalizedRpId || host.endsWith(`.${normalizedRpId}`);
}
