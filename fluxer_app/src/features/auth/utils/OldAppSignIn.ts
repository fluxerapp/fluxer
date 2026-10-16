// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type DomainMigrationInstallKind,
	resolveDomainMigrationSide,
} from '@app/features/app/domain_migration/DomainMigrationCore';

const OLD_APP_SIGN_IN_INSTALL_KINDS: ReadonlySet<DomainMigrationInstallKind> = new Set<DomainMigrationInstallKind>([
	'webkit',
	'chromium-android',
]);

export interface OldAppSignInInput {
	origin: string;
	installKind: DomainMigrationInstallKind;
	hasStoredAccounts: boolean;
}

export function shouldOfferOldAppSignIn({origin, installKind, hasStoredAccounts}: OldAppSignInInput): boolean {
	if (hasStoredAccounts || !OLD_APP_SIGN_IN_INSTALL_KINDS.has(installKind)) {
		return false;
	}
	return resolveDomainMigrationSide(origin)?.role === 'target';
}
