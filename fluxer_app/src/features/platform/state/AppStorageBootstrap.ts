// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AppStorageBootstrapHandle} from '@app/features/platform/state/AppStorageBootstrapContract';
import {DesktopStorageAuthority} from '@app/features/platform/state/AppStorageDesktopAuthority';
import {AppStorageFinalizationSession} from '@app/features/platform/state/AppStorageFinalizationSession';
import {AppStorageMigrationCoordinator} from '@app/features/platform/state/AppStorageMigrationCoordinator';
import {activateAppStorageScope, initializeAppStorage} from '@app/features/platform/state/PersistentStorage';
import {getPersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {Logger} from '@app/features/platform/utils/AppLogger';

export type {
	AppStorageBootstrapHandle,
	AppStorageSessionAccount,
} from '@app/features/platform/state/AppStorageBootstrapContract';

async function initializeSignedOutStorage(): Promise<void> {
	await initializeAppStorage();
	await activateAppStorageScope(null);
	Logger.refreshGlobalLogLevel();
}

export async function startAppStorage(): Promise<AppStorageBootstrapHandle> {
	const now = Date.now();
	const desktopAuthority = await DesktopStorageAuthority.prepareBoot();
	if (desktopAuthority.isCommitted) {
		await initializeSignedOutStorage();
		return AppStorageFinalizationSession.forCommittedAuthority(desktopAuthority);
	}

	const migration = await AppStorageMigrationCoordinator.prepare(now);
	const webBackend = getPersistentStorageBackend();
	await initializeSignedOutStorage();
	return AppStorageFinalizationSession.forMigration({
		desktopAuthority,
		migration,
		now,
		webBackend,
	});
}
