// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AppStorageBootstrapHandle} from '@app/features/platform/state/AppStorageBootstrapContract';
import {
	DesktopStorageAuthority,
	DesktopStorageAuthorityUnavailableError,
} from '@app/features/platform/state/AppStorageDesktopAuthority';
import {AppStorageFinalizationSession} from '@app/features/platform/state/AppStorageFinalizationSession';
import {AppStorageMigrationCoordinator} from '@app/features/platform/state/AppStorageMigrationCoordinator';
import {activateAppStorageScope, initializeAppStorage} from '@app/features/platform/state/PersistentStorage';
import {getPersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {Logger} from '@app/features/platform/utils/AppLogger';

export type {
	AppStorageBootstrapHandle,
	AppStorageFinalizationResult,
	AppStorageSessionAccount,
} from '@app/features/platform/state/AppStorageBootstrapContract';
export {DesktopStorageAuthorityUnavailableError};

async function initializeSignedOutStorage(options?: {readonly scoped?: boolean}): Promise<void> {
	await initializeAppStorage(options);
	await activateAppStorageScope(null);
	Logger.refreshGlobalLogLevel();
}

export async function startAppStorage(options?: {readonly scoped?: boolean}): Promise<AppStorageBootstrapHandle> {
	const now = Date.now();
	const desktopAuthority = await DesktopStorageAuthority.prepareBoot();
	if (desktopAuthority.isCommitted) {
		await initializeSignedOutStorage(options);
		return AppStorageFinalizationSession.forCommittedAuthority(desktopAuthority);
	}

	const migration = await AppStorageMigrationCoordinator.prepare(now);
	const webBackend = getPersistentStorageBackend();
	await initializeSignedOutStorage(options);
	return AppStorageFinalizationSession.forMigration({
		desktopAuthority,
		migration,
		now,
		webBackend,
	});
}
