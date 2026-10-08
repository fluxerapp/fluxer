// SPDX-License-Identifier: AGPL-3.0-or-later

import accountStorage, {BrowserAccountStorageUnavailableError} from '@app/features/auth/state/AccountStorage';
import type {AppStorageSessionAccount} from '@app/features/platform/state/AppStorageBootstrapContract';
import type {DesktopLegacyImportRequest} from '@app/features/platform/state/DesktopLegacyImport';
import {
	completePendingLegacyAppStorageScope,
	type LegacyAppStorageMigrationRequest,
	type LegacyAppStorageMigrationResult,
	LegacyAppStorageMigrationStatus,
	runLegacyAppStorageMigration,
} from '@app/features/platform/state/LegacyAppStorageMigration';
import {loadLegacyOriginHarvest} from '@app/features/platform/state/LegacyOriginHarvestSource';
import {
	markLegacyReplantComplete,
	replantLegacyMediaDeviceSelections,
	replantLegacyOriginDatabases,
} from '@app/features/platform/state/LegacyOriginReplant';
import {
	createDesktopLegacySessionStore,
	createWebLegacySessionStore,
	type LegacySessionAccountStore,
	type LegacySessionReconciliationResult,
	reconcileLegacySession,
} from '@app/features/platform/state/LegacySessionReconciliation';
import {getPersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {countTelemetryEvent, TelemetryEvent} from '@app/features/platform/utils/AppTelemetry';
import type {DesktopLegacyHarvest} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';

const logger = new Logger('AppStorageMigrationCoordinator');

const AccountStoreReadiness = Object.freeze({
	OPEN: 'open',
	RETRY_REQUIRED: 'retry-required',
} as const);

type AccountStoreReadiness = (typeof AccountStoreReadiness)[keyof typeof AccountStoreReadiness];

async function openAccountStore(): Promise<AccountStoreReadiness> {
	try {
		await accountStorage.init();
		return AccountStoreReadiness.OPEN;
	} catch (error) {
		const unavailable = error instanceof Error ? error : new Error(String(error));
		logger.warn('The account store could not be opened before session resolution', unavailable);
		return AccountStoreReadiness.RETRY_REQUIRED;
	}
}

async function replantLegacyOrigin(harvest: DesktopLegacyHarvest): Promise<boolean> {
	try {
		await replantLegacyOriginDatabases(harvest);
		await replantLegacyMediaDeviceSelections(harvest);
	} catch (error) {
		logger.error('The legacy origin replant failed, retrying on the next launch', error);
		return false;
	}
	return true;
}

async function prepareLegacyOrigin(): Promise<boolean> {
	try {
		const harvest = await loadLegacyOriginHarvest();
		if (harvest === null) {
			return true;
		}
		if (!(await replantLegacyOrigin(harvest))) {
			return false;
		}
		await markLegacyReplantComplete();
		return true;
	} catch (error) {
		logger.error('The legacy origin preparation failed, deferring the desktop import', error);
		return false;
	}
}

function recordMigrationTelemetry(result: LegacyAppStorageMigrationResult | null): void {
	if (result === null || result.status === LegacyAppStorageMigrationStatus.FAILED) {
		countTelemetryEvent(TelemetryEvent.STORAGE_LEGACY_MIGRATION_FAILED);
		return;
	}
	if (result.status === LegacyAppStorageMigrationStatus.MIGRATED) {
		countTelemetryEvent(TelemetryEvent.STORAGE_LEGACY_MIGRATION_OK, 1, result.written);
	} else if (result.status === LegacyAppStorageMigrationStatus.PARTIAL) {
		countTelemetryEvent(TelemetryEvent.STORAGE_LEGACY_MIGRATION_PARTIAL, 1, result.pendingScopes);
	}
	if (result.unclassifiedKeys.length > 0) {
		countTelemetryEvent(
			TelemetryEvent.STORAGE_UNCLASSIFIED_KEYS,
			result.unclassifiedKeys.length,
			result.unclassifiedKeys,
		);
	}
}

function createMigrationRequest(account: AppStorageSessionAccount, now: number): LegacyAppStorageMigrationRequest {
	return {
		backend: getPersistentStorageBackend(),
		accounts: accountStorage,
		currentAccount: {userId: account.userId, token: account.token, instance: account.instance},
		now,
	};
}

async function migrateLegacyStorage(
	request: LegacyAppStorageMigrationRequest,
): Promise<LegacyAppStorageMigrationResult | null> {
	try {
		return await runLegacyAppStorageMigration(request);
	} catch (error) {
		if (error instanceof BrowserAccountStorageUnavailableError) {
			throw error;
		}
		logger.error('The legacy storage migration failed, booting without it', error);
		return null;
	}
}

async function completeBootScope(
	request: LegacyAppStorageMigrationRequest,
	migration: LegacyAppStorageMigrationResult | null,
	scope: string,
): Promise<void> {
	if (migration === null || !migration.pendingScopes.includes(scope)) {
		return;
	}
	try {
		recordMigrationTelemetry(await completePendingLegacyAppStorageScope(request, scope));
	} catch (error) {
		if (error instanceof BrowserAccountStorageUnavailableError) {
			throw error;
		}
		countTelemetryEvent(TelemetryEvent.STORAGE_LEGACY_MIGRATION_FAILED);
		logger.error('Completing the pending legacy storage scope failed', scope, error);
	}
}

async function reconcileSession(
	store: LegacySessionAccountStore,
	currentAccount: AppStorageSessionAccount,
	now: number,
	legacyStorage: Storage | null,
): Promise<LegacySessionReconciliationResult | null> {
	try {
		return await reconcileLegacySession({
			store,
			legacyStorage,
			currentAccount: {
				userId: currentAccount.userId,
				token: currentAccount.token,
				instance: currentAccount.instance,
			},
			now,
		});
	} catch (error) {
		if (error instanceof BrowserAccountStorageUnavailableError) {
			throw error;
		}
		logger.error('Session reconciliation could not run, so the stored session is unchanged', error);
		return null;
	}
}

export class AppStorageMigrationCoordinator {
	private constructor(
		private readonly now: number,
		private readonly accountStoreReadiness: AccountStoreReadiness,
		public readonly legacyOriginReady: boolean,
	) {}

	public static async prepare(now: number): Promise<AppStorageMigrationCoordinator> {
		const legacyOriginReady = await prepareLegacyOrigin();
		const accountStoreReadiness = await openAccountStore();
		return new AppStorageMigrationCoordinator(now, accountStoreReadiness, legacyOriginReady);
	}

	public async requireAccountStore(): Promise<void> {
		if (this.accountStoreReadiness === AccountStoreReadiness.OPEN) {
			return;
		}
		await accountStorage.init();
	}

	public reconcileWebSession(
		currentAccount: AppStorageSessionAccount,
	): Promise<LegacySessionReconciliationResult | null> {
		return reconcileSession(
			createWebLegacySessionStore({
				source: accountStorage,
				backend: getPersistentStorageBackend(),
			}),
			currentAccount,
			this.now,
			getProtectedLocalStorage(),
		);
	}

	public reconcileDesktopSession(
		currentAccount: AppStorageSessionAccount,
		request: DesktopLegacyImportRequest,
	): Promise<LegacySessionReconciliationResult | null> {
		return reconcileSession(
			createDesktopLegacySessionStore({accounts: request.accounts, storage: request.storage}),
			currentAccount,
			this.now,
			request.legacyStorage,
		);
	}

	public async migrateAccountScope(
		account: AppStorageSessionAccount,
		scope: string,
		completeScope: boolean,
	): Promise<LegacyAppStorageMigrationResult | null> {
		if (!this.legacyOriginReady) {
			throw new Error('Legacy app storage migration cannot run before origin preparation');
		}
		const request = createMigrationRequest(account, this.now);
		const migration = await migrateLegacyStorage(request);
		recordMigrationTelemetry(migration);
		if (completeScope) {
			await completeBootScope(request, migration, scope);
		}
		return migration;
	}
}
