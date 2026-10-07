// SPDX-License-Identifier: AGPL-3.0-or-later

import {accountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import type {
	AppStorageBootstrapHandle,
	AppStorageFinalizationResult,
	AppStorageSessionAccount,
} from '@app/features/platform/state/AppStorageBootstrapContract';
import type {DesktopStorageAuthority} from '@app/features/platform/state/AppStorageDesktopAuthority';
import type {AppStorageMigrationCoordinator} from '@app/features/platform/state/AppStorageMigrationCoordinator';
import {LegacySessionReconciliationSkipReason} from '@app/features/platform/state/LegacySessionReconciliation';
import {
	activateAppStorageScope,
	UNAUTHENTICATED_APP_STORAGE_SCOPE,
} from '@app/features/platform/state/PersistentStorage';
import type {PersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {Logger} from '@app/features/platform/utils/AppLogger';

interface CommittedStorageFinalizationContext {
	readonly kind: 'committed';
	readonly desktopAuthority: DesktopStorageAuthority;
}

interface MigratingStorageFinalizationContext {
	readonly kind: 'migrating';
	readonly desktopAuthority: DesktopStorageAuthority;
	readonly migration: AppStorageMigrationCoordinator;
	readonly now: number;
	readonly webBackend: PersistentStorageBackend;
}

type StorageFinalizationContext = CommittedStorageFinalizationContext | MigratingStorageFinalizationContext;

interface MigratingStorageFinalizationOptions {
	readonly desktopAuthority: DesktopStorageAuthority;
	readonly migration: AppStorageMigrationCoordinator;
	readonly now: number;
	readonly webBackend: PersistentStorageBackend;
}

type FinalizationBinding =
	| {readonly kind: 'unbound'}
	| {
			readonly kind: 'bound';
			readonly account: AppStorageSessionAccount | null;
			readonly result: Promise<AppStorageFinalizationResult>;
	  };

class AppStorageSessionAccountMismatchError extends Error {
	public constructor(accountKey: string) {
		super(`AuthSession supplied an invalid app-storage account context for ${accountKey}`);
		this.name = 'AppStorageSessionAccountMismatchError';
	}
}

class AppStorageFinalizationConflictError extends Error {
	public constructor() {
		super('App storage was finalized for a different AuthSession account');
		this.name = 'AppStorageFinalizationConflictError';
	}
}

class AppStorageSessionChangedDuringFinalizationError extends Error {
	public constructor() {
		super('The AuthSession credential mirror changed during app-storage finalization');
		this.name = 'AppStorageSessionChangedDuringFinalizationError';
	}
}

function requireQualifiedSessionAccount(account: AppStorageSessionAccount): void {
	if (accountStorageKey(account.userId, account.instance) !== account.accountKey) {
		throw new AppStorageSessionAccountMismatchError(account.accountKey);
	}
}

function sameSessionAccount(left: AppStorageSessionAccount | null, right: AppStorageSessionAccount | null): boolean {
	if (left === null || right === null) {
		return left === right;
	}
	return (
		left.accountKey === right.accountKey &&
		left.userId === right.userId &&
		left.token === right.token &&
		accountStorageKey(left.userId, left.instance) === accountStorageKey(right.userId, right.instance)
	);
}

export class AppStorageFinalizationSession implements AppStorageBootstrapHandle {
	private binding: FinalizationBinding = {kind: 'unbound'};

	private constructor(private readonly context: StorageFinalizationContext) {}

	public static forCommittedAuthority(desktopAuthority: DesktopStorageAuthority): AppStorageFinalizationSession {
		if (!desktopAuthority.isCommitted) {
			throw new Error('Committed app-storage finalization requires a committed desktop authority');
		}
		return new AppStorageFinalizationSession({kind: 'committed', desktopAuthority});
	}

	public static forMigration(options: MigratingStorageFinalizationOptions): AppStorageFinalizationSession {
		if (options.desktopAuthority.isCommitted) {
			throw new Error('Migrating app-storage finalization cannot replace a committed desktop authority');
		}
		return new AppStorageFinalizationSession({kind: 'migrating', ...options});
	}

	public finalizeAfterSessionResolution(
		account: AppStorageSessionAccount | null,
	): Promise<AppStorageFinalizationResult> {
		if (account !== null) {
			requireQualifiedSessionAccount(account);
		}
		if (this.binding.kind === 'bound') {
			if (!sameSessionAccount(this.binding.account, account)) {
				throw new AppStorageFinalizationConflictError();
			}
			return this.binding.result;
		}
		const result = this.finalize(account);
		this.binding = {kind: 'bound', account, result};
		return result;
	}

	private async finalize(account: AppStorageSessionAccount | null): Promise<AppStorageFinalizationResult> {
		if (account === null) {
			return this.finalizeSignedOut();
		}
		return this.finalizeAccount(account);
	}

	private async finalizeSignedOut(): Promise<AppStorageFinalizationResult> {
		await activateAppStorageScope(null);
		Logger.refreshGlobalLogLevel();
		return {
			scope: UNAUTHENTICATED_APP_STORAGE_SCOPE,
			migration: null,
			desktopImport: this.context.desktopAuthority.committedResult,
			desktopBackendInstalled: this.context.desktopAuthority.isCommitted,
			reconciliation: null,
		};
	}

	private async finalizeAccount(account: AppStorageSessionAccount): Promise<AppStorageFinalizationResult> {
		const scope = account.accountKey;
		if (this.context.kind === 'migrating') {
			await this.context.migration.requireAccountStore();
		}
		await activateAppStorageScope(scope);
		if (this.context.kind === 'committed') {
			Logger.refreshGlobalLogLevel();
			return {
				scope,
				migration: null,
				desktopImport: this.context.desktopAuthority.committedResult,
				desktopBackendInstalled: true,
				reconciliation: null,
			};
		}

		const {desktopAuthority, migration, now, webBackend} = this.context;
		let reconciliation = await migration.reconcileWebSession(account);
		if (reconciliation?.reason === LegacySessionReconciliationSkipReason.SESSION_MISMATCH) {
			throw new AppStorageSessionChangedDuringFinalizationError();
		}
		const desktopImport = migration.legacyOriginReady
			? await desktopAuthority.stageImport(account, now, webBackend)
			: null;
		let desktopBackendInstalled = false;
		if (desktopImport?.candidate != null) {
			const desktopReconciliation = await migration.reconcileDesktopSession(account, desktopImport.candidate.request);
			desktopBackendInstalled = await desktopImport.candidate.commitIfVerified(desktopReconciliation);
			if (desktopBackendInstalled) {
				reconciliation = desktopReconciliation;
			}
		}

		const migrationResult = migration.legacyOriginReady
			? await migration.migrateAccountScope(account, scope, !desktopBackendInstalled)
			: null;
		await activateAppStorageScope(scope);
		Logger.refreshGlobalLogLevel();
		return {
			scope,
			migration: migrationResult,
			desktopImport: desktopImport?.result ?? null,
			desktopBackendInstalled,
			reconciliation,
		};
	}
}
