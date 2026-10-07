// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type PreparedRuntimeConfig,
	type RuntimeConfigSnapshot,
	runtimeConfigSnapshotsAreSameInstance,
} from '@app/features/app/state/RuntimeConfig';
import type {AccountPresenceIntent, KeyedStoredAccount, StoredAccount} from '@app/features/auth/state/AccountStorage';
import {getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import type {AuthSessionAccountStorage} from '@app/features/platform/state/auth_session/AuthSessionDependencies';
import type {AuthSessionRuntimeCommitter} from '@app/features/platform/state/auth_session/AuthSessionRuntimeCommitter';
import {
	type AuthSessionSnapshot,
	selectAuthSessionAccountKey,
} from '@app/features/platform/state/auth_session/AuthSessionStateMachine';
import type {
	SessionCredentialMirror,
	StoredSessionMirror,
} from '@app/features/platform/state/auth_session/SessionCredentialMirror';
import {UNAUTHENTICATED_APP_STORAGE_SCOPE} from '@app/features/platform/state/PersistentStorage';
import type {UserSettingsAccountTransitionCheckpoint} from '@app/features/user/state/UserSettings';

export class AuthSessionRuntimeUnavailableError extends Error {
	constructor(operation: string) {
		super(`Cannot ${operation} without an active instance runtime`);
		this.name = 'AuthSessionRuntimeUnavailableError';
	}
}

export interface AccountRecordCheckpoint {
	readonly accountKey: string;
	readonly record: StoredAccount | null;
}

export interface SessionCheckpoint {
	session: AuthSessionSnapshot;
	runtime: RuntimeConfigSnapshot | null;
	storageScope: string;
	credentialMirror: StoredSessionMirror;
	presenceIntent: AccountPresenceIntent | null;
	syncedUserSettings: UserSettingsAccountTransitionCheckpoint | null;
	accountRecords: ReadonlyArray<AccountRecordCheckpoint>;
	commitStarted: boolean;
	runtimeActivationAttempted: boolean;
}

export interface AuthSessionCheckpointState {
	readonly capture: () => AuthSessionSnapshot;
	readonly restore: (snapshot: AuthSessionSnapshot) => void;
}

export interface AuthSessionCheckpointDependencies {
	readonly accountStorage: AuthSessionAccountStorage;
	readonly credentialMirror: SessionCredentialMirror;
	readonly getRuntimeSnapshot: () => RuntimeConfigSnapshot | null;
	readonly getStorageScope: () => string;
	readonly resolveAndPrepareRuntimeSnapshot: (snapshot: RuntimeConfigSnapshot) => Promise<PreparedRuntimeConfig>;
	readonly prepareRuntimeSnapshot: (snapshot: RuntimeConfigSnapshot) => Promise<PreparedRuntimeConfig>;
	readonly deactivateRuntime: () => Promise<void>;
	readonly activateStorageScope: (accountKey: string | null) => Promise<void>;
	readonly captureSyncedUserSettingsCheckpoint: () => Promise<UserSettingsAccountTransitionCheckpoint>;
	readonly restoreSyncedUserSettingsCheckpoint: (checkpoint: UserSettingsAccountTransitionCheckpoint) => Promise<void>;
	readonly captureLocalPresenceIntent: () => AccountPresenceIntent | null;
	readonly restoreLocalPresenceIntent: (intent: AccountPresenceIntent | null | undefined) => void;
}

type CheckpointOperation = () => void | Promise<void>;

export class AuthSessionCheckpointManager {
	constructor(
		private readonly dependencies: AuthSessionCheckpointDependencies,
		private readonly runtimeCommitter: AuthSessionRuntimeCommitter,
		private readonly sessionState: AuthSessionCheckpointState,
	) {}

	currentStorageScope(): string {
		return this.dependencies.getStorageScope();
	}

	async activateStorageScope(accountKey: string | null): Promise<void> {
		const label = accountKey ?? UNAUTHENTICATED_APP_STORAGE_SCOPE;
		await this.dependencies.activateStorageScope(accountKey);
		if (this.currentStorageScope() !== label) {
			throw new Error(`Storage scope did not activate ${label}`);
		}
	}

	async prepareAccountScope(
		instance: RuntimeConfigSnapshot,
		accountKey: string,
		checkpoint: SessionCheckpoint,
	): Promise<PreparedRuntimeConfig | null> {
		const previousAccountKey = selectAuthSessionAccountKey(checkpoint.session);
		await this.activateStorageScope(accountKey);
		if (
			previousAccountKey !== null &&
			runtimeConfigSnapshotsAreSameInstance(instance, this.dependencies.getRuntimeSnapshot())
		) {
			return null;
		}
		checkpoint.runtimeActivationAttempted = true;
		return await this.dependencies.resolveAndPrepareRuntimeSnapshot(instance);
	}

	async capture(
		credentialMirror: StoredSessionMirror,
		accountKeys: ReadonlyArray<string> = [],
	): Promise<SessionCheckpoint> {
		return {
			session: this.sessionState.capture(),
			runtime: this.dependencies.getRuntimeSnapshot(),
			storageScope: this.currentStorageScope(),
			credentialMirror,
			presenceIntent: this.dependencies.captureLocalPresenceIntent(),
			syncedUserSettings: null,
			accountRecords: await this.captureAccountRecords(accountKeys),
			commitStarted: false,
			runtimeActivationAttempted: false,
		};
	}

	async refresh(checkpoint: SessionCheckpoint, credentialMirror: StoredSessionMirror): Promise<void> {
		const accountRecords = await this.captureAccountRecords(
			checkpoint.accountRecords.map(({accountKey}) => accountKey),
		);
		checkpoint.session = this.sessionState.capture();
		checkpoint.runtime = this.dependencies.getRuntimeSnapshot();
		checkpoint.storageScope = this.currentStorageScope();
		checkpoint.credentialMirror = credentialMirror;
		checkpoint.presenceIntent = this.dependencies.captureLocalPresenceIntent();
		checkpoint.accountRecords = accountRecords;
		checkpoint.runtimeActivationAttempted = false;
	}

	markCommitStarted(checkpoint: SessionCheckpoint): void {
		checkpoint.commitStarted = true;
	}

	async captureSyncedUserSettings(checkpoint: SessionCheckpoint): Promise<void> {
		checkpoint.syncedUserSettings = await this.dependencies.captureSyncedUserSettingsCheckpoint();
	}

	restoreSessionState(checkpoint: SessionCheckpoint): void {
		this.sessionState.restore(checkpoint.session);
	}

	async runBeforeCommit<Result>(
		checkpoint: SessionCheckpoint,
		operation: () => Promise<Result>,
		label: string,
	): Promise<Result> {
		try {
			return await operation();
		} catch (error) {
			if (!checkpoint.commitStarted) {
				throw error;
			}
			return await this.restore(checkpoint, error, label);
		}
	}

	private async captureAccountRecords(
		accountKeys: ReadonlyArray<string>,
	): Promise<ReadonlyArray<AccountRecordCheckpoint>> {
		const keys = [...new Set(accountKeys)];
		if (keys.length === 0) {
			return [];
		}
		const {records} = await this.dependencies.accountStorage.getAllAccounts();
		const byKey = new Map<string, KeyedStoredAccount>();
		for (const record of records) {
			const storageKey = record.storageKey ?? getAccountKey(record);
			const keyed = {...record, storageKey};
			byKey.set(storageKey, keyed);
			byKey.set(record.userId, keyed);
		}
		return keys.map((accountKey) => {
			const record = byKey.get(accountKey);
			return {
				accountKey,
				record: record === undefined ? null : structuredClone(record),
			};
		});
	}

	private async restore(checkpoint: SessionCheckpoint, error: unknown, operation: string): Promise<never> {
		const recoveryErrors = await this.collectRecoveryErrors(checkpoint);
		if (recoveryErrors.length === 0) {
			throw error;
		}
		const reason = error instanceof Error ? error.message : String(error);
		throw new AggregateError([error, ...recoveryErrors], `Failed to restore the session after ${operation}: ${reason}`);
	}

	private async collectRecoveryErrors(checkpoint: SessionCheckpoint): Promise<Array<unknown>> {
		const restoresStoredSession =
			checkpoint.credentialMirror.token !== null && checkpoint.credentialMirror.userId !== null;
		const operations: Array<CheckpointOperation> = [
			async () => {
				if (checkpoint.runtime == null) {
					if (this.dependencies.getRuntimeSnapshot() !== null) {
						await this.dependencies.deactivateRuntime();
					}
					return;
				}
				if (
					checkpoint.runtimeActivationAttempted ||
					!runtimeConfigSnapshotsAreSameInstance(checkpoint.runtime, this.dependencies.getRuntimeSnapshot())
				) {
					const prepared = await this.dependencies.prepareRuntimeSnapshot(checkpoint.runtime);
					await this.runtimeCommitter.commit(prepared, {
						publishSessionState: () => {},
						rollbackSessionState: () => {},
					});
				}
				if (!runtimeConfigSnapshotsAreSameInstance(checkpoint.runtime, this.dependencies.getRuntimeSnapshot())) {
					throw new Error('Previous runtime did not reactivate');
				}
			},
			async () => {
				await this.dependencies.activateStorageScope(checkpoint.storageScope);
				if (this.currentStorageScope() !== checkpoint.storageScope) {
					throw new Error(`Previous storage scope did not reactivate ${checkpoint.storageScope}`);
				}
				if (
					checkpoint.runtime != null &&
					!runtimeConfigSnapshotsAreSameInstance(checkpoint.runtime, this.dependencies.getRuntimeSnapshot())
				) {
					throw new Error('Runtime changed while restoring the previous storage scope');
				}
			},
			async () => {
				for (const account of checkpoint.accountRecords) {
					if (account.record === null) {
						continue;
					}
					const runtime = checkpoint.runtime ?? account.record.instance;
					if (runtime == null) {
						throw new AuthSessionRuntimeUnavailableError(`restore account ${account.accountKey}`);
					}
					await this.dependencies.accountStorage.upsertAccount(account.record, runtime);
				}
			},
			async () => {
				if (checkpoint.syncedUserSettings === null) {
					return;
				}
				if (this.currentStorageScope() !== checkpoint.storageScope) {
					throw new Error('Previous storage scope is unavailable for UserSettings restoration');
				}
				if (
					checkpoint.runtime != null &&
					!runtimeConfigSnapshotsAreSameInstance(checkpoint.runtime, this.dependencies.getRuntimeSnapshot())
				) {
					throw new Error('Previous runtime is unavailable for UserSettings restoration');
				}
				await this.dependencies.restoreSyncedUserSettingsCheckpoint(checkpoint.syncedUserSettings);
			},
			async () => {
				if (restoresStoredSession) {
					await this.dependencies.credentialMirror.persist(checkpoint.credentialMirror);
				}
			},
			() => this.dependencies.restoreLocalPresenceIntent(checkpoint.presenceIntent),
			() => this.restoreSessionState(checkpoint),
		];
		return await this.collectOperationErrors(operations);
	}

	private async collectOperationErrors(operations: ReadonlyArray<CheckpointOperation>): Promise<Array<unknown>> {
		const errors: Array<unknown> = [];
		for (const operation of operations) {
			try {
				await operation();
			} catch (error) {
				errors.push(error);
			}
		}
		return errors;
	}
}
