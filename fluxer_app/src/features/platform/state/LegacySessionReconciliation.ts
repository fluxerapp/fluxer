// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RuntimeConfigSnapshot, runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import type {AccountRekeyTarget, KeyedStoredAccount, StoredAccountList} from '@app/features/auth/state/AccountStorage';
import {BrowserAccountStorageUnavailableError} from '@app/features/auth/state/AccountStorage';
import {accountStorageKey, getAccountKey} from '@app/features/auth/state/AccountStorageKey';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {
	readStoredSessionToken,
	readStoredSessionUserId,
} from '@app/features/platform/state/auth_session/AuthSessionStorage';
import {requireDesktopAccountRecord} from '@app/features/platform/state/DesktopAccountStorageAccess';
import {GLOBAL_APP_STORAGE_SCOPE} from '@app/features/platform/state/PersistentStorage';
import type {PersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {writeRawStorageItem} from '@app/features/platform/state/PrebootMirror';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {DesktopAccountStorageAPI} from '@fluxer/desktop_ipc/src/AccountContract';
import type {DesktopStorageAPI} from '@fluxer/desktop_ipc/src/StorageContract';
import {DESKTOP_LEGACY_SESSION_MARKER_KEY} from '@fluxer/desktop_ipc/src/StorageContract';

const logger = new Logger('LegacySessionReconciliation');

export interface LegacySessionCredentials {
	readonly userId: string;
	readonly token: string;
}

export interface LegacySessionAccountStore {
	findAccountByCredentials(
		credentials: LegacySessionCredentials,
		instance: RuntimeConfigSnapshot,
	): Promise<KeyedStoredAccount | null>;
	readActiveAccountKey(): Promise<string | null>;
	setActiveAccountKey(storageKey: string, reconciledAt: number): Promise<void>;
}

export const LegacySessionReconciliationOutcome = Object.freeze({
	UNCHANGED: 'unchanged',
	SKIPPED: 'skipped',
} as const);

export type LegacySessionReconciliationOutcome =
	(typeof LegacySessionReconciliationOutcome)[keyof typeof LegacySessionReconciliationOutcome];

export const LegacySessionReconciliationSkipReason = Object.freeze({
	NO_LOCAL_STORAGE: 'no-local-storage',
	NO_INSTANCE_KEY: 'no-instance-key',
	NO_LIVE_SESSION: 'no-live-session',
	SESSION_MISMATCH: 'session-mismatch',
	ACCOUNT_NOT_QUALIFIED: 'account-not-qualified',
	FAILED: 'failed',
} as const);

export type LegacySessionReconciliationSkipReason =
	(typeof LegacySessionReconciliationSkipReason)[keyof typeof LegacySessionReconciliationSkipReason];

export interface LegacySessionReconciliationRequest {
	readonly store: LegacySessionAccountStore;
	readonly legacyStorage: Storage | null;
	readonly currentAccount: AccountRekeyTarget;
	readonly now: number;
}

export interface LegacySessionReconciliationResult {
	readonly outcome: LegacySessionReconciliationOutcome;
	readonly storageKey: string | null;
	readonly reason: LegacySessionReconciliationSkipReason | null;
}

export class LegacySessionAccountsNotAuthoritativeError extends Error {
	public constructor(source: string) {
		super(`The stored account list is not authoritative (source: ${source})`);
		this.name = 'LegacySessionAccountsNotAuthoritativeError';
	}
}

export interface WebLegacySessionAccountSource {
	getAllAccounts(): Promise<StoredAccountList>;
}

export function readLegacySessionCredentials(storage: Storage | null): LegacySessionCredentials | null {
	if (storage == null) {
		return null;
	}
	const userId = readStoredSessionUserId(storage);
	const token = readStoredSessionToken(storage);
	if (userId === null || token === null) {
		return null;
	}
	return {userId, token};
}

export function matchLegacySessionAccount(
	accounts: ReadonlyArray<KeyedStoredAccount>,
	credentials: LegacySessionCredentials,
	instance: RuntimeConfigSnapshot,
): KeyedStoredAccount | null {
	const storageKey = accountStorageKey(credentials.userId, instance);
	if (storageKey === null) {
		return null;
	}
	const exactCurrent = accounts.filter(
		(account) =>
			account.userId === credentials.userId && account.token === credentials.token && account.storageKey === storageKey,
	);
	if (exactCurrent.length === 1) {
		return exactCurrent[0];
	}
	if (exactCurrent.length > 1) {
		throw new Error(`The current instance has multiple stored accounts for ${credentials.userId}`);
	}
	return null;
}

export function writeActiveAccountKeyMirror(storageKey: string): void {
	writeRawStorageItem(AppStorageKey.AUTH_ACCOUNT_KEY, storageKey);
}

export function createDesktopLegacySessionStore(options: {
	readonly accounts: DesktopAccountStorageAPI;
	readonly storage: DesktopStorageAPI;
}): LegacySessionAccountStore {
	return {
		async findAccountByCredentials(credentials, instance) {
			const accounts = (await options.accounts.getAll()).map(requireDesktopAccountRecord);
			return matchLegacySessionAccount(accounts, credentials, instance);
		},

		async readActiveAccountKey() {
			return (await options.storage.get(GLOBAL_APP_STORAGE_SCOPE, AppStorageKey.AUTH_ACCOUNT_KEY))?.value ?? null;
		},

		async setActiveAccountKey(storageKey, reconciledAt) {
			await options.storage.set(GLOBAL_APP_STORAGE_SCOPE, AppStorageKey.AUTH_ACCOUNT_KEY, storageKey);
			writeActiveAccountKeyMirror(storageKey);
			await options.storage.setMarker(DESKTOP_LEGACY_SESSION_MARKER_KEY, String(reconciledAt));
		},
	};
}

export function createWebLegacySessionStore(options: {
	readonly source: WebLegacySessionAccountSource;
	readonly backend: PersistentStorageBackend;
}): LegacySessionAccountStore {
	return {
		async findAccountByCredentials(credentials, instance) {
			const {records, source} = await options.source.getAllAccounts();
			if (source !== 'idb') {
				throw new LegacySessionAccountsNotAuthoritativeError(source);
			}
			return matchLegacySessionAccount(
				records.map((account) => ({...account, storageKey: getAccountKey(account)})),
				credentials,
				instance,
			);
		},

		async readActiveAccountKey() {
			return (await options.backend.get(GLOBAL_APP_STORAGE_SCOPE, AppStorageKey.AUTH_ACCOUNT_KEY))?.value ?? null;
		},

		async setActiveAccountKey(storageKey) {
			await options.backend.set(GLOBAL_APP_STORAGE_SCOPE, AppStorageKey.AUTH_ACCOUNT_KEY, storageKey);
			writeActiveAccountKeyMirror(storageKey);
		},
	};
}

function skipped(reason: LegacySessionReconciliationSkipReason): LegacySessionReconciliationResult {
	return {outcome: LegacySessionReconciliationOutcome.SKIPPED, storageKey: null, reason};
}

export async function reconcileLegacySession(
	request: LegacySessionReconciliationRequest,
): Promise<LegacySessionReconciliationResult> {
	const instance = request.currentAccount.instance;
	if (request.legacyStorage == null) {
		return skipped(LegacySessionReconciliationSkipReason.NO_LOCAL_STORAGE);
	}
	if (runtimeInstanceKey(instance) === null) {
		return skipped(LegacySessionReconciliationSkipReason.NO_INSTANCE_KEY);
	}
	const live = readLegacySessionCredentials(request.legacyStorage);
	if (live === null) {
		return skipped(LegacySessionReconciliationSkipReason.NO_LIVE_SESSION);
	}
	if (live.userId !== request.currentAccount.userId || live.token !== request.currentAccount.token) {
		return skipped(LegacySessionReconciliationSkipReason.SESSION_MISMATCH);
	}
	const liveStorageKey = accountStorageKey(live.userId, instance);
	let matchingAccount: KeyedStoredAccount | null;
	try {
		if (liveStorageKey !== null && (await request.store.readActiveAccountKey()) === liveStorageKey) {
			return {outcome: LegacySessionReconciliationOutcome.UNCHANGED, storageKey: liveStorageKey, reason: null};
		}
		matchingAccount = await request.store.findAccountByCredentials(live, instance);
	} catch (error) {
		if (error instanceof BrowserAccountStorageUnavailableError) {
			throw error;
		}
		logger.error('Session reconciliation failed, so the stored session is unchanged', error);
		return skipped(LegacySessionReconciliationSkipReason.FAILED);
	}
	if (matchingAccount === null) {
		return skipped(LegacySessionReconciliationSkipReason.ACCOUNT_NOT_QUALIFIED);
	}
	const storageKey = matchingAccount.storageKey;
	try {
		await request.store.setActiveAccountKey(storageKey, request.now);
		return {outcome: LegacySessionReconciliationOutcome.UNCHANGED, storageKey, reason: null};
	} catch (error) {
		if (error instanceof BrowserAccountStorageUnavailableError) {
			throw error;
		}
		logger.error('Session reconciliation failed, so the stored session is unchanged', error);
		return skipped(LegacySessionReconciliationSkipReason.FAILED);
	}
}
