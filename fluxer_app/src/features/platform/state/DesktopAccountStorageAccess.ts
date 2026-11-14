// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import type {KeyedStoredAccount, StoredAccount} from '@app/features/auth/state/AccountStorageContract';
import {accountStorageKey, parseAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {type DesktopStoreAccessOptions, resolveDesktopStoreAPI} from '@app/features/platform/state/DesktopStoreAccess';
import type {DesktopAccountRecord, DesktopAccountStorageAPI} from '@fluxer/desktop_ipc/src/AccountContract';

const DESKTOP_ACCOUNT_METHODS = [
	'getAll',
	'get',
	'upsert',
	'compareAndSwap',
	'delete',
	'import',
	'prune',
] as const satisfies ReadonlyArray<keyof DesktopAccountStorageAPI>;

export function getDesktopAccountStorageAPI(options?: DesktopStoreAccessOptions): DesktopAccountStorageAPI | null {
	return resolveDesktopStoreAPI((electron) => electron.desktopAccounts, DESKTOP_ACCOUNT_METHODS, options);
}

export function requireDesktopAccountRecord(account: DesktopAccountRecord): KeyedStoredAccount {
	const {storageKey} = account;
	if (typeof storageKey !== 'string') {
		throw new Error('Desktop account storage key is not a string');
	}
	const parsedKey = parseAccountStorageKey(storageKey);
	if (parsedKey === null) {
		throw new Error(`Desktop account ${storageKey} has an unusable storage key`);
	}
	const value: unknown = account.record;
	if (value == null || typeof value !== 'object' || Array.isArray(value)) {
		throw new Error(`Desktop account ${storageKey} is not an object`);
	}
	const record = value as StoredAccount;
	if (typeof record.userId !== 'string' || record.userId.length === 0 || parsedKey.userId !== record.userId) {
		throw new Error(`Desktop account ${storageKey} has a mismatched user ID`);
	}
	if (record.storageKey !== undefined && record.storageKey !== storageKey) {
		throw new Error(`Desktop account ${storageKey} carries a mismatched record key`);
	}
	if (record.instance == null || typeof record.instance !== 'object') {
		throw new Error(`Desktop account ${storageKey} has no usable instance`);
	}
	if (accountStorageKey(record.userId, record.instance as RuntimeConfigSnapshot) !== storageKey) {
		throw new Error(`Desktop account ${storageKey} carries a mismatched instance`);
	}
	return {...record, storageKey};
}
