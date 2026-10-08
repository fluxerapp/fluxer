// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RuntimeConfigSnapshot, runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';

export const ACCOUNT_STORAGE_KEY_SEPARATOR = '::';
export const ACCOUNT_STORAGE_KEY_MAX_BYTES = 1024;

const TEXT_ENCODER = new TextEncoder();

export interface ParsedAccountStorageKey {
	instanceKey: string;
	userId: string;
}

export interface AccountKeyIdentity {
	userId: string;
	instance?: RuntimeConfigSnapshot;
	storageKey?: string;
}

function accountStorageKeyByteLength(value: string): number {
	return TEXT_ENCODER.encode(value).length;
}

export function accountStorageKeyFromInstanceKey(userId: string, instanceKey: string): string | null {
	if (runtimeInstanceKey({apiEndpoint: instanceKey}) !== instanceKey) {
		return null;
	}
	if (userId.trim().length === 0 || userId.includes('\0') || userId.includes(ACCOUNT_STORAGE_KEY_SEPARATOR)) {
		return null;
	}
	const key = `${instanceKey}${ACCOUNT_STORAGE_KEY_SEPARATOR}${userId}`;
	if (accountStorageKeyByteLength(key) > ACCOUNT_STORAGE_KEY_MAX_BYTES) {
		return null;
	}
	return key;
}

export function accountStorageKey(userId: string, instance: RuntimeConfigSnapshot): string | null {
	const instanceKey = runtimeInstanceKey(instance);
	return instanceKey === null ? null : accountStorageKeyFromInstanceKey(userId, instanceKey);
}

export function parseAccountStorageKey(key: string | null | undefined): ParsedAccountStorageKey | null {
	if (key == null || key.trim().length === 0) {
		return null;
	}
	if (accountStorageKeyByteLength(key) > ACCOUNT_STORAGE_KEY_MAX_BYTES) {
		return null;
	}
	if (key.includes('\0')) {
		return null;
	}
	const separatorIndex = key.lastIndexOf(ACCOUNT_STORAGE_KEY_SEPARATOR);
	if (separatorIndex === -1) {
		return null;
	}
	const instanceKey = key.slice(0, separatorIndex);
	const userId = key.slice(separatorIndex + ACCOUNT_STORAGE_KEY_SEPARATOR.length);
	if (instanceKey.trim().length === 0 || userId.trim().length === 0) {
		return null;
	}
	return {instanceKey, userId};
}

export function deriveAccountStorageKey(account: AccountKeyIdentity): string | null {
	if (account.storageKey != null) {
		const parsed = parseAccountStorageKey(account.storageKey);
		if (parsed !== null && parsed.userId === account.userId) {
			return account.storageKey;
		}
	}
	if (account.instance == null) {
		return null;
	}
	return accountStorageKey(account.userId, account.instance);
}

export function getAccountKey(account: AccountKeyIdentity): string {
	return deriveAccountStorageKey(account) ?? account.userId;
}

export function accountKeyUserId(accountKey: string): string {
	return parseAccountStorageKey(accountKey)?.userId ?? accountKey;
}
