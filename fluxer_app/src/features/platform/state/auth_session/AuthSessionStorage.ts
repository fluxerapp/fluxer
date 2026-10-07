// SPDX-License-Identifier: AGPL-3.0-or-later

export const AuthSessionStorageKey = {
	ActiveAccountKey: 'fluxer:auth:active-account-key',
	Token: 'token',
	UserId: 'userId',
} as const;

interface AuthSessionValueReader {
	getItem(key: string): string | null;
}

export function parseStoredSessionValue(value: string | null): string | null {
	if (!value || value === 'undefined' || value === 'null') {
		return null;
	}
	return value;
}

function readStoredSessionValue(storage: AuthSessionValueReader, key: string): string | null {
	try {
		return parseStoredSessionValue(storage.getItem(key));
	} catch {
		return null;
	}
}

export function readStoredSessionUserId(storage: AuthSessionValueReader): string | null {
	return readStoredSessionValue(storage, AuthSessionStorageKey.UserId);
}

export function readStoredSessionToken(storage: AuthSessionValueReader): string | null {
	return readStoredSessionValue(storage, AuthSessionStorageKey.Token);
}

export function readStoredActiveAccountKey(storage: AuthSessionValueReader): string | null {
	return readStoredSessionValue(storage, AuthSessionStorageKey.ActiveAccountKey);
}
