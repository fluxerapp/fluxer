// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {
	AuthSessionStorageKey,
	parseStoredSessionValue,
} from '@app/features/platform/state/auth_session/AuthSessionStorage';
import {GLOBAL_APP_STORAGE_SCOPE} from '@app/features/platform/state/PersistentStorage';

export interface StoredSessionMirror {
	readonly storageKey: string | null;
	readonly userId: string | null;
	readonly token: string | null;
}

export interface StoredSessionCredential {
	readonly userId: string;
	readonly token: string;
}

export type StoredSessionRestorationPointer =
	| {readonly kind: 'none'}
	| {
			readonly kind: 'qualified';
			readonly storageKey: string;
			readonly credential: StoredSessionCredential | null;
	  }
	| {readonly kind: 'legacy-credential'; readonly credential: StoredSessionCredential}
	| {readonly kind: 'invalid'; readonly mirror: StoredSessionMirror};

export interface SessionCredentialMirrorStorage {
	getItem(key: string): string | null;
	setItem(key: string, value: string): void;
	removeItem(key: string): void;
}

export interface SessionCredentialMirror {
	read(): StoredSessionMirror;
	write(mirror: StoredSessionMirror): void;
	persist(mirror: StoredSessionMirror): Promise<void>;
}

export class SessionCredentialMirrorReadError extends Error {
	constructor(cause: unknown) {
		super('The session credential mirror is unreadable', {cause});
		this.name = 'SessionCredentialMirrorReadError';
	}
}

export class DesktopAccountAuthorityError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'DesktopAccountAuthorityError';
	}
}

export function emptyStoredSessionMirror(): StoredSessionMirror {
	return {storageKey: null, userId: null, token: null};
}

function completeStoredSessionCredential(mirror: StoredSessionMirror): StoredSessionCredential | null {
	if (mirror.userId === null || mirror.token === null) {
		return null;
	}
	return {userId: mirror.userId, token: mirror.token};
}

export function parseStoredSessionRestorationPointer(mirror: StoredSessionMirror): StoredSessionRestorationPointer {
	const credential = completeStoredSessionCredential(mirror);
	const hasPartialCredential = (mirror.userId === null) !== (mirror.token === null);
	if (hasPartialCredential) {
		if (mirror.storageKey !== null && mirror.userId === null && mirror.token !== null) {
			const parsedStorageKey = parseAccountStorageKey(mirror.storageKey);
			if (parsedStorageKey === null) {
				return {
					kind: 'legacy-credential',
					credential: {userId: mirror.storageKey, token: mirror.token},
				};
			}
		}
		return {kind: 'invalid', mirror};
	}
	if (mirror.storageKey !== null) {
		const parsedStorageKey = parseAccountStorageKey(mirror.storageKey);
		if (parsedStorageKey !== null) {
			if (credential !== null && credential.userId !== parsedStorageKey.userId) {
				return {kind: 'invalid', mirror};
			}
			return {kind: 'qualified', storageKey: mirror.storageKey, credential};
		}
		if (credential !== null && credential.userId === mirror.storageKey) {
			return {kind: 'legacy-credential', credential};
		}
		return {kind: 'invalid', mirror};
	}
	if (credential !== null) {
		return {kind: 'legacy-credential', credential};
	}
	if (mirror.userId === null && mirror.token === null) {
		return {kind: 'none'};
	}
	return {kind: 'invalid', mirror};
}

export class DurableSessionCredentialMirror implements SessionCredentialMirror {
	constructor(
		private readonly storage: SessionCredentialMirrorStorage,
		private readonly mirrorGatewayPrebootSession: (sessionPresent: boolean) => void,
	) {}

	read(): StoredSessionMirror {
		try {
			return {
				storageKey: this.readValue(AuthSessionStorageKey.ActiveAccountKey),
				token: this.readValue(AuthSessionStorageKey.Token),
				userId: this.readValue(AuthSessionStorageKey.UserId),
			};
		} catch (error) {
			throw new SessionCredentialMirrorReadError(error);
		}
	}

	write(mirror: StoredSessionMirror): void {
		this.writeValue(AuthSessionStorageKey.ActiveAccountKey, mirror.storageKey);
		this.writeValue(AuthSessionStorageKey.Token, mirror.token);
		this.writeValue(AuthSessionStorageKey.UserId, mirror.userId);
		this.mirrorGatewayPrebootSession(mirror.token !== null && mirror.userId !== null);
	}

	async persist(mirror: StoredSessionMirror): Promise<void> {
		this.write(mirror);
		const storage = globalThis.window?.electron?.desktopStorage;
		if (
			storage == null ||
			typeof storage.getStatus !== 'function' ||
			typeof storage.get !== 'function' ||
			typeof storage.set !== 'function' ||
			typeof storage.delete !== 'function'
		) {
			return;
		}
		const status = await storage.getStatus();
		if (!status.available) {
			if (status.authorityExpected) {
				throw new DesktopAccountAuthorityError(
					'The desktop session mirror is unavailable after desktop authority was established',
				);
			}
			return;
		}
		const values: ReadonlyArray<readonly [string, string | null]> = [
			[AuthSessionStorageKey.ActiveAccountKey, mirror.storageKey],
			[AuthSessionStorageKey.Token, mirror.token],
			[AuthSessionStorageKey.UserId, mirror.userId],
		];
		for (const [key, value] of values) {
			if (value === null) {
				await storage.delete(GLOBAL_APP_STORAGE_SCOPE, key);
			} else {
				await storage.set(GLOBAL_APP_STORAGE_SCOPE, key, value);
			}
		}
		for (const [key, value] of values) {
			const stored = await storage.get(GLOBAL_APP_STORAGE_SCOPE, key);
			if ((stored?.value ?? null) !== value) {
				throw new Error(`The desktop session mirror did not persist ${key}`);
			}
		}
	}

	private readValue(key: string): string | null {
		return parseStoredSessionValue(this.storage.getItem(key));
	}

	private writeValue(key: string, value: string | null): void {
		if (value === null) {
			this.storage.removeItem(key);
			return;
		}
		this.storage.setItem(key, value);
	}
}
