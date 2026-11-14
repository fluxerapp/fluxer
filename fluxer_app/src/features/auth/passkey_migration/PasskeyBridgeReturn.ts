// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MfaChallenge} from '@app/features/auth/state/AuthFlow';
import {getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {PASSKEY_BRIDGE_RETURN_FRAGMENT_KEY} from '@fluxer/constants/src/PasskeyConstants';
import type {PasskeyBridgeLoginStartRequest} from '@fluxer/schema/src/domains/auth/PasskeyBridgeSchemas';

const BRIDGE_TOKEN_PATTERN = /^[A-Za-z0-9_-]{43}$/u;
const STORED_LOGIN_KEY_PREFIX = 'fluxer:passkey-bridge:';
const STORED_LOGIN_MAX_AGE_MS = 10 * 60 * 1000;

export type PasskeyBridgeLoginPurpose = PasskeyBridgeLoginStartRequest['purpose'];

interface StoredPasskeyBridgePageLogin {
	nonce: string;
	purpose: PasskeyBridgeLoginPurpose;
	return_path: string;
	mfa: MfaChallenge | null;
	created_at: number;
}

export interface PasskeyBridgePageLogin {
	nonce: string;
	mfa: MfaChallenge | null;
}

export interface PasskeyBridgeReturn {
	ceremonyId: string;
	completionCode: string;
}

export function readPasskeyBridgeReturn(hash: string): PasskeyBridgeReturn | null {
	const value = new URLSearchParams(hash.startsWith('#') ? hash.slice(1) : hash).get(
		PASSKEY_BRIDGE_RETURN_FRAGMENT_KEY,
	);
	const parts = value?.split('.') ?? [];
	if (parts.length !== 2 || !parts.every((part) => BRIDGE_TOKEN_PATTERN.test(part))) {
		return null;
	}
	return {ceremonyId: parts[0], completionCode: parts[1]};
}

function parseMfaChallenge(value: unknown): MfaChallenge | null {
	const mfa = value as Partial<MfaChallenge> | null;
	if (
		typeof mfa?.ticket !== 'string' ||
		typeof mfa.totp !== 'boolean' ||
		typeof mfa.webauthn !== 'boolean' ||
		typeof mfa.backupCodes !== 'boolean'
	) {
		return null;
	}
	return {ticket: mfa.ticket, totp: mfa.totp, webauthn: mfa.webauthn, backupCodes: mfa.backupCodes};
}

function parseStoredLogin(raw: string | null): StoredPasskeyBridgePageLogin | null {
	if (!raw) {
		return null;
	}
	try {
		const value = JSON.parse(raw) as Partial<StoredPasskeyBridgePageLogin> | null;
		if (
			typeof value?.nonce !== 'string' ||
			(value.purpose !== 'login' && value.purpose !== 'login_mfa') ||
			typeof value.return_path !== 'string' ||
			!value.return_path.startsWith('/') ||
			typeof value.created_at !== 'number'
		) {
			return null;
		}
		const mfa = parseMfaChallenge(value.mfa);
		if ((value.purpose === 'login_mfa') !== (mfa !== null)) {
			return null;
		}
		return {
			nonce: value.nonce,
			purpose: value.purpose,
			return_path: value.return_path,
			mfa,
			created_at: value.created_at,
		};
	} catch {
		return null;
	}
}

function readStoredLogin(ceremonyId: string): StoredPasskeyBridgePageLogin | null {
	try {
		return parseStoredLogin(getProtectedSessionStorage()?.getItem(`${STORED_LOGIN_KEY_PREFIX}${ceremonyId}`) ?? null);
	} catch {
		return null;
	}
}

export function storePasskeyBridgePageLogin(ceremonyId: string, login: StoredPasskeyBridgePageLogin): void {
	const storage = getProtectedSessionStorage();
	if (storage === null) {
		throw new Error('Session storage is unavailable');
	}
	storage.setItem(`${STORED_LOGIN_KEY_PREFIX}${ceremonyId}`, JSON.stringify(login));
}

export function readPasskeyBridgePageLoginReturnPath(ceremonyId: string): string | null {
	return readStoredLogin(ceremonyId)?.return_path ?? null;
}

export function takePasskeyBridgePageLogin(ceremonyId: string): PasskeyBridgePageLogin | null {
	const stored = readStoredLogin(ceremonyId);
	try {
		getProtectedSessionStorage()?.removeItem(`${STORED_LOGIN_KEY_PREFIX}${ceremonyId}`);
	} catch {}
	return stored === null ? null : {nonce: stored.nonce, mfa: stored.mfa};
}

export function prunePasskeyBridgePageLogins(now: number): void {
	try {
		const storage = getProtectedSessionStorage();
		if (storage === null) {
			return;
		}
		const expired: Array<string> = [];
		for (let index = 0; index < storage.length; index++) {
			const key = storage.key(index);
			if (key === null || !key.startsWith(STORED_LOGIN_KEY_PREFIX)) {
				continue;
			}
			const stored = parseStoredLogin(storage.getItem(key));
			if (stored === null || now - stored.created_at > STORED_LOGIN_MAX_AGE_MS) {
				expired.push(key);
			}
		}
		for (const key of expired) {
			storage.removeItem(key);
		}
	} catch {}
}
