// SPDX-License-Identifier: AGPL-3.0-or-later

import {type EngineAccountRecord, parseEngineAccountRecord} from '@electron/engine/HostProtocol';
import type {DesktopAccountRecord} from '@fluxer/desktop_ipc/src/AccountContract';
import {ACCOUNT_ENGINE_LIMITS, accountEngineInstanceKey} from '@fluxer/desktop_ipc/src/AccountEngineContract';

export interface AccountEngineAccountLabel {
	readonly accountKey: string;
	readonly displayName: string;
	readonly instanceHost: string;
	readonly switcherPosition: number | null;
}

export interface AccountEngineAccountEntry {
	readonly record: EngineAccountRecord;
	readonly label: AccountEngineAccountLabel;
	readonly apiCodeVersion: number;
}

function asRecord(value: unknown): Record<string, unknown> | null {
	return typeof value === 'object' && value !== null && !Array.isArray(value)
		? (value as Record<string, unknown>)
		: null;
}

function nonEmptyString(value: unknown): string | null {
	return typeof value === 'string' && value.trim().length > 0 ? value : null;
}

function hostOf(endpoint: string): string {
	try {
		return new URL(endpoint).host;
	} catch {
		return endpoint;
	}
}

function parseAccountEngineEntry(stored: DesktopAccountRecord): AccountEngineAccountEntry | null {
	const record = asRecord(stored.record);
	const instance = asRecord(record?.instance);
	const instanceKey = accountEngineInstanceKey(stored.storageKey);
	if (record === null || instance === null || instanceKey === null) {
		return null;
	}
	const apiCodeVersion = instance.apiCodeVersion;
	if (typeof apiCodeVersion !== 'number' || !Number.isSafeInteger(apiCodeVersion) || apiCodeVersion <= 0) {
		return null;
	}
	const engineRecord = parseEngineAccountRecord({
		accountKey: stored.storageKey,
		userId: record.userId,
		instanceKey,
		token: record.token,
		gatewayEndpoint: instance.gatewayEndpoint,
		apiEndpoint: instance.apiEndpoint,
		isValid: record.isValid !== false,
		lastActive: typeof record.lastActive === 'number' && Number.isFinite(record.lastActive) ? record.lastActive : 0,
	});
	if (engineRecord === null) {
		return null;
	}
	const userData = asRecord(record.userData);
	const displayName =
		nonEmptyString(userData?.global_name) ??
		nonEmptyString(userData?.globalName) ??
		nonEmptyString(userData?.username) ??
		engineRecord.userId;
	return {
		record: engineRecord,
		label: {
			accountKey: engineRecord.accountKey,
			displayName,
			instanceHost: hostOf(nonEmptyString(instance.webAppEndpoint) ?? engineRecord.apiEndpoint),
			switcherPosition:
				typeof record.switcherPosition === 'number' && Number.isSafeInteger(record.switcherPosition)
					? record.switcherPosition
					: null,
		},
		apiCodeVersion,
	};
}

export function parseAccountEngineEntries(
	stored: ReadonlyArray<DesktopAccountRecord>,
): Array<AccountEngineAccountEntry> {
	const entries: Array<AccountEngineAccountEntry> = [];
	for (const record of stored) {
		const entry = parseAccountEngineEntry(record);
		if (entry !== null) {
			entries.push(entry);
		}
	}
	return entries;
}

export function parseProvidedAccountEngineEntries(value: unknown): Array<AccountEngineAccountEntry> | null {
	if (!Array.isArray(value) || value.length > ACCOUNT_ENGINE_LIMITS.providedAccountsMax) {
		return null;
	}
	const records: Array<DesktopAccountRecord> = [];
	for (const entry of value) {
		if (typeof entry !== 'object' || entry === null || Array.isArray(entry)) {
			return null;
		}
		const {storageKey, record} = entry as {storageKey?: unknown; record?: unknown};
		if (typeof storageKey !== 'string' || typeof record !== 'object' || record === null || Array.isArray(record)) {
			return null;
		}
		records.push({storageKey, record: record as DesktopAccountRecord['record']});
	}
	return parseAccountEngineEntries(records);
}

export function rotateStoredAccountToken(
	stored: DesktopAccountRecord,
	previousToken: string,
	token: string,
): DesktopAccountRecord | null {
	const record = asRecord(stored.record);
	if (record === null || record.token !== previousToken) {
		return null;
	}
	return {storageKey: stored.storageKey, record: {...stored.record, token}};
}

export function rendererHoldsAccount(provided: ReadonlyArray<AccountEngineAccountEntry>, accountKey: string): boolean {
	return provided.some((entry) => entry.record.accountKey === accountKey);
}
