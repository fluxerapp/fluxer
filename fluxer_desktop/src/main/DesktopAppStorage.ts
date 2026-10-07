// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import fsPromises from 'node:fs/promises';
import path from 'node:path';
import type {AppStoreBoundary, AppStoreBoundaryFactory} from '@electron/main/AppStoreNativeBoundary';
import {desktopAppStoreStateFile} from '@electron/main/DesktopAppStoreHealth';
import type {
	DesktopAccountCompareAndSwapRequest,
	DesktopAccountImportReport,
	DesktopAccountPruneReport,
	DesktopAccountRecord,
	DesktopStoreJSONObject,
	DesktopStoreMarker,
	DesktopStoreSkippedRecord,
} from '@fluxer/desktop_ipc/src/AccountContract';
import type {DesktopKnownInstanceRecord} from '@fluxer/desktop_ipc/src/KnownInstanceContract';
import type {
	DesktopStorageEntry,
	DesktopStorageImportReport,
	DesktopStorageScopedEntry,
	DesktopStoreQuarantineReason,
	DesktopStoreStatus,
} from '@fluxer/desktop_ipc/src/StorageContract';
import {
	DESKTOP_LEGACY_AUTHORITY_MARKER_KEY,
	DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE,
} from '@fluxer/desktop_ipc/src/StorageContract';

export const DESKTOP_APP_STORE_FILE_NAME = 'desktop-app-store.sqlite3';
export const DESKTOP_APP_STORE_DERIVATION_VERSION = '1';

const ENTRY_STORE = 'app';
const SCHEMA_VERSION_MARKER = 'schema.version';
const DESKTOP_AUTHORITY_SENTINEL_FILE_NAME = 'desktop-app-authority-v1';

const PENDING_OPERATIONS_MAX = 64;
const PENDING_INPUT_BYTES_MAX = 128 * 1024 * 1024;
const PENDING_RESPONSE_BYTES_MAX = 128 * 1024 * 1024;
const NATIVE_JSON_MAX_BYTES = 64 * 1024 * 1024;
const LOOKUP_KEY_MAX_BYTES = 1024;
const ENTRY_VALUE_MAX_BYTES = 8 * 1024 * 1024;
const MARKER_VALUE_MAX_BYTES = 64 * 1024;
const RECORD_MAX_BYTES = 8 * 1024 * 1024;
const BATCH_ENTRIES_MAX = 65_536;
const BATCH_RECORDS_MAX = 1024;
const BATCH_KEYS_MAX = 65_536;

interface DesktopAppStorageDependencies {
	readonly userDataPath: string;
	readonly createBoundary: AppStoreBoundaryFactory;
	readonly describeLoadFailure?: () => string | null;
}

interface DesktopAppStorageOpenResult {
	readonly status: DesktopStoreStatus;
	readonly storeFile: string;
	readonly quarantinedFile: string | null;
}

interface DesktopAppStorageRecoveryResult {
	readonly status: DesktopStoreStatus;
	readonly reseedRequired: boolean;
}

export class DesktopAppStoreRequestError extends TypeError {
	public constructor(context: string, reason: string) {
		super(`Desktop app store request is invalid: ${context} ${reason}`);
		this.name = 'DesktopAppStoreRequestError';
	}
}

export class DesktopAppStoreUnavailableError extends Error {
	public constructor(reason: string) {
		super(`Desktop app store is unavailable: ${reason}`);
		this.name = 'DesktopAppStoreUnavailableError';
	}
}

export class DesktopAppStoreShutdownError extends Error {
	public constructor() {
		super('Desktop app store is shutting down');
		this.name = 'DesktopAppStoreShutdownError';
	}
}

export class DesktopAppStoreCapacityError extends Error {
	public constructor(resource: string, limit: number) {
		super(`Desktop app store ${resource} is limited to ${limit}`);
		this.name = 'DesktopAppStoreCapacityError';
	}
}

export class DesktopAppStoreWriteRefusedError extends Error {
	public constructor(refusals: ReadonlyArray<string>) {
		super(`Desktop app store refused ${refusals.length} write(s): ${refusals.join('; ')}`);
		this.name = 'DesktopAppStoreWriteRefusedError';
	}
}

function byteLength(value: string): number {
	return Buffer.byteLength(value, 'utf8');
}

function describeError(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

function requireString(value: unknown, context: string): string {
	if (typeof value !== 'string') {
		throw new DesktopAppStoreRequestError(context, 'must be a string');
	}
	return value;
}

function requireBoundedString(value: unknown, context: string, maxBytes: number): string {
	const text = requireString(value, context);
	if (byteLength(text) > maxBytes) {
		throw new DesktopAppStoreRequestError(context, `must be at most ${maxBytes} bytes`);
	}
	return text;
}

function requireLookupKey(value: unknown, context: string): string {
	const text = requireBoundedString(value, context, LOOKUP_KEY_MAX_BYTES);
	if (text.length === 0 || text.includes('\0')) {
		throw new DesktopAppStoreRequestError(context, 'must be a non-empty NUL-free string');
	}
	return text;
}

function requireFiniteNumber(value: unknown, context: string): number {
	if (typeof value !== 'number' || !Number.isFinite(value)) {
		throw new DesktopAppStoreRequestError(context, 'must be a finite number');
	}
	return value;
}

function requireBoolean(value: unknown, context: string): boolean {
	if (typeof value !== 'boolean') {
		throw new DesktopAppStoreRequestError(context, 'must be a boolean');
	}
	return value;
}

function requireArray(value: unknown, context: string, maxLength: number): Array<unknown> {
	if (!Array.isArray(value)) {
		throw new DesktopAppStoreRequestError(context, 'must be an array');
	}
	if (value.length > maxLength) {
		throw new DesktopAppStoreRequestError(context, `must hold at most ${maxLength} items`);
	}
	return value;
}

function requireRecord(value: unknown, context: string): Record<string, unknown> {
	if (value == null || typeof value !== 'object' || Array.isArray(value)) {
		throw new DesktopAppStoreRequestError(context, 'must be an object');
	}
	return value as Record<string, unknown>;
}

function requireJSONObject(value: unknown, context: string, maxBytes: number): DesktopStoreJSONObject {
	const record = requireRecord(value, context);
	let encoded: string | undefined;
	try {
		encoded = JSON.stringify(record);
	} catch {
		encoded = undefined;
	}
	if (encoded == null) {
		throw new DesktopAppStoreRequestError(context, 'must be JSON serialisable');
	}
	if (byteLength(encoded) > maxBytes) {
		throw new DesktopAppStoreRequestError(context, `must be at most ${maxBytes} bytes`);
	}
	return record as DesktopStoreJSONObject;
}

function requireMarker(value: unknown, context: string): DesktopStoreMarker | null {
	if (value == null) {
		return null;
	}
	const record = requireRecord(value, context);
	return {
		key: requireLookupKey(record.key, `${context}.key`),
		value: requireBoundedString(record.value, `${context}.value`, MARKER_VALUE_MAX_BYTES),
	};
}

function requireAccountRecord(value: unknown, context: string): DesktopAccountRecord {
	const record = requireRecord(value, context);
	return {
		storageKey: requireLookupKey(record.storageKey, `${context}.storageKey`),
		record: requireJSONObject(record.record, `${context}.record`, RECORD_MAX_BYTES),
	};
}

function requireKnownInstanceRecord(value: unknown, context: string): DesktopKnownInstanceRecord {
	const record = requireJSONObject(value, context, RECORD_MAX_BYTES);
	requireLookupKey(record.instanceKey, `${context}.instanceKey`);
	requireBoundedString(record.domain, `${context}.domain`, LOOKUP_KEY_MAX_BYTES);
	requireBoundedString(record.displayName, `${context}.displayName`, LOOKUP_KEY_MAX_BYTES);
	requireFiniteNumber(record.lastUsed, `${context}.lastUsed`);
	return record as unknown as DesktopKnownInstanceRecord;
}

function readAccountRecord(value: unknown): DesktopStoreJSONObject | null {
	if (value == null || typeof value !== 'object' || Array.isArray(value)) {
		return null;
	}
	return value as DesktopStoreJSONObject;
}

function readKnownInstanceRecord(value: unknown): DesktopKnownInstanceRecord | null {
	if (value == null || typeof value !== 'object' || Array.isArray(value)) {
		return null;
	}
	const record = value as Record<string, unknown>;
	if (typeof record.instanceKey !== 'string' || typeof record.domain !== 'string') {
		return null;
	}
	if (typeof record.displayName !== 'string' || typeof record.lastUsed !== 'number') {
		return null;
	}
	return record as unknown as DesktopKnownInstanceRecord;
}

const UNOPENED_STATUS: DesktopStoreStatus = Object.freeze({
	available: false,
	authorityExpected: false,
	schemaVersion: 0,
	quarantined: false,
	quarantineReason: null,
	unavailableReason: 'the desktop app store has not been opened',
});

interface DesktopAppStoreOperation<T> {
	readonly inputBytes: number;
	readonly maximumResponseBytes: number;
	readonly operation: (boundary: AppStoreBoundary) => Promise<T>;
}

interface PendingEntryWrite {
	readonly scope: string;
	readonly key: string;
	readonly value: string;
	readonly ifAbsent: boolean;
}

export class DesktopAppStorage {
	private readonly storeFile: string;
	private readonly createBoundary: AppStoreBoundaryFactory;
	private boundary: AppStoreBoundary | null = null;
	private status: DesktopStoreStatus = UNOPENED_STATUS;
	private accepting = false;
	private tail: Promise<unknown> = Promise.resolve();
	private readonly pending = new Set<Promise<unknown>>();
	private pendingInputBytes = 0;
	private pendingResponseBytes = 0;
	private stamp = 0;
	private authorityExpected: boolean;
	private readonly authoritySentinelPath: string;
	private readonly describeLoadFailure: () => string | null;

	public constructor(dependencies: DesktopAppStorageDependencies) {
		this.storeFile = path.join(dependencies.userDataPath, DESKTOP_APP_STORE_FILE_NAME);
		this.createBoundary = dependencies.createBoundary;
		this.describeLoadFailure = dependencies.describeLoadFailure ?? (() => null);
		this.authoritySentinelPath = path.join(dependencies.userDataPath, DESKTOP_AUTHORITY_SENTINEL_FILE_NAME);
		this.authorityExpected =
			fs.existsSync(this.authoritySentinelPath) || fs.existsSync(desktopAppStoreStateFile(dependencies.userDataPath));
		this.status = {...UNOPENED_STATUS, authorityExpected: this.authorityExpected};
	}

	public open(): DesktopAppStorageOpenResult {
		if (this.boundary != null) {
			return {status: this.status, storeFile: this.storeFile, quarantinedFile: null};
		}
		let opened: AppStoreBoundary | null = null;
		try {
			opened = this.createBoundary({
				path: this.storeFile,
				derivationVersion: DESKTOP_APP_STORE_DERIVATION_VERSION,
			});
		} catch (error) {
			this.status = {
				...UNOPENED_STATUS,
				authorityExpected: this.authorityExpected,
				unavailableReason: describeError(error),
			};
			return {status: this.status, storeFile: this.storeFile, quarantinedFile: null};
		}
		if (opened == null) {
			this.status = {
				...UNOPENED_STATUS,
				authorityExpected: this.authorityExpected,
				unavailableReason: this.describeLoadFailure() ?? 'the app store addon did not load',
			};
			return {status: this.status, storeFile: this.storeFile, quarantinedFile: null};
		}
		this.boundary = opened;
		this.accepting = true;
		const initialization = opened.initialization;
		this.status = {
			available: true,
			authorityExpected: this.authorityExpected,
			schemaVersion: initialization.schemaVersion,
			quarantined: initialization.quarantinedPath != null,
			quarantineReason: initialization.quarantineReason as DesktopStoreQuarantineReason | null,
			unavailableReason: null,
		};
		return {status: this.status, storeFile: this.storeFile, quarantinedFile: initialization.quarantinedPath};
	}

	public async recover(): Promise<DesktopAppStorageRecoveryResult> {
		if (this.boundary == null) {
			return {status: this.status, reseedRequired: false};
		}
		try {
			await this.runOperation({
				inputBytes: byteLength(SCHEMA_VERSION_MARKER),
				maximumResponseBytes: MARKER_VALUE_MAX_BYTES,
				operation: (boundary) => boundary.getMetadata({key: SCHEMA_VERSION_MARKER}),
			});
		} catch (error) {
			this.accepting = false;
			this.status = {...this.status, available: false, unavailableReason: describeError(error)};
			return {status: this.status, reseedRequired: false};
		}
		return {status: this.status, reseedRequired: this.status.quarantined};
	}

	public close(): void {
		this.accepting = false;
		const boundary = this.boundary;
		this.boundary = null;
		this.status = {...this.status, available: false, unavailableReason: 'the desktop app store is closed'};
		boundary?.close();
	}

	public getStoreStatus(): Promise<DesktopStoreStatus> {
		return Promise.resolve(this.status);
	}

	public getAllAccounts(): Promise<Array<DesktopAccountRecord>> {
		return this.runOperation({
			inputBytes: 0,
			maximumResponseBytes: NATIVE_JSON_MAX_BYTES,
			operation: async (boundary) => {
				const accounts = await boundary.getAllAccounts();
				return accounts.flatMap((account) => {
					const record = readAccountRecord(account.record);
					return record == null ? [] : [{storageKey: account.storageKey, record}];
				});
			},
		});
	}

	public async getAccount(storageKey: unknown): Promise<DesktopAccountRecord | null> {
		const key = requireLookupKey(storageKey, 'account storage key');
		return this.runOperation({
			inputBytes: byteLength(key),
			maximumResponseBytes: RECORD_MAX_BYTES,
			operation: async (boundary) => {
				const account = await boundary.getAccount({storageKey: key});
				const record = account == null ? null : readAccountRecord(account.record);
				return account == null || record == null ? null : {storageKey: account.storageKey, record};
			},
		});
	}

	public async upsertAccount(account: unknown): Promise<void> {
		const entry = requireAccountRecord(account, 'account');
		return this.runOperation({
			inputBytes: this.recordBytes(entry.storageKey, entry.record),
			maximumResponseBytes: 0,
			operation: (boundary) => boundary.upsertAccount(entry),
		});
	}

	public async compareAndSwapAccount(request: unknown): Promise<boolean> {
		const payload = requireRecord(request, 'account compare-and-swap request');
		const expected = requireAccountRecord(payload.expected, 'account compare-and-swap request.expected');
		const replacement = requireAccountRecord(payload.replacement, 'account compare-and-swap request.replacement');
		if (expected.storageKey !== replacement.storageKey) {
			throw new DesktopAppStoreRequestError(
				'account compare-and-swap request',
				'must use the same storageKey for expected and replacement records',
			);
		}
		const validated: DesktopAccountCompareAndSwapRequest = {expected, replacement};
		return this.runOperation({
			inputBytes:
				this.recordBytes(expected.storageKey, expected.record) +
				this.recordBytes(replacement.storageKey, replacement.record),
			maximumResponseBytes: 0,
			operation: (boundary) => boundary.compareAndSwapAccount(validated),
		});
	}

	public async deleteAccount(storageKey: unknown): Promise<void> {
		const key = requireLookupKey(storageKey, 'account storage key');
		return this.runOperation({
			inputBytes: byteLength(key),
			maximumResponseBytes: 0,
			operation: (boundary) => boundary.deleteAccount({storageKey: key}),
		});
	}

	public async importAccounts(request: unknown): Promise<DesktopAccountImportReport> {
		const payload = requireRecord(request, 'account import request');
		const records = requireArray(payload.records, 'account import request.records', BATCH_RECORDS_MAX).map(
			(value, index) => requireAccountRecord(value, `account import request.records[${index}]`),
		);
		const marker = requireMarker(payload.marker, 'account import request.marker');
		let inputBytes = 0;
		for (const record of records) {
			inputBytes += this.recordBytes(record.storageKey, record.record);
		}
		return this.runOperation({
			inputBytes,
			maximumResponseBytes: NATIVE_JSON_MAX_BYTES,
			operation: (boundary) => boundary.importAccounts({records, marker}),
		});
	}

	public async pruneAccounts(request: unknown): Promise<DesktopAccountPruneReport> {
		const payload = requireRecord(request, 'account prune request');
		const knownStorageKeys = requireArray(
			payload.knownStorageKeys,
			'account prune request.knownStorageKeys',
			BATCH_KEYS_MAX,
		).map((value, index) => requireLookupKey(value, `account prune request.knownStorageKeys[${index}]`));
		const listIsAuthoritative = requireBoolean(
			payload.listIsAuthoritative,
			'account prune request.listIsAuthoritative',
		);
		return this.runOperation({
			inputBytes: knownStorageKeys.reduce((total, key) => total + byteLength(key), 0),
			maximumResponseBytes: NATIVE_JSON_MAX_BYTES,
			operation: (boundary) => boundary.prune({knownStorageKeys, listIsAuthoritative}),
		});
	}

	public async loadEntries(scope: unknown): Promise<Array<DesktopStorageEntry>> {
		const entryScope = requireLookupKey(scope, 'storage scope');
		return this.runOperation({
			inputBytes: byteLength(entryScope),
			maximumResponseBytes: NATIVE_JSON_MAX_BYTES,
			operation: async (boundary) => {
				const entries = await boundary.getEntries({store: ENTRY_STORE, scope: entryScope});
				return entries.map((entry) => {
					this.observeStamp(entry.updatedAt);
					return {key: entry.key, value: entry.value, updatedAt: entry.updatedAt};
				});
			},
		});
	}

	public async getEntry(scope: unknown, key: unknown): Promise<DesktopStorageEntry | null> {
		const entryScope = requireLookupKey(scope, 'storage scope');
		const entryKey = requireLookupKey(key, 'storage key');
		return this.runOperation({
			inputBytes: byteLength(entryScope) + byteLength(entryKey),
			maximumResponseBytes: ENTRY_VALUE_MAX_BYTES,
			operation: async (boundary) => {
				const entry = await boundary.getEntry({store: ENTRY_STORE, scope: entryScope, key: entryKey});
				if (entry == null) {
					return null;
				}
				this.observeStamp(entry.updatedAt);
				return {key: entry.key, value: entry.value, updatedAt: entry.updatedAt};
			},
		});
	}

	public async setEntry(scope: unknown, key: unknown, value: unknown): Promise<void> {
		const entryScope = requireLookupKey(scope, 'storage scope');
		const entryKey = requireLookupKey(key, 'storage key');
		const entryValue = requireBoundedString(value, 'storage value', ENTRY_VALUE_MAX_BYTES);
		return this.runOperation({
			inputBytes: byteLength(entryScope) + byteLength(entryKey) + byteLength(entryValue),
			maximumResponseBytes: 0,
			operation: (boundary) =>
				boundary.setEntry({
					store: ENTRY_STORE,
					scope: entryScope,
					key: entryKey,
					value: entryValue,
					updatedAt: this.nextStamp(),
				}),
		});
	}

	public async deleteEntry(scope: unknown, key: unknown): Promise<void> {
		const entryScope = requireLookupKey(scope, 'storage scope');
		const entryKey = requireLookupKey(key, 'storage key');
		return this.runOperation({
			inputBytes: byteLength(entryScope) + byteLength(entryKey),
			maximumResponseBytes: 0,
			operation: (boundary) => boundary.deleteEntry({store: ENTRY_STORE, scope: entryScope, key: entryKey}),
		});
	}

	public async clearAllForScope(scope: unknown): Promise<void> {
		const entryScope = requireLookupKey(scope, 'storage scope');
		return this.runOperation({
			inputBytes: byteLength(entryScope),
			maximumResponseBytes: 0,
			operation: (boundary) => boundary.clearScope({scope: entryScope}),
		});
	}

	public async clearAllExcept(keysToKeep: unknown): Promise<void> {
		const keys = requireArray(keysToKeep, 'storage retained keys', BATCH_KEYS_MAX).map((value, index) =>
			requireLookupKey(value, `storage retained keys[${index}]`),
		);
		return this.runOperation({
			inputBytes: keys.reduce((total, key) => total + byteLength(key), 0),
			maximumResponseBytes: 0,
			operation: (boundary) => boundary.clearStoreExcept({store: ENTRY_STORE, keysToKeep: keys}),
		});
	}

	public async setManyEntries(writes: unknown): Promise<void> {
		const requested = requireArray(writes, 'storage writes', BATCH_ENTRIES_MAX).map(
			(value, index): PendingEntryWrite => {
				const record = requireRecord(value, `storage writes[${index}]`);
				return {
					scope: requireLookupKey(record.scope, `storage writes[${index}].scope`),
					key: requireLookupKey(record.key, `storage writes[${index}].key`),
					value: requireBoundedString(record.value, `storage writes[${index}].value`, ENTRY_VALUE_MAX_BYTES),
					ifAbsent: record.ifAbsent === true,
				};
			},
		);
		let inputBytes = 0;
		for (const write of requested) {
			inputBytes += byteLength(write.scope) + byteLength(write.key) + byteLength(write.value);
		}
		return this.runOperation({
			inputBytes,
			maximumResponseBytes: NATIVE_JSON_MAX_BYTES,
			operation: async (boundary) => {
				const present = await this.readPresentKeys(boundary, requested);
				const entries = requested
					.filter((write) => !(write.ifAbsent && present.has(`${write.scope}\u0000${write.key}`)))
					.map((write) => ({
						store: ENTRY_STORE,
						scope: write.scope,
						key: write.key,
						value: write.value,
						updatedAt: this.nextStamp(),
					}));
				if (entries.length === 0) {
					return;
				}
				const report = await boundary.importEntries({entries});
				this.requireNoRefusals(report.skipped);
			},
		});
	}

	public async importEntries(request: unknown): Promise<DesktopStorageImportReport> {
		const payload = requireRecord(request, 'storage import request');
		const entries = requireArray(payload.entries, 'storage import request.entries', BATCH_ENTRIES_MAX).map(
			(value, index): DesktopStorageScopedEntry => {
				const record = requireRecord(value, `storage import request.entries[${index}]`);
				return {
					scope: requireLookupKey(record.scope, `storage import request.entries[${index}].scope`),
					key: requireLookupKey(record.key, `storage import request.entries[${index}].key`),
					value: requireBoundedString(
						record.value,
						`storage import request.entries[${index}].value`,
						ENTRY_VALUE_MAX_BYTES,
					),
					updatedAt: requireFiniteNumber(record.updatedAt, `storage import request.entries[${index}].updatedAt`),
				};
			},
		);
		const marker = requireMarker(payload.marker, 'storage import request.marker');
		let inputBytes = 0;
		for (const entry of entries) {
			inputBytes += byteLength(entry.scope) + byteLength(entry.key) + byteLength(entry.value);
		}
		return this.runOperation({
			inputBytes,
			maximumResponseBytes: NATIVE_JSON_MAX_BYTES,
			operation: (boundary) => {
				for (const entry of entries) {
					this.observeStamp(entry.updatedAt);
				}
				return boundary.importEntries({
					entries: entries.map((entry) => ({
						store: ENTRY_STORE,
						scope: entry.scope,
						key: entry.key,
						value: entry.value,
						updatedAt: entry.updatedAt,
					})),
					marker,
				});
			},
		});
	}

	public async getMarker(key: unknown): Promise<string | null> {
		const markerKey = requireLookupKey(key, 'store marker key');
		return this.runOperation({
			inputBytes: byteLength(markerKey),
			maximumResponseBytes: MARKER_VALUE_MAX_BYTES,
			operation: (boundary) => boundary.getMetadata({key: markerKey}),
		});
	}

	public async setMarker(key: unknown, value: unknown): Promise<void> {
		const markerKey = requireLookupKey(key, 'store marker key');
		const markerValue = requireBoundedString(value, 'store marker value', MARKER_VALUE_MAX_BYTES);
		await this.writeMetadata(markerKey, markerValue);
		if (markerKey !== DESKTOP_LEGACY_AUTHORITY_MARKER_KEY) {
			return;
		}
		if (markerValue === DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE) {
			await this.writeAuthoritySentinel();
			return;
		}
		if (markerValue === '') {
			await this.removeAuthoritySentinel();
		}
	}

	private writeMetadata(markerKey: string, markerValue: string): Promise<void> {
		return this.runOperation({
			inputBytes: byteLength(markerKey) + byteLength(markerValue),
			maximumResponseBytes: 0,
			operation: (boundary) => boundary.setMetadata({key: markerKey, value: markerValue}),
		});
	}

	private async writeAuthoritySentinel(): Promise<void> {
		const temporary = `${this.authoritySentinelPath}.tmp`;
		await fsPromises.writeFile(temporary, '1\n', {encoding: 'utf8', mode: 0o600});
		await fsPromises.rename(temporary, this.authoritySentinelPath);
		this.authorityExpected = true;
		this.status = {...this.status, authorityExpected: true};
	}

	private async removeAuthoritySentinel(): Promise<void> {
		await fsPromises.rm(this.authoritySentinelPath, {force: true});
		this.authorityExpected = false;
		this.status = {...this.status, authorityExpected: false};
	}

	public getAllKnownInstances(): Promise<Array<DesktopKnownInstanceRecord>> {
		return this.runOperation({
			inputBytes: 0,
			maximumResponseBytes: NATIVE_JSON_MAX_BYTES,
			operation: async (boundary) => {
				const stored = await boundary.getAllKnownInstances();
				return stored.flatMap((instance) => {
					const record = readKnownInstanceRecord(instance.record);
					return record == null ? [] : [record];
				});
			},
		});
	}

	public async upsertKnownInstance(instance: unknown): Promise<void> {
		const record = requireKnownInstanceRecord(instance, 'known instance');
		return this.runOperation({
			inputBytes: this.recordBytes(record.instanceKey, record),
			maximumResponseBytes: 0,
			operation: (boundary) => boundary.upsertKnownInstance({key: record.instanceKey, record}),
		});
	}

	public async deleteKnownInstance(instanceKey: unknown): Promise<void> {
		const key = requireLookupKey(instanceKey, 'known instance key');
		return this.runOperation({
			inputBytes: byteLength(key),
			maximumResponseBytes: 0,
			operation: (boundary) => boundary.deleteKnownInstance({key}),
		});
	}

	private async readPresentKeys(
		boundary: AppStoreBoundary,
		writes: ReadonlyArray<PendingEntryWrite>,
	): Promise<Set<string>> {
		const scopes = new Set(writes.filter((write) => write.ifAbsent).map((write) => write.scope));
		const present = new Set<string>();
		for (const scope of scopes) {
			const entries = await boundary.getEntries({store: ENTRY_STORE, scope});
			for (const entry of entries) {
				present.add(`${scope}\u0000${entry.key}`);
			}
		}
		return present;
	}

	private requireNoRefusals(skipped: ReadonlyArray<DesktopStoreSkippedRecord>): void {
		if (skipped.length === 0) {
			return;
		}
		throw new DesktopAppStoreWriteRefusedError(skipped.map((entry) => `${entry.key}: ${entry.reason}`));
	}

	private recordBytes(key: string, record: unknown): number {
		return byteLength(key) + byteLength(JSON.stringify(record) ?? '');
	}

	private nextStamp(): number {
		const now = Date.now();
		this.stamp = now > this.stamp ? now : this.stamp + 1;
		return this.stamp;
	}

	private observeStamp(value: number): void {
		if (value > this.stamp) {
			this.stamp = value;
		}
	}

	private runOperation<T>({inputBytes, maximumResponseBytes, operation}: DesktopAppStoreOperation<T>): Promise<T> {
		if (this.boundary == null) {
			return Promise.reject(
				new DesktopAppStoreUnavailableError(this.status.unavailableReason ?? 'the store did not open'),
			);
		}
		if (!this.accepting) {
			return Promise.reject(new DesktopAppStoreShutdownError());
		}
		if (this.pending.size >= PENDING_OPERATIONS_MAX) {
			return Promise.reject(new DesktopAppStoreCapacityError('concurrent operations', PENDING_OPERATIONS_MAX));
		}
		if (this.pendingInputBytes + inputBytes > PENDING_INPUT_BYTES_MAX) {
			return Promise.reject(new DesktopAppStoreCapacityError('pending request bytes', PENDING_INPUT_BYTES_MAX));
		}
		if (this.pendingResponseBytes + maximumResponseBytes > PENDING_RESPONSE_BYTES_MAX) {
			return Promise.reject(new DesktopAppStoreCapacityError('pending response bytes', PENDING_RESPONSE_BYTES_MAX));
		}
		this.pendingInputBytes += inputBytes;
		this.pendingResponseBytes += maximumResponseBytes;
		const result = this.tail.then(() => {
			const boundary = this.boundary;
			if (!this.accepting || boundary == null) {
				throw new DesktopAppStoreShutdownError();
			}
			return operation(boundary);
		});
		const tracked = result.finally(() => {
			this.pending.delete(tracked);
			this.pendingInputBytes -= inputBytes;
			this.pendingResponseBytes -= maximumResponseBytes;
		});
		this.pending.add(tracked);
		this.tail = tracked.then(
			() => {},
			() => {},
		);
		return tracked;
	}
}

let sharedStorage: DesktopAppStorage | null = null;

export function openDesktopAppStorage(dependencies: DesktopAppStorageDependencies): DesktopAppStorageOpenResult {
	sharedStorage ??= new DesktopAppStorage(dependencies);
	return sharedStorage.open();
}

export function getDesktopAppStorage(): DesktopAppStorage | null {
	return sharedStorage;
}

export function recoverDesktopAppStorage(): Promise<DesktopAppStorageRecoveryResult> {
	if (sharedStorage == null) {
		return Promise.resolve({status: UNOPENED_STATUS, reseedRequired: false});
	}
	return sharedStorage.recover();
}

export function closeDesktopAppStorage(): void {
	sharedStorage?.close();
}
