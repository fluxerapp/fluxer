// SPDX-License-Identifier: AGPL-3.0-or-later

import {runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import type {
	AccountRekeyTarget,
	KeyedStoredAccount,
	StoredAccount,
	StoredAccountList,
} from '@app/features/auth/state/AccountStorage';
import {BrowserAccountStorageUnavailableError, planAccountRekey} from '@app/features/auth/state/AccountStorage';
import {accountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {AppStorageKey, LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY} from '@app/features/platform/state/AppStorageKeys';
import {
	getDesktopAccountStorageAPI,
	requireDesktopAccountRecord,
} from '@app/features/platform/state/DesktopAccountStorageAccess';
import {getDesktopStorageAPI} from '@app/features/platform/state/DesktopPersistentStorageBackend';
import {
	buildLegacyAppStorageMigrationPlan,
	completedLegacyAppStorageMigrationMarker,
	type LegacyContentCarry,
	resolveLegacyContentCarry,
} from '@app/features/platform/state/LegacyAppStorageMigration';
import {
	readLegacySessionCredentials,
	writeActiveAccountKeyMirror,
} from '@app/features/platform/state/LegacySessionReconciliation';
import {GLOBAL_APP_STORAGE_SCOPE} from '@app/features/platform/state/PersistentStorage';
import type {PersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {
	DesktopAccountStorageAPI,
	DesktopStoreJSONObject,
	DesktopStoreSkippedRecord,
} from '@fluxer/desktop_ipc/src/AccountContract';
import type {DesktopStorageAPI, DesktopStorageScopedEntry} from '@fluxer/desktop_ipc/src/StorageContract';
import {
	DESKTOP_LEGACY_AUTHORITY_MARKER_KEY,
	DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE,
	DESKTOP_LEGACY_IMPORT_FAILURE_MARKER_KEY,
	DESKTOP_LEGACY_IMPORT_MARKER_KEY,
	DesktopLegacyImportPhase,
	desktopLegacyImportMarker,
	readDesktopLegacyImportPhase,
} from '@fluxer/desktop_ipc/src/StorageContract';

const logger = new Logger('DesktopLegacyImport');

export const DESKTOP_LEGACY_IMPORT_MAX_FAILURES = 10;

const LEGACY_CORPUS_STAMP = 0;

export const DesktopLegacyImportStatus = Object.freeze({
	IMPORTED: 'imported',
	DEFERRED: 'deferred',
	ALREADY_DONE: 'already-done',
	REFUSED: 'refused',
	FAILED: 'failed',
} as const);

export type DesktopLegacyImportStatus = (typeof DesktopLegacyImportStatus)[keyof typeof DesktopLegacyImportStatus];

export const DesktopLegacyImportRefusal = Object.freeze({
	STORE_UNAVAILABLE: 'store-unavailable',
	STICKY_FALLBACK: 'sticky-fallback',
	NO_LOCAL_STORAGE: 'no-local-storage',
	NO_INSTANCE_KEY: 'no-instance-key',
	ACCOUNTS_NOT_AUTHORITATIVE: 'accounts-not-authoritative',
} as const);

export type DesktopLegacyImportRefusal = (typeof DesktopLegacyImportRefusal)[keyof typeof DesktopLegacyImportRefusal];

export interface DesktopLegacyImportAccountSource {
	getAllAccounts(): Promise<StoredAccountList>;
}

export type DesktopLegacyImportActiveAccount = AccountRekeyTarget;

export interface DesktopLegacyImportRequest {
	readonly accounts: DesktopAccountStorageAPI;
	readonly storage: DesktopStorageAPI;
	readonly webBackend: PersistentStorageBackend;
	readonly legacyStorage: Storage | null;
	readonly accountSource: DesktopLegacyImportAccountSource;
	readonly currentAccount: DesktopLegacyImportActiveAccount;
	readonly now: number;
}

export interface DesktopLegacyImportResult {
	readonly status: DesktopLegacyImportStatus;
	readonly phase: DesktopLegacyImportPhase;
	readonly refusal: DesktopLegacyImportRefusal | null;
	readonly accountsImported: number;
	readonly entriesImported: number;
	readonly accountKeys: ReadonlyArray<string>;
	readonly deferredAccounts: number;
	readonly failures: number;
}

interface DesktopLegacyImportContext {
	readonly legacyStorage: Storage;
	readonly accounts: ReadonlyArray<KeyedStoredAccount>;
	readonly deferredAccounts: ReadonlyArray<StoredAccount>;
	readonly activeAccountKey: string;
	readonly activeScope: string;
}

function parsePhase(value: string | null): DesktopLegacyImportPhase {
	const phase = readDesktopLegacyImportPhase(value);
	if (phase === null && value !== null) {
		logger.warn('Discarding an unreadable desktop import marker, re-running every phase');
	}
	return phase ?? DesktopLegacyImportPhase.ACCOUNTS;
}

function parseFailureCount(value: string | null): number {
	if (value == null) {
		return 0;
	}
	const parsed = Number(value);
	return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : 0;
}

function toDesktopAccountRecord(account: KeyedStoredAccount): DesktopStoreJSONObject {
	const record: StoredAccount & {storageKey: string} = {...account, localStorageData: {}};
	delete record.managedStorageData;
	return JSON.parse(JSON.stringify(record)) as DesktopStoreJSONObject;
}

class DesktopLegacyImportIncompleteError extends Error {
	constructor(phase: DesktopLegacyImportPhase) {
		super(`The desktop legacy import did not preserve every record in the ${phase} phase`);
		this.name = 'DesktopLegacyImportIncompleteError';
	}
}

async function requireCompleteImportReport(
	storage: DesktopStorageAPI,
	phase: DesktopLegacyImportPhase,
	updatedAt: number,
	skipped: ReadonlyArray<DesktopStoreSkippedRecord>,
	unusableInstances: ReadonlyArray<string> = [],
): Promise<void> {
	if (skipped.length === 0 && unusableInstances.length === 0) return;
	logger.warn(
		`The desktop store refused ${skipped.length} record(s) during the ${phase} phase`,
		skipped.map((record) => `${record.key}: ${record.reason}`),
	);
	if (unusableInstances.length > 0) {
		logger.warn('The desktop store found unusable instance snapshots', unusableInstances);
	}
	const marker = desktopLegacyImportMarker(phase, updatedAt);
	await storage.setMarker(marker.key, marker.value);
	throw new DesktopLegacyImportIncompleteError(phase);
}

function entryIdentity(scope: string, key: string): string {
	return JSON.stringify([scope, key]);
}

async function resolveContext(
	request: DesktopLegacyImportRequest,
): Promise<DesktopLegacyImportContext | DesktopLegacyImportRefusal> {
	if (request.legacyStorage == null) {
		return DesktopLegacyImportRefusal.NO_LOCAL_STORAGE;
	}
	const currentAccount = request.currentAccount;
	if (runtimeInstanceKey(currentAccount.instance) === null) {
		return DesktopLegacyImportRefusal.NO_INSTANCE_KEY;
	}
	const instance = currentAccount.instance;
	const activeAccountKey = accountStorageKey(currentAccount.userId, instance);
	if (activeAccountKey === null) {
		return DesktopLegacyImportRefusal.NO_INSTANCE_KEY;
	}
	const {records, source} = await request.accountSource.getAllAccounts();
	if (source !== 'idb') {
		logger.warn('Refusing the desktop import because the account list is not authoritative', source);
		return DesktopLegacyImportRefusal.ACCOUNTS_NOT_AUTHORITATIVE;
	}
	const accountPlan = planAccountRekey(records, {target: currentAccount, now: request.now});
	const accounts = accountPlan.qualifiedRecords;
	if (
		accounts.some(
			(account) =>
				account.instance == null ||
				runtimeInstanceKey(account.instance) === null ||
				accountStorageKey(account.userId, account.instance) !== account.storageKey,
		)
	) {
		logger.warn('Refusing the desktop import because an account instance does not match its storage identity');
		return DesktopLegacyImportRefusal.ACCOUNTS_NOT_AUTHORITATIVE;
	}
	const live = readLegacySessionCredentials(request.legacyStorage);
	if (live !== null && (live.userId !== currentAccount.userId || live.token !== currentAccount.token)) {
		logger.warn('Refusing the desktop import because the live session does not match the resolved AuthSession account');
		return DesktopLegacyImportRefusal.ACCOUNTS_NOT_AUTHORITATIVE;
	}
	const activeAccount = accounts.find((account) => account.storageKey === activeAccountKey);
	if (
		activeAccount === undefined ||
		activeAccount.userId !== currentAccount.userId ||
		activeAccount.token !== currentAccount.token
	) {
		logger.warn('Refusing the desktop import because the active AuthSession account was not qualified exactly');
		return DesktopLegacyImportRefusal.ACCOUNTS_NOT_AUTHORITATIVE;
	}
	return {
		legacyStorage: request.legacyStorage,
		accounts,
		deferredAccounts: accountPlan.deferredRecords,
		activeAccountKey,
		activeScope: activeAccountKey,
	};
}

async function importAccountRecords(
	request: DesktopLegacyImportRequest,
	context: DesktopLegacyImportContext,
): Promise<number> {
	const records = context.accounts.map((account) => ({
		storageKey: account.storageKey,
		record: toDesktopAccountRecord(account),
	}));
	const report = await request.accounts.import({
		records,
		marker: null,
	});
	await requireCompleteImportReport(
		request.storage,
		DesktopLegacyImportPhase.ACCOUNTS,
		request.now,
		report.skipped,
		report.unusableInstances,
	);
	return report.imported;
}

async function importAccountsPhase(
	request: DesktopLegacyImportRequest,
	context: DesktopLegacyImportContext,
): Promise<number> {
	const imported = await importAccountRecords(request, context);
	const marker = desktopLegacyImportMarker(DesktopLegacyImportPhase.ENTRIES, request.now);
	await request.storage.setMarker(marker.key, marker.value);
	return imported;
}

async function loadWebEntries(
	backend: PersistentStorageBackend,
	scopes: ReadonlyArray<string>,
): Promise<Map<string, DesktopStorageScopedEntry>> {
	const entries = new Map<string, DesktopStorageScopedEntry>();
	for (const scope of scopes) {
		for (const [key, entry] of await backend.load(scope)) {
			entries.set(entryIdentity(scope, key), {scope, key, value: entry.value, updatedAt: entry.updatedAt.wall});
		}
	}
	return entries;
}

async function loadDesktopStamps(
	storage: DesktopStorageAPI,
	scopes: ReadonlyArray<string>,
): Promise<Map<string, number>> {
	const stamps = new Map<string, number>();
	for (const scope of scopes) {
		for (const entry of await storage.load(scope)) {
			stamps.set(entryIdentity(scope, entry.key), entry.updatedAt);
		}
	}
	return stamps;
}

async function pruneUnexpectedDesktopAccounts(
	request: DesktopLegacyImportRequest,
	knownStorageKeys: ReadonlyArray<string>,
): Promise<void> {
	const expected = [...new Set(knownStorageKeys)].sort();
	if (expected.length === 0) {
		const stored = await request.accounts.getAll();
		for (const account of stored) {
			await request.storage.clearAllForScope(account.storageKey);
			await request.accounts.delete(account.storageKey);
		}
		return;
	}
	const report = await request.accounts.prune({knownStorageKeys: expected, listIsAuthoritative: true});
	if (report.refusedReason !== null) {
		throw new Error(`The desktop account prune was refused: ${report.refusedReason}`);
	}
}

async function pruneUnexpectedDesktopEntries(
	storage: DesktopStorageAPI,
	scopes: ReadonlyArray<string>,
	planned: ReadonlyMap<string, DesktopStorageScopedEntry>,
): Promise<void> {
	for (const scope of scopes) {
		for (const entry of await storage.load(scope)) {
			if (!planned.has(entryIdentity(scope, entry.key))) {
				await storage.delete(scope, entry.key);
			}
		}
	}
}

interface DesktopEntryImportPlan {
	readonly scopes: ReadonlyArray<string>;
	readonly staged: ReadonlyMap<string, DesktopStorageScopedEntry>;
}

async function resolveContentCarry(
	request: DesktopLegacyImportRequest,
	context: DesktopLegacyImportContext,
): Promise<LegacyContentCarry> {
	const committed = [
		(await request.webBackend.get(GLOBAL_APP_STORAGE_SCOPE, LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY))?.value ?? null,
		(await request.storage.get(GLOBAL_APP_STORAGE_SCOPE, LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY))?.value ?? null,
	];
	return resolveLegacyContentCarry(committed, context);
}

async function buildDesktopEntryImportPlan(
	request: DesktopLegacyImportRequest,
	context: DesktopLegacyImportContext,
): Promise<DesktopEntryImportPlan> {
	const carry = await resolveContentCarry(request, context);
	const plan = buildLegacyAppStorageMigrationPlan({
		storage: context.legacyStorage,
		accounts: context.accounts,
		contentScope: carry.contentScope,
		sharedContentScopes: carry.sharedContentScopes,
	});
	const scopes = [...new Set([GLOBAL_APP_STORAGE_SCOPE, ...plan.scopes])];
	const staged = new Map<string, DesktopStorageScopedEntry>();
	for (const write of plan.writes) {
		staged.set(entryIdentity(write.scope, write.key), {
			scope: write.scope,
			key: write.key,
			value: write.value,
			updatedAt: LEGACY_CORPUS_STAMP,
		});
	}
	for (const [identity, entry] of await loadWebEntries(request.webBackend, scopes)) {
		staged.set(identity, entry);
	}
	const marker = completedLegacyAppStorageMigrationMarker({...carry, contentScope: plan.contentScope});
	const markerIdentity = entryIdentity(marker.scope, marker.key);
	staged.set(markerIdentity, {
		scope: marker.scope,
		key: marker.key,
		value: marker.value,
		updatedAt: staged.get(markerIdentity)?.updatedAt ?? LEGACY_CORPUS_STAMP,
	});
	return {scopes, staged};
}

async function importEntriesPhase(
	request: DesktopLegacyImportRequest,
	context: DesktopLegacyImportContext,
	replaceEntries: boolean,
): Promise<number> {
	const {scopes, staged} = await buildDesktopEntryImportPlan(request, context);
	let entries = [...staged.values()];
	if (!replaceEntries) {
		await pruneUnexpectedDesktopEntries(request.storage, scopes, staged);
		const committed = await loadDesktopStamps(request.storage, scopes);
		entries = entries.filter((entry) => {
			const stamp = committed.get(entryIdentity(entry.scope, entry.key));
			return stamp === undefined || stamp < entry.updatedAt;
		});
	} else {
		await request.storage.clearAllExcept([]);
	}
	const report = await request.storage.import({
		entries,
		marker: null,
	});
	await requireCompleteImportReport(request.storage, DesktopLegacyImportPhase.ENTRIES, request.now, report.skipped);
	const marker = desktopLegacyImportMarker(DesktopLegacyImportPhase.SESSION, request.now);
	await request.storage.setMarker(marker.key, marker.value);
	return report.imported;
}

async function stageDeferredDesktopImport(
	request: DesktopLegacyImportRequest,
	context: DesktopLegacyImportContext,
): Promise<{readonly accountsImported: number; readonly entriesImported: number}> {
	await resetDesktopLegacyImportCandidate(request);
	const accountsImported = await importAccountRecords(request, context);
	const {staged} = await buildDesktopEntryImportPlan(request, context);
	const report = await request.storage.import({entries: [...staged.values()], marker: null});
	await requireCompleteImportReport(request.storage, DesktopLegacyImportPhase.ACCOUNTS, request.now, report.skipped);
	await resetDesktopLegacyImportCandidate(request);
	return {accountsImported, entriesImported: report.imported};
}

async function importSessionPhase(
	request: DesktopLegacyImportRequest,
	context: DesktopLegacyImportContext,
): Promise<void> {
	const activeKey = context.activeAccountKey;
	const entries: Array<DesktopStorageScopedEntry> = [
		{
			scope: GLOBAL_APP_STORAGE_SCOPE,
			key: AppStorageKey.AUTH_ACCOUNT_KEY,
			value: activeKey,
			updatedAt: request.now,
		},
	];
	const report = await request.storage.import({
		entries,
		marker: null,
	});
	await requireCompleteImportReport(request.storage, DesktopLegacyImportPhase.SESSION, request.now, report.skipped);
	const marker = desktopLegacyImportMarker(DesktopLegacyImportPhase.DONE, request.now);
	await request.storage.setMarker(marker.key, marker.value);
	writeActiveAccountKeyMirror(activeKey);
}

async function readFailureCount(storage: DesktopStorageAPI): Promise<number> {
	return parseFailureCount(await storage.getMarker(DESKTOP_LEGACY_IMPORT_FAILURE_MARKER_KEY));
}

async function storeNeedsReseed(
	request: DesktopLegacyImportRequest,
	context: DesktopLegacyImportContext,
): Promise<boolean> {
	const stored = await request.accounts.getAll();
	let storedKeys: Set<string>;
	try {
		storedKeys = new Set(stored.map((account) => requireDesktopAccountRecord(account).storageKey));
	} catch {
		return true;
	}
	const expectedKeys = new Set(context.accounts.map((account) => account.storageKey));
	if (storedKeys.size !== expectedKeys.size) {
		return true;
	}
	for (const key of expectedKeys) {
		if (!storedKeys.has(key)) {
			return true;
		}
	}
	return !expectedKeys.has(context.activeAccountKey);
}

export async function pruneDesktopLegacyAccounts(
	request: DesktopLegacyImportRequest,
	knownStorageKeys: ReadonlyArray<string>,
): Promise<ReadonlyArray<string>> {
	const expected = [...new Set(knownStorageKeys)].sort();
	await pruneUnexpectedDesktopAccounts(request, expected);
	if (expected.length === 0) {
		if ((await request.accounts.getAll()).length > 0) {
			throw new Error('The desktop account store did not become empty after pruning');
		}
		return expected;
	}
	const stored = await request.accounts.getAll();
	const actual = stored.map((account) => requireDesktopAccountRecord(account).storageKey).sort();
	if (actual.length !== expected.length || actual.some((key, index) => key !== expected[index])) {
		throw new Error('The desktop account store does not match the authoritative account list after pruning');
	}
	return expected;
}

export async function commitDesktopLegacyImport(request: DesktopLegacyImportRequest): Promise<void> {
	const phase = readDesktopLegacyImportPhase(await request.storage.getMarker(DESKTOP_LEGACY_IMPORT_MARKER_KEY));
	if (phase !== DesktopLegacyImportPhase.DONE) {
		throw new Error('The desktop legacy import is not complete');
	}
	await request.storage.setMarker(DESKTOP_LEGACY_AUTHORITY_MARKER_KEY, DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE);
}

export async function resetDesktopLegacyImportCandidate(request: DesktopLegacyImportRequest): Promise<void> {
	await request.storage.setMarker(DESKTOP_LEGACY_AUTHORITY_MARKER_KEY, '');
	const marker = desktopLegacyImportMarker(DesktopLegacyImportPhase.ACCOUNTS, request.now);
	await request.storage.setMarker(marker.key, marker.value);
}

async function writeFailureCount(storage: DesktopStorageAPI, failures: number): Promise<void> {
	try {
		await storage.setMarker(DESKTOP_LEGACY_IMPORT_FAILURE_MARKER_KEY, String(failures));
	} catch (error) {
		logger.warn('Failed to record the desktop import failure count', error);
	}
}

function refused(
	refusal: DesktopLegacyImportRefusal,
	phase: DesktopLegacyImportPhase,
	failures: number,
	deferredAccounts: number,
): DesktopLegacyImportResult {
	return {
		status: DesktopLegacyImportStatus.REFUSED,
		phase,
		refusal,
		accountsImported: 0,
		entriesImported: 0,
		accountKeys: [],
		deferredAccounts,
		failures,
	};
}

function alreadyDone(failures: number, accountKeys: ReadonlyArray<string>): DesktopLegacyImportResult {
	return {
		status: DesktopLegacyImportStatus.ALREADY_DONE,
		phase: DesktopLegacyImportPhase.DONE,
		refusal: null,
		accountsImported: 0,
		entriesImported: 0,
		accountKeys,
		deferredAccounts: 0,
		failures,
	};
}

function deferred(
	context: DesktopLegacyImportContext,
	accountsImported: number,
	entriesImported: number,
	failures: number,
): DesktopLegacyImportResult {
	return {
		status: DesktopLegacyImportStatus.DEFERRED,
		phase: DesktopLegacyImportPhase.ACCOUNTS,
		refusal: null,
		accountsImported,
		entriesImported,
		accountKeys: context.accounts.map((account) => account.storageKey),
		deferredAccounts: context.deferredAccounts.length,
		failures,
	};
}

export async function runDesktopLegacyImport(request: DesktopLegacyImportRequest): Promise<DesktopLegacyImportResult> {
	let failures = 0;
	let failureCountRead = false;
	let phase: DesktopLegacyImportPhase = DesktopLegacyImportPhase.ACCOUNTS;
	let accountsImported = 0;
	let entriesImported = 0;
	let accountKeys: ReadonlyArray<string> = [];
	let deferredAccounts = 0;
	let replaceEntries = false;
	let authorityWasCommitted = false;
	try {
		const status = await request.storage.getStatus();
		if (!status.available) {
			return refused(DesktopLegacyImportRefusal.STORE_UNAVAILABLE, phase, failures, deferredAccounts);
		}
		if (status.quarantined) {
			logger.warn('The previous desktop store was quarantined, re-seeding the fresh one', status.quarantineReason);
		}
		failures = await readFailureCount(request.storage);
		failureCountRead = true;
		const importMarker = await request.storage.getMarker(DESKTOP_LEGACY_IMPORT_MARKER_KEY);
		phase = parsePhase(importMarker);
		const authority = await request.storage.getMarker(DESKTOP_LEGACY_AUTHORITY_MARKER_KEY);
		authorityWasCommitted = authority === DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE;
		if (phase === DesktopLegacyImportPhase.ACCOUNTS && importMarker !== null && !authorityWasCommitted) {
			replaceEntries = true;
		}
		if (phase !== DesktopLegacyImportPhase.DONE && authorityWasCommitted) {
			await resetDesktopLegacyImportCandidate(request);
			authorityWasCommitted = false;
			phase = DesktopLegacyImportPhase.ACCOUNTS;
			replaceEntries = true;
		}
		if (status.quarantined) {
			const marker = desktopLegacyImportMarker(DesktopLegacyImportPhase.ACCOUNTS, request.now);
			await request.storage.setMarker(marker.key, marker.value);
			phase = DesktopLegacyImportPhase.ACCOUNTS;
			replaceEntries = true;
		}
		const context = await resolveContext(request);
		if (typeof context === 'string') {
			return refused(context, phase, failures, deferredAccounts);
		}
		deferredAccounts = context.deferredAccounts.length;
		if (deferredAccounts > 0) {
			if (failures >= DESKTOP_LEGACY_IMPORT_MAX_FAILURES) {
				logger.error(`The desktop import failed ${failures} times, serving this install from the web backend`);
				return refused(DesktopLegacyImportRefusal.STICKY_FALLBACK, phase, failures, deferredAccounts);
			}
			const staged = await stageDeferredDesktopImport(request, context);
			logger.info(
				`Staged ${context.accounts.length} qualified desktop account(s), retaining web authority for ${deferredAccounts} deferred account(s)`,
			);
			return deferred(context, staged.accountsImported, staged.entriesImported, failures);
		}
		if (phase === DesktopLegacyImportPhase.DONE) {
			if (!(await storeNeedsReseed(request, context))) {
				accountKeys = await pruneDesktopLegacyAccounts(
					request,
					context.accounts.map((account) => account.storageKey),
				);
				return alreadyDone(failures, accountKeys);
			}
			logger.warn('Re-running the desktop import because the completed account store needs reseeding');
			await resetDesktopLegacyImportCandidate(request);
			phase = DesktopLegacyImportPhase.ACCOUNTS;
			replaceEntries = true;
		}
		if (failures >= DESKTOP_LEGACY_IMPORT_MAX_FAILURES) {
			logger.error(`The desktop import failed ${failures} times, serving this install from the web backend`);
			return refused(DesktopLegacyImportRefusal.STICKY_FALLBACK, phase, failures, deferredAccounts);
		}
		await pruneUnexpectedDesktopAccounts(
			request,
			context.accounts.map((account) => account.storageKey),
		);
		if (phase === DesktopLegacyImportPhase.ACCOUNTS) {
			accountsImported = await importAccountsPhase(request, context);
			phase = DesktopLegacyImportPhase.ENTRIES;
		}
		entriesImported = await importEntriesPhase(request, context, replaceEntries);
		if (phase === DesktopLegacyImportPhase.ENTRIES) {
			phase = DesktopLegacyImportPhase.SESSION;
		}
		await importSessionPhase(request, context);
		phase = DesktopLegacyImportPhase.DONE;
		accountKeys = await pruneDesktopLegacyAccounts(
			request,
			context.accounts.map((account) => account.storageKey),
		);
		if (failures > 0) {
			await writeFailureCount(request.storage, 0);
		}
		return {
			status: DesktopLegacyImportStatus.IMPORTED,
			phase,
			refusal: null,
			accountsImported,
			entriesImported,
			accountKeys,
			deferredAccounts: 0,
			failures: 0,
		};
	} catch (error) {
		if (error instanceof BrowserAccountStorageUnavailableError) {
			throw error;
		}
		logger.error('The desktop import failed, serving this session from the web backend', error);
		if (phase === DesktopLegacyImportPhase.DONE && authorityWasCommitted) {
			await resetDesktopLegacyImportCandidate(request).catch((resetError) =>
				logger.error('The failed committed desktop import could not be rewound', resetError),
			);
		}
		const nextFailures = failureCountRead ? failures + 1 : failures;
		if (failureCountRead) {
			await writeFailureCount(request.storage, nextFailures);
		}
		return {
			status: DesktopLegacyImportStatus.FAILED,
			phase,
			refusal: null,
			accountsImported,
			entriesImported,
			accountKeys,
			deferredAccounts,
			failures: nextFailures,
		};
	}
}

export function resolveDesktopLegacyImportRequest(options: {
	readonly webBackend: PersistentStorageBackend;
	readonly accountSource: DesktopLegacyImportAccountSource;
	readonly currentAccount: DesktopLegacyImportActiveAccount;
	readonly now: number;
	readonly legacyStorage?: Storage | null;
}): DesktopLegacyImportRequest | null {
	const accounts = getDesktopAccountStorageAPI();
	const storage = getDesktopStorageAPI();
	if (accounts === null || storage === null) {
		return null;
	}
	return {
		accounts,
		storage,
		webBackend: options.webBackend,
		legacyStorage: options.legacyStorage !== undefined ? options.legacyStorage : getProtectedLocalStorage(),
		accountSource: options.accountSource,
		currentAccount: options.currentAccount,
		now: options.now,
	};
}
