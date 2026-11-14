// SPDX-License-Identifier: AGPL-3.0-or-later

export interface AppStoreOptions {
	path: string;
	derivationVersion?: string | null;
}

export interface AppStoreInitialization {
	path: string;
	schemaVersion: number;
	previousSchemaVersion: number;
	appliedMigrations: number;
	quarantinedPath: string | null;
	quarantineReason: 'newer' | 'schema' | 'corrupt' | 'derivation' | null;
}

export interface AppStoreAccount {
	storageKey: string;
	record: unknown;
}

export interface AppStoreAccountCompareAndSwapRequest {
	expected: AppStoreAccount;
	replacement: AppStoreAccount;
}

export interface AppStoreKnownInstance {
	key: string;
	record: unknown;
}

export interface AppStoreEntry {
	store: string;
	scope: string;
	key: string;
	value: string;
	updatedAt: number;
}

export interface AppStoreMarker {
	key: string;
	value: string;
}

export interface AppStoreAccountImport {
	records: ReadonlyArray<AppStoreAccount>;
	marker?: AppStoreMarker | null;
}

export interface AppStoreEntryImport {
	entries: ReadonlyArray<AppStoreEntry>;
	marker?: AppStoreMarker | null;
}

export interface AppStoreSkippedRecord {
	key: string;
	reason: string;
}

export interface AppStoreAccountImportReport {
	imported: number;
	skipped: ReadonlyArray<AppStoreSkippedRecord>;
	unusableInstances: ReadonlyArray<string>;
}

export interface AppStoreEntryImportReport {
	imported: number;
	skipped: ReadonlyArray<AppStoreSkippedRecord>;
}

export interface AppStorePruneRequest {
	knownStorageKeys: ReadonlyArray<string>;
	listIsAuthoritative: boolean;
}

export interface AppStorePruneReport {
	pruned: ReadonlyArray<string>;
	refusedReason: string | null;
}

export interface AppStoreClearStoreExceptRequest {
	store: string;
	keysToKeep: ReadonlyArray<string>;
}

export declare class AppStoreBinding {
	constructor(options: AppStoreOptions);
	readonly initialization: string;
	close(): void;
	getMetadata(key: string): Promise<string>;
	setMetadata(key: string, value: string): Promise<void>;
	getAllAccounts(): Promise<string>;
	getAccount(storageKey: string): Promise<string>;
	upsertAccount(payload: string): Promise<void>;
	compareAndSwapAccount(payload: string): Promise<boolean>;
	deleteAccount(storageKey: string): Promise<void>;
	importAccounts(payload: string): Promise<string>;
	getAllKnownInstances(): Promise<string>;
	upsertKnownInstance(payload: string): Promise<void>;
	deleteKnownInstance(key: string): Promise<void>;
	getEntries(store: string, scope: string): Promise<string>;
	getEntry(store: string, scope: string, key: string): Promise<string>;
	setEntry(payload: string): Promise<void>;
	deleteEntry(store: string, scope: string, key: string): Promise<void>;
	clearScope(scope: string): Promise<void>;
	clearStoreExcept(payload: string): Promise<void>;
	importEntries(payload: string): Promise<string>;
	prune(payload: string): Promise<string>;
}

export declare const AppStore: (new (options: AppStoreOptions) => AppStoreBinding) | null;
export declare const loadError: Error | null;
