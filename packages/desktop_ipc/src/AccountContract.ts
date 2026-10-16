// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_ACCOUNT_CHANNELS = Object.freeze({
	getAll: 'desktop-accounts:get-all',
	get: 'desktop-accounts:get',
	upsert: 'desktop-accounts:upsert',
	compareAndSwap: 'desktop-accounts:compare-and-swap',
	delete: 'desktop-accounts:delete',
	import: 'desktop-accounts:import',
	prune: 'desktop-accounts:prune',
} as const);

export interface DesktopStoreJSONObject {
	[key: string]: DesktopStoreJSONValue;
}

export type DesktopStoreJSONValue =
	| string
	| number
	| boolean
	| null
	| Array<DesktopStoreJSONValue>
	| DesktopStoreJSONObject;

export interface DesktopAccountRecord {
	readonly storageKey: string;
	readonly record: DesktopStoreJSONObject;
}

export interface DesktopAccountCompareAndSwapRequest {
	readonly expected: DesktopAccountRecord;
	readonly replacement: DesktopAccountRecord;
}

export interface DesktopStoreMarker {
	readonly key: string;
	readonly value: string;
}

export interface DesktopStoreSkippedRecord {
	readonly key: string;
	readonly reason: string;
}

export interface DesktopAccountImportRequest {
	readonly records: ReadonlyArray<DesktopAccountRecord>;
	readonly marker?: DesktopStoreMarker | null;
}

export interface DesktopAccountImportReport {
	readonly imported: number;
	readonly skipped: ReadonlyArray<DesktopStoreSkippedRecord>;
	readonly unusableInstances: ReadonlyArray<string>;
}

export interface DesktopAccountPruneRequest {
	readonly knownStorageKeys: ReadonlyArray<string>;
	readonly listIsAuthoritative: boolean;
}

export interface DesktopAccountPruneReport {
	readonly pruned: ReadonlyArray<string>;
	readonly refusedReason: string | null;
}

export interface DesktopAccountStorageAPI {
	getAll: () => Promise<Array<DesktopAccountRecord>>;
	get: (storageKey: string) => Promise<DesktopAccountRecord | null>;
	upsert: (account: DesktopAccountRecord) => Promise<void>;
	compareAndSwap: (request: DesktopAccountCompareAndSwapRequest) => Promise<boolean>;
	delete: (storageKey: string) => Promise<void>;
	import: (request: DesktopAccountImportRequest) => Promise<DesktopAccountImportReport>;
	prune: (request: DesktopAccountPruneRequest) => Promise<DesktopAccountPruneReport>;
}
