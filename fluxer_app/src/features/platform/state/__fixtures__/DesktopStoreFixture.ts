// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	DesktopAccountImportRequest,
	DesktopAccountRecord,
	DesktopAccountStorageAPI,
	DesktopStoreJSONObject,
	DesktopStoreSkippedRecord,
} from '@fluxer/desktop_ipc/src/AccountContract';
import type {
	DesktopStorageAPI,
	DesktopStorageEntry,
	DesktopStorageImportRequest,
	DesktopStorageWrite,
	DesktopStoreStatus,
} from '@fluxer/desktop_ipc/src/StorageContract';

export const FAKE_DESKTOP_STORE_AVAILABLE: DesktopStoreStatus = Object.freeze({
	available: true,
	authorityExpected: false,
	schemaVersion: 1,
	quarantined: false,
	quarantineReason: null,
	unavailableReason: null,
});

export interface FakeDesktopStoreSnapshot {
	readonly accounts: ReadonlyArray<readonly [string, string]>;
	readonly entries: ReadonlyArray<readonly [string, string, string]>;
	readonly markers: ReadonlyArray<readonly [string, string]>;
}

export interface FakeDesktopStore {
	readonly accounts: DesktopAccountStorageAPI;
	readonly storage: DesktopStorageAPI;
	failOnce(operation: string): void;
	refuseNext(key: string, reason: string): void;
	setStatus(status: DesktopStoreStatus): void;
	seedAccount(storageKey: string, record: DesktopStoreJSONObject): void;
	seedEntry(scope: string, key: string, value: string, updatedAt: number): void;
	readEntry(scope: string, key: string): DesktopStorageEntry | null;
	readAccount(storageKey: string): DesktopStoreJSONObject | null;
	snapshot(): FakeDesktopStoreSnapshot;
}

export class FakeDesktopStoreFailure extends Error {
	public constructor(operation: string) {
		super(`Injected desktop store failure: ${operation}`);
		this.name = 'FakeDesktopStoreFailure';
	}
}

function desktopStoreValuesEqual(left: unknown, right: unknown): boolean {
	if (Object.is(left, right)) {
		return true;
	}
	if (Array.isArray(left) || Array.isArray(right)) {
		return (
			Array.isArray(left) &&
			Array.isArray(right) &&
			left.length === right.length &&
			left.every((entry, index) => desktopStoreValuesEqual(entry, right[index]))
		);
	}
	if (left === null || right === null || typeof left !== 'object' || typeof right !== 'object') {
		return false;
	}
	const leftEntries = Object.entries(left);
	const rightRecord = right as Record<string, unknown>;
	return (
		leftEntries.length === Object.keys(rightRecord).length &&
		leftEntries.every(
			([key, value]) => Object.hasOwn(rightRecord, key) && desktopStoreValuesEqual(value, rightRecord[key]),
		)
	);
}

export function createFakeDesktopStore(options?: {readonly status?: DesktopStoreStatus}): FakeDesktopStore {
	const accountRows = new Map<string, DesktopStoreJSONObject>();
	const refusals = new Map<string, string>();
	const takeRefusals = (keys: ReadonlyArray<string>): Array<DesktopStoreSkippedRecord> => {
		const skipped: Array<DesktopStoreSkippedRecord> = [];
		for (const key of keys) {
			const reason = refusals.get(key);
			if (reason === undefined) continue;
			refusals.delete(key);
			skipped.push({key, reason});
		}
		return skipped;
	};
	const entryRows = new Map<string, Map<string, DesktopStorageEntry>>();
	const markers = new Map<string, string>();
	const failures = new Set<string>();
	let status = options?.status ?? FAKE_DESKTOP_STORE_AVAILABLE;
	let stamp = 0;

	const nextStamp = (): number => {
		const now = Date.now();
		stamp = now > stamp ? now : stamp + 1;
		return stamp;
	};

	const observe = (value: number): void => {
		stamp = value > stamp ? value : stamp;
	};

	const gate = (operation: string): void => {
		if (failures.delete(operation)) {
			throw new FakeDesktopStoreFailure(operation);
		}
	};

	const scopeOf = (scope: string): Map<string, DesktopStorageEntry> => {
		let entries = entryRows.get(scope);
		if (entries == null) {
			entries = new Map<string, DesktopStorageEntry>();
			entryRows.set(scope, entries);
		}
		return entries;
	};

	const writeEntry = ({scope, key, value, ifAbsent}: DesktopStorageWrite): void => {
		const entries = scopeOf(scope);
		if (ifAbsent === true && entries.has(key)) {
			return;
		}
		entries.set(key, {key, value, updatedAt: nextStamp()});
	};

	const accounts: DesktopAccountStorageAPI = {
		getAll: async () => {
			gate('accounts.getAll');
			return [...accountRows].map(([storageKey, record]) => ({storageKey, record}));
		},

		get: async (storageKey) => {
			gate('accounts.get');
			const record = accountRows.get(storageKey);
			return record == null ? null : {storageKey, record};
		},

		upsert: async (account: DesktopAccountRecord) => {
			gate('accounts.upsert');
			accountRows.set(account.storageKey, account.record);
		},

		compareAndSwap: async ({expected, replacement}) => {
			gate('accounts.compareAndSwap');
			const current = accountRows.get(expected.storageKey);
			if (
				current === undefined ||
				replacement.storageKey !== expected.storageKey ||
				!desktopStoreValuesEqual(current, expected.record)
			) {
				return false;
			}
			accountRows.set(replacement.storageKey, replacement.record);
			return true;
		},

		delete: async (storageKey) => {
			gate('accounts.delete');
			accountRows.delete(storageKey);
		},

		import: async (request: DesktopAccountImportRequest) => {
			gate('accounts.import');
			const skipped = takeRefusals(request.records.map((record) => record.storageKey));
			const refused = new Set(skipped.map((record) => record.key));
			for (const record of request.records) {
				if (refused.has(record.storageKey)) continue;
				accountRows.set(record.storageKey, record.record);
			}
			if (request.marker != null) {
				markers.set(request.marker.key, request.marker.value);
			}
			return {imported: request.records.length - skipped.length, skipped, unusableInstances: []};
		},

		prune: async (request) => {
			gate('accounts.prune');
			if (!request.listIsAuthoritative) {
				return {pruned: [], refusedReason: 'the known account list was not authoritative'};
			}
			const known = new Set(request.knownStorageKeys);
			const pruned: Array<string> = [];
			for (const storageKey of [...accountRows.keys()]) {
				if (known.has(storageKey)) {
					continue;
				}
				accountRows.delete(storageKey);
				entryRows.delete(storageKey);
				pruned.push(storageKey);
			}
			return {pruned, refusedReason: null};
		},
	};

	const storage: DesktopStorageAPI = {
		getStatus: async () => {
			gate('storage.getStatus');
			return status;
		},

		load: async (scope) => {
			gate('storage.load');
			return [...(entryRows.get(scope)?.values() ?? [])];
		},

		get: async (scope, key) => {
			gate('storage.get');
			return entryRows.get(scope)?.get(key) ?? null;
		},

		set: async (scope, key, value) => {
			gate('storage.set');
			writeEntry({scope, key, value});
		},

		delete: async (scope, key) => {
			gate('storage.delete');
			entryRows.get(scope)?.delete(key);
		},

		clearAllForScope: async (scope) => {
			gate('storage.clearAllForScope');
			entryRows.delete(scope);
		},

		clearAllExcept: async (keysToKeep) => {
			gate('storage.clearAllExcept');
			const keep = new Set(keysToKeep);
			for (const entries of entryRows.values()) {
				for (const key of [...entries.keys()]) {
					if (!keep.has(key)) {
						entries.delete(key);
					}
				}
			}
		},

		setMany: async (writes) => {
			gate('storage.setMany');
			for (const write of writes) {
				writeEntry(write);
			}
		},

		import: async (request: DesktopStorageImportRequest) => {
			gate('storage.import');
			const skipped = takeRefusals(request.entries.map((entry) => entry.key));
			const refused = new Set(skipped.map((entry) => entry.key));
			for (const entry of request.entries) {
				if (refused.has(entry.key)) continue;
				observe(entry.updatedAt);
				scopeOf(entry.scope).set(entry.key, {key: entry.key, value: entry.value, updatedAt: entry.updatedAt});
			}
			if (request.marker != null) {
				markers.set(request.marker.key, request.marker.value);
			}
			return {imported: request.entries.length - skipped.length, skipped};
		},

		getMarker: async (key) => {
			gate('storage.getMarker');
			return markers.get(key) ?? null;
		},

		setMarker: async (key, value) => {
			gate('storage.setMarker');
			markers.set(key, value);
		},
	};

	return {
		accounts,
		storage,

		refuseNext(key, reason) {
			refusals.set(key, reason);
		},

		failOnce(operation) {
			failures.add(operation);
		},

		setStatus(next) {
			status = next;
		},

		seedAccount(storageKey, record) {
			accountRows.set(storageKey, record);
		},

		seedEntry(scope, key, value, updatedAt) {
			observe(updatedAt);
			scopeOf(scope).set(key, {key, value, updatedAt});
		},

		readEntry(scope, key) {
			return entryRows.get(scope)?.get(key) ?? null;
		},

		readAccount(storageKey) {
			return accountRows.get(storageKey) ?? null;
		},

		snapshot() {
			const entries: Array<readonly [string, string, string]> = [];
			for (const [scope, scoped] of entryRows) {
				for (const entry of scoped.values()) {
					entries.push([scope, entry.key, entry.value]);
				}
			}
			return {
				accounts: [...accountRows]
					.map(([storageKey, record]) => [storageKey, JSON.stringify(record)] as const)
					.sort((left, right) => (left[0] < right[0] ? -1 : 1)),
				entries: entries.sort((left, right) => (`${left[0]} ${left[1]}` < `${right[0]} ${right[1]}` ? -1 : 1)),
				markers: [...markers].sort((left, right) => (left[0] < right[0] ? -1 : 1)),
			};
		},
	};
}
