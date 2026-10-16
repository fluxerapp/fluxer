// SPDX-License-Identifier: AGPL-3.0-or-later

import {mkdtempSync, readFileSync, renameSync, writeFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {tmpdir} from 'node:os';
import nodePath from 'node:path';
import {fileURLToPath, pathToFileURL} from 'node:url';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const DESKTOP_ROOT = nodePath.resolve(fileURLToPath(new URL('../..', import.meta.url)));

const ENTRY = [
	"export {createAppStoreBoundary} from './src/main/AppStoreNativeBoundary.ts';",
	"export {DesktopAppStorage} from './src/main/DesktopAppStorage.ts';",
	"export {createDesktopAppStorageIpcRoutes} from './src/main/DesktopAppStorageIpc.ts';",
	"export {createDesktopStoragePreloadAPI} from './src/preload/DesktopStoragePreload.ts';",
].join('\n');

let loaded = null;

export async function loadDesktopStorageIpcBundle() {
	if (loaded != null) {
		return loaded;
	}
	const bundle = esbuild.buildSync({
		absWorkingDir: DESKTOP_ROOT,
		stdin: {contents: ENTRY, resolveDir: DESKTOP_ROOT, loader: 'ts', sourcefile: 'DesktopStorageIpcBundle.ts'},
		bundle: true,
		format: 'esm',
		platform: 'node',
		target: 'node20',
		write: false,
		tsconfig: 'tsconfig.json',
	});
	const directory = mkdtempSync(nodePath.join(tmpdir(), 'fluxer-app-store-bundle-'));
	const file = nodePath.join(directory, 'DesktopStorageIpcBundle.mjs');
	writeFileSync(file, bundle.outputFiles[0].text, 'utf8');
	loaded = await import(pathToFileURL(file).href);
	return loaded;
}

export function readDesktopAppStorageSource() {
	return readFileSync(nodePath.join(DESKTOP_ROOT, 'src/main/DesktopAppStorage.ts'), 'utf8');
}

const SIDECAR_SUFFIXES = ['', '-journal', '-wal', '-shm'];

function emptyState() {
	return {metadata: {}, accounts: {}, knownInstances: {}, entries: {}};
}

function quarantine(storeFile, reason) {
	const quarantined = `${storeFile}.${reason}-${Date.now()}`;
	for (const suffix of SIDECAR_SUFFIXES) {
		try {
			renameSync(`${storeFile}${suffix}`, `${quarantined}${suffix}`);
		} catch (error) {
			if (error.code !== 'ENOENT') {
				throw error;
			}
		}
	}
	return quarantined;
}

function entryAddress(store, scope, key) {
	return JSON.stringify([store, scope, key]);
}

function lookupKeyIsUsable(value) {
	return typeof value === 'string' && value.length > 0 && !value.includes('\0') && Buffer.byteLength(value) <= 1024;
}

export function createFakeAppStoreBinding({delays = new Map(), maxValueBytes = 8 * 1024 * 1024} = {}) {
	const observed = {constructed: [], closed: 0, calls: []};

	class FakeAppStore {
		#file;
		#state;
		#open = true;
		#initialization;

		constructor(options) {
			observed.constructed.push(options);
			this.#file = options.path;
			let quarantinedPath = null;
			let quarantineReason = null;
			let state = null;
			let raw = null;
			try {
				raw = readFileSync(this.#file, 'utf8');
			} catch (error) {
				if (error.code !== 'ENOENT') {
					throw error;
				}
			}
			if (raw != null) {
				try {
					state = JSON.parse(raw);
				} catch {
					quarantineReason = 'corrupt';
				}
			}
			if (state != null && options.derivationVersion != null) {
				const recorded = state.metadata['derivation.version'];
				if (recorded != null && recorded !== options.derivationVersion) {
					quarantineReason = 'derivation';
					state = null;
				}
			}
			if (quarantineReason != null) {
				quarantinedPath = quarantine(this.#file, quarantineReason);
				state = null;
			}
			this.#state = state ?? emptyState();
			if (options.derivationVersion != null) {
				this.#state.metadata['derivation.version'] ??= options.derivationVersion;
			}
			this.#state.metadata['schema.version'] ??= '1';
			this.#flush();
			this.#initialization = JSON.stringify({
				path: this.#file,
				schemaVersion: 1,
				previousSchemaVersion: state == null ? 0 : 1,
				appliedMigrations: state == null ? 1 : 0,
				quarantinedPath,
				quarantineReason,
			});
		}

		get initialization() {
			return this.#initialization;
		}

		close() {
			observed.closed += 1;
			this.#open = false;
			this.#flush();
		}

		#flush() {
			writeFileSync(this.#file, JSON.stringify(this.#state), 'utf8');
		}

		async #run(name, work) {
			observed.calls.push(name);
			const delay = delays.get(name);
			if (delay != null) {
				await delay;
			}
			if (!this.#open) {
				throw new Error('app store is closed');
			}
			const result = work();
			this.#flush();
			return result;
		}

		getMetadata(key) {
			return this.#run('getMetadata', () => JSON.stringify(this.#state.metadata[key] ?? null));
		}

		setMetadata(key, value) {
			return this.#run('setMetadata', () => {
				this.#state.metadata[key] = value;
			});
		}

		getAllAccounts() {
			return this.#run('getAllAccounts', () =>
				JSON.stringify(
					Object.entries(this.#state.accounts)
						.sort((left, right) => (right[1].lastActive ?? 0) - (left[1].lastActive ?? 0))
						.map(([storageKey, stored]) => ({storageKey, record: JSON.parse(stored.recordJson)})),
				),
			);
		}

		getAccount(storageKey) {
			return this.#run('getAccount', () => {
				const stored = this.#state.accounts[storageKey];
				return JSON.stringify(stored == null ? null : {storageKey, record: JSON.parse(stored.recordJson)});
			});
		}

		#writeAccount(entry) {
			if (!lookupKeyIsUsable(entry.storageKey)) {
				return 'account storage key is unusable';
			}
			if (entry.record == null || typeof entry.record !== 'object' || Array.isArray(entry.record)) {
				return 'account record must be a JSON object';
			}
			this.#state.accounts[entry.storageKey] = {
				recordJson: JSON.stringify(entry.record),
				lastActive: typeof entry.record.lastActive === 'number' ? entry.record.lastActive : 0,
			};
			return null;
		}

		upsertAccount(payload) {
			return this.#run('upsertAccount', () => {
				const refusal = this.#writeAccount(JSON.parse(payload));
				if (refusal != null) {
					throw new Error(refusal);
				}
			});
		}

		deleteAccount(storageKey) {
			return this.#run('deleteAccount', () => {
				delete this.#state.accounts[storageKey];
			});
		}

		importAccounts(payload) {
			return this.#run('importAccounts', () => {
				const request = JSON.parse(payload);
				const report = {imported: 0, skipped: [], unusableInstances: []};
				for (const entry of request.records) {
					const refusal = this.#writeAccount(entry);
					if (refusal == null) {
						report.imported += 1;
					} else {
						report.skipped.push({key: entry.storageKey, reason: refusal});
					}
				}
				if (request.marker != null) {
					this.#state.metadata[request.marker.key] = request.marker.value;
				}
				return JSON.stringify(report);
			});
		}

		getAllKnownInstances() {
			return this.#run('getAllKnownInstances', () =>
				JSON.stringify(
					Object.entries(this.#state.knownInstances)
						.sort((left, right) => right[1].lastUsed - left[1].lastUsed)
						.map(([key, stored]) => ({key, record: JSON.parse(stored.recordJson)})),
				),
			);
		}

		upsertKnownInstance(payload) {
			return this.#run('upsertKnownInstance', () => {
				const entry = JSON.parse(payload);
				if (!lookupKeyIsUsable(entry.key)) {
					throw new Error('known instance key is unusable');
				}
				this.#state.knownInstances[entry.key] = {
					recordJson: JSON.stringify(entry.record),
					lastUsed: typeof entry.record.lastUsed === 'number' ? entry.record.lastUsed : 0,
				};
			});
		}

		deleteKnownInstance(key) {
			return this.#run('deleteKnownInstance', () => {
				delete this.#state.knownInstances[key];
			});
		}

		getEntries(store, scope) {
			return this.#run('getEntries', () =>
				JSON.stringify(
					Object.values(this.#state.entries)
						.filter((entry) => entry.store === store && entry.scope === scope)
						.sort((left, right) => (left.key < right.key ? -1 : 1)),
				),
			);
		}

		getEntry(store, scope, key) {
			return this.#run('getEntry', () => JSON.stringify(this.#state.entries[entryAddress(store, scope, key)] ?? null));
		}

		#writeEntry(entry) {
			if (!lookupKeyIsUsable(entry.store) || !lookupKeyIsUsable(entry.scope) || !lookupKeyIsUsable(entry.key)) {
				return 'scoped storage key is unusable';
			}
			if (Buffer.byteLength(entry.value) > maxValueBytes) {
				return 'scoped storage value is too large';
			}
			this.#state.entries[entryAddress(entry.store, entry.scope, entry.key)] = entry;
			return null;
		}

		setEntry(payload) {
			return this.#run('setEntry', () => {
				const refusal = this.#writeEntry(JSON.parse(payload));
				if (refusal != null) {
					throw new Error(refusal);
				}
			});
		}

		deleteEntry(store, scope, key) {
			return this.#run('deleteEntry', () => {
				delete this.#state.entries[entryAddress(store, scope, key)];
			});
		}

		clearScope(scope) {
			return this.#run('clearScope', () => {
				for (const [address, entry] of Object.entries(this.#state.entries)) {
					if (entry.scope === scope) {
						delete this.#state.entries[address];
					}
				}
			});
		}

		clearStoreExcept(payload) {
			return this.#run('clearStoreExcept', () => {
				const request = JSON.parse(payload);
				const keep = new Set(request.keysToKeep);
				for (const [address, entry] of Object.entries(this.#state.entries)) {
					if (entry.store === request.store && !keep.has(entry.key)) {
						delete this.#state.entries[address];
					}
				}
			});
		}

		importEntries(payload) {
			return this.#run('importEntries', () => {
				const request = JSON.parse(payload);
				const report = {imported: 0, skipped: []};
				for (const entry of request.entries) {
					const refusal = this.#writeEntry(entry);
					if (refusal == null) {
						report.imported += 1;
					} else {
						report.skipped.push({key: `${entry.store}/${entry.scope}/${entry.key}`, reason: refusal});
					}
				}
				if (request.marker != null) {
					this.#state.metadata[request.marker.key] = request.marker.value;
				}
				return JSON.stringify(report);
			});
		}

		prune(payload) {
			return this.#run('prune', () => {
				const request = JSON.parse(payload);
				if (!request.listIsAuthoritative) {
					return JSON.stringify({pruned: [], refusedReason: 'the known account list was not authoritative'});
				}
				const known = new Set(request.knownStorageKeys);
				const pruned = [];
				for (const storageKey of Object.keys(this.#state.accounts)) {
					if (!known.has(storageKey)) {
						pruned.push(storageKey);
						delete this.#state.accounts[storageKey];
					}
				}
				return JSON.stringify({pruned, refusedReason: null});
			});
		}
	}

	return {FakeAppStore, observed};
}

export function createTemporaryUserData() {
	return mkdtempSync(nodePath.join(tmpdir(), 'fluxer-user-data-'));
}

export function createInvoker(routes) {
	return {
		invoke: async (channel, ...args) => {
			const handler = routes[channel];
			if (handler == null) {
				throw new Error(`No main-process handler is registered for ${channel}`);
			}
			return handler(...structuredClone(args));
		},
	};
}
