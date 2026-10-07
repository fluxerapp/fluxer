// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {createAppStoreBoundary, loadAppStoreBinding} from './AppStoreNativeBoundary.ts';

const OPTIONS = {path: '/userData/app-store.sqlite'};

function createFakeBinding(overrides = {}) {
	const calls = [];
	const record =
		(name) =>
		(...args) => {
			calls.push({name, args});
			return Promise.resolve(overrides[name] ?? undefined);
		};
	class FakeAppStore {
		constructor(options) {
			calls.push({name: 'constructor', args: [options]});
			this.initialization = JSON.stringify({
				path: options.path,
				schemaVersion: 1,
				previousSchemaVersion: 0,
				appliedMigrations: 0,
				quarantinedPath: null,
				quarantineReason: null,
			});
		}
		close() {
			calls.push({name: 'close', args: []});
		}
		getMetadata = record('getMetadata');
		setMetadata = record('setMetadata');
		getAllAccounts = record('getAllAccounts');
		getAccount = record('getAccount');
		upsertAccount = record('upsertAccount');
		deleteAccount = record('deleteAccount');
		importAccounts = record('importAccounts');
		getAllKnownInstances = record('getAllKnownInstances');
		upsertKnownInstance = record('upsertKnownInstance');
		deleteKnownInstance = record('deleteKnownInstance');
		getEntries = record('getEntries');
		getEntry = record('getEntry');
		setEntry = record('setEntry');
		deleteEntry = record('deleteEntry');
		clearScope = record('clearScope');
		clearStoreExcept = record('clearStoreExcept');
		importEntries = record('importEntries');
		prune = record('prune');
	}
	return {FakeAppStore, calls};
}

describe('AppStoreNativeBoundary', () => {
	test('returns null instead of throwing when the binding is unavailable', () => {
		assert.equal(createAppStoreBoundary(OPTIONS, null), null);
	});

	test('caches the binding probe so a missing addon is resolved once', () => {
		assert.equal(loadAppStoreBinding(), loadAppStoreBinding());
	});

	test('decodes the initialization JSON the binding exposes as a string', () => {
		const {FakeAppStore} = createFakeBinding();
		const boundary = createAppStoreBoundary(OPTIONS, FakeAppStore);
		assert.deepEqual(boundary.initialization, {
			path: OPTIONS.path,
			schemaVersion: 1,
			previousSchemaVersion: 0,
			appliedMigrations: 0,
			quarantinedPath: null,
			quarantineReason: null,
		});
	});

	test('encodes object-form record writes as a single JSON payload', async () => {
		const {FakeAppStore, calls} = createFakeBinding();
		const boundary = createAppStoreBoundary(OPTIONS, FakeAppStore);
		await boundary.upsertAccount({storageKey: 'https://api.fluxer.app::42', record: {userId: '42'}});
		const call = calls.find((entry) => entry.name === 'upsertAccount');
		assert.deepEqual(call.args, ['{"storageKey":"https://api.fluxer.app::42","record":{"userId":"42"}}']);
	});

	test('splits object-form addresses back into the positional binding arguments', async () => {
		const {FakeAppStore, calls} = createFakeBinding({getEntry: JSON.stringify(null)});
		const boundary = createAppStoreBoundary(OPTIONS, FakeAppStore);
		assert.equal(await boundary.getEntry({store: 'app', scope: 'global', key: 'theme'}), null);
		const call = calls.find((entry) => entry.name === 'getEntry');
		assert.deepEqual(call.args, ['app', 'global', 'theme']);
	});

	test('decodes list and report results', async () => {
		const accounts = [{storageKey: 'https://api.fluxer.app::42', record: {userId: '42'}}];
		const {FakeAppStore} = createFakeBinding({
			getAllAccounts: JSON.stringify(accounts),
			prune: JSON.stringify({pruned: ['https://api.fluxer.app::7'], refusedReason: null}),
		});
		const boundary = createAppStoreBoundary(OPTIONS, FakeAppStore);
		assert.deepEqual(await boundary.getAllAccounts(), accounts);
		assert.deepEqual(await boundary.prune({knownStorageKeys: [], listIsAuthoritative: true}), {
			pruned: ['https://api.fluxer.app::7'],
			refusedReason: null,
		});
	});
});
