// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from '../main/LocalAppTestSupport.test.mjs';

const {DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY, DESKTOP_LEGACY_HARVEST_CHANNELS} = await import(
	'../../../packages/desktop_ipc/src/LegacyHarvestContract.ts'
);

const SUBMIT_CHANNEL = DESKTOP_LEGACY_HARVEST_CHANNELS.submit;
const FAILING_STORE = Symbol('failing store');
const FAILING_DATABASE = Symbol('failing database');

function fakeDatabase(name, stores) {
	return {
		name,
		version: 2,
		objectStoreNames: Object.keys(stores),
		close() {},
		transaction(storeName) {
			const transaction = {abort() {}, db: this};
			const store = {
				name: storeName,
				keyPath: null,
				transaction,
				openCursor() {
					const request = {};
					const records = stores[storeName];
					if (records === FAILING_STORE) {
						queueMicrotask(() => {
							request.error = new Error('cursor failed');
							request.onerror?.();
						});
						return request;
					}
					let index = -1;
					const cursor = {continue: () => queueMicrotask(() => step())};
					const step = () => {
						index += 1;
						if (index >= records.length) {
							request.result = null;
						} else {
							cursor.key = records[index].key;
							cursor.value = records[index].value;
							request.result = cursor;
						}
						request.onsuccess?.();
					};
					queueMicrotask(step);
					return request;
				},
			};
			transaction.objectStore = () => store;
			return transaction;
		},
	};
}

function installHarvestDocument(
	stores,
	localStorage,
	location = {protocol: 'https:', hostname: 'web.fluxer.app', pathname: '/__fluxer-desktop-storage-harvest'},
) {
	const submissions = [];
	let settle = () => undefined;
	const submitted = new Promise((resolve) => {
		settle = resolve;
	});
	submissions.submitted = submitted;
	installElectronStub({
		ipcRenderer: {
			invoke: (channel, submission) => {
				submissions.push({channel, submission});
				settle();
				return Promise.resolve();
			},
		},
	});
	globalThis.window = {
		location,
		setTimeout: (callback, delay) => setTimeout(callback, delay),
		clearTimeout: (timer) => clearTimeout(timer),
		localStorage,
		indexedDB: {
			open(name) {
				if (stores[name] === FAILING_DATABASE) throw new Error('database is wedged');
				const request = {};
				queueMicrotask(() => {
					if (stores[name] === undefined) {
						request.onupgradeneeded?.();
						request.onerror?.();
						return;
					}
					request.result = fakeDatabase(name, stores[name]);
					request.onsuccess?.();
				});
				return request;
			},
		},
	};
	return submissions;
}

function readableStorage(entries) {
	const keys = Object.keys(entries);
	return {length: keys.length, key: (index) => keys[index] ?? null, getItem: (key) => entries[key] ?? null};
}

async function runHarvest(caseName, stores, localStorage = readableStorage({token: 'tok'})) {
	const submissions = installHarvestDocument(stores, localStorage);
	await import(`./LegacyHarvestPreload.ts?${caseName}`);
	await Promise.race([
		submissions.submitted,
		new Promise((resolve) => {
			const guard = setTimeout(resolve, 5_000);
			guard.unref?.();
		}),
	]);
	assert.equal(submissions.length, 1);
	assert.equal(submissions[0].channel, SUBMIT_CHANNEL);
	return submissions[0].submission;
}

async function harvestRuns(caseName, location) {
	const submissions = installHarvestDocument({}, readableStorage({token: 'tok'}), location);
	await import(`./LegacyHarvestPreload.ts?${caseName}`);
	await Promise.race([
		submissions.submitted,
		new Promise((resolve) => {
			const guard = setTimeout(resolve, 750);
			guard.unref?.();
		}),
	]);
	return submissions.length > 0;
}

describe('which origins the harvest will read storage from', () => {
	const sentinel = '/__fluxer-desktop-storage-harvest';

	test('https is trusted, whatever the host', async () => {
		assert.equal(
			await harvestRuns('https-remote', {protocol: 'https:', hostname: 'web.fluxer.app', pathname: sentinel}),
			true,
		);
	});

	test('http on loopback is trusted, so a local dev instance can be migrated', async () => {
		for (const hostname of ['127.0.0.1', 'localhost', '[::1]']) {
			assert.equal(
				await harvestRuns(`http-loopback-${hostname}`, {protocol: 'http:', hostname, pathname: sentinel}),
				true,
				`${hostname} over http is a local dev instance and must be harvestable, otherwise the development channel can never migrate an existing profile`,
			);
		}
	});

	test('http off loopback is refused, so storage is never read over a plaintext network origin', async () => {
		for (const hostname of ['web.fluxer.app', 'example.com', '10.0.0.1']) {
			assert.equal(
				await harvestRuns(`http-remote-${hostname}`, {protocol: 'http:', hostname, pathname: sentinel}),
				false,
				`${hostname} over plaintext http must never have its storage harvested`,
			);
		}
	});

	test('a non http scheme is refused', async () => {
		assert.equal(await harvestRuns('file-scheme', {protocol: 'file:', hostname: '', pathname: sentinel}), false);
	});
});

describe('a value the codec cannot represent', () => {
	test('costs its own record, not the whole legacy origin', async () => {
		const oversized = new Blob([new Uint8Array(33 * 1024 * 1024)], {type: 'video/mp4'});
		const submission = await runHarvest('oversized-blob', {
			'fluxer-theme-library': {
				assets: [{key: null, value: {id: 'a', data: oversized}}],
				meta: [{key: null, value: {enabledThemeIds: ['t1']}}],
			},
		});

		assert.equal(submission.ok, true);
		assert.ok(submission.payload.truncated.includes('fluxer-theme-library/assets/0.data'));
		assert.deepEqual({...submission.payload.localStorage}, {token: 'tok'});
		const byStore = new Map(submission.payload.stores.map((store) => [store.store, store.records.length]));
		assert.equal(byStore.get('assets'), 1);
		assert.equal(byStore.get('meta'), 1);
		assert.equal(submission.payload.stores.find((store) => store.store === 'assets').records[0].value.data, null);
		assert.deepEqual(submission.payload.blobs, []);
	});

	test('an Error instance and a cycle are reduced without losing the other records', async () => {
		const cyclic = {name: 'theme'};
		cyclic.self = cyclic;
		const submission = await runHarvest('unencodable-shapes', {
			'fluxer-app-storage': {
				entries: [
					{key: null, value: {scope: 'global', lastError: new Error('boom')}},
					{key: null, value: cyclic},
					{key: null, value: {scope: 'global', key: 'Theme', value: 'dark'}},
				],
			},
		});

		assert.equal(submission.ok, true);
		assert.equal(submission.payload.stores[0].records.length, 3);
		assert.deepEqual({...submission.payload.stores[0].records[0].value}, {scope: 'global', lastError: {}});
		assert.deepEqual({...submission.payload.stores[0].records[1].value}, {name: 'theme', self: null});
		assert.deepEqual(
			{...submission.payload.stores[0].records[2].value},
			{
				scope: 'global',
				key: 'Theme',
				value: 'dark',
			},
		);
	});

	test('a sub-object referenced twice survives at both sites, because aliasing is not a cycle', async () => {
		const shared = {id: 'guild-1', name: 'Fluxer'};
		const nested = {tree: {left: shared, right: shared}};
		const submission = await runHarvest('aliased-references', {
			'fluxer-app-storage': {
				entries: [
					{key: null, value: {primary: shared, mirror: shared, list: [shared, shared]}},
					{key: null, value: nested},
				],
			},
		});

		assert.equal(submission.ok, true);
		const [aliased, deep] = submission.payload.stores[0].records;
		assert.deepEqual({...aliased.value.primary}, {id: 'guild-1', name: 'Fluxer'});
		assert.deepEqual({...aliased.value.mirror}, {id: 'guild-1', name: 'Fluxer'});
		assert.deepEqual(
			aliased.value.list.map((item) => ({...item})),
			[
				{id: 'guild-1', name: 'Fluxer'},
				{id: 'guild-1', name: 'Fluxer'},
			],
		);
		assert.deepEqual({...deep.value.tree.left}, {id: 'guild-1', name: 'Fluxer'});
		assert.deepEqual({...deep.value.tree.right}, {id: 'guild-1', name: 'Fluxer'});
		assert.equal(
			submission.payload.truncated.some((label) => label.startsWith('fluxer-app-storage/')),
			false,
			'a repeated reference is not a loss, so no record may be reported as truncated',
		);
	});

	test('a cycle still collapses once the recursion stack unwinds past it', async () => {
		const root = {name: 'root'};
		root.self = root;
		root.child = {parent: root, label: 'child'};
		const submission = await runHarvest('cycle-after-alias-fix', {
			'fluxer-app-storage': {entries: [{key: null, value: root}]},
		});

		assert.equal(submission.ok, true);
		const encoded = submission.payload.stores[0].records[0].value;
		assert.equal(encoded.self, null);
		assert.equal(encoded.child.parent, null);
		assert.equal(encoded.child.label, 'child');
	});

	test('a blob is encoded under the key the contract declares, so the app-side decoder still finds it', async () => {
		const submission = await runHarvest('blob-reference-key', {
			'fluxer-theme-library': {
				assets: [{key: null, value: {id: 'a', data: new Blob([new Uint8Array([1, 2, 3])], {type: 'image/png'})}}],
			},
		});

		assert.equal(submission.ok, true);
		const reference = submission.payload.stores[0].records[0].value.data[DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY];
		assert.equal(typeof reference.blobId, 'string');
		assert.equal(reference.type, 'image/png');
		assert.equal(reference.size, 3);
		assert.equal(submission.payload.blobs.length, 1);
		assert.equal(submission.payload.blobs[0].blobId, reference.blobId);
	});

	test('an unreadable localStorage fails the submission so the harvest is retried', async () => {
		const blocked = {
			get length() {
				throw new Error('storage is blocked');
			},
			key: () => null,
			getItem: () => null,
		};
		const submission = await runHarvest('blocked-local-storage', {'fluxer-app-storage': {entries: []}}, blocked);

		assert.equal(submission.ok, false);
		assert.equal(submission.payload, null);
		assert.match(submission.error, /localStorage/);
	});

	test('an unreadable FluxerAccounts store fails the submission so saved accounts are not lost', async () => {
		const submission = await runHarvest('failing-accounts-store', {
			FluxerAccounts: {accounts: FAILING_STORE},
			'fluxer-app-storage': {entries: []},
		});

		assert.equal(submission.ok, false);
		assert.equal(submission.payload, null);
		assert.match(submission.error, /FluxerAccounts\/accounts/);
	});

	test('a FluxerAccounts database that cannot be opened fails the submission', async () => {
		const submission = await runHarvest('failing-accounts-database', {FluxerAccounts: FAILING_DATABASE});

		assert.equal(submission.ok, false);
		assert.match(submission.error, /FluxerAccounts/);
	});

	test('an unreadable store in a non-critical database is still only a truncation', async () => {
		const submission = await runHarvest('failing-voice-stats-store', {
			FluxerAccounts: {accounts: [{key: null, value: {userId: '1'}}]},
			FluxerVoiceStats: {samples: FAILING_STORE},
		});

		assert.equal(submission.ok, true);
		assert.ok(submission.payload.truncated.includes('FluxerVoiceStats/samples'));
		assert.equal(submission.payload.stores.find((store) => store.store === 'accounts').records.length, 1);
	});

	test('an origin with no legacy databases is not a failure', async () => {
		const submission = await runHarvest('no-databases', {});

		assert.equal(submission.ok, true);
		assert.deepEqual(submission.payload.stores, []);
	});
});
