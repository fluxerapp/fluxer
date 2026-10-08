// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {
	createFakeAppStoreBinding,
	createInvoker,
	createTemporaryUserData,
	loadDesktopStorageIpcBundle,
} from './DesktopAppStorageHarness.mjs';

const {createAppStoreBoundary, createDesktopAppStorageIpcRoutes, createDesktopStoragePreloadAPI, DesktopAppStorage} =
	await loadDesktopStorageIpcBundle();

const ACCOUNT_KEY = 'https://self.hosted.example/api::42';

const ACCOUNT_RECORD = {
	userId: '42',
	token: 'token-42',
	lastActive: 1_755_000_000_000,
	instance: {apiEndpoint: 'https://self.hosted.example/api'},
};

function createBridge() {
	const binding = createFakeAppStoreBinding();
	const storage = new DesktopAppStorage({
		userDataPath: createTemporaryUserData(),
		createBoundary: (options) => createAppStoreBoundary(options, binding.FakeAppStore),
	});
	storage.open();
	const routes = createDesktopAppStorageIpcRoutes(storage);
	return {storage, routes, api: createDesktopStoragePreloadAPI(createInvoker(routes))};
}

describe('desktop app store IPC', () => {
	test('exposes exactly the three account, storage and known-instance namespaces', () => {
		const {storage, api} = createBridge();
		assert.deepEqual(Object.keys(api).sort(), ['desktopAccounts', 'desktopKnownInstances', 'desktopStorage']);
		assert.ok(Object.isFrozen(api.desktopAccounts));
		assert.ok(Object.isFrozen(api.desktopStorage));
		assert.ok(Object.isFrozen(api.desktopKnownInstances));
		storage.close();
	});

	test('no channel names a filesystem path and every preload call reaches a registered handler', async () => {
		const {storage, routes} = createBridge();
		for (const channel of Object.keys(routes)) {
			assert.equal(/path/i.test(channel), false, `${channel} must not name a filesystem path`);
		}
		const invoked = [];
		const probe = createDesktopStoragePreloadAPI({
			invoke: (channel, ...args) => {
				invoked.push(channel);
				return createInvoker(routes).invoke(channel, ...args);
			},
		});
		await probe.desktopStorage.getStatus();
		await probe.desktopAccounts.getAll();
		await probe.desktopKnownInstances.getAll();
		assert.deepEqual(invoked, [
			'desktop-storage:get-status',
			'desktop-accounts:get-all',
			'desktop-known-instances:get-all',
		]);
		storage.close();
	});

	test('round-trips an account and a scoped entry through the whole preload and channel path', async () => {
		const {storage, api} = createBridge();

		await api.desktopAccounts.upsert({storageKey: ACCOUNT_KEY, record: ACCOUNT_RECORD});
		assert.deepEqual(await api.desktopAccounts.get(ACCOUNT_KEY), {
			storageKey: ACCOUNT_KEY,
			record: ACCOUNT_RECORD,
		});

		await api.desktopStorage.set(ACCOUNT_KEY, 'Theme', '{"theme":"dark"}');
		const entry = await api.desktopStorage.get(ACCOUNT_KEY, 'Theme');
		assert.equal(entry.value, '{"theme":"dark"}');
		assert.equal(typeof entry.updatedAt, 'number');
		assert.deepEqual(
			(await api.desktopStorage.load(ACCOUNT_KEY)).map((row) => row.key),
			['Theme'],
		);

		await api.desktopKnownInstances.upsert({
			instanceKey: 'https://self.hosted.example/api',
			domain: 'self.hosted.example',
			displayName: 'Self Hosted',
			lastUsed: 1_755_000_000_001,
		});
		assert.equal((await api.desktopKnownInstances.getAll()).length, 1);

		assert.deepEqual(await api.desktopStorage.getStatus(), {
			available: true,
			authorityExpected: false,
			schemaVersion: 1,
			quarantined: false,
			quarantineReason: null,
			unavailableReason: null,
		});
		storage.close();
	});

	test('carries the import request and its phase marker over one channel call', async () => {
		const {storage, api} = createBridge();

		const accounts = await api.desktopAccounts.import({
			records: [{storageKey: ACCOUNT_KEY, record: ACCOUNT_RECORD}],
			marker: {key: 'import.phase', value: 'accounts'},
		});
		assert.deepEqual(accounts, {imported: 1, skipped: [], unusableInstances: []});

		const entries = await api.desktopStorage.import({
			entries: [{scope: ACCOUNT_KEY, key: 'Theme', value: 'dark', updatedAt: 7}],
			marker: {key: 'import.phase', value: 'entries'},
		});
		assert.deepEqual(entries, {imported: 1, skipped: []});
		assert.equal(await api.desktopStorage.getMarker('import.phase'), 'entries');
		storage.close();
	});

	test('surfaces a rejected request to the renderer rather than resolving with a wrong value', async () => {
		const {storage, api} = createBridge();
		await assert.rejects(() => api.desktopAccounts.get(''), /must be a non-empty NUL-free string/);
		storage.close();
	});
});
