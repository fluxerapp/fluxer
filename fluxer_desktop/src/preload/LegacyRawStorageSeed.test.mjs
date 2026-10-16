// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import '../main/LocalAppTestSupport.test.mjs';

const {DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY, DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE} = await import(
	'../../../packages/desktop_ipc/src/LegacyHarvestContract.ts'
);
const {applyLegacyRawLocalStorage} = await import('./LegacyRawStorageSeed.ts');

function createStorage(initial = {}, refuseKey = null) {
	const entries = new Map(Object.entries(initial));
	return {
		entries,
		getItem: (key) => entries.get(key) ?? null,
		setItem: (key, value) => {
			if (refuseKey === key) throw new Error('quota exceeded');
			entries.set(key, value);
		},
	};
}

function installWindow(protocol, storage) {
	globalThis.window = {location: {protocol}, localStorage: storage};
	return storage;
}

function createRenderer(payload) {
	const calls = [];
	return {
		calls,
		sendSync: (channel) => {
			calls.push(channel);
			return payload;
		},
	};
}

describe('seeding the legacy localStorage onto the new origin', () => {
	test('does nothing outside the local app scheme', () => {
		const storage = installWindow('https:', createStorage());
		const renderer = createRenderer({token: 'tok'});
		applyLegacyRawLocalStorage(renderer);
		assert.deepEqual(renderer.calls, []);
		assert.equal(storage.entries.size, 0);
	});

	test('seeds every harvested string key and records the witness once', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		const renderer = createRenderer({token: 'tok', theme: 'dark', count: 3});
		applyLegacyRawLocalStorage(renderer);
		assert.equal(storage.getItem('token'), 'tok');
		assert.equal(storage.getItem('theme'), 'dark');
		assert.equal(storage.getItem('count'), null);
		applyLegacyRawLocalStorage(renderer);
		assert.equal(renderer.calls.length, 1);
	});

	test('a harvested session marks the preboot session so the first frame is the signed-in shell', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer({token: 'tok', userId: '42'}));
		assert.equal(storage.getItem('fluxer:gateway:preboot:session'), '1');
	});

	test('a half session or one the new origin already holds leaves the preboot marker alone', () => {
		const tokenOnly = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer({token: 'tok'}));
		assert.equal(tokenOnly.getItem('fluxer:gateway:preboot:session'), null);
		const current = installWindow('fluxer-app:', createStorage({token: 'the new token', userId: '7'}));
		applyLegacyRawLocalStorage(createRenderer({token: 'tok', userId: '42'}));
		assert.equal(current.getItem('fluxer:gateway:preboot:session'), null);
	});

	test('never overwrites a value the new origin already holds', () => {
		const storage = installWindow('fluxer-app:', createStorage({token: 'the new token'}));
		applyLegacyRawLocalStorage(createRenderer({token: 'the legacy token'}));
		assert.equal(storage.getItem('token'), 'the new token');
	});

	test('ignores a witness carried inside the harvested corpus', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer({[DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY]: 'stale', token: 'tok'}));
		assert.equal(
			storage.getItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY),
			DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE,
		);
	});

	test('a refused preboot read leaves the witness unset so a later launch retries', () => {
		const storage = installWindow('fluxer-app:', createStorage());
		applyLegacyRawLocalStorage(createRenderer(null));
		assert.equal(storage.getItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY), null);
	});

	test('a storage that refuses a write leaves the witness unset', () => {
		const storage = installWindow('fluxer-app:', createStorage({}, 'theme'));
		applyLegacyRawLocalStorage(createRenderer({token: 'tok', theme: 'dark'}));
		assert.equal(storage.getItem('token'), 'tok');
		assert.equal(storage.getItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY), null);
	});
});
