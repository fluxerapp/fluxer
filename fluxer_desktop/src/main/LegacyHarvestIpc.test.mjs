// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {mkdtempSync} from 'node:fs';
import {rm} from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {after, describe, test} from 'node:test';
import {installDesktopAppStorageStub, installElectronStub, installWindowStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();
installWindowStub(null);
installDesktopAppStorageStub(null);

const {
	DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY,
	DESKTOP_LEGACY_HARVEST_CHANNELS,
	DESKTOP_LEGACY_HARVEST_VALUE_ENCODING_KEY,
	DESKTOP_LEGACY_REPLANT_MARKER_KEY,
	desktopLegacyReplantMarker,
	isDesktopLegacyReplantMarker,
} = await import('../../../packages/desktop_ipc/src/LegacyHarvestContract.ts');
const {
	DESKTOP_LEGACY_AUTHORITY_MARKER_KEY,
	DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE,
	DesktopLegacyImportPhase,
	desktopLegacyImportMarker,
} = await import('../../../packages/desktop_ipc/src/StorageContract.ts');
const {discardStagedHarvest, readStagedHarvest, readStagedHarvestLocalStorageSync, writeStagedHarvest} = await import(
	'./LegacyOriginHarvestStore.ts'
);
const {cleanupLegacyHarvestHandlers, registerLegacyHarvestHandlers} = await import('./LegacyHarvestIpc.ts');
const {getLegacyAppOrigin} = await import('../common/DesktopConfig.ts');
const OFFICIAL_ORIGIN = getLegacyAppOrigin();

const temporaryRoots = [];

function harvestFixture() {
	return {
		version: 1,
		origin: OFFICIAL_ORIGIN,
		capturedAt: 1_700_000_000_000,
		localStorage: {token: 'live.session.token', userId: '100000000000000002', theme: 'dark'},
		stores: [
			{
				database: 'FluxerAccounts',
				version: 2,
				store: 'accounts',
				records: [{key: null, value: {userId: '100000000000000002', token: 'tok'}}],
			},
		],
		serviceWorkersUnregistered: 0,
		cachesDeleted: 0,
		truncated: [],
	};
}

function nestedMapValue(levels, {innermostEmpty = false} = {}) {
	let value = innermostEmpty ? {[DESKTOP_LEGACY_HARVEST_VALUE_ENCODING_KEY]: [1, 'map', []]} : 0;
	for (let index = 0; index < levels; index += 1) {
		value = {[DESKTOP_LEGACY_HARVEST_VALUE_ENCODING_KEY]: [1, 'map', [['k', value]]]};
	}
	return value;
}

function createStoreStub() {
	const markers = new Map();
	return {
		markers,
		getMarker: (key) => Promise.resolve(markers.get(key) ?? null),
		setMarker: (key, value) => {
			markers.set(key, value);
			return Promise.resolve();
		},
	};
}

function createRefusingStoreStub() {
	return {
		getMarker: () => Promise.resolve(null),
		setMarker: () => Promise.reject(new Error('the desktop app store is unavailable')),
	};
}

function createUserData(registered) {
	const userData = mkdtempSync(path.join(os.tmpdir(), 'legacy-harvest-ipc-'));
	temporaryRoots.push(userData);
	installElectronStub({
		app: {isReady: () => true, getPath: () => userData, getAppPath: () => userData},
		ipcMain: {
			handle: (channel, listener) => registered.set(channel, listener),
			on: (channel, listener) => registered.set(channel, listener),
			removeHandler: (channel) => registered.delete(channel),
			removeAllListeners: (channel) => registered.delete(channel),
		},
	});
	return userData;
}

function senderEvent() {
	const frame = {detached: false, parent: null, url: 'fluxer-app://app/'};
	frame.top = frame;
	const sender = {isDestroyed: () => false, isLoadingMainFrame: () => false, mainFrame: frame};
	frame.owner = sender;
	installWindowStub({webContents: sender});
	return {sender, senderFrame: frame};
}

after(async () => {
	cleanupLegacyHarvestHandlers();
	for (const root of temporaryRoots) {
		await rm(root, {recursive: true, force: true});
	}
});

describe('marking the harvest replanted', () => {
	test('writes the filesystem witness even when the app store cannot take the marker', async () => {
		const handlers = new Map();
		createUserData(handlers);
		await writeStagedHarvest(harvestFixture(), new Map());
		installDesktopAppStorageStub(createRefusingStoreStub());
		const document = senderEvent();
		cleanupLegacyHarvestHandlers();
		registerLegacyHarvestHandlers();

		await assert.rejects(() => handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.markReplanted)(document));

		assert.equal(readStagedHarvestLocalStorageSync(), null);
		cleanupLegacyHarvestHandlers();
	});

	test('writes the filesystem witness when there is no app store at all', async () => {
		const handlers = new Map();
		createUserData(handlers);
		await writeStagedHarvest(harvestFixture(), new Map());
		installDesktopAppStorageStub(null);
		const document = senderEvent();
		cleanupLegacyHarvestHandlers();
		registerLegacyHarvestHandlers();

		await handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.markReplanted)(document);

		assert.equal(readStagedHarvestLocalStorageSync(), null);
		cleanupLegacyHarvestHandlers();
	});

	test('records both witnesses when the store is healthy', async () => {
		const handlers = new Map();
		createUserData(handlers);
		await writeStagedHarvest(harvestFixture(), new Map());
		const store = createStoreStub();
		installDesktopAppStorageStub(store);
		const document = senderEvent();
		cleanupLegacyHarvestHandlers();
		registerLegacyHarvestHandlers();

		await handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.markReplanted)(document);

		assert.equal(readStagedHarvestLocalStorageSync(), null);
		assert.ok(store.markers.get(DESKTOP_LEGACY_REPLANT_MARKER_KEY));
		cleanupLegacyHarvestHandlers();
	});
});

describe('the preboot localStorage read', () => {
	test('serves the harvested keys until the replant is recorded', async () => {
		const handlers = new Map();
		createUserData(handlers);
		await writeStagedHarvest(harvestFixture(), new Map());
		installDesktopAppStorageStub(null);

		assert.equal(readStagedHarvestLocalStorageSync().theme, 'dark');

		const document = senderEvent();
		cleanupLegacyHarvestHandlers();
		registerLegacyHarvestHandlers();
		await handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.markReplanted)(document);
		assert.equal(readStagedHarvestLocalStorageSync(), null);
		cleanupLegacyHarvestHandlers();
	});
});

describe('serving the staged harvest for the replant', () => {
	test('serves the harvest until the replant is recorded, then never again', async () => {
		const handlers = new Map();
		createUserData(handlers);
		await writeStagedHarvest(harvestFixture(), new Map());
		installDesktopAppStorageStub(createStoreStub());
		const document = senderEvent();
		cleanupLegacyHarvestHandlers();
		registerLegacyHarvestHandlers();
		const read = handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.read);

		assert.equal((await read(document)).stores[0].records.length, 1);
		assert.equal((await read(document)).stores[0].records.length, 1);

		await handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.markReplanted)(document);

		assert.equal(await read(document), null);
		assert.notEqual(await readStagedHarvest(), null);
		cleanupLegacyHarvestHandlers();
	});
});

describe('discarding the staged harvest', () => {
	test('reports the removal once and does nothing on later launches', async () => {
		const handlers = new Map();
		createUserData(handlers);
		await writeStagedHarvest(harvestFixture(), new Map());

		assert.equal(await discardStagedHarvest(), true);
		assert.equal(await readStagedHarvest(), null);
		assert.equal(await discardStagedHarvest(), false);
		assert.equal(readStagedHarvestLocalStorageSync(), null);
	});

	test('keeps the harvest until authority is committed and the import is done', async () => {
		const handlers = new Map();
		createUserData(handlers);
		await writeStagedHarvest(harvestFixture(), new Map());
		const store = createStoreStub();
		installDesktopAppStorageStub(store);
		const document = senderEvent();
		cleanupLegacyHarvestHandlers();
		registerLegacyHarvestHandlers();
		await handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.markReplanted)(document);

		await handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.discard)(document);
		assert.notEqual(await readStagedHarvest(), null);

		store.markers.set(DESKTOP_LEGACY_AUTHORITY_MARKER_KEY, DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE);
		await handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.discard)(document);
		assert.notEqual(await readStagedHarvest(), null);

		const done = desktopLegacyImportMarker(DesktopLegacyImportPhase.DONE, 1);
		store.markers.set(done.key, done.value);
		await handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.discard)(document);
		assert.equal(await readStagedHarvest(), null);
		cleanupLegacyHarvestHandlers();
	});
});

describe('serving a deeply nested harvest to the renderer', () => {
	test('a Map chain the encoder accepts survives the read', async () => {
		const handlers = new Map();
		createUserData(handlers);
		const harvest = harvestFixture();
		const records = [{key: null, value: nestedMapValue(64, {innermostEmpty: true})}];
		await writeStagedHarvest({...harvest, stores: [{...harvest.stores[0], records}]}, new Map());
		installDesktopAppStorageStub(null);
		const document = senderEvent();
		cleanupLegacyHarvestHandlers();
		registerLegacyHarvestHandlers();

		const served = await handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.read)(document);

		assert.equal(served.stores[0].records.length, 1);
		cleanupLegacyHarvestHandlers();
	});
});

describe('serving a harvest whose blob reference is damaged', () => {
	test('drops the field instead of failing the whole read', async () => {
		const handlers = new Map();
		createUserData(handlers);
		const harvest = harvestFixture();
		const records = [{key: null, value: {avatar: {[DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY]: {blobId: '../escape'}}}}];
		await writeStagedHarvest({...harvest, stores: [{...harvest.stores[0], records}]}, new Map());
		installDesktopAppStorageStub(null);
		const document = senderEvent();
		cleanupLegacyHarvestHandlers();
		registerLegacyHarvestHandlers();

		const served = await handlers.get(DESKTOP_LEGACY_HARVEST_CHANNELS.read)(document);

		assert.equal(served.stores[0].records[0].value.avatar, null);
		assert.equal(served.localStorage.theme, 'dark');
		cleanupLegacyHarvestHandlers();
	});
});

describe('the replant marker', () => {
	test('the writer produces exactly what the reader accepts', () => {
		assert.equal(isDesktopLegacyReplantMarker(desktopLegacyReplantMarker(1_700_000_000_000)), true);
		assert.equal(isDesktopLegacyReplantMarker(JSON.stringify({version: 2, replantedAt: 1})), false);
		assert.equal(isDesktopLegacyReplantMarker(null), false);
	});
});
