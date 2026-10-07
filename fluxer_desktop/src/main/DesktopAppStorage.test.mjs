// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readdirSync, readFileSync, writeFileSync} from 'node:fs';
import nodePath from 'node:path';
import {describe, test} from 'node:test';
import {createAppStoreBoundary} from './AppStoreNativeBoundary.ts';
import {
	createFakeAppStoreBinding,
	createTemporaryUserData,
	readDesktopAppStorageSource,
} from './DesktopAppStorageHarness.mjs';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {
	DESKTOP_APP_STORE_DERIVATION_VERSION,
	DESKTOP_APP_STORE_FILE_NAME,
	DesktopAppStorage,
	DesktopAppStoreCapacityError,
	DesktopAppStoreRequestError,
	DesktopAppStoreShutdownError,
	DesktopAppStoreUnavailableError,
	DesktopAppStoreWriteRefusedError,
} = await import('./DesktopAppStorage.ts');

const ACCOUNT_KEY = 'https://self.hosted.example/api::42';

function accountRecord(overrides = {}) {
	return {
		userId: '42',
		token: 'token-42',
		lastActive: 1_755_000_000_000,
		instance: {apiEndpoint: 'https://self.hosted.example/api'},
		...overrides,
	};
}

function createStorage({userDataPath = createTemporaryUserData(), binding = createFakeAppStoreBinding()} = {}) {
	const storage = new DesktopAppStorage({
		userDataPath,
		createBoundary: (options) => createAppStoreBoundary(options, binding.FakeAppStore),
	});
	return {storage, userDataPath, observed: binding.observed};
}

describe('DesktopAppStorage', () => {
	test('derives the single store file from the user data directory it was constructed with', () => {
		const {storage, userDataPath, observed} = createStorage();
		const opened = storage.open();

		assert.equal(opened.status.available, true);
		assert.equal(opened.storeFile, nodePath.join(userDataPath, DESKTOP_APP_STORE_FILE_NAME));
		assert.equal(observed.constructed.length, 1);
		assert.deepEqual(observed.constructed[0], {
			path: nodePath.join(userDataPath, DESKTOP_APP_STORE_FILE_NAME),
			derivationVersion: DESKTOP_APP_STORE_DERIVATION_VERSION,
		});
		storage.close();
	});

	test('never reaches a dialog when it recovers a store', () => {
		assert.equal(/dialog|showErrorBox|showMessageBox/.test(readDesktopAppStorageSource()), false);
	});

	test('round-trips an account, a scoped entry and a known instance', async () => {
		const {storage} = createStorage();
		storage.open();

		await storage.upsertAccount({storageKey: ACCOUNT_KEY, record: accountRecord()});
		assert.deepEqual(await storage.getAccount(ACCOUNT_KEY), {storageKey: ACCOUNT_KEY, record: accountRecord()});
		assert.deepEqual(await storage.getAllAccounts(), [{storageKey: ACCOUNT_KEY, record: accountRecord()}]);

		await storage.setEntry(ACCOUNT_KEY, 'Theme', '{"theme":"dark"}');
		assert.equal((await storage.getEntry(ACCOUNT_KEY, 'Theme')).value, '{"theme":"dark"}');
		assert.deepEqual(
			(await storage.loadEntries(ACCOUNT_KEY)).map((entry) => entry.key),
			['Theme'],
		);

		await storage.upsertKnownInstance({
			instanceKey: 'https://self.hosted.example/api',
			domain: 'self.hosted.example',
			displayName: 'Self Hosted',
			lastUsed: 1_755_000_000_001,
		});
		assert.deepEqual(await storage.getAllKnownInstances(), [
			{
				instanceKey: 'https://self.hosted.example/api',
				domain: 'self.hosted.example',
				displayName: 'Self Hosted',
				lastUsed: 1_755_000_000_001,
			},
		]);

		await storage.deleteAccount(ACCOUNT_KEY);
		assert.equal(await storage.getAccount(ACCOUNT_KEY), null);
		storage.close();
	});

	test('an account record written by a newer renderer round-trips with its unknown fields intact', async () => {
		const {storage} = createStorage();
		storage.open();
		const record = accountRecord({futureField: {nested: [1, 2, 3]}, presenceIntent: null});

		await storage.upsertAccount({storageKey: ACCOUNT_KEY, record});
		assert.deepEqual((await storage.getAccount(ACCOUNT_KEY)).record, record);
		storage.close();
	});

	test('refuses malformed requests before they reach the store', async () => {
		const {storage} = createStorage();
		storage.open();

		await assert.rejects(() => storage.getAccount(42), DesktopAppStoreRequestError);
		await assert.rejects(() => storage.getAccount(''), /must be a non-empty NUL-free string/);
		await assert.rejects(() => storage.getEntry('bad\0scope', 'k'), /NUL-free/);
		await assert.rejects(() => storage.setEntry('scope', 'key', 'x'.repeat(9 * 1024 * 1024)), /at most/);
		await assert.rejects(() => storage.upsertAccount({storageKey: ACCOUNT_KEY, record: []}), /must be an object/);
		await assert.rejects(
			() => storage.pruneAccounts({knownStorageKeys: [], listIsAuthoritative: 'yes'}),
			/must be a boolean/,
		);
		await assert.rejects(
			() =>
				storage.compareAndSwapAccount({
					expected: {storageKey: ACCOUNT_KEY, record: accountRecord()},
					replacement: {storageKey: `${ACCOUNT_KEY}9`, record: accountRecord({userId: '429'})},
				}),
			/must use the same storageKey/,
		);
		storage.close();
	});

	test('serialises operations in call order even when the store answers out of order', async () => {
		let release = () => {};
		const gate = new Promise((resolve) => {
			release = resolve;
		});
		const binding = createFakeAppStoreBinding({delays: new Map([['setEntry', gate]])});
		const {storage, observed} = createStorage({binding});
		storage.open();

		const slow = storage.setEntry('scope', 'key', 'first');
		const fast = storage.deleteEntry('scope', 'key');
		release();
		await Promise.all([slow, fast]);

		assert.deepEqual(
			observed.calls.filter((name) => name === 'setEntry' || name === 'deleteEntry'),
			['setEntry', 'deleteEntry'],
		);
		assert.equal(await storage.getEntry('scope', 'key'), null);
		storage.close();
	});

	test('stamps every write with a strictly increasing value so newest-wins arbitration is total', async () => {
		const {storage} = createStorage();
		storage.open();

		await storage.setEntry('scope', 'key', 'one');
		const first = await storage.getEntry('scope', 'key');
		await storage.setEntry('scope', 'key', 'two');
		const second = await storage.getEntry('scope', 'key');

		assert.ok(second.updatedAt > first.updatedAt);
		storage.close();
	});

	test('an imported stamp from the web backend never lets a later desktop write look older', async () => {
		const {storage} = createStorage();
		storage.open();
		const future = Date.now() + 60_000;

		await storage.importEntries({
			entries: [{scope: 'scope', key: 'key', value: 'imported', updatedAt: future}],
			marker: {key: 'import.phase', value: 'entries'},
		});
		await storage.setEntry('scope', 'key', 'written-after');
		const entry = await storage.getEntry('scope', 'key');

		assert.equal(entry.value, 'written-after');
		assert.ok(entry.updatedAt > future);
		assert.equal(await storage.getMarker('import.phase'), 'entries');
		storage.close();
	});

	test('setMany writes every entry in one batch and honours ifAbsent against what is already stored', async () => {
		const {storage} = createStorage();
		storage.open();

		await storage.setEntry('scope', 'kept', 'original');
		await storage.setManyEntries([
			{scope: 'scope', key: 'kept', value: 'replacement', ifAbsent: true},
			{scope: 'scope', key: 'fresh', value: 'new'},
		]);

		assert.equal((await storage.getEntry('scope', 'kept')).value, 'original');
		assert.equal((await storage.getEntry('scope', 'fresh')).value, 'new');
		storage.close();
	});

	test('reports a refused batch write instead of pretending every entry landed', async () => {
		const binding = createFakeAppStoreBinding({maxValueBytes: 8});
		const {storage} = createStorage({binding});
		storage.open();

		await assert.rejects(
			() => storage.setManyEntries([{scope: 'scope', key: 'key', value: 'far too long to store'}]),
			DesktopAppStoreWriteRefusedError,
		);
		storage.close();
	});

	test('clearAllForScope and clearAllExcept leave every other scope and retained key alone', async () => {
		const {storage} = createStorage();
		storage.open();

		await storage.setManyEntries([
			{scope: 'a', key: 'keep', value: '1'},
			{scope: 'a', key: 'drop', value: '2'},
			{scope: 'b', key: 'keep', value: '3'},
		]);

		await storage.clearAllForScope('b');
		assert.deepEqual(await storage.loadEntries('b'), []);
		assert.equal((await storage.loadEntries('a')).length, 2);

		await storage.clearAllExcept(['keep']);
		assert.deepEqual(
			(await storage.loadEntries('a')).map((entry) => entry.key),
			['keep'],
		);
		storage.close();
	});

	test('prune refuses a list that is not authoritative and keeps every account row', async () => {
		const {storage} = createStorage();
		storage.open();
		await storage.upsertAccount({storageKey: ACCOUNT_KEY, record: accountRecord()});

		const refused = await storage.pruneAccounts({knownStorageKeys: [], listIsAuthoritative: false});
		assert.equal(refused.refusedReason, 'the known account list was not authoritative');
		assert.equal((await storage.getAllAccounts()).length, 1);
		storage.close();
	});

	test('refuses new work past the concurrent operation budget instead of queueing without bound', async () => {
		let release = () => {};
		const gate = new Promise((resolve) => {
			release = resolve;
		});
		const binding = createFakeAppStoreBinding({delays: new Map([['getMetadata', gate]])});
		const {storage} = createStorage({binding});
		storage.open();

		const inFlight = [];
		for (let index = 0; index < 64; index += 1) {
			inFlight.push(storage.getMarker('import.phase'));
		}
		await assert.rejects(() => storage.getMarker('import.phase'), DesktopAppStoreCapacityError);
		release();
		await Promise.all(inFlight);
		storage.close();
	});
});

describe('DesktopAppStorage recovery', () => {
	test('an identity mismatch renames the store, opens a fresh one and lets the renderer re-seed it', async () => {
		const userDataPath = createTemporaryUserData();
		writeFileSync(
			nodePath.join(userDataPath, DESKTOP_APP_STORE_FILE_NAME),
			JSON.stringify({
				metadata: {'derivation.version': 'a-previous-derivation', 'import.phase': 'done'},
				accounts: {[ACCOUNT_KEY]: {recordJson: JSON.stringify(accountRecord()), lastActive: 1}},
				knownInstances: {},
				entries: {},
			}),
			'utf8',
		);

		const {storage} = createStorage({userDataPath});
		const opened = storage.open();

		assert.equal(opened.status.available, true);
		assert.equal(opened.status.quarantined, true);
		assert.equal(opened.status.quarantineReason, 'derivation');
		assert.ok(opened.quarantinedFile != null);

		const quarantined = readdirSync(userDataPath).filter((name) => name.includes('derivation-'));
		assert.equal(quarantined.length, 1);
		const preserved = JSON.parse(readFileSync(nodePath.join(userDataPath, quarantined[0]), 'utf8'));
		assert.ok(preserved.accounts[ACCOUNT_KEY] != null, 'the quarantined store still holds the account row');

		const recovery = await storage.recover();
		assert.equal(recovery.reseedRequired, true);
		assert.equal(recovery.status.available, true);

		assert.deepEqual(await storage.getAllAccounts(), []);
		assert.equal(await storage.getMarker('import.phase'), null);

		const report = await storage.importAccounts({
			records: [{storageKey: ACCOUNT_KEY, record: accountRecord()}],
			marker: {key: 'import.phase', value: 'accounts'},
		});
		assert.equal(report.imported, 1);
		assert.deepEqual(await storage.getAllAccounts(), [{storageKey: ACCOUNT_KEY, record: accountRecord()}]);
		assert.equal(await storage.getMarker('import.phase'), 'accounts');
		storage.close();
	});

	test('a corrupt store file is renamed rather than unlinked and never blocks startup', async () => {
		const userDataPath = createTemporaryUserData();
		writeFileSync(nodePath.join(userDataPath, DESKTOP_APP_STORE_FILE_NAME), 'this is not a database', 'utf8');

		const {storage} = createStorage({userDataPath});
		const opened = storage.open();

		assert.equal(opened.status.available, true);
		assert.equal(opened.status.quarantineReason, 'corrupt');
		const quarantined = readdirSync(userDataPath).filter((name) => name.includes('corrupt-'));
		assert.equal(quarantined.length, 1);
		assert.equal(readFileSync(nodePath.join(userDataPath, quarantined[0]), 'utf8'), 'this is not a database');

		await storage.upsertAccount({storageKey: ACCOUNT_KEY, record: accountRecord()});
		assert.equal((await storage.getAllAccounts()).length, 1);
		storage.close();
	});

	test('a store that cannot open at all reports itself unavailable instead of throwing at startup', async () => {
		const storage = new DesktopAppStorage({
			userDataPath: createTemporaryUserData(),
			createBoundary: () => {
				throw new Error('SQLITE_CANTOPEN: unable to open database file');
			},
		});
		const opened = storage.open();

		assert.equal(opened.status.available, false);
		assert.match(opened.status.unavailableReason, /SQLITE_CANTOPEN/);
		await assert.rejects(() => storage.getAllAccounts(), DesktopAppStoreUnavailableError);
		assert.deepEqual(await storage.getStoreStatus(), opened.status);
	});

	test('a missing addon leaves the store unavailable and the renderer free to serve the web backend', async () => {
		const storage = new DesktopAppStorage({
			userDataPath: createTemporaryUserData(),
			createBoundary: () => null,
		});
		const opened = storage.open();

		assert.equal(opened.status.available, false);
		assert.equal(opened.status.unavailableReason, 'the app store addon did not load');
		assert.equal((await storage.getStoreStatus()).available, false);
	});

	test('a known load failure is reported instead of the generic addon message', async () => {
		const storage = new DesktopAppStorage({
			userDataPath: createTemporaryUserData(),
			createBoundary: () => null,
			describeLoadFailure: () => 'safety probe terminated by signal SIGKILL',
		});
		const opened = storage.open();

		assert.equal(opened.status.available, false);
		assert.equal(opened.status.unavailableReason, 'safety probe terminated by signal SIGKILL');
	});
});

describe('DesktopAppStorage shutdown', () => {
	test('closing checkpoints everything already committed and closes the native store exactly once', async () => {
		const {storage, userDataPath, observed} = createStorage();
		storage.open();
		await storage.upsertAccount({storageKey: ACCOUNT_KEY, record: accountRecord()});
		await storage.setEntry(ACCOUNT_KEY, 'Theme', '{"theme":"dark"}');

		storage.close();

		assert.equal(observed.closed, 1);
		const persisted = JSON.parse(readFileSync(nodePath.join(userDataPath, DESKTOP_APP_STORE_FILE_NAME), 'utf8'));
		assert.ok(persisted.accounts[ACCOUNT_KEY] != null);
		assert.equal(Object.keys(persisted.entries).length, 1);
	});

	test('a queued import phase that never committed is refused at shutdown so it re-runs on the next launch', async () => {
		let release = () => {};
		const gate = new Promise((resolve) => {
			release = resolve;
		});
		const binding = createFakeAppStoreBinding({delays: new Map([['getMetadata', gate]])});
		const {storage, userDataPath} = createStorage({binding});
		storage.open();

		await storage.setMarker('import.phase', 'accounts');
		const inFlight = storage.getMarker('import.phase');
		const queued = storage.importEntries({
			entries: [{scope: 'scope', key: 'key', value: 'value', updatedAt: 1}],
			marker: {key: 'import.phase', value: 'entries'},
		});

		storage.close();
		release();

		await assert.rejects(() => inFlight, /closed|shutting down/);
		await assert.rejects(() => queued, DesktopAppStoreShutdownError);

		const persisted = JSON.parse(readFileSync(nodePath.join(userDataPath, DESKTOP_APP_STORE_FILE_NAME), 'utf8'));
		assert.equal(persisted.metadata['import.phase'], 'accounts');
		assert.equal(Object.keys(persisted.entries).length, 0);
	});

	test('operations issued after shutdown are refused rather than reopening the store', async () => {
		const {storage} = createStorage();
		storage.open();
		storage.close();

		await assert.rejects(() => storage.getAllAccounts(), /the desktop app store is closed/);
	});
});

describe('DesktopAppStorage authority sentinel', () => {
	const AUTHORITY_KEY = 'legacy_import.authority';
	const AUTHORITY_VALUE = 'committed.v1';
	const sentinelPath = (userDataPath) => nodePath.join(userDataPath, 'desktop-app-authority-v1');

	test('a fresh install expects no authority and writes no sentinel', () => {
		const {storage, userDataPath} = createStorage();
		assert.equal(storage.open().status.authorityExpected, false);
		assert.equal(readdirSync(userDataPath).includes('desktop-app-authority-v1'), false);
		storage.close();
	});

	test('committing authority records the marker before the sentinel that witnesses it', async () => {
		const {storage, userDataPath, observed} = createStorage();
		storage.open();

		await storage.setMarker(AUTHORITY_KEY, AUTHORITY_VALUE);

		assert.equal(readFileSync(sentinelPath(userDataPath), 'utf8'), '1\n');
		assert.equal((await storage.getStoreStatus()).authorityExpected, true);
		assert.ok(observed.calls.includes('setMetadata'), 'the authority marker reached the store');
		storage.close();
	});

	test('a store that cannot take the marker leaves no sentinel claiming authority', async () => {
		const {storage, userDataPath} = createStorage();
		await assert.rejects(() => storage.setMarker(AUTHORITY_KEY, AUTHORITY_VALUE));
		assert.equal(readdirSync(userDataPath).includes('desktop-app-authority-v1'), false);
		assert.equal((await storage.getStoreStatus()).authorityExpected, false);
	});

	test('clearing authority removes the sentinel and the expectation together', async () => {
		const {storage, userDataPath} = createStorage();
		storage.open();
		await storage.setMarker(AUTHORITY_KEY, AUTHORITY_VALUE);
		assert.equal((await storage.getStoreStatus()).authorityExpected, true);

		await storage.setMarker(AUTHORITY_KEY, '');

		assert.equal(readdirSync(userDataPath).includes('desktop-app-authority-v1'), false);
		assert.equal((await storage.getStoreStatus()).authorityExpected, false);
		storage.close();
	});

	test('a sentinel left by an earlier launch makes the next one expect authority', () => {
		const {storage, userDataPath} = createStorage();
		storage.open();
		writeFileSync(sentinelPath(userDataPath), '1\n');
		storage.close();

		const next = createStorage({userDataPath});
		assert.equal(next.storage.open().status.authorityExpected, true);
		next.storage.close();
	});
});
