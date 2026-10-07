// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync, writeFileSync} from 'node:fs';
import {describe, test} from 'node:test';
import {createTemporaryUserData} from './DesktopAppStorageHarness.mjs';
import {
	DESKTOP_APP_STORE_STATE_FILE_NAME,
	DesktopAppStoreHealth,
	desktopAppStoreStateFile,
	evaluateDesktopAppStoreHealth,
	readDesktopAppStoreState,
	recordDesktopAppStoreSuccess,
} from './DesktopAppStoreHealth.ts';

describe('DesktopAppStoreHealth', () => {
	test('names the state file the plan requires, beside the store', () => {
		const userDataPath = createTemporaryUserData();
		assert.equal(DESKTOP_APP_STORE_STATE_FILE_NAME, 'account-store-state-v1.json');
		assert.equal(desktopAppStoreStateFile(userDataPath).endsWith(DESKTOP_APP_STORE_STATE_FILE_NAME), true);
	});

	test('an addon that never worked here falls back silently rather than reporting a regression', () => {
		const userDataPath = createTemporaryUserData();
		assert.equal(readDesktopAppStoreState(userDataPath), null);
		assert.equal(
			evaluateDesktopAppStoreHealth({available: false, state: readDesktopAppStoreState(userDataPath)}),
			DesktopAppStoreHealth.UNPROVEN,
		);
	});

	test('a store that worked yesterday and cannot open today is a regression', () => {
		const userDataPath = createTemporaryUserData();
		recordDesktopAppStoreSuccess({userDataPath, now: 1_755_000_000_000, schemaVersion: 1});
		assert.equal(
			evaluateDesktopAppStoreHealth({available: false, state: readDesktopAppStoreState(userDataPath)}),
			DesktopAppStoreHealth.REGRESSED,
		);
	});

	test('records the first success once and moves the last success forward', () => {
		const userDataPath = createTemporaryUserData();
		const first = recordDesktopAppStoreSuccess({userDataPath, now: 1_755_000_000_000, schemaVersion: 1});
		const second = recordDesktopAppStoreSuccess({userDataPath, now: 1_755_000_600_000, schemaVersion: 2});

		assert.deepEqual(first, {
			version: 1,
			firstSuccessAt: 1_755_000_000_000,
			lastSuccessAt: 1_755_000_000_000,
			lastSchemaVersion: 1,
		});
		assert.deepEqual(second, {
			version: 1,
			firstSuccessAt: 1_755_000_000_000,
			lastSuccessAt: 1_755_000_600_000,
			lastSchemaVersion: 2,
		});
		assert.deepEqual(readDesktopAppStoreState(userDataPath), second);
		assert.equal(
			evaluateDesktopAppStoreHealth({available: true, state: readDesktopAppStoreState(userDataPath)}),
			DesktopAppStoreHealth.HEALTHY,
		);
	});

	test('an unreadable or foreign state file reads as never having worked', () => {
		const userDataPath = createTemporaryUserData();
		writeFileSync(desktopAppStoreStateFile(userDataPath), 'not json', 'utf8');
		assert.equal(readDesktopAppStoreState(userDataPath), null);

		writeFileSync(
			desktopAppStoreStateFile(userDataPath),
			JSON.stringify({version: 99, firstSuccessAt: 1, lastSuccessAt: 2, lastSchemaVersion: 1}),
			'utf8',
		);
		assert.equal(readDesktopAppStoreState(userDataPath), null);
	});

	test('never removes or truncates the store and only writes its own state file', () => {
		const userDataPath = createTemporaryUserData();
		recordDesktopAppStoreSuccess({userDataPath, now: 1_755_000_000_000, schemaVersion: 1});
		const source = readFileSync(new URL('./DesktopAppStoreHealth.ts', import.meta.url), 'utf8');

		assert.equal(/unlinkSync|rmSync|rimraf|dialog|showErrorBox|showMessageBox/.test(source), false);
	});
});
