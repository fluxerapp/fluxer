// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {chmodSync, mkdtempSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {
	DesktopAccountStoreEvent,
	DesktopStorePermissionState,
	formatDesktopAccountStoreTelemetry,
	probeDesktopStorePermissions,
	readDesktopAccountStoreTelemetry,
	recordDesktopAccountStoreEvent,
} = await import('./DesktopAccountStoreTelemetry.ts');

function userDataDir() {
	return mkdtempSync(path.join(tmpdir(), 'fluxer-account-store-telemetry-'));
}

describe('DesktopAccountStoreTelemetry', () => {
	test('an unwritten store reports every counter at zero', () => {
		const counts = readDesktopAccountStoreTelemetry(userDataDir());
		assert.deepEqual(counts, {});
		assert.equal(
			formatDesktopAccountStoreTelemetry(counts),
			'desktop.account_store.migrated=0, desktop.account_store.quarantined=0, desktop.account_store.fallback_to_web=0, desktop.account_store.permission_hardening_failed=0',
		);
	});

	test('counts accumulate across launches', () => {
		const userDataPath = userDataDir();
		recordDesktopAccountStoreEvent(userDataPath, DesktopAccountStoreEvent.QUARANTINED);
		recordDesktopAccountStoreEvent(userDataPath, DesktopAccountStoreEvent.QUARANTINED);
		recordDesktopAccountStoreEvent(userDataPath, DesktopAccountStoreEvent.FALLBACK_TO_WEB);
		assert.deepEqual(readDesktopAccountStoreTelemetry(userDataPath), {
			'desktop.account_store.quarantined': 2,
			'desktop.account_store.fallback_to_web': 1,
		});
	});

	test('an unreadable or foreign counter file never throws and never invents counts', () => {
		const userDataPath = userDataDir();
		writeFileSync(path.join(userDataPath, 'account-store-telemetry-v1.json'), '{"version":9,"counts":{"x":1}}');
		assert.deepEqual(readDesktopAccountStoreTelemetry(userDataPath), {});
		writeFileSync(path.join(userDataPath, 'account-store-telemetry-v1.json'), 'not json');
		assert.deepEqual(readDesktopAccountStoreTelemetry(userDataPath), {});
	});

	test('a missing store file is unknown, not a hardening failure', () => {
		assert.equal(
			probeDesktopStorePermissions(path.join(userDataDir(), 'nope.sqlite3')),
			DesktopStorePermissionState.UNKNOWN,
		);
	});

	test('a group-readable store file is reported as widened', {skip: process.platform === 'win32'}, () => {
		const userDataPath = userDataDir();
		chmodSync(userDataPath, 0o700);
		const storeFile = path.join(userDataPath, 'desktop-app-store.sqlite3');
		writeFileSync(storeFile, '', {mode: 0o600});
		assert.equal(probeDesktopStorePermissions(storeFile), DesktopStorePermissionState.HARDENED);
		chmodSync(storeFile, 0o640);
		assert.equal(probeDesktopStorePermissions(storeFile), DesktopStorePermissionState.WIDENED);
		chmodSync(storeFile, 0o600);
		chmodSync(userDataPath, 0o755);
		assert.equal(probeDesktopStorePermissions(storeFile), DesktopStorePermissionState.WIDENED);
	});
});
