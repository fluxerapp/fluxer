// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import path from 'node:path';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

const PRIMARY_APP_DATA = path.join('/tmp', 'fluxer-desktop-user-data-test', 'primary');
const RELOCATED_APP_DATA = path.join('/tmp', 'fluxer-desktop-user-data-test', 'relocated');

const setPathCalls = [];
let appDataPath = PRIMARY_APP_DATA;

installElectronStub({
	app: {
		isReady: () => true,
		getAppPath: () => appDataPath,
		getPath: (name) => (name === 'appData' ? appDataPath : path.join(appDataPath, name)),
		setPath: (name, value) => {
			setPathCalls.push([name, value]);
		},
	},
});

let moduleInstance = 0;

async function loadUserDataPath() {
	moduleInstance += 1;
	setPathCalls.length = 0;
	appDataPath = PRIMARY_APP_DATA;
	return import(`../common/UserDataPath.ts?instance=${moduleInstance}`);
}

describe('user data path configuration', () => {
	test('memoises the resolved paths and sets userData exactly once', async () => {
		const {configureUserDataPath, isPortableMode} = await loadUserDataPath();

		const first = configureUserDataPath();
		const second = configureUserDataPath();

		assert.equal(second, first);
		assert.equal(first.portable, false);
		assert.equal(isPortableMode(), false);
		assert.equal(first.base, path.join(PRIMARY_APP_DATA, first.directoryName));
		assert.deepEqual(setPathCalls, [['userData', first.base]]);
	});

	test('refuses to reconfigure userData at a different path', async () => {
		const {configureUserDataPath, UserDataPathConflictError} = await loadUserDataPath();

		const configured = configureUserDataPath();
		appDataPath = RELOCATED_APP_DATA;

		assert.throws(() => configureUserDataPath(), UserDataPathConflictError);
		assert.deepEqual(setPathCalls, [['userData', configured.base]]);

		appDataPath = PRIMARY_APP_DATA;
		assert.equal(configureUserDataPath(), configured);
		assert.deepEqual(setPathCalls, [['userData', configured.base]]);
	});
});
