// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import test from 'node:test';
import appStorePure from '../pure.cjs';

test('loader resolves native binaries for supported platforms and architectures', () => {
	assert.equal(appStorePure.nativeFileName('darwin', 'x64'), 'app-store.darwin-x64.node');
	assert.equal(appStorePure.nativeFileName('darwin', 'arm64'), 'app-store.darwin-arm64.node');
	assert.equal(appStorePure.nativeFileName('linux', 'x64'), 'app-store.linux-x64-gnu.node');
	assert.equal(appStorePure.nativeFileName('linux', 'arm64'), 'app-store.linux-arm64-gnu.node');
	assert.equal(appStorePure.nativeFileName('win32', 'x64'), 'app-store.win32-x64-msvc.node');
	assert.equal(appStorePure.nativeFileName('win32', 'arm64'), 'app-store.win32-arm64-msvc.node');
});
