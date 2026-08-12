// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import test from 'node:test';
import platformInfoPure from '../pure.cjs';

test('loader resolves native binaries for supported platforms and architectures', () => {
	assert.equal(platformInfoPure.nativeFileName('darwin', 'x64'), 'platform-info.darwin-x64.node');
	assert.equal(platformInfoPure.nativeFileName('darwin', 'arm64'), 'platform-info.darwin-arm64.node');
	assert.equal(platformInfoPure.nativeFileName('linux', 'x64'), 'platform-info.linux-x64-gnu.node');
	assert.equal(platformInfoPure.nativeFileName('linux', 'arm64'), 'platform-info.linux-arm64-gnu.node');
	assert.equal(platformInfoPure.nativeFileName('win32', 'x64'), 'platform-info.win32-x64-msvc.node');
	assert.equal(platformInfoPure.nativeFileName('win32', 'arm64'), 'platform-info.win32-arm64-msvc.node');
});
