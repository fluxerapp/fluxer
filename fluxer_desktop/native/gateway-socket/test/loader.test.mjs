// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import test from 'node:test';
import gatewaySocketPure from '../pure.cjs';

test('loader resolves native binaries for supported platforms and architectures', () => {
	assert.equal(gatewaySocketPure.nativeFileName('darwin', 'x64'), 'gateway-socket.darwin-x64.node');
	assert.equal(gatewaySocketPure.nativeFileName('darwin', 'arm64'), 'gateway-socket.darwin-arm64.node');
	assert.equal(gatewaySocketPure.nativeFileName('linux', 'x64'), 'gateway-socket.linux-x64-gnu.node');
	assert.equal(gatewaySocketPure.nativeFileName('linux', 'arm64'), 'gateway-socket.linux-arm64-gnu.node');
	assert.equal(gatewaySocketPure.nativeFileName('win32', 'x64'), 'gateway-socket.win32-x64-msvc.node');
	assert.equal(gatewaySocketPure.nativeFileName('win32', 'arm64'), 'gateway-socket.win32-arm64-msvc.node');
});

test('loader rejects unsupported platforms and architectures', () => {
	assert.throws(() => gatewaySocketPure.nativeFileName('darwin', 'ia32'), /Unsupported gateway-socket target/);
	assert.throws(() => gatewaySocketPure.nativeFileName('aix', 'x64'), /Unsupported gateway-socket target/);
});
