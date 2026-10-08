// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installTestModuleStub} from './LocalAppTestSupport.test.mjs';

installTestModuleStub(
	'electron-log',
	`export const calls = [];
		const record = (level) => (...args) => calls.push({level, args});
		export default {debug: record('debug'), error: record('error'), info: record('info'), warn: record('warn')};`,
);

process.env.FLUXER_DISABLE_NATIVE_GATEWAY = '1';

const {createGatewaySocketBoundary, isNativeGatewayAvailable, loadGatewaySocketBinding} = await import(
	'./GatewaySocketNativeBoundary.ts'
);
const {calls: electronLogCalls} = await import('electron-log');

function createFakeConnection() {
	return {
		sendText: () => {},
		sendBinary: () => {},
		close: () => {},
		dispose: () => {},
	};
}

describe('GatewaySocketNativeBoundary', () => {
	test('returns null instead of throwing when the binding is unavailable', () => {
		assert.equal(createGatewaySocketBoundary(null), null);
	});

	test('honours the environment kill switch and logs the disable reason once', () => {
		assert.equal(loadGatewaySocketBinding(), null);
		assert.equal(loadGatewaySocketBinding(), null);
		assert.equal(isNativeGatewayAvailable(), false);
		assert.equal(createGatewaySocketBoundary(), null);
		const disableLogs = electronLogCalls.filter((entry) =>
			String(entry.args[0]).includes('native gateway transport is disabled'),
		);
		assert.equal(disableLogs.length, 1);
		assert.equal(disableLogs[0].args[1], 'FLUXER_DISABLE_NATIVE_GATEWAY');
	});

	test('pins the transport mode to gateway and forwards both callbacks positionally', () => {
		const calls = [];
		const connection = createFakeConnection();
		const boundary = createGatewaySocketBoundary((options, onEvent, onTerminalEvent) => {
			calls.push({options, onEvent, onTerminalEvent});
			return connection;
		});
		const onEvent = () => {};
		const onTerminalEvent = () => {};
		const result = boundary.connect({url: 'wss://gateway.fluxer.app', address: null, onEvent, onTerminalEvent});
		assert.equal(result, connection);
		assert.equal(calls.length, 1);
		assert.deepEqual(calls[0].options, {url: 'wss://gateway.fluxer.app', address: null, mode: 'gateway'});
		assert.equal(calls[0].onEvent, onEvent);
		assert.equal(calls[0].onTerminalEvent, onTerminalEvent);
	});

	test('forwards a pinned address unchanged', () => {
		const calls = [];
		const boundary = createGatewaySocketBoundary((options) => {
			calls.push(options);
			return createFakeConnection();
		});
		boundary.connect({
			url: 'wss://gateway.fluxer.app',
			address: '203.0.113.7',
			onEvent: () => {},
			onTerminalEvent: () => {},
		});
		assert.equal(calls[0].address, '203.0.113.7');
	});
});
