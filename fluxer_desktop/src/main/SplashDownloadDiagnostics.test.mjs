// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {buildSplashDiagnosticsText, redactDiagnosticText} = await import('@electron/main/SplashDiagnostics');

function diagnosticsInput(overrides = {}) {
	return {
		generatedAt: Date.UTC(2026, 9, 8, 1, 0, 0),
		splashOpenedAt: Date.UTC(2026, 9, 8, 0, 59, 0),
		appVersion: '2026.1008.1',
		channel: 'canary',
		platform: 'win32',
		arch: 'x64',
		osVersion: '10.0.26100',
		electronVersion: '44.4.1',
		logsPath: 'C:\\Users\\u\\AppData\\Roaming\\fluxer_desktop_canary\\logs',
		userDataPath: 'C:\\Users\\u\\AppData\\Roaming\\fluxercanary',
		packageOrigin: 'https://pkgs.fluxer.com',
		proxyRoute: 'PROXY 10.0.0.2:3128',
		splashStatus: 'download-stalled',
		updaterStatus: 'retry-wait',
		pendingModule: 'fluxer_renderer',
		receivedBytes: 42000000,
		totalBytes: 146684915,
		bytesPerSecond: 3100000,
		committed: {fluxer_sourcemaps: 'b'.repeat(64), fluxer_renderer: 'a'.repeat(64)},
		lastError: 'package download stalled for 30000ms',
		lastErrorAt: Date.UTC(2026, 9, 8, 0, 59, 40),
		...overrides,
	};
}

describe('splash diagnostics', () => {
	test('never carries query values or credential-looking pairs', () => {
		const text = buildSplashDiagnosticsText(
			diagnosticsInput({
				lastError: 'failed to reach https://pkgs.fluxer.com/x?token=abc123&sig=zzz: authorization: Bearer secretvalue',
				proxyRoute: 'PROXY user:pass@proxy:8080',
			}),
		);
		assert.ok(!text.includes('abc123'));
		assert.ok(!text.includes('zzz'));
		assert.ok(!text.includes('secretvalue'));
		assert.ok(!text.includes('user:pass'));
		assert.ok(text.includes('?token=…&sig=…'));
		assert.ok(text.includes('Proxy route: PROXY …@proxy:8080'));
	});

	test('redaction leaves plain diagnostic text alone', () => {
		assert.equal(
			redactDiagnosticText('Package origin: https://pkgs.fluxer.com'),
			'Package origin: https://pkgs.fluxer.com',
		);
	});
});
