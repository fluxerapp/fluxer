// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {ModuleDownloadRate} = await import('@electron/main/ModuleDownloadRate');
const {buildSplashDiagnosticsText, redactDiagnosticText} = await import('@electron/main/SplashDiagnostics');
const {isModulePackageStall, ModulePackageFetchError, ModulePackageStallError, MODULE_PACKAGE_STALL_TIMEOUT_MS} =
	await import('@electron/main/ModulePackageInstaller');
const {serializeSplashState, SplashStatus} = await import('@electron/main/SplashWindow');

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

describe('module download rate', () => {
	test('reports nothing until a second sample and then the transfer rate', () => {
		const rate = new ModuleDownloadRate();
		assert.equal(rate.sample(0, 0), null);
		assert.equal(rate.sample(1_000_000, 1000), 1_000_000);
	});

	test('smooths a sudden change instead of jumping to it', () => {
		const rate = new ModuleDownloadRate();
		rate.sample(0, 0);
		rate.sample(1_000_000, 1000);
		const smoothed = rate.sample(1_000_000 + 4_000_000, 2000);
		assert.ok(smoothed > 1_000_000 && smoothed < 4_000_000, String(smoothed));
	});

	test('keeps the last rate for samples closer together than the minimum window', () => {
		const rate = new ModuleDownloadRate();
		rate.sample(0, 0);
		const first = rate.sample(1_000_000, 1000);
		assert.equal(rate.sample(1_100_000, 1050), first);
	});

	test('starts over when the byte count goes backwards or after a reset', () => {
		const rate = new ModuleDownloadRate();
		rate.sample(0, 0);
		rate.sample(5_000_000, 1000);
		assert.equal(rate.sample(100, 1500), null);
		rate.reset();
		assert.equal(rate.sample(0, 3000), null);
	});
});

describe('package download stalls', () => {
	test('a stall is a transient fetch failure the updater can name', () => {
		const stall = new ModulePackageStallError('fluxer_renderer', MODULE_PACKAGE_STALL_TIMEOUT_MS);
		assert.ok(stall instanceof ModulePackageFetchError);
		assert.equal(stall.status, null);
		assert.equal(isModulePackageStall(new Error('install failed', {cause: stall})), true);
		assert.equal(isModulePackageStall(new ModulePackageFetchError('fluxer_renderer', 503, 'busy')), false);
	});

	test('gives up on a silent transfer after thirty seconds', () => {
		assert.equal(MODULE_PACKAGE_STALL_TIMEOUT_MS, 30000);
	});
});

describe('splash download state', () => {
	test('carries byte counts and speed across the IPC boundary as whole numbers', () => {
		const serialized = serializeSplashState({
			status: SplashStatus.DOWNLOADING_UPDATES,
			receivedBytes: 42_000_000.7,
			totalBytes: 146_684_915,
			bytesPerSecond: 3_100_000.4,
		});
		assert.equal(serialized.receivedBytes, 42_000_000);
		assert.equal(serialized.totalBytes, 146_684_915);
		assert.equal(serialized.bytesPerSecond, 3_100_000);
	});

	test('keeps the stalled status and drops byte values that are not numbers', () => {
		const serialized = serializeSplashState({
			status: SplashStatus.DOWNLOAD_STALLED,
			seconds: 4,
			receivedBytes: Number.NaN,
			totalBytes: '9',
			bytesPerSecond: Number.POSITIVE_INFINITY,
		});
		assert.equal(serialized.status, 'download-stalled');
		assert.equal(serialized.receivedBytes, null);
		assert.equal(serialized.totalBytes, null);
		assert.equal(serialized.bytesPerSecond, null);
	});
});

describe('splash diagnostics', () => {
	test('lists what support needs to place a stuck download', () => {
		const text = buildSplashDiagnosticsText(diagnosticsInput());
		for (const expected of [
			'App version: 2026.1008.1 (canary)',
			'Platform: win32 x64, OS 10.0.26100, Electron 44.4.1',
			'Package origin: https://pkgs.fluxer.com',
			'Proxy route: PROXY 10.0.0.2:3128',
			'Splash status: download-stalled',
			'Pending module: fluxer_renderer',
			'Downloaded: 42000000 bytes of 146684915 bytes',
			'Speed: 3100000 bytes/s',
			'Last updater error: package download stalled for 30000ms',
			'Last updater error at: 2026-10-08T00:59:40.000Z',
			'  fluxer_renderer aaaaaaaaaaaa',
			'  fluxer_sourcemaps bbbbbbbbbbbb',
		]) {
			assert.ok(text.includes(expected), `missing ${expected}\n${text}`);
		}
		assert.ok(text.indexOf('fluxer_renderer aaaa') < text.indexOf('fluxer_sourcemaps bbbb'));
	});

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

	test('says so when nothing is installed yet', () => {
		const text = buildSplashDiagnosticsText(diagnosticsInput({committed: {}, lastError: null, lastErrorAt: null}));
		assert.ok(text.includes('Committed modules:\n  none'));
		assert.ok(text.includes('Last updater error: none'));
	});
});
