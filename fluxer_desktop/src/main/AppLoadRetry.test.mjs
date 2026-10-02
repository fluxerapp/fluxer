// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./AppLoadRetry.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function loadAppLoadRetry() {
	const module = {exports: {}};
	const context = vm.createContext({
		module,
		exports: module.exports,
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return module.exports;
}

const APP_URL = 'https://web.canary.fluxer.app/';
const MIGRATED_URL = 'https://canary.fluxer.com/';
const silentLogger = {info() {}, warn() {}, error() {}};

function createHarness({appUrl = APP_URL, fallbackUrl = null} = {}) {
	const listeners = new Map();
	const timers = [];
	const loads = [];
	const prompts = [];
	let commits = 0;
	let outcome = 'dns-failure';
	const webContents = {
		on(event, listener) {
			listeners.set(event, [...(listeners.get(event) ?? []), listener]);
		},
		emit(event, ...args) {
			for (const listener of listeners.get(event) ?? []) listener({}, ...args);
		},
		isDestroyed: () => false,
		isLoadingMainFrame: () => false,
		loadURL(url) {
			loads.push(url);
			if (outcome === 'ok') {
				webContents.emit('did-navigate', url, 200, 'OK');
				webContents.emit('did-finish-load');
				return Promise.resolve();
			}
			if (outcome === 'aborted') {
				return Promise.reject(new Error(`ERR_ABORTED (-3) loading '${url}'`));
			}
			webContents.emit('did-fail-load', -105, 'ERR_NAME_NOT_RESOLVED', url, true);
			webContents.emit('did-finish-load');
			return Promise.reject(new Error(`ERR_NAME_NOT_RESOLVED (-105) loading '${url}'`));
		},
	};
	const {createAppLoadRetry} = loadAppLoadRetry();
	const retry = createAppLoadRetry({
		webContents,
		appUrl,
		logger: silentLogger,
		isTrustedUrl: () => true,
		getFallbackUrl: (url) => (url === MIGRATED_URL ? fallbackUrl : null),
		onRepeatedFailure: (failure) => prompts.push(failure),
		onCommitted: () => {
			commits += 1;
		},
		setTimer: (callback, delay) => {
			const timer = {callback, delay, cleared: false};
			timers.push(timer);
			return timer;
		},
		clearTimer: (timer) => {
			timer.cleared = true;
		},
	});
	const settle = () => new Promise((resolve) => setImmediate(resolve));
	return {
		retry,
		loads,
		prompts,
		get commits() {
			return commits;
		},
		setOutcome(next) {
			outcome = next;
		},
		pendingDelays: () => timers.filter((timer) => !timer.cleared && !timer.fired).map((timer) => timer.delay),
		async fireNextTimer() {
			const timer = timers.find((candidate) => !candidate.cleared && !candidate.fired);
			assert.ok(timer, 'expected a pending retry timer');
			timer.fired = true;
			timer.callback();
			await settle();
			return timer.delay;
		},
		settle,
	};
}

describe('AppLoadRetry', () => {
	test('backs off on a DNS failure even though the error page fires did-finish-load', async () => {
		const harness = createHarness();
		harness.retry.start();
		await harness.settle();

		const delays = [];
		for (let i = 0; i < 8; i += 1) {
			assert.equal(harness.pendingDelays().length, 1);
			delays.push(await harness.fireNextTimer());
		}

		assert.deepEqual(delays, [1000, 2000, 4000, 8000, 16000, 30000, 30000, 30000]);
		assert.equal(harness.loads.length, 9);
	});

	test('resets the backoff and dismisses the prompt once the app commits', async () => {
		const harness = createHarness();
		harness.retry.start();
		await harness.settle();
		await harness.fireNextTimer();
		await harness.fireNextTimer();
		assert.equal(harness.prompts.length, 1);

		harness.setOutcome('ok');
		await harness.fireNextTimer();
		assert.equal(harness.commits, 1);
		assert.deepEqual(harness.pendingDelays(), []);

		harness.setOutcome('dns-failure');
		harness.retry.retryNow();
		await harness.settle();
		assert.deepEqual(harness.pendingDelays(), [1000]);
	});

	test('asks the user only after three consecutive failures', async () => {
		const harness = createHarness();
		harness.retry.start();
		await harness.settle();
		await harness.fireNextTimer();
		assert.equal(harness.prompts.length, 0);

		await harness.fireNextTimer();
		assert.equal(harness.prompts.length, 1);
		assert.equal(harness.prompts[0].errorCode, -105);
		assert.equal(harness.prompts[0].url, APP_URL);
	});

	test('does not schedule a retry for an aborted load', async () => {
		const harness = createHarness();
		harness.setOutcome('aborted');
		harness.retry.start();
		await harness.settle();

		assert.deepEqual(harness.pendingDelays(), []);
	});

	test('falls back from the migrated origin without backing off', async () => {
		const harness = createHarness({appUrl: MIGRATED_URL, fallbackUrl: APP_URL});
		harness.retry.start();
		await harness.settle();

		assert.deepEqual(harness.loads.slice(0, 2), [MIGRATED_URL, APP_URL]);
		assert.equal(harness.retry.getAppUrl(), APP_URL);
		assert.deepEqual(harness.pendingDelays(), [1000]);
	});
});
