// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {mkdtempSync, readFileSync, rmSync, writeFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {beforeEach, describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

const velopack = {
	pendingRestart: null,
	pendingRestartError: null,
	checkResult: null,
	checkError: null,
	checkPromise: null,
	downloadError: null,
	downloadProgress: [],
	downloadPromise: null,
	installError: null,
	currentVersion: '2026.800.0',
	calls: [],
	constructedWith: [],
};

class FakeUpdateManager {
	constructor(urlOrPath) {
		velopack.constructedWith.push(urlOrPath);
	}

	getCurrentVersion() {
		return velopack.currentVersion;
	}

	getUpdatePendingRestart() {
		velopack.calls.push('getUpdatePendingRestart');
		if (velopack.pendingRestartError != null) throw velopack.pendingRestartError;
		return velopack.pendingRestart;
	}

	async checkForUpdatesAsync() {
		velopack.calls.push('checkForUpdatesAsync');
		if (velopack.checkPromise != null) return await velopack.checkPromise;
		if (velopack.checkError != null) throw velopack.checkError;
		return velopack.checkResult;
	}

	async downloadUpdateAsync(_update, onProgress) {
		velopack.calls.push('downloadUpdateAsync');
		for (const percent of velopack.downloadProgress) {
			onProgress(percent);
		}
		if (velopack.downloadPromise != null) return await velopack.downloadPromise;
		if (velopack.downloadError != null) throw velopack.downloadError;
	}

	waitExitThenApplyUpdate(update, silent, restart) {
		velopack.calls.push(`waitExitThenApplyUpdate:${update.id}:${silent}:${restart}`);
		if (velopack.installError != null) throw velopack.installError;
	}
}

const requireFromHere = createRequire(import.meta.url);
const VELOPACK_PATH = requireFromHere.resolve('velopack');
requireFromHere.cache[VELOPACK_PATH] = {
	id: VELOPACK_PATH,
	filename: VELOPACK_PATH,
	loaded: true,
	exports: {UpdateManager: FakeUpdateManager},
};

class FakeAutoUpdater {
	constructor() {
		this.handlers = new Map();
		this.feedUrls = [];
		this.checks = 0;
		this.installs = 0;
		this.checkError = null;
		this.installError = null;
	}

	on(eventName, listener) {
		const existing = this.handlers.get(eventName) ?? [];
		existing.push(listener);
		this.handlers.set(eventName, existing);
		return this;
	}

	removeListener(eventName, listener) {
		const existing = this.handlers.get(eventName) ?? [];
		this.handlers.set(
			eventName,
			existing.filter((entry) => entry !== listener),
		);
		return this;
	}

	listenerCount(eventName) {
		return (this.handlers.get(eventName) ?? []).length;
	}

	totalListenerCount() {
		let total = 0;
		for (const listeners of this.handlers.values()) {
			total += listeners.length;
		}
		return total;
	}

	emit(eventName, ...args) {
		for (const listener of [...(this.handlers.get(eventName) ?? [])]) {
			listener(...args);
		}
	}

	setFeedURL(options) {
		this.feedUrls.push(options);
	}

	checkForUpdates() {
		this.checks += 1;
		if (this.checkError != null) throw this.checkError;
	}

	quitAndInstall() {
		this.installs += 1;
		if (this.installError != null) throw this.installError;
	}
}

const autoUpdater = new FakeAutoUpdater();
const quits = [];
const userDataPath = mkdtempSync(path.join(tmpdir(), 'fluxer-shell-self-update-'));
const applyStatePath = path.join(userDataPath, 'update-apply-state.json');
process.on('exit', () => {
	rmSync(userDataPath, {recursive: true, force: true});
});

installElectronStub({
	app: {
		getVersion: () => '2026.800.0',
		getPath: () => userDataPath,
		quit: () => {
			quits.push('quit');
		},
	},
	autoUpdater,
});

const {
	getElectronUpdateFeedUrl,
	resetElectronUpdateSessionForTests,
	runShellSelfUpdate,
	SHELL_SELF_UPDATE_CHECK_TIMEOUT_MS,
	SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS,
} = await import('@electron/main/ShellSelfUpdate');
const {getUpdateBaseUrl} = await import('@electron/main/ShellDownloadFormats');

function createHooks() {
	const calls = [];
	return {
		calls,
		hooks: {
			onDownloading: (progress) => {
				calls.push({hook: 'downloading', progress});
			},
			onRestarting: () => {
				calls.push({hook: 'restarting'});
			},
		},
	};
}

function settle() {
	return new Promise((resolve) => {
		setImmediate(resolve);
	});
}

function selfUpdateTest(name, body) {
	test(name, async () => {
		const timers = [];
		const originalSetTimeout = globalThis.setTimeout;
		const originalClearTimeout = globalThis.clearTimeout;
		globalThis.setTimeout = (callback, delayMs) => {
			const timer = {
				callback,
				delayMs,
				cleared: false,
				unref() {
					return this;
				},
			};
			timers.push(timer);
			return timer;
		};
		globalThis.clearTimeout = (timer) => {
			if (timer != null && typeof timer === 'object') {
				timer.cleared = true;
			}
		};
		try {
			await body({
				timers,
				start: (updater, hooks) => runShellSelfUpdate({capability: 'self-update', updater}, hooks),
				fireTimer: (delayMs) => {
					const timer = timers.find((entry) => entry.delayMs === delayMs);
					assert.ok(timer != null, `no timer armed at ${delayMs}ms`);
					assert.equal(timer.cleared, false, `the timer at ${delayMs}ms was already disarmed`);
					timer.callback();
				},
				timerCleared: (delayMs) => timers.find((entry) => entry.delayMs === delayMs)?.cleared ?? null,
				stillPending: async (promise) => {
					let resolved = false;
					void promise.then(() => {
						resolved = true;
					});
					for (let turn = 0; turn < 8; turn += 1) {
						await settle();
					}
					return !resolved;
				},
			});
		} finally {
			globalThis.setTimeout = originalSetTimeout;
			globalThis.clearTimeout = originalClearTimeout;
		}
	});
}

beforeEach(() => {
	velopack.pendingRestart = null;
	velopack.pendingRestartError = null;
	velopack.checkResult = null;
	velopack.checkError = null;
	velopack.checkPromise = null;
	velopack.downloadError = null;
	velopack.downloadProgress = [];
	velopack.downloadPromise = null;
	velopack.installError = null;
	velopack.currentVersion = '2026.800.0';
	velopack.calls.length = 0;
	rmSync(applyStatePath, {force: true});
	velopack.constructedWith.length = 0;
	autoUpdater.handlers.clear();
	resetElectronUpdateSessionForTests();
	autoUpdater.feedUrls.length = 0;
	autoUpdater.checks = 0;
	autoUpdater.installs = 0;
	autoUpdater.checkError = null;
	autoUpdater.installError = null;
	quits.length = 0;
});

describe('velopack self update', () => {
	selfUpdateTest('talks to the same feed the in-app updater uses', async ({start}) => {
		const {hooks} = createHooks();

		await start('velopack', hooks);

		assert.deepEqual(velopack.constructedWith, [getUpdateBaseUrl()]);
	});

	selfUpdateTest('a feed with nothing newer resolves without ever touching the splash', async ({start}) => {
		const {calls, hooks} = createHooks();

		assert.deepEqual(await start('velopack', hooks), {reason: 'no-update', detail: null});
		assert.deepEqual(calls, []);
		assert.deepEqual(velopack.calls, ['getUpdatePendingRestart', 'checkForUpdatesAsync']);
	});

	selfUpdateTest('a failed check resolves with the reason so the caller can fall back', async ({start}) => {
		velopack.checkError = new Error('the feed is a 404');
		const {hooks} = createHooks();

		assert.deepEqual(await start('velopack', hooks), {reason: 'check-failed', detail: 'the feed is a 404'});
	});

	selfUpdateTest('a failed staging probe never reaches the network', async ({start}) => {
		velopack.pendingRestartError = new Error('the packages directory is unreadable');
		const {hooks} = createHooks();

		assert.deepEqual(await start('velopack', hooks), {
			reason: 'check-failed',
			detail: 'the packages directory is unreadable',
		});
		assert.deepEqual(velopack.calls, ['getUpdatePendingRestart']);
	});

	selfUpdateTest('a lost update lock resolves as a download failure rather than retrying forever', async ({start}) => {
		velopack.checkResult = {id: 'v2'};
		velopack.downloadProgress = [0, 40];
		velopack.downloadError = new Error('another update operation is in progress');
		const {calls, hooks} = createHooks();

		assert.deepEqual(await start('velopack', hooks), {
			reason: 'download-failed',
			detail: 'another update operation is in progress',
		});
		assert.deepEqual(calls, [
			{hook: 'downloading', progress: 0},
			{hook: 'downloading', progress: 0},
			{hook: 'downloading', progress: 40},
		]);
	});

	selfUpdateTest(
		'a downloaded update restarts the shell explicitly and never resolves',
		async ({start, stillPending}) => {
			velopack.checkResult = {id: 'v2'};
			velopack.downloadProgress = [12, 100];
			const {calls, hooks} = createHooks();

			assert.equal(await stillPending(start('velopack', hooks)), true);
			assert.deepEqual(calls, [
				{hook: 'downloading', progress: 0},
				{hook: 'downloading', progress: 12},
				{hook: 'downloading', progress: 100},
				{hook: 'restarting'},
			]);
			assert.deepEqual(velopack.calls, [
				'getUpdatePendingRestart',
				'checkForUpdatesAsync',
				'downloadUpdateAsync',
				'waitExitThenApplyUpdate:v2:true:true',
			]);
			assert.deepEqual(
				quits,
				['quit'],
				'the restart goes through app.quit so the will-quit cleanup checkpoints the app store',
			);
		},
	);

	selfUpdateTest('an update already staged on disk skips straight to the restart', async ({start, stillPending}) => {
		velopack.pendingRestart = {id: 'staged'};
		const {calls, hooks} = createHooks();

		assert.equal(await stillPending(start('velopack', hooks)), true);
		assert.deepEqual(calls, [{hook: 'restarting'}]);
		assert.deepEqual(velopack.calls, ['getUpdatePendingRestart', 'waitExitThenApplyUpdate:staged:true:true']);
	});

	selfUpdateTest('an apply that never landed falls back instead of applying it again', async ({start}) => {
		writeFileSync(applyStatePath, JSON.stringify({version: '2026.900.0', attemptedAt: Date.now() - 60_000}));
		velopack.pendingRestart = {id: 'staged', Version: '2026.900.0'};
		const {calls, hooks} = createHooks();

		assert.deepEqual(await start('velopack', hooks), {
			reason: 'install-failed',
			detail: 'the last downloaded update was never installed',
		});
		assert.deepEqual(calls, []);
		assert.deepEqual(velopack.calls, []);
		assert.deepEqual(quits, []);
	});

	selfUpdateTest(
		'an old apply that never landed retries from the feed instead of blocking every update',
		async ({start, stillPending}) => {
			writeFileSync(applyStatePath, JSON.stringify({version: '2026.900.0', attemptedAt: Date.now() - 3_600_000}));
			velopack.pendingRestart = {id: 'staged', Version: '2026.900.0'};
			velopack.checkResult = {id: 'v3', TargetFullRelease: {Version: '2026.950.0'}};
			const {hooks} = createHooks();

			assert.equal(await stillPending(start('velopack', hooks)), true);
			assert.deepEqual(velopack.calls, [
				'checkForUpdatesAsync',
				'downloadUpdateAsync',
				'waitExitThenApplyUpdate:v3:true:true',
			]);
			assert.equal(JSON.parse(readFileSync(applyStatePath, 'utf8')).version, '2026.950.0');
		},
	);

	selfUpdateTest('an apply that landed is forgotten and the next one is recorded', async ({start, stillPending}) => {
		writeFileSync(applyStatePath, JSON.stringify({version: '2026.800.0', attemptedAt: 1}));
		velopack.pendingRestart = {id: 'staged', Version: '2026.900.0'};
		const {hooks} = createHooks();

		assert.equal(await stillPending(start('velopack', hooks)), true);
		assert.deepEqual(velopack.calls, ['getUpdatePendingRestart', 'waitExitThenApplyUpdate:staged:true:true']);
		assert.equal(JSON.parse(readFileSync(applyStatePath, 'utf8')).version, '2026.900.0');
	});

	selfUpdateTest(
		'an install that throws resolves instead of leaving the restart message up forever',
		async ({start}) => {
			velopack.pendingRestart = {id: 'staged'};
			velopack.installError = new Error('the updater binary is missing');
			const {hooks} = createHooks();

			assert.deepEqual(await start('velopack', hooks), {
				reason: 'install-failed',
				detail: 'the updater binary is missing',
			});
			assert.deepEqual(quits, []);
		},
	);

	selfUpdateTest('a check that never answers hits the check deadline', async ({start, fireTimer, timerCleared}) => {
		let releaseCheck = () => {};
		velopack.checkPromise = new Promise((resolve) => {
			releaseCheck = () => {
				resolve(null);
			};
		});
		const {calls, hooks} = createHooks();
		const promise = start('velopack', hooks);
		await settle();

		fireTimer(SHELL_SELF_UPDATE_CHECK_TIMEOUT_MS);

		assert.deepEqual(await promise, {reason: 'timed-out', detail: 'the update check produced no answer'});

		releaseCheck();
		await settle();

		assert.deepEqual(calls, []);
		assert.equal(timerCleared(SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS), true);
	});

	selfUpdateTest(
		'a download that never finishes hits the total deadline, and the check deadline is already gone',
		async ({start, fireTimer, timerCleared}) => {
			velopack.checkResult = {id: 'v2'};
			velopack.downloadPromise = new Promise(() => {});
			const {hooks} = createHooks();
			const promise = start('velopack', hooks);
			await settle();

			assert.equal(timerCleared(SHELL_SELF_UPDATE_CHECK_TIMEOUT_MS), true);

			fireTimer(SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS);

			assert.deepEqual(await promise, {reason: 'timed-out', detail: 'the update did not finish'});
		},
	);

	selfUpdateTest(
		'a late download failure after a deadline never re-settles the promise',
		async ({start, fireTimer}) => {
			velopack.checkResult = {id: 'v2'};
			let failDownload = () => {};
			velopack.downloadPromise = new Promise((_resolve, reject) => {
				failDownload = () => {
					reject(new Error('too late'));
				};
			});
			const {calls, hooks} = createHooks();
			const promise = start('velopack', hooks);
			await settle();
			fireTimer(SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS);
			const failure = await promise;

			failDownload();
			await settle();

			assert.deepEqual(failure, {reason: 'timed-out', detail: 'the update did not finish'});
			assert.deepEqual(await promise, failure);
			assert.deepEqual(calls, [{hook: 'downloading', progress: 0}]);
		},
	);
});

describe('velopack self update across repeated clicks', () => {
	selfUpdateTest(
		'a second run for the same version joins the download already in flight',
		async ({start, fireTimer}) => {
			velopack.checkResult = {id: 'v3', TargetFullRelease: {Version: '2026.900.0'}};
			let finishDownload = () => {};
			velopack.downloadPromise = new Promise((resolve) => {
				finishDownload = resolve;
			});
			const first = start('velopack', createHooks().hooks);
			await settle();
			fireTimer(SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS);
			assert.deepEqual(await first, {reason: 'timed-out', detail: 'the update did not finish'});

			const {calls, hooks} = createHooks();
			void start('velopack', hooks);
			await settle();
			finishDownload();
			await settle();
			await settle();

			assert.equal(velopack.calls.filter((call) => call === 'downloadUpdateAsync').length, 1);
			assert.deepEqual(calls, [{hook: 'downloading', progress: 0}, {hook: 'restarting'}]);
			assert.deepEqual(quits, ['quit']);
		},
	);
});

describe('electron self update', () => {
	selfUpdateTest('points squirrel at the json feed with an identifying user agent', async ({start}) => {
		const {hooks} = createHooks();
		const promise = start('electron', hooks);
		autoUpdater.emit('update-not-available');
		await promise;

		assert.equal(getElectronUpdateFeedUrl(), `${getUpdateBaseUrl()}/RELEASES.json`);
		assert.deepEqual(autoUpdater.feedUrls, [
			{
				url: getElectronUpdateFeedUrl(),
				serverType: 'json',
				headers: {'User-Agent': `Fluxer/2026.800.0 (${process.platform}: ${process.arch})`},
			},
		]);
		assert.equal(autoUpdater.checks, 1);
	});

	selfUpdateTest('nothing newer resolves and later runs reuse the same four listeners', async ({start}) => {
		const {calls, hooks} = createHooks();
		const promise = start('electron', hooks);

		assert.equal(autoUpdater.totalListenerCount(), 4);

		autoUpdater.emit('update-not-available');

		assert.deepEqual(await promise, {reason: 'no-update', detail: null});
		assert.deepEqual(calls, []);

		const again = start('electron', createHooks().hooks);
		autoUpdater.emit('update-not-available');
		await again;

		assert.equal(autoUpdater.totalListenerCount(), 4);
		assert.equal(autoUpdater.checks, 2);
	});

	selfUpdateTest(
		'a check that throws synchronously resolves rather than hanging on a feed that was never set',
		async ({start}) => {
			autoUpdater.checkError = new Error('Update URL is not set');
			const {hooks} = createHooks();

			assert.deepEqual(await start('electron', hooks), {reason: 'check-failed', detail: 'Update URL is not set'});

			autoUpdater.checkError = null;
			const retry = start('electron', createHooks().hooks);
			autoUpdater.emit('update-not-available');
			assert.deepEqual(await retry, {reason: 'no-update', detail: null});
			assert.equal(autoUpdater.checks, 2);
		},
	);

	selfUpdateTest('an error before the download is a check failure, after it a download failure', async ({start}) => {
		const first = start('electron', createHooks().hooks);
		autoUpdater.emit('error', new Error('dns'));
		assert.deepEqual(await first, {reason: 'check-failed', detail: 'dns'});

		const {calls, hooks} = createHooks();
		const second = start('electron', hooks);
		autoUpdater.emit('update-available');
		autoUpdater.emit('error', new Error('the bundle is not signed'));

		assert.deepEqual(await second, {reason: 'download-failed', detail: 'the bundle is not signed'});
		assert.deepEqual(calls, [{hook: 'downloading', progress: null}]);
	});

	selfUpdateTest(
		'squirrel gives no progress, so the splash is told to keep its placeholder',
		async ({start, stillPending, timerCleared}) => {
			const {calls, hooks} = createHooks();
			const promise = start('electron', hooks);

			autoUpdater.emit('update-available');

			assert.deepEqual(calls, [{hook: 'downloading', progress: null}]);
			assert.equal(timerCleared(SHELL_SELF_UPDATE_CHECK_TIMEOUT_MS), true);
			assert.equal(await stillPending(promise), true);
		},
	);

	selfUpdateTest('a downloaded update restarts the shell and never resolves', async ({start, stillPending}) => {
		const {calls, hooks} = createHooks();
		const promise = start('electron', hooks);

		autoUpdater.emit('update-available');
		autoUpdater.emit('update-downloaded');

		assert.deepEqual(calls, [{hook: 'downloading', progress: null}, {hook: 'restarting'}]);
		assert.equal(autoUpdater.installs, 1);
		assert.equal(await stillPending(promise), true);
	});

	selfUpdateTest(
		'an install that throws resolves instead of stranding the user on the restart message',
		async ({start}) => {
			autoUpdater.installError = new Error('the app is on a read only mount');
			const {hooks} = createHooks();
			const promise = start('electron', hooks);

			autoUpdater.emit('update-downloaded');

			assert.deepEqual(await promise, {reason: 'install-failed', detail: 'the app is on a read only mount'});
		},
	);

	selfUpdateTest(
		'a silent squirrel hits the check deadline and a late download installs on the next run',
		async ({start, fireTimer, stillPending}) => {
			const {hooks} = createHooks();
			const promise = start('electron', hooks);

			fireTimer(SHELL_SELF_UPDATE_CHECK_TIMEOUT_MS);

			assert.deepEqual(await promise, {reason: 'timed-out', detail: 'the update check produced no answer'});

			autoUpdater.emit('update-downloaded');

			assert.equal(autoUpdater.installs, 0);

			const {calls, hooks: nextHooks} = createHooks();
			const next = start('electron', nextHooks);

			assert.deepEqual(calls, [{hook: 'restarting'}]);
			assert.equal(autoUpdater.installs, 1);
			assert.equal(autoUpdater.checks, 1);
			assert.equal(await stillPending(next), true);
		},
	);

	selfUpdateTest(
		'a second run while squirrel is still downloading joins it instead of asking squirrel again',
		async ({start, fireTimer, stillPending}) => {
			const first = start('electron', createHooks().hooks);
			autoUpdater.emit('update-available');
			fireTimer(SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS);
			assert.deepEqual(await first, {reason: 'timed-out', detail: 'the update did not finish'});

			const {calls, hooks} = createHooks();
			const second = start('electron', hooks);

			assert.equal(autoUpdater.checks, 1);
			assert.deepEqual(calls, [{hook: 'downloading', progress: null}]);
			assert.equal(await stillPending(second), true);

			autoUpdater.emit('update-downloaded');

			assert.deepEqual(calls, [{hook: 'downloading', progress: null}, {hook: 'restarting'}]);
			assert.equal(autoUpdater.installs, 1);
		},
	);

	selfUpdateTest(
		'two runs in flight both settle on one squirrel answer and squirrel is asked once',
		async ({start}) => {
			const first = start('electron', createHooks().hooks);
			const second = start('electron', createHooks().hooks);

			autoUpdater.emit('error', new Error('dns'));

			assert.deepEqual(await first, {reason: 'check-failed', detail: 'dns'});
			assert.deepEqual(await second, {reason: 'check-failed', detail: 'dns'});
			assert.equal(autoUpdater.checks, 1);
		},
	);

	selfUpdateTest('a download that never finishes hits the total deadline', async ({start, fireTimer}) => {
		const {hooks} = createHooks();
		const promise = start('electron', hooks);

		autoUpdater.emit('update-available');
		fireTimer(SHELL_SELF_UPDATE_TOTAL_TIMEOUT_MS);

		assert.deepEqual(await promise, {reason: 'timed-out', detail: 'the update did not finish'});
	});
});
