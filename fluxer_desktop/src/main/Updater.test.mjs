// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {chmodSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, writeFileSync} from 'node:fs';
import {createServer} from 'node:http';
import {createRequire} from 'node:module';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {after, before, describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

function transform(name) {
	const url = new URL(`./${name}`, import.meta.url);
	const path = fileURLToPath(url);
	return {
		path,
		code: esbuild.transformSync(readFileSync(path, 'utf8'), {
			loader: 'ts',
			format: 'cjs',
			platform: 'node',
			target: 'node20',
			define: {'import.meta.url': JSON.stringify(url.href)},
		}).code,
	};
}

const appImageUpdateSource = transform('AppImageUpdate.ts');
const updaterDownloadsSource = transform('UpdaterDownloads.ts');
const updaterSource = transform('Updater.ts');
const updaterPlatformUtilsSource = transform('../../../fluxer_app/src/features/app/utils/UpdaterPlatformUtils.ts');

function loadUpdaterPlatformUtils() {
	const module = {exports: {}};
	const context = vm.createContext({module, exports: module.exports, require});
	vm.runInContext(updaterPlatformUtilsSource.code, context, {filename: updaterPlatformUtilsSource.path});
	return module.exports;
}

const INSTALLED_NAME = 'My Fluxer.AppImage';
const CURRENT_VERSION = '2026.903.231208';
const PUBLISHED_VERSION = '2026.904.135113';
const OLD_BYTES = Buffer.from('installed-appimage');
const NEW_BYTES = Buffer.alloc(300_000, 3);
const NEW_SHA256 = createHash('sha256').update(NEW_BYTES).digest('hex');

let server;
let baseUrl;
let serveCorruptBytes = false;
let appImageRequests = 0;
let failAppImageRequests = 0;

before(async () => {
	server = createServer((request, response) => {
		if (request.url === '/latest') {
			response.writeHead(200, {'content-type': 'application/json'});
			response.end(
				JSON.stringify({
					version: PUBLISHED_VERSION,
					pub_date: '2026-09-04T13:51:13Z',
					files: {
						appimage: {url: `${baseUrl}/appimage`, sha256: NEW_SHA256},
						deb: {url: `${baseUrl}/deb`, sha256: 'deadbeef'},
						setup: {url: `${baseUrl}/setup`, sha256: 'cafebabe'},
					},
				}),
			);
			return;
		}
		if (request.url === `/${PUBLISHED_VERSION}/appimage`) {
			appImageRequests += 1;
			if (failAppImageRequests > 0) {
				failAppImageRequests -= 1;
				response.writeHead(503);
				response.end('try again');
				return;
			}
			const body = serveCorruptBytes ? Buffer.concat([Buffer.from([0]), NEW_BYTES.subarray(1)]) : NEW_BYTES;
			response.writeHead(200, {'content-type': 'application/octet-stream', 'content-length': body.length});
			response.end(body);
			return;
		}
		response.writeHead(404);
		response.end('missing');
	});
	await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
	baseUrl = `http://127.0.0.1:${server.address().port}`;
});

after(async () => {
	await new Promise((resolve) => server.close(resolve));
});

function createInstall() {
	const root = mkdtempSync(join(tmpdir(), 'fluxer-updater-test-'));
	const applications = join(root, 'Applications');
	mkdirSync(applications);
	const mount = join(root, '.mount_Fluxer');
	mkdirSync(mount);
	writeFileSync(join(mount, 'AppRun'), '#!/usr/bin/env bash\n', {mode: 0o755});
	const installedPath = join(applications, INSTALLED_NAME);
	writeFileSync(installedPath, OLD_BYTES, {mode: 0o755});
	return {applications, installedPath, mount};
}

function loadUpdater({
	appImagePath,
	version = CURRENT_VERSION,
	appDir,
	execPath,
	stagingErrno,
	platform = 'linux',
	arch = 'arm64',
	velopack,
	applyAttempt = null,
}) {
	const events = [];
	const handlers = new Map();
	const appEvents = new Map();
	const state = {relaunched: false, electronRelaunched: false};
	const applyState = {attempt: applyAttempt, recorded: [], cleared: 0};
	const module = {exports: {}};
	const stubs = {
		'@electron/main/UpdaterApplyState': {
			readVelopackApplyAttempt: () => applyState.attempt,
			recordVelopackApplyAttempt: (recordedVersion) => {
				applyState.recorded.push(recordedVersion);
				applyState.attempt = {version: recordedVersion, attemptedAt: 0};
			},
			clearVelopackApplyAttempt: () => {
				applyState.cleared += 1;
				applyState.attempt = null;
			},
		},
		'@electron/common/BuildChannel': {BUILD_CHANNEL: 'canary'},
		'@electron/common/UserDataPath': {isPortableMode: () => false},
		'@electron/main/DesktopTray': {destroyDesktopTray() {}},
		'@electron/main/LinuxSandbox': {isFlatpakRuntime: () => false},
		'@electron/main/Troubleshooting': {
			relaunchAndExit() {
				state.relaunched = true;
			},
		},
		'@electron/main/Window': {setQuitting() {}},
		'electron-log': {info() {}, warn() {}, error() {}, debug() {}},
		electron: {
			app: {
				isPackaged: true,
				getVersion: () => version,
				on(event, handler) {
					appEvents.set(event, handler);
				},
				relaunch() {
					state.electronRelaunched = true;
				},
				exit() {},
			},
			autoUpdater: {on() {}},
			ipcMain: {
				handle(channel, handler) {
					handlers.set(channel, handler);
				},
			},
		},
	};
	const sandbox = {
		console,
		Buffer,
		process: {
			...process,
			platform,
			arch,
			execPath: execPath ?? (appDir ? join(appDir, 'fluxer-canary') : process.execPath),
			env: {
				...(appImagePath ? {APPIMAGE: appImagePath} : {}),
				...(appDir ? {APPDIR: appDir} : {}),
			},
		},
		setTimeout,
		clearTimeout,
		setImmediate,
		fetch: (input, init) => {
			const url = String(input).replace(/https:\/\/pkgs\.fluxer\.com\/desktop\/canary\/[^/]+\/[^/]+/, baseUrl);
			return fetch(url, init);
		},
		require: (specifier) => {
			if (specifier === 'node:module' && velopack) {
				return {
					...require('node:module'),
					createRequire: () => (name) => (name === 'velopack' ? velopack : require(name)),
				};
			}
			if (specifier === 'node:fs/promises' && stagingErrno) {
				return {
					...require(specifier),
					open: async () => {
						const error = new Error(`${stagingErrno}: staging refused`);
						error.code = stagingErrno;
						throw error;
					},
				};
			}
			return stubs[specifier] ?? require(specifier);
		},
	};
	const context = vm.createContext(sandbox);

	const appImageModule = {exports: {}};
	sandbox.module = appImageModule;
	sandbox.exports = appImageModule.exports;
	sandbox.__filename = appImageUpdateSource.path;
	vm.runInContext(appImageUpdateSource.code, context, {filename: appImageUpdateSource.path});
	stubs['@electron/main/AppImageUpdate'] = appImageModule.exports;

	const updaterDownloadsModule = {exports: {}};
	sandbox.module = updaterDownloadsModule;
	sandbox.exports = updaterDownloadsModule.exports;
	sandbox.__filename = updaterDownloadsSource.path;
	vm.runInContext(updaterDownloadsSource.code, context, {filename: updaterDownloadsSource.path});
	stubs['@electron/main/UpdaterDownloads'] = updaterDownloadsModule.exports;

	sandbox.module = module;
	sandbox.exports = module.exports;
	sandbox.__filename = updaterSource.path;
	vm.runInContext(updaterSource.code, context, {filename: updaterSource.path});

	module.exports.registerUpdater(() => ({webContents: {send: (_channel, event) => events.push(event)}}));
	return {
		applyState,
		events,
		state,
		check: () => handlers.get('updater-check')({}, 'user'),
		install: () => handlers.get('updater-install')({}),
		quit: () => appEvents.get('will-quit')?.(),
	};
}

function types(events) {
	return events.map((event) => event.type);
}

function stagingLeftovers(directory) {
	return readdirSync(directory).filter((entry) => entry.startsWith('.fluxer-update-'));
}

describe('Updater AppImage lifecycle', () => {
	test('downloads on check, reports the macOS lifecycle, and swaps in place', async () => {
		const install = createInstall();
		const updater = loadUpdater({appImagePath: install.installedPath, appDir: install.mount});

		await updater.check();

		const available = updater.events.find((event) => event.type === 'available');
		assert.equal(available.downloadStarted, true);
		assert.equal(available.version, PUBLISHED_VERSION);
		assert.equal(available.downloadUrl, undefined);
		assert.equal(available.downloadOptions, undefined);
		const progress = updater.events.filter((event) => event.type === 'progress');
		assert.ok(progress.length > 0);
		assert.equal(progress.at(-1).total, NEW_BYTES.length);
		assert.equal(progress.at(-1).transferred, NEW_BYTES.length);
		assert.equal(progress.at(-1).percent, 100);
		const downloaded = updater.events.find((event) => event.type === 'downloaded');
		assert.equal(downloaded.version, PUBLISHED_VERSION);
		assert.deepEqual(
			types(updater.events).filter((type) => type !== 'progress'),
			['checking', 'available', 'downloaded'],
		);
		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);

		await updater.install();

		assert.equal(updater.state.relaunched, true);
		assert.equal(
			updater.state.electronRelaunched,
			false,
			"app.relaunch cannot start an AppImage: Electron's relauncher runs under PR_SET_NO_NEW_PRIVS and the setuid fusermount the AppImage runtime needs fails with EPERM",
		);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
		assert.equal(readFileSync(install.installedPath).equals(NEW_BYTES), true);
	});

	test('refuses to swap when the download does not match the published sha256', async () => {
		const install = createInstall();
		const updater = loadUpdater({appImagePath: install.installedPath, appDir: install.mount});
		serveCorruptBytes = true;
		try {
			await updater.check();
		} finally {
			serveCorruptBytes = false;
		}

		assert.deepEqual(
			types(updater.events).filter((type) => type !== 'progress'),
			['checking', 'available', 'error', 'available'],
		);
		const failure = updater.events.find((event) => event.type === 'error');
		assert.equal(failure.phase, 'download');
		assert.match(failure.message, /checksum mismatch/);
		const fallback = updater.events.at(-1);
		assert.equal(fallback.downloadStarted, false);
		assert.ok(fallback.downloadOptions.length > 0);
		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
		assert.deepEqual(stagingLeftovers(install.applications), []);
		await assert.rejects(updater.install(), /No AppImage update is ready to install/);
	});

	test('offers the manual download when the install directory is not writable', {
		skip: process.getuid?.() === 0,
	}, async () => {
		const install = createInstall();
		chmodSync(install.applications, 0o500);
		try {
			const updater = loadUpdater({appImagePath: install.installedPath, appDir: install.mount});

			await updater.check();

			assert.deepEqual(types(updater.events), ['checking', 'available']);
			const available = updater.events.at(-1);
			assert.equal(available.downloadStarted, false);
			assert.equal(available.version, PUBLISHED_VERSION);
			assert.ok(available.downloadOptions.length > 0);
			assert.equal(available.downloadOptions[0].format, 'appimage');
			assert.equal(
				available.downloadOptions[0].suggestedName,
				`Fluxer-Canary-${PUBLISHED_VERSION}-linux-arm64.AppImage`,
			);
		} finally {
			chmodSync(install.applications, 0o755);
		}
		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('falls back to the manual updater when APPIMAGE is unset', async () => {
		const updater = loadUpdater({appImagePath: null});

		await updater.check();

		assert.deepEqual(types(updater.events), ['checking', 'available']);
		assert.equal(updater.events.at(-1).downloadStarted, false);
		await assert.rejects(updater.install(), /In-app updates are not supported on this platform/);
	});

	test('never rewrites an inherited APPIMAGE that this process did not come from', async () => {
		const install = createInstall();
		const victim = join(install.applications, 'Kdenlive-24.12.AppImage');
		writeFileSync(victim, OLD_BYTES, {mode: 0o755});
		const optDir = mkdtempSync(join(tmpdir(), 'fluxer-opt-test-'));
		const updater = loadUpdater({appImagePath: victim, execPath: join(optDir, 'fluxer')});

		await updater.check();

		assert.deepEqual(types(updater.events), ['checking', 'available']);
		assert.equal(updater.events.at(-1).downloadStarted, false);
		assert.equal(readFileSync(victim).equals(OLD_BYTES), true);
		assert.deepEqual(stagingLeftovers(install.applications), []);
		await assert.rejects(updater.install(), /In-app updates are not supported on this platform/);
	});

	test('never rewrites the AppImage that only the shell around this process came from', async () => {
		const install = createInstall();
		const victim = join(install.applications, 'Kdenlive-24.12.AppImage');
		writeFileSync(victim, OLD_BYTES, {mode: 0o755});
		const extracted = join(mkdtempSync(join(tmpdir(), 'fluxer-extracted-test-')), 'squashfs-root');
		mkdirSync(extracted);
		writeFileSync(join(extracted, 'AppRun'), '#!/usr/bin/env bash\n', {mode: 0o755});
		const updater = loadUpdater({
			appImagePath: victim,
			appDir: install.mount,
			execPath: join(extracted, 'fluxer-canary'),
		});

		await updater.check();

		assert.deepEqual(types(updater.events), ['checking', 'available']);
		assert.equal(updater.events.at(-1).downloadStarted, false);
		assert.equal(readFileSync(victim).equals(OLD_BYTES), true);
		assert.deepEqual(stagingLeftovers(install.applications), []);
		await assert.rejects(updater.install(), /In-app updates are not supported on this platform/);
	});

	test('gives up on a local write failure instead of downloading the AppImage again', async () => {
		const install = createInstall();
		const updater = loadUpdater({
			appImagePath: install.installedPath,
			appDir: install.mount,
			stagingErrno: 'ENOSPC',
		});
		const before = appImageRequests;

		await updater.check();

		assert.equal(appImageRequests - before, 1, 'a full disk must not pull the whole AppImage down five more times');
		const failure = updater.events.find((event) => event.type === 'error');
		assert.equal(failure.phase, 'download');
		assert.match(failure.message, /ENOSPC/);
		assert.equal(updater.events.at(-1).type, 'available');
		assert.equal(updater.events.at(-1).downloadStarted, false);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('downloads the AppImage again after a transient server failure', async () => {
		const install = createInstall();
		const updater = loadUpdater({appImagePath: install.installedPath, appDir: install.mount});
		const before = appImageRequests;
		failAppImageRequests = 1;

		try {
			await updater.check();
		} finally {
			failAppImageRequests = 0;
		}

		assert.equal(appImageRequests - before, 2);
		assert.equal(updater.events.at(-1).type, 'downloaded');

		await updater.install();

		assert.equal(readFileSync(install.installedPath).equals(NEW_BYTES), true);
	});

	test('reports no update when the published version is not newer', async () => {
		const install = createInstall();
		const updater = loadUpdater({
			appImagePath: install.installedPath,
			appDir: install.mount,
			version: PUBLISHED_VERSION,
		});

		await updater.check();

		assert.deepEqual(types(updater.events), ['checking', 'not-available']);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('reclaims a staged update abandoned by a process that never reached will-quit', async () => {
		const install = createInstall();
		const abandoned = loadUpdater({appImagePath: install.installedPath, appDir: install.mount});

		await abandoned.check();
		assert.equal(stagingLeftovers(install.applications).length, 1);

		loadUpdater({appImagePath: install.installedPath, appDir: install.mount});

		assert.deepEqual(stagingLeftovers(install.applications), []);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
	});

	test('shows the renderer the download progress the main process reports', async () => {
		const install = createInstall();
		const updater = loadUpdater({appImagePath: install.installedPath, appDir: install.mount});
		const {shouldShowNativeDesktopUpdateDownloadProgress} = loadUpdaterPlatformUtils();

		await updater.check();

		const reportsProgress = updater.events.some((event) => event.type === 'progress');
		assert.equal(reportsProgress, true);
		assert.equal(shouldShowNativeDesktopUpdateDownloadProgress('linux'), reportsProgress);
	});

	test('discards a staged update that the user never installed', async () => {
		const install = createInstall();
		const updater = loadUpdater({appImagePath: install.installedPath, appDir: install.mount});

		await updater.check();
		assert.equal(stagingLeftovers(install.applications).length, 1);

		updater.quit();

		assert.deepEqual(stagingLeftovers(install.applications), []);
		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
	});
});

function createVelopackStub({installedVersion, pendingRestart = null, remoteUpdate = null}) {
	const applied = [];
	class UpdateManager {
		getCurrentVersion() {
			return installedVersion;
		}
		getUpdatePendingRestart() {
			return pendingRestart;
		}
		checkForUpdatesAsync() {
			return Promise.resolve(remoteUpdate);
		}
		downloadUpdateAsync() {
			return Promise.resolve();
		}
		waitExitThenApplyUpdate(update) {
			applied.push(update);
		}
	}
	return {applied, module: {UpdateManager}};
}

function loadWindowsUpdater({installedVersion, pendingRestart, remoteUpdate, applyAttempt = null}) {
	const velopack = createVelopackStub({installedVersion, pendingRestart, remoteUpdate});
	const updater = loadUpdater({
		platform: 'win32',
		arch: 'x64',
		velopack: velopack.module,
		applyAttempt,
	});
	return {...updater, applied: velopack.applied};
}

describe('Updater Windows apply failures', () => {
	test('offers the installer when a downloaded update never applied', async () => {
		const updater = loadWindowsUpdater({
			installedVersion: CURRENT_VERSION,
			pendingRestart: {Version: PUBLISHED_VERSION, Size: 100},
			applyAttempt: {version: PUBLISHED_VERSION, attemptedAt: 0},
		});

		await updater.check();

		assert.deepEqual(types(updater.events), ['checking', 'error', 'available']);
		assert.equal(updater.events[1].phase, 'install');
		assert.ok(updater.events[1].message.includes(PUBLISHED_VERSION));
		assert.equal(updater.events[2].downloadStarted, false);
		assert.equal(updater.events[2].downloadUrl, `${baseUrl}/setup`);
		assert.equal(updater.applyState.attempt.version, PUBLISHED_VERSION);
	});

	test('resumes normal updates once the installed version catches up', async () => {
		const updater = loadWindowsUpdater({
			installedVersion: PUBLISHED_VERSION,
			applyAttempt: {version: PUBLISHED_VERSION, attemptedAt: 0},
		});

		await updater.check();

		assert.equal(updater.applyState.cleared, 1);
		assert.equal(updater.applyState.attempt, null);
		assert.deepEqual(types(updater.events), ['checking', 'not-available']);
	});

	test('records the version it hands to the updater before quitting', async () => {
		const updater = loadWindowsUpdater({
			installedVersion: CURRENT_VERSION,
			pendingRestart: {Version: PUBLISHED_VERSION, Size: 100},
		});

		await updater.install();

		assert.deepEqual(updater.applyState.recorded, [PUBLISHED_VERSION]);
		assert.deepEqual(updater.applied, [{Version: PUBLISHED_VERSION, Size: 100}]);
	});

	test('refuses to re-apply a version that already failed to install', async () => {
		const updater = loadWindowsUpdater({
			installedVersion: CURRENT_VERSION,
			pendingRestart: {Version: PUBLISHED_VERSION, Size: 100},
			applyAttempt: {version: PUBLISHED_VERSION, attemptedAt: 0},
		});

		await assert.rejects(() => updater.install(), /Download the installer/);

		assert.deepEqual(updater.applied, []);
		assert.deepEqual(updater.applyState.recorded, []);
	});
});
