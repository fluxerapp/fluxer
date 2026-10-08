// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {mkdirSync, mkdtempSync, readdirSync, readFileSync, writeFileSync} from 'node:fs';
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
const shellDownloadFormatsSource = transform('ShellDownloadFormats.ts');
const shellUpdateCapabilitySource = transform('ShellUpdateCapability.ts');
const updaterDownloadsSource = transform('UpdaterDownloads.ts');
const updaterSource = transform('Updater.ts');

const INSTALLED_NAME = 'My Fluxer.AppImage';
const CURRENT_VERSION = '2026.903.231208';
const PUBLISHED_VERSION = '2026.904.135113';
const OLD_BYTES = Buffer.from('installed-appimage');
const NEW_BYTES = Buffer.alloc(300_000, 3);
const NEW_SHA256 = createHash('sha256').update(NEW_BYTES).digest('hex');

let server;
let baseUrl;
let appImageRequests = 0;

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
			response.writeHead(200, {'content-type': 'application/octet-stream', 'content-length': NEW_BYTES.length});
			response.end(NEW_BYTES);
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

function loadUpdater({
	appImagePath,
	version = CURRENT_VERSION,
	appDir,
	platform = 'linux',
	arch = 'arm64',
	packaged = true,
	flatpak = false,
	gateCheck = async () => ({available: true}),
}) {
	const events = [];
	const handlers = new Map();
	const gate = {checks: 0};
	const module = {exports: {}};
	const stubs = {
		'@electron/common/BuildChannel': {BUILD_CHANNEL: 'canary'},
		'@electron/common/Constants': {DOWNLOAD_PAGE_URLS: {canary: 'https://canary.fluxer.app/download'}},
		'@electron/common/DesktopIdentity': {DESKTOP_ARTIFACT_PRODUCT_NAME: 'Fluxer-Canary'},
		'@electron/common/UserDataPath': {isPortableMode: () => false},
		'@electron/main/DesktopUpdateGate': {
			checkDesktopUpdateNow: async () => {
				gate.checks += 1;
				return await gateCheck();
			},
		},
		'@electron/main/LinuxSandbox': {isFlatpakRuntime: () => flatpak},
		'electron-log': {info() {}, warn() {}, error() {}, debug() {}},
		electron: {
			app: {
				isPackaged: packaged,
				getVersion: () => version,
			},
			net: {fetch: (input, init) => sandbox.fetch(input, init)},
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
		URL,
		process: {
			...process,
			platform,
			arch,
			execPath: appDir ? join(appDir, 'fluxer-canary') : process.execPath,
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
		require: (specifier) => stubs[specifier] ?? require(specifier),
	};
	const context = vm.createContext(sandbox);
	const load = (source, specifier) => {
		const loaded = {exports: {}};
		sandbox.module = loaded;
		sandbox.exports = loaded.exports;
		sandbox.__filename = source.path;
		vm.runInContext(source.code, context, {filename: source.path});
		stubs[specifier] = loaded.exports;
	};
	load(appImageUpdateSource, '@electron/main/AppImageUpdate');
	load(shellUpdateCapabilitySource, '@electron/main/ShellUpdateCapability');
	load(shellDownloadFormatsSource, '@electron/main/ShellDownloadFormats');
	load(updaterDownloadsSource, '@electron/main/UpdaterDownloads');
	sandbox.module = module;
	sandbox.exports = module.exports;
	sandbox.__filename = updaterSource.path;
	vm.runInContext(updaterSource.code, context, {filename: updaterSource.path});

	module.exports.registerUpdater(() => ({webContents: {send: (_channel, event) => events.push(event)}}));
	return {
		events,
		gate,
		channels: () => [...handlers.keys()],
		check: (context = 'user') => handlers.get('updater-check')({}, context),
	};
}

function types(events) {
	return events.map((event) => event.type);
}

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

describe('Updater on a shell that updates itself', () => {
	test('registers only the check, never a download or an install channel', () => {
		const install = createInstall();
		const updater = loadUpdater({appImagePath: install.installedPath, appDir: install.mount});

		assert.deepEqual(updater.channels(), ['updater-check']);
	});

	test('a background check downloads nothing and leaves the poll to the bootstrap', async () => {
		const install = createInstall();
		const updater = loadUpdater({appImagePath: install.installedPath, appDir: install.mount});
		appImageRequests = 0;

		await updater.check('background');
		await updater.check('focus');

		assert.equal(updater.gate.checks, 0);
		assert.deepEqual(updater.events, []);
		assert.equal(appImageRequests, 0);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
		assert.equal(readFileSync(install.installedPath).equals(OLD_BYTES), true);
	});

	test('a user check asks the manifest and leaves the update itself to the click', async () => {
		const install = createInstall();
		const updater = loadUpdater({appImagePath: install.installedPath, appDir: install.mount});
		appImageRequests = 0;

		await updater.check('user');

		assert.equal(updater.gate.checks, 1);
		assert.deepEqual(types(updater.events), ['not-available']);
		assert.equal(appImageRequests, 0);
		assert.deepEqual(readdirSync(install.applications), [INSTALLED_NAME]);
	});

	test('a user check that cannot reach the manifest says so', async () => {
		const install = createInstall();
		const updater = loadUpdater({
			appImagePath: install.installedPath,
			appDir: install.mount,
			gateCheck: async () => {
				throw new Error('manifest unreachable');
			},
		});

		await updater.check('user');

		assert.deepEqual(types(updater.events), ['error']);
		assert.match(updater.events[0].message, /manifest unreachable/);
	});

	test('macOS and Windows go through the same check', async () => {
		for (const platform of ['darwin', 'win32']) {
			const updater = loadUpdater({platform, arch: 'x64'});

			await updater.check('user');

			assert.equal(updater.gate.checks, 1, platform);
			assert.deepEqual(updater.channels(), ['updater-check'], platform);
		}
	});
});

describe('Updater on a shell that cannot update itself', () => {
	test('a Linux package install offers the newer package for manual download', async () => {
		const updater = loadUpdater({appImagePath: null});

		await updater.check('user');

		assert.equal(updater.gate.checks, 0);
		assert.deepEqual(types(updater.events), ['checking', 'available']);
		const available = updater.events.at(-1);
		assert.equal(available.version, PUBLISHED_VERSION);
		assert.equal(Object.hasOwn(available, 'downloadStarted'), false);
		assert.ok(available.downloadOptions.some((option) => option.format === 'deb'));
	});

	test('a Linux package install that is current reports no update', async () => {
		const updater = loadUpdater({appImagePath: null, version: PUBLISHED_VERSION});

		await updater.check('user');

		assert.deepEqual(types(updater.events), ['checking', 'not-available']);
	});

	test('Flatpak and development builds point at their own update path', async () => {
		const flatpak = loadUpdater({appImagePath: null, flatpak: true});
		await flatpak.check('user');
		assert.deepEqual(JSON.parse(JSON.stringify(flatpak.events)), [
			{type: 'unsupported', context: 'user', reason: 'managed-package'},
		]);

		const unpackaged = loadUpdater({appImagePath: null, packaged: false});
		await unpackaged.check('user');
		assert.equal(unpackaged.events[0].reason, 'unpackaged');
	});
});
