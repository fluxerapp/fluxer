// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

const events = [];
const ipcRegistrations = [];
let openExternalFailure = null;

class FakeWebContents {
	constructor() {
		this.sent = [];
		this.destroyed = false;
	}

	on() {}

	setWindowOpenHandler() {}

	isDestroyed() {
		return this.destroyed;
	}

	send(channel, payload) {
		this.sent.push({channel, payload});
	}
}

class FakeBrowserWindow {
	constructor() {
		this.webContents = new FakeWebContents();
		this.destroyed = false;
		this.visible = false;
	}

	on() {}

	once() {}

	isDestroyed() {
		return this.destroyed;
	}

	isVisible() {
		return this.visible;
	}

	show() {
		this.visible = true;
	}

	showInactive() {
		this.visible = true;
	}

	hide() {
		this.visible = false;
	}

	setSkipTaskbar() {}

	setAlwaysOnTop() {}

	close() {
		this.destroyed = true;
	}

	destroy() {
		this.destroyed = true;
	}

	loadURL() {
		return Promise.resolve();
	}
}

installElectronStub({
	app: {
		isPackaged: true,
		isReady: () => true,
		getAppPath: () => process.cwd(),
		getPath: () => process.cwd(),
		quit: () => {
			events.push('quit');
		},
	},
	BrowserWindow: FakeBrowserWindow,
	ipcMain: {
		on: (channel, handler) => {
			ipcRegistrations.push({channel, handler});
		},
	},
	shell: {
		openExternal: async (url) => {
			if (openExternalFailure != null) {
				const failure = openExternalFailure;
				openExternalFailure = null;
				throw failure;
			}
			events.push(`open:${url}`);
		},
	},
});

const {openExternalDeduped, shouldOpenExternalUrl} = await import('@electron/main/OpenExternal');
const {openSplashWindow} = await import('@electron/main/SplashWindow');
const {getDesktopDownloadArch} = await import('@electron/main/ShellDownloadFormats');
const {BUILD_CHANNEL} = await import('@electron/common/BuildChannel');
const {decideShellUpdatePlan, resolveShellUpdatePlan} = await import('@electron/main/ShellUpdateCapability');
const {DOWNLOAD_PAGE_URL} = await import('@electron/main/UpdaterDownloads');
const {armBlockedShellUpdate} = await import('@electron/main/ShellUpdateSplash');

const APPIMAGE_TARGET = {
	installedPath: '/home/user/Applications/Fluxer.AppImage',
	directory: '/home/user/Applications',
};

function emitIpc(channel, event, payload) {
	for (const registration of ipcRegistrations) {
		if (registration.channel === channel) {
			registration.handler(event, payload);
		}
	}
}

function settle() {
	return new Promise((resolve) => {
		setImmediate(resolve);
	});
}

function planFor(runtime) {
	return decideShellUpdatePlan({
		channel: 'canary',
		packaged: true,
		portable: false,
		flatpak: false,
		appImage: null,
		platform: 'linux',
		...runtime,
	});
}

describe('shell update capability', () => {
	test('every runtime signal picks one updater, first match wins', () => {
		assert.deepEqual(planFor({packaged: false, platform: 'win32'}), {
			capability: 'manual-download',
			reason: 'unpackaged',
		});
		assert.deepEqual(planFor({portable: true, platform: 'win32'}), {capability: 'manual-download', reason: 'platform'});
		assert.deepEqual(planFor({flatpak: true, platform: 'linux'}), {capability: 'managed-package'});
		assert.deepEqual(planFor({platform: 'win32'}), {capability: 'self-update', updater: 'velopack'});
		assert.deepEqual(planFor({platform: 'darwin'}), {capability: 'self-update', updater: 'electron'});
		assert.deepEqual(planFor({platform: 'linux', appImage: APPIMAGE_TARGET}), {
			capability: 'self-update',
			updater: 'appimage',
			target: APPIMAGE_TARGET,
		});
		assert.deepEqual(planFor({platform: 'linux'}), {capability: 'managed-package'});
		assert.deepEqual(planFor({platform: 'freebsd'}), {capability: 'manual-download', reason: 'platform'});
	});

	test('only a writable AppImage updates itself on Linux, every other install belongs to its package manager', () => {
		assert.deepEqual(planFor({platform: 'linux', appImage: APPIMAGE_TARGET}).capability, 'self-update');
		for (const runtime of [
			{platform: 'linux'},
			{platform: 'linux', portable: true},
			{platform: 'linux', portable: true, appImage: APPIMAGE_TARGET},
			{platform: 'linux', appImage: null},
			{platform: 'linux', flatpak: true},
		]) {
			assert.deepEqual(planFor(runtime), {capability: 'managed-package'}, JSON.stringify(runtime));
		}
		for (const platform of ['win32', 'darwin']) {
			assert.deepEqual(planFor({platform, portable: true}), {capability: 'manual-download', reason: 'platform'});
		}
	});

	test('a development shell never self-updates, because no feed publishes development builds', () => {
		for (const platform of ['darwin', 'win32', 'linux']) {
			assert.deepEqual(planFor({channel: 'development', platform, appImage: APPIMAGE_TARGET}), {
				capability: 'manual-download',
				reason: 'unpackaged',
			});
		}
		assert.deepEqual(planFor({channel: 'stable', platform: 'darwin'}), {
			capability: 'self-update',
			updater: 'electron',
		});
	});

	test('a store runtime outranks a portable marker, so a sandboxed install is never offered a deb or rpm it cannot apply', () => {
		assert.deepEqual(planFor({portable: true, flatpak: true, platform: 'linux'}), {capability: 'managed-package'});
		assert.deepEqual(planFor({portable: true, appImage: APPIMAGE_TARGET, platform: 'linux'}), {
			capability: 'managed-package',
		});
		assert.deepEqual(planFor({packaged: false, flatpak: true, platform: 'linux'}), {
			capability: 'manual-download',
			reason: 'unpackaged',
		});
	});

	test('resolves the live runtime through the same decision', () => {
		assert.deepEqual(
			resolveShellUpdatePlan(),
			decideShellUpdatePlan({
				channel: BUILD_CHANNEL,
				packaged: true,
				portable: false,
				flatpak: false,
				appImage: null,
				platform: process.platform,
			}),
		);
	});
});

describe('blocked shell update states', () => {
	test('the download page is a channel aware page, never an artifact', () => {
		assert.equal(shouldOpenExternalUrl(DOWNLOAD_PAGE_URL), true);
		assert.equal(new URL(DOWNLOAD_PAGE_URL).pathname, '/download');
	});

	test('the download arch collapses to the two we publish', () => {
		assert.equal(getDesktopDownloadArch('arm64'), 'arm64');
		assert.equal(getDesktopDownloadArch('x64'), 'x64');
		assert.equal(getDesktopDownloadArch('ia32'), 'x64');
		assert.equal(getDesktopDownloadArch('ppc64'), 'x64');
	});
});

describe('arming the blocked shell update', () => {
	test('the download action opens the allowlisted page and then quits', async () => {
		const window = openSplashWindow();
		events.length = 0;

		armBlockedShellUpdate();

		assert.deepEqual(window.webContents.sent.at(-1), {
			channel: 'desktop-splash:state',
			payload: {
				status: 'blocked-shell-update',
				current: null,
				total: null,
				progress: null,
				seconds: null,
				receivedBytes: null,
				totalBytes: null,
				bytesPerSecond: null,
				action: {kind: 'download', label: 'Download update'},
				message: null,
			},
		});

		emitIpc('desktop-splash:open-download', {sender: window.webContents});
		await settle();

		assert.deepEqual(events, [`open:${DOWNLOAD_PAGE_URL}`, 'quit']);
	});

	test('a download that cannot be opened still resolves the screen instead of stranding the user', async () => {
		const window = openSplashWindow();
		armBlockedShellUpdate();
		events.length = 0;
		openExternalFailure = new Error('no handler for https');

		emitIpc('desktop-splash:open-download', {sender: window.webContents});
		await settle();

		assert.deepEqual(events, ['quit']);
	});

	test('a protocol outside the allowlist never reaches the shell', async () => {
		events.length = 0;

		for (const url of [
			'file:///etc/passwd',
			'javascript:alert(1)',
			'vbscript:msgbox(1)',
			'data:text/html,hello',
			'about:blank',
			'chrome://settings',
			'shell:Startup',
			'ms-appinstaller:?source=https://example.com',
			'a:payload',
		]) {
			await assert.rejects(openExternalDeduped(url), /External URL open request blocked/, url);
		}

		assert.deepEqual(events, []);
	});
});
