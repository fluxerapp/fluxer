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
const {buildManualLatestDownloadUrl, DESKTOP_DOWNLOAD_ARCH, getDesktopDownloadArch, getUpdateBaseUrl} = await import(
	'@electron/main/ShellDownloadFormats'
);
const {BUILD_CHANNEL} = await import('@electron/common/BuildChannel');
const {decideShellUpdatePlan, resolveShellUpdatePlan, ShellUpdateCapability} = await import(
	'@electron/main/ShellUpdateCapability'
);
const {DOWNLOAD_PAGE_URL} = await import('@electron/main/UpdaterDownloads');
const {armBlockedShellUpdate, getBlockedShellUpdate, SPLASH_MANUAL_UPDATE_MESSAGE} = await import(
	'@electron/main/ShellUpdateSplash'
);

const SELF_UPDATE_PLAN = {capability: ShellUpdateCapability.SELF_UPDATE, updater: 'velopack'};
const MANUAL_PLATFORM_PLAN = {capability: ShellUpdateCapability.MANUAL_DOWNLOAD, reason: 'platform'};
const MANUAL_UNPACKAGED_PLAN = {capability: ShellUpdateCapability.MANUAL_DOWNLOAD, reason: 'unpackaged'};
const MANAGED_PLAN = {capability: ShellUpdateCapability.MANAGED_PACKAGE};
const APPIMAGE_TARGET = {
	installedPath: '/home/user/Applications/Fluxer.AppImage',
	directory: '/home/user/Applications',
};

const EXPECTED_PICKER_OPTIONS = [
	{value: 'deb', label: 'Debian (deb)', buttonLabel: 'Download', kind: 'download'},
	{value: 'rpm', label: 'Fedora (rpm)', buttonLabel: 'Download', kind: 'download'},
	{value: 'appimage', label: 'Linux (AppImage)', buttonLabel: 'Download', kind: 'download'},
	{value: 'tar_gz', label: 'Linux (tar.gz)', buttonLabel: 'Download', kind: 'download'},
	{value: 'nope', label: "I'll figure it out", buttonLabel: 'Okay', kind: 'quit'},
];

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

function withProperty(target, name, value, run) {
	const original = Object.getOwnPropertyDescriptor(target, name);
	Object.defineProperty(target, name, {value, configurable: true, writable: true});
	try {
		return run();
	} finally {
		if (original == null) {
			delete target[name];
		} else {
			Object.defineProperty(target, name, original);
		}
	}
}

function withPlatform(platform, run) {
	return withProperty(process, 'platform', platform, run);
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

function hasAffordance(blocked) {
	return blocked.action != null || blocked.options != null;
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
		assert.deepEqual(planFor({platform: 'linux'}), {capability: 'manual-download', reason: 'platform'});
		assert.deepEqual(planFor({platform: 'freebsd'}), {capability: 'manual-download', reason: 'platform'});
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
			capability: 'manual-download',
			reason: 'platform',
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
	test('every plan lands on an affordance that can resolve the block', () => {
		for (const plan of [SELF_UPDATE_PLAN, MANUAL_PLATFORM_PLAN, MANUAL_UNPACKAGED_PLAN, MANAGED_PLAN]) {
			for (const platform of ['linux', 'darwin', 'win32']) {
				const blocked = withPlatform(platform, () => getBlockedShellUpdate(plan, '2026.823.1'));
				assert.ok(hasAffordance(blocked), `${plan.capability} on ${platform} has no way out`);
				assert.notEqual(
					blocked.action?.kind,
					'retry',
					`${plan.capability} cannot escape a relaunch: the same binary refetches the same manifest and blocks again`,
				);
				for (const label of [blocked.action?.label, ...(blocked.options ?? []).map((option) => option.label)]) {
					assert.ok(label == null || label.length > 0);
				}
			}
		}
	});

	test('a shell that could have updated itself and did not falls back to the download page', () => {
		const blocked = withPlatform('win32', () => getBlockedShellUpdate(SELF_UPDATE_PLAN, '2026.823.1'));

		assert.deepEqual(blocked, {
			layout: 'splash',
			status: 'blocked-shell-update',
			action: {kind: 'download', label: 'Download update'},
			message: null,
			versionLabel: null,
			options: null,
			downloadUrl: DOWNLOAD_PAGE_URL,
			downloadUrls: new Map(),
		});
	});

	test('a packaged linux install gets the picker, with the version main already knows', () => {
		const blocked = withPlatform('linux', () => getBlockedShellUpdate(MANUAL_PLATFORM_PLAN, '2026.823.1'));

		assert.equal(blocked.layout, 'manual-update');
		assert.equal(blocked.status, 'blocked-shell-update');
		assert.equal(blocked.action, null);
		assert.equal(blocked.message, SPLASH_MANUAL_UPDATE_MESSAGE);
		assert.equal(blocked.versionLabel, 'Version 2026.823.1 available');
		assert.deepEqual(blocked.options, EXPECTED_PICKER_OPTIONS);
		assert.equal(blocked.downloadUrl, null);
	});

	test('an unknown latest version leaves the bottom line off rather than inventing one', () => {
		const blocked = withPlatform('linux', () => getBlockedShellUpdate(MANUAL_PLATFORM_PLAN, null));

		assert.equal(blocked.versionLabel, null);
		assert.deepEqual(blocked.options, EXPECTED_PICKER_OPTIONS);
	});

	test('a dev run from source is never handed a distro artifact', () => {
		const blocked = withPlatform('linux', () => getBlockedShellUpdate(MANUAL_UNPACKAGED_PLAN, '2026.823.1'));

		assert.equal(blocked.layout, 'splash');
		assert.equal(blocked.options, null);
		assert.deepEqual(blocked.action, {kind: 'download', label: 'Download update'});
		assert.equal(blocked.downloadUrl, DOWNLOAD_PAGE_URL);
	});

	test('a portable win32 or darwin build gets the page, never the linux picker', () => {
		for (const platform of ['win32', 'darwin']) {
			const blocked = withPlatform(platform, () => getBlockedShellUpdate(MANUAL_PLATFORM_PLAN, '2026.823.1'));
			assert.equal(blocked.layout, 'splash', platform);
			assert.equal(blocked.options, null, platform);
			assert.equal(blocked.downloadUrl, DOWNLOAD_PAGE_URL, platform);
		}
	});

	test('a package manager owned install is offered no download url at all', () => {
		const blocked = withPlatform('linux', () => getBlockedShellUpdate(MANAGED_PLAN, '2026.823.1'));

		assert.equal(blocked.downloadUrl, null);
		assert.equal(blocked.downloadUrls.size, 0);
		assert.equal(blocked.options, null);
		assert.equal(blocked.status, 'blocked-shell-update-managed');
		assert.deepEqual(blocked.action, {kind: 'quit', label: 'Quit'});
	});

	test('the download page is a channel aware page, never an artifact', () => {
		assert.equal(shouldOpenExternalUrl(DOWNLOAD_PAGE_URL), true);
		assert.equal(new URL(DOWNLOAD_PAGE_URL).pathname, '/download');
	});

	test('every picker entry resolves to the latest linux artifact route for the running arch', () => {
		const blocked = withPlatform('linux', () => getBlockedShellUpdate(MANUAL_PLATFORM_PLAN, '2026.823.1'));

		assert.deepEqual(
			[...blocked.downloadUrls.keys()],
			EXPECTED_PICKER_OPTIONS.filter((option) => option.kind === 'download').map((option) => option.value),
		);
		for (const [format, url] of blocked.downloadUrls) {
			assert.equal(url, `${getUpdateBaseUrl('linux')}/latest/${format}`);
			assert.equal(new URL(url).pathname.endsWith(`/linux/${DESKTOP_DOWNLOAD_ARCH}/latest/${format}`), true, url);
			assert.equal(shouldOpenExternalUrl(url), true);
		}
		assert.equal(blocked.downloadUrls.has('nope'), false);
	});

	test('the artifact route reads the platform at call time, so a darwin runner still proves the linux shape', () => {
		const linux = withPlatform('linux', () => buildManualLatestDownloadUrl('deb'));
		const win32 = withPlatform('win32', () => buildManualLatestDownloadUrl('setup'));

		assert.equal(linux, `${getUpdateBaseUrl('linux')}/latest/deb`);
		assert.ok(linux.includes('/linux/'), linux);
		assert.ok(win32.includes('/win32/'), win32);
	});

	test('the download arch collapses to the two we publish', () => {
		assert.equal(getDesktopDownloadArch('arm64'), 'arm64');
		assert.equal(getDesktopDownloadArch('x64'), 'x64');
		assert.equal(getDesktopDownloadArch('ia32'), 'x64');
		assert.equal(getDesktopDownloadArch('ppc64'), 'x64');
	});
});

describe('arming the blocked shell update', () => {
	test('a package manager owned install arms nothing that could open a url', async () => {
		const window = openSplashWindow();
		events.length = 0;

		armBlockedShellUpdate(MANAGED_PLAN, '2026.823.1');

		assert.deepEqual(window.webContents.sent.at(-1), {
			channel: 'desktop-splash:state',
			payload: {
				layout: 'splash',
				status: 'blocked-shell-update-managed',
				requiredSecurityUpdate: false,
				current: null,
				total: null,
				progress: null,
				seconds: null,
				receivedBytes: null,
				totalBytes: null,
				bytesPerSecond: null,
				action: {kind: 'quit', label: 'Quit'},
				message: null,
				versionLabel: null,
				options: null,
			},
		});

		emitIpc('desktop-splash:open-download', {sender: window.webContents});
		await settle();

		assert.deepEqual(events, []);
	});

	test('the download action opens the allowlisted page and then quits', async () => {
		const window = openSplashWindow();
		events.length = 0;

		withPlatform('win32', () => armBlockedShellUpdate(SELF_UPDATE_PLAN, '2026.823.1'));

		assert.deepEqual(window.webContents.sent.at(-1), {
			channel: 'desktop-splash:state',
			payload: {
				layout: 'splash',
				status: 'blocked-shell-update',
				requiredSecurityUpdate: false,
				current: null,
				total: null,
				progress: null,
				seconds: null,
				receivedBytes: null,
				totalBytes: null,
				bytesPerSecond: null,
				action: {kind: 'download', label: 'Download update'},
				message: null,
				versionLabel: null,
				options: null,
			},
		});

		emitIpc('desktop-splash:open-download', {sender: window.webContents});
		await settle();

		assert.deepEqual(events, [`open:${DOWNLOAD_PAGE_URL}`, 'quit']);
	});

	test('the picker sends the whole manual layout and opens the artifact the user chose', async () => {
		const window = openSplashWindow();
		events.length = 0;

		const blocked = withPlatform('linux', () => armBlockedShellUpdate(MANUAL_PLATFORM_PLAN, '2026.823.1'));

		assert.deepEqual(window.webContents.sent.at(-1), {
			channel: 'desktop-splash:state',
			payload: {
				layout: 'manual-update',
				status: 'blocked-shell-update',
				requiredSecurityUpdate: false,
				current: null,
				total: null,
				progress: null,
				seconds: null,
				receivedBytes: null,
				totalBytes: null,
				bytesPerSecond: null,
				action: null,
				message: SPLASH_MANUAL_UPDATE_MESSAGE,
				versionLabel: 'Version 2026.823.1 available',
				options: EXPECTED_PICKER_OPTIONS,
			},
		});

		emitIpc('desktop-splash:open-download', {sender: window.webContents}, 'rpm');
		await settle();

		assert.deepEqual(events, [`open:${blocked.downloadUrls.get('rpm')}`, 'quit']);
	});

	test('a token that was never armed is dropped instead of being turned into a url', async () => {
		const window = openSplashWindow();
		withPlatform('linux', () => armBlockedShellUpdate(MANUAL_PLATFORM_PLAN, '2026.823.1'));
		events.length = 0;

		for (const value of ['setup', 'dmg', 'nope', 'unknown_format']) {
			emitIpc('desktop-splash:open-download', {sender: window.webContents}, value);
		}
		await settle();

		assert.deepEqual(events, []);
	});

	test('a download that cannot be opened still resolves the screen instead of stranding the user', async () => {
		const window = openSplashWindow();
		withPlatform('linux', () => armBlockedShellUpdate(MANUAL_PLATFORM_PLAN, '2026.823.1'));
		events.length = 0;
		openExternalFailure = new Error('no handler for https');

		emitIpc('desktop-splash:open-download', {sender: window.webContents}, 'appimage');
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
