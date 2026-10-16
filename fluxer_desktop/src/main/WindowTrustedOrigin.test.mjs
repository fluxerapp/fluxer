// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {mkdtempSync, readFileSync, rmSync} from 'node:fs';
import {tmpdir} from 'node:os';
import nodePath from 'node:path';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

const noop = () => undefined;
const ipcHandlers = new Map();

class ElectronEmitterStub {
	on() {
		return this;
	}

	once() {
		return this;
	}

	off() {
		return this;
	}

	removeListener() {
		return this;
	}

	removeAllListeners() {
		return this;
	}

	emit() {
		return false;
	}
}

installElectronStub({
	app: Object.assign(new ElectronEmitterStub(), {
		isReady: () => true,
		isPackaged: false,
		getPath: () => '/tmp/fluxer-desktop-test',
		getAppPath: () => '/tmp/fluxer-desktop-test',
		getVersion: () => '0.0.0',
		getLocale: () => 'en-US',
		setPath: noop,
		exit: noop,
		quit: noop,
		whenReady: () => Promise.resolve(),
		commandLine: {appendSwitch: noop, appendArgument: noop},
	}),
	BrowserWindow: class extends ElectronEmitterStub {
		static fromWebContents() {
			return null;
		}

		static getAllWindows() {
			return [];
		}

		static getFocusedWindow() {
			return null;
		}
	},
	Menu: {buildFromTemplate: () => ({}), setApplicationMenu: noop},
	Notification: Object.assign(class extends ElectronEmitterStub {}, {isSupported: () => false}),
	clipboard: {},
	desktopCapturer: {},
	dialog: {showMessageBoxSync: () => 0, showErrorBox: noop},
	globalShortcut: {},
	ipcMain: Object.assign(new ElectronEmitterStub(), {
		handle: (channel, handler) => {
			ipcHandlers.set(channel, handler);
		},
		removeHandler: noop,
	}),
	nativeImage: {createFromPath: () => ({isEmpty: () => true})},
	nativeTheme: new ElectronEmitterStub(),
	net: {fetch: () => Promise.reject(new Error('outbound network is not available in tests'))},
	powerMonitor: new ElectronEmitterStub(),
	powerSaveBlocker: {start: () => 0, stop: noop, isStarted: () => false},
	protocol: {registerSchemesAsPrivileged: noop, handle: noop, unhandle: noop, isProtocolHandled: () => false},
	safeStorage: {isEncryptionAvailable: () => false},
	screen: Object.assign(new ElectronEmitterStub(), {
		getPrimaryDisplay: () => ({workAreaSize: {width: 1920, height: 1080}}),
		getAllDisplays: () => [],
	}),
	session: {defaultSession: {}},
	shell: {openExternal: noop, showItemInFolder: noop},
	systemPreferences: {},
});

const {isTrustedOrigin} = await import('@electron/main/Window');
const {DESKTOP_APP_URL} = await import('@electron/common/Constants');
const {resolveDesktopLandingUrl} = await import('@electron/main/DesktopLastRoute');
const {initializeDeepLinks} = await import('@electron/main/DeepLinks');

function readMainSource(name) {
	return readFileSync(new URL(`./${name}`, import.meta.url), 'utf8');
}

function withArgv(argv, run) {
	const original = process.argv;
	process.argv = [original[0], original[1], ...argv];
	try {
		return run();
	} finally {
		process.argv = original;
	}
}

const HOSTILE_ARGV = [
	'--app-url=https://evil.example',
	'--app-url',
	'https://evil.example',
	'--url',
	'https://evil.example/channels/@me',
	'--fluxer-instance=https://api.evil.example',
	'https://evil.example/',
	'file:///etc/passwd',
];

describe('the window trust boundary is exactly the local app document origin', () => {
	test('the local app origin and its routes are trusted', () => {
		assert.equal(isTrustedOrigin(DESKTOP_APP_URL), true);
		assert.equal(isTrustedOrigin('fluxer-app://app/channels/@me'), true);
		assert.equal(isTrustedOrigin('fluxer-app://app/settings/user?tab=1#x'), true);
	});

	test('the reserved proxy routes are never trusted documents', () => {
		assert.equal(isTrustedOrigin('fluxer-app://app/api/k/v1/users/@me'), false);
		assert.equal(isTrustedOrigin('fluxer-app://app/proxy/k?url=https://evil.example'), false);
	});

	test('the credential, port and host bypasses are rejected', () => {
		assert.equal(isTrustedOrigin('fluxer-app://evil.example/x'), false);
		assert.equal(isTrustedOrigin('fluxer-app://u:p@app/'), false);
		assert.equal(isTrustedOrigin('fluxer-app://app:8080/'), false);
	});

	test('an opaque origin is rejected, which is what protects the data: voice-debug window', () => {
		assert.equal(isTrustedOrigin('data:text/html,x'), false);
		assert.equal(isTrustedOrigin('about:blank'), false);
		assert.equal(isTrustedOrigin('file:///etc/passwd'), false);
	});

	test('no remote origin is trusted any more', () => {
		assert.equal(isTrustedOrigin('https://web.fluxer.app/channels/@me'), false);
		assert.equal(isTrustedOrigin('https://web.canary.fluxer.app/'), false);
		assert.equal(isTrustedOrigin('https://evil.example/'), false);
	});

	test('the OS deep link scheme is not a document origin', () => {
		assert.equal(isTrustedOrigin('fluxer://guild/channel'), false);
	});

	test('an empty or absent URL is rejected', () => {
		assert.equal(isTrustedOrigin(''), false);
		assert.equal(isTrustedOrigin(undefined), false);
		assert.equal(isTrustedOrigin('not a url'), false);
	});
});

describe('the main window only ever loads the local app document', () => {
	test('the window hands its only load to the retry loop, aimed at the landing url', () => {
		const windowSource = readMainSource('Window.ts');
		assert.doesNotMatch(windowSource, /\.loadURL\(|\.loadFile\(/);
		assert.match(
			windowSource,
			/createAppLoadRetry\(\{\s*webContents,\s*appUrl: resolveDesktopLandingUrl\(app\.getPath\('userData'\)\),/,
		);
		const retrySource = readMainSource('AppLoadRetry.ts');
		assert.equal(retrySource.match(/\.loadURL\(/g)?.length, 1);
		assert.match(retrySource, /webContents\.loadURL\(appUrl\)/);
		assert.match(retrySource, /const appUrl = options\.appUrl;/);
	});

	test('no launch flag moves the landing url off the local app document', () => {
		const userData = mkdtempSync(nodePath.join(tmpdir(), 'fluxer-main-window-document-'));
		try {
			const landing = withArgv(HOSTILE_ARGV, () => resolveDesktopLandingUrl(userData));
			assert.equal(landing, `${DESKTOP_APP_URL}channels/@me`);
			assert.equal(isTrustedOrigin(landing), true);
		} finally {
			rmSync(userData, {recursive: true, force: true});
		}
	});

	test('a deep link reaches the renderer as an in-app path, never as a document to load', async () => {
		assert.doesNotMatch(readMainSource('DeepLinks.ts'), /\.loadURL\(|\.loadFile\(/);
		for (const deepLink of ['fluxer://channels/@me', 'fluxer://evil.example/https://evil.example', 'fluxer://-/x']) {
			ipcHandlers.delete('get-initial-deep-link');
			withArgv([...HOSTILE_ARGV, deepLink], () => initializeDeepLinks());
			const payload = await ipcHandlers.get('get-initial-deep-link')();
			assert.equal(typeof payload, 'string', deepLink);
			assert.ok(payload.startsWith('/'), deepLink);
			assert.equal(isTrustedOrigin(new URL(payload.replace(/^\/+/u, ''), DESKTOP_APP_URL).href), true, deepLink);
		}
	});

	test('a sign-in return link never reaches the renderer with its grant', async () => {
		ipcHandlers.delete('get-initial-deep-link');
		withArgv(['fluxer://handoff?code=ABCDEF-GHJKMN&grant=secret-grant'], () => initializeDeepLinks());
		assert.equal(await ipcHandlers.get('get-initial-deep-link')(), null);
	});

	test('a deep link that names a site is not forwarded as one', async () => {
		ipcHandlers.delete('get-initial-deep-link');
		withArgv(['https://evil.example/channels/@me'], () => initializeDeepLinks());
		assert.equal(await ipcHandlers.get('get-initial-deep-link')(), null);
	});
});
