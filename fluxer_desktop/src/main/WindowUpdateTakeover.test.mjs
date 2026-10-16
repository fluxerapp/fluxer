// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

const openWindows = [];

class FakeWindow {
	constructor(name, {vetoUnload = false, closable = true} = {}) {
		this.name = name;
		this.vetoUnload = vetoUnload;
		this.closable = closable;
		this.visible = true;
		this.destroyed = false;
		this.listeners = new Map();
		this.unloadListeners = [];
		this.webContents = {
			once: (event, listener) => {
				if (event === 'will-prevent-unload') this.unloadListeners.push(listener);
			},
		};
		openWindows.push(this);
	}

	once(event, listener) {
		this.listeners.set(event, listener);
	}

	isDestroyed() {
		return this.destroyed;
	}

	isVisible() {
		return this.visible;
	}

	isClosable() {
		return this.closable;
	}

	hide() {
		this.visible = false;
	}

	show() {
		this.visible = true;
	}

	close() {
		if (!this.closable) return;
		if (this.vetoUnload) {
			let prevented = false;
			for (const listener of this.unloadListeners) {
				listener({preventDefault: () => (prevented = true)});
			}
			if (!prevented) return;
		}
		this.destroy();
	}

	destroy() {
		this.destroyed = true;
		this.visible = false;
		this.listeners.get('closed')?.();
	}
}

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
			return openWindows.filter((window) => !window.isDestroyed());
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

const {
	beginMainWindowTakeover,
	closeAppWindowsForUpdate,
	endMainWindowTakeover,
	hideAppWindowsForUpdate,
	onMainWindowTakeoverEnded,
	restoreAppWindowsAfterUpdate,
	isMainWindowTakenOver,
	showWindow,
} = await import('@electron/main/Window');

describe('the update takes the app windows over', () => {
	test('the windows hide at once and only the splash stays on screen', () => {
		openWindows.length = 0;
		const splash = new FakeWindow('splash');
		const main = new FakeWindow('main');
		const popout = new FakeWindow('voice popout');

		hideAppWindowsForUpdate(splash);

		assert.equal(splash.isVisible(), true);
		assert.equal(main.isVisible(), false);
		assert.equal(popout.isVisible(), false);
	});

	test('a check that finds nothing gives back exactly the windows it hid', () => {
		openWindows.length = 0;
		const splash = new FakeWindow('splash');
		const main = new FakeWindow('main');
		const trayHidden = new FakeWindow('hidden popout');
		trayHidden.hide();

		const hidden = hideAppWindowsForUpdate(splash);
		restoreAppWindowsAfterUpdate(hidden);

		assert.deepEqual(hidden, [main]);
		assert.equal(main.isVisible(), true);
		assert.equal(trayHidden.isVisible(), false, 'a window the user had hidden stays hidden');
	});

	test('a window that cannot be closed is destroyed at once instead of holding the update back', async () => {
		openWindows.length = 0;
		const splash = new FakeWindow('splash');
		const guard = new FakeWindow('screen capture guard', {closable: false});
		const startedAt = Date.now();

		await closeAppWindowsForUpdate(splash);

		assert.equal(guard.isDestroyed(), true);
		assert.ok(Date.now() - startedAt < 1000, 'the 5 second close deadline never came into it');
	});

	test('ending the takeover tells its listeners once', () => {
		let ended = 0;
		const stop = onMainWindowTakeoverEnded(() => {
			ended += 1;
		});
		beginMainWindowTakeover(() => undefined);
		endMainWindowTakeover();
		endMainWindowTakeover();
		stop();

		assert.equal(ended, 1);
	});

	test('the windows really close, past a renderer that would veto the unload', async () => {
		openWindows.length = 0;
		const splash = new FakeWindow('splash');
		const main = new FakeWindow('main', {vetoUnload: true});
		const popout = new FakeWindow('theme studio');

		await closeAppWindowsForUpdate(splash);

		assert.equal(main.isDestroyed(), true);
		assert.equal(popout.isDestroyed(), true);
		assert.equal(splash.isDestroyed(), false);
	});

	test('showing the app during the update raises the splash instead of a window that is gone', () => {
		let focused = 0;
		beginMainWindowTakeover(() => {
			focused += 1;
		});
		assert.equal(isMainWindowTakenOver(), true);

		showWindow();

		assert.equal(focused, 1);
		endMainWindowTakeover();
		assert.equal(isMainWindowTakenOver(), false);
		showWindow();
		assert.equal(focused, 1);
	});

	test('close to hide never swallows the close the update asks for', () => {
		const source = readFileSync(new URL('./Window.ts', import.meta.url), 'utf8');
		assert.match(source, /if \(!isQuitting && !closingMainWindowForUpdate && shouldHideMainWindowOnClose\(\)\) \{/);
	});
});
