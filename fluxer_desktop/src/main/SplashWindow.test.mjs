// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

const MODULE_URL = new URL('./SplashWindow.ts', import.meta.url).href;
const PRELOAD_PATH = fileURLToPath(new URL('../preload/splash.cjs', import.meta.url));
const PRELOAD_SOURCE = readFileSync(PRELOAD_PATH, 'utf8');
const STYLESHEET_PATH = fileURLToPath(new URL('../splash/index.css', import.meta.url));
const STYLESHEET_SOURCE = readFileSync(STYLESHEET_PATH, 'utf8');

const MANUAL_UPDATE_OPTIONS = Object.freeze([
	{value: 'deb', label: 'Debian (deb)', buttonLabel: 'Download', kind: 'download'},
	{value: 'rpm', label: 'Fedora (rpm)', buttonLabel: 'Download', kind: 'download'},
	{value: 'appimage', label: 'Linux (AppImage)', buttonLabel: 'Download', kind: 'download'},
	{value: 'tar_gz', label: 'Linux (tar.gz)', buttonLabel: 'Download', kind: 'download'},
	{value: 'nope', label: "I'll figure it out", buttonLabel: 'Okay', kind: 'quit'},
]);

const quitCalls = [];
const ipcRegistrations = [];

class FakeWebContents {
	constructor() {
		this.listeners = new Map();
		this.sent = [];
		this.windowOpenHandler = null;
		this.destroyed = false;
	}

	on(eventName, listener) {
		const existing = this.listeners.get(eventName) ?? [];
		existing.push(listener);
		this.listeners.set(eventName, existing);
	}

	setWindowOpenHandler(handler) {
		this.windowOpenHandler = handler;
	}

	isDestroyed() {
		return this.destroyed;
	}

	send(channel, payload) {
		this.sent.push({channel, payload});
	}

	emit(eventName, ...args) {
		for (const listener of this.listeners.get(eventName) ?? []) {
			listener(...args);
		}
	}
}

class FakeBrowserWindow {
	static last = null;

	constructor(options) {
		FakeBrowserWindow.last = this;
		this.options = options;
		this.webContents = new FakeWebContents();
		this.listeners = new Map();
		this.destroyed = false;
		this.visible = false;
		this.shownInactive = 0;
		this.alwaysOnTop = false;
		this.skipTaskbar = false;
		this.minimized = false;
		this.focusCount = 0;
		this.loadedUrls = [];
	}

	on(eventName, listener) {
		const existing = this.listeners.get(eventName) ?? [];
		existing.push(listener);
		this.listeners.set(eventName, existing);
	}

	once(eventName, listener) {
		this.on(eventName, listener);
	}

	isDestroyed() {
		return this.destroyed;
	}

	isVisible() {
		return this.visible;
	}

	show() {
		this.visible = true;
	}

	focus() {
		this.focusCount += 1;
	}

	isMinimized() {
		return this.minimized;
	}

	restore() {
		this.minimized = false;
	}

	showInactive() {
		this.visible = true;
		this.shownInactive += 1;
	}

	hide() {
		this.visible = false;
	}

	setSkipTaskbar(value) {
		this.skipTaskbar = value;
	}

	setAlwaysOnTop(value) {
		this.alwaysOnTop = value;
	}

	close() {
		if (this.destroyed) return;
		this.destroyed = true;
		this.webContents.destroyed = true;
		this.emit('closed');
	}

	destroy() {
		this.close();
	}

	loadURL(url) {
		this.loadedUrls.push(url);
		return Promise.resolve();
	}

	emit(eventName, ...args) {
		for (const listener of this.listeners.get(eventName) ?? []) {
			listener(...args);
		}
	}
}

const nativeTheme = {themeSource: 'system'};

installElectronStub({
	app: {
		quit: () => {
			quitCalls.push('quit');
		},
	},
	BrowserWindow: FakeBrowserWindow,
	nativeTheme,
	ipcMain: {
		on: (channel, handler) => {
			ipcRegistrations.push({channel, handler});
		},
	},
});

function emitIpc(channel, event, payload) {
	for (const registration of ipcRegistrations) {
		if (registration.channel === channel) {
			registration.handler(event, payload);
		}
	}
}

async function loadSplashWindow(instance) {
	return await import(`${MODULE_URL}?instance=${instance}`);
}

function withPlatform(platform, run) {
	const original = Object.getOwnPropertyDescriptor(process, 'platform');
	Object.defineProperty(process, 'platform', {value: platform, configurable: true});
	try {
		run();
	} finally {
		Object.defineProperty(process, 'platform', original);
	}
}

function withPlatformResult(platform, run) {
	let result;
	withPlatform(platform, () => {
		result = run();
	});
	return result;
}

function withCapturedTimers(run) {
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
	let result;
	try {
		result = run();
	} finally {
		globalThis.setTimeout = originalSetTimeout;
		globalThis.clearTimeout = originalClearTimeout;
	}
	return {result, timers};
}

function fireTimers(timers) {
	for (const timer of timers) {
		if (!timer.cleared) {
			timer.callback();
		}
	}
}

function withPlatformTimers(platform, run) {
	const captured = withCapturedTimers(() => withPlatformResult(platform, run));
	return {
		result: captured.result,
		timers: captured.timers,
		flush: () => {
			withPlatform(platform, () => {
				fireTimers(captured.timers);
			});
		},
	};
}

describe('splash window geometry', () => {
	test('is 300 tall on darwin and 350 tall everywhere else', async () => {
		const {getSplashWindowHeight} = await loadSplashWindow('geometry');

		assert.equal(getSplashWindowHeight('darwin'), 300);
		assert.equal(getSplashWindowHeight('win32'), 350);
		assert.equal(getSplashWindowHeight('linux'), 350);
	});
});

describe('splash launch latch', () => {
	test('quits on a non-darwin close only while the latch is unset', async () => {
		const {shouldQuitOnSplashClosed} = await loadSplashWindow('latch-decision');

		assert.equal(shouldQuitOnSplashClosed('win32', false), true);
		assert.equal(shouldQuitOnSplashClosed('linux', false), true);
		assert.equal(shouldQuitOnSplashClosed('win32', true), false);
		assert.equal(shouldQuitOnSplashClosed('linux', true), false);
		assert.equal(shouldQuitOnSplashClosed('darwin', false), false);
		assert.equal(shouldQuitOnSplashClosed('darwin', true), false);
	});

	test('a user close before launch quits the app on win32', async () => {
		const {openSplashWindow} = await loadSplashWindow('latch-unset');
		quitCalls.length = 0;

		withPlatform('win32', () => {
			const window = openSplashWindow();
			window.emit('closed');
		});

		assert.deepEqual(quitCalls, ['quit']);
	});

	test('markSplashLaunching stops the handoff close from quitting the app', async () => {
		const {closeSplashWindow, markSplashLaunching, openSplashWindow} = await loadSplashWindow('latch-set');
		quitCalls.length = 0;

		const {result: window, flush} = withPlatformTimers('win32', () => {
			const opened = openSplashWindow();
			markSplashLaunching();
			closeSplashWindow();
			return opened;
		});
		flush();

		assert.equal(window.destroyed, true);
		assert.deepEqual(quitCalls, []);
	});

	test('a handoff close without the latch still quits, so an aborted boot never leaves a headless process', async () => {
		const {closeSplashWindow, openSplashWindow} = await loadSplashWindow('latch-abort');
		quitCalls.length = 0;

		const {flush} = withPlatformTimers('linux', () => {
			openSplashWindow();
			closeSplashWindow();
		});
		flush();

		assert.deepEqual(quitCalls, ['quit']);
	});

	test('darwin never quits from the splash close handler', async () => {
		const {closeSplashWindow, openSplashWindow} = await loadSplashWindow('latch-darwin');
		quitCalls.length = 0;

		const {flush} = withPlatformTimers('darwin', () => {
			openSplashWindow();
			closeSplashWindow();
		});
		flush();

		assert.deepEqual(quitCalls, []);
	});
});

describe('splash window configuration', () => {
	test('opens frameless, sandboxed and hidden, loaded from file://', async () => {
		const {openSplashWindow} = await loadSplashWindow('configuration');

		const window = withPlatformResult('darwin', () => openSplashWindow());

		assert.equal(window.options.width, 300);
		assert.equal(window.options.height, 300);
		assert.equal(window.options.frame, false);
		assert.equal(window.options.resizable, false);
		assert.equal(window.options.center, true);
		assert.equal(window.options.show, false);
		assert.equal(window.options.transparent, false);
		assert.equal(window.options.webPreferences.contextIsolation, true);
		assert.equal(window.options.webPreferences.sandbox, true);
		assert.equal(window.options.webPreferences.nodeIntegration, false);
		assert.match(window.options.webPreferences.preload.replaceAll('\\', '/'), /\/preload\/splash\.cjs$/);
		assert.equal(window.loadedUrls.length, 1);
		assert.match(window.loadedUrls[0], /^file:\/\//);
		assert.match(window.loadedUrls[0], /\/splash\/index\.html$/);
	});

	test('forces the dark app appearance before the window exists', async () => {
		const {openSplashWindow} = await loadSplashWindow('appearance');
		nativeTheme.themeSource = 'system';

		withPlatformResult('darwin', () => openSplashWindow());

		assert.equal(nativeTheme.themeSource, 'dark');
	});

	test('restores the previous app appearance once the splash closes', async () => {
		const {closeSplashWindow, openSplashWindow} = await loadSplashWindow('appearance-restore');
		nativeTheme.themeSource = 'system';

		withPlatformResult('darwin', () => openSplashWindow());
		closeSplashWindow();

		assert.equal(nativeTheme.themeSource, 'system');
	});

	test('blocks navigation and denies every window open', async () => {
		const {openSplashWindow} = await loadSplashWindow('hardening');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		let prevented = false;
		window.webContents.emit('will-navigate', {
			preventDefault: () => {
				prevented = true;
			},
		});

		assert.equal(prevented, true);
		assert.deepEqual(window.webContents.windowOpenHandler({url: 'https://fluxer.app'}), {action: 'deny'});
	});

	test('stays hidden until the preload reports ready, then shows without stealing focus', async () => {
		const {openSplashWindow} = await loadSplashWindow('show-on-ready');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		assert.equal(window.visible, false);

		window.emit('ready-to-show');
		assert.equal(window.visible, false);

		emitIpc('desktop-splash:ready', {sender: window.webContents});

		assert.equal(window.visible, true);
		assert.equal(window.shownInactive, 1);
	});

	test('a preloaded splash loads hidden, stays hidden once ready, and reveals on demand', async () => {
		const {preloadSplashWindow, revealPreloadedSplashWindow} = await loadSplashWindow('preload');

		const {timers} = withPlatformTimers('darwin', () => preloadSplashWindow());
		const splash = FakeBrowserWindow.last;
		assert.equal(timers.length, 0);
		assert.equal(splash.visible, false);
		assert.equal(splash.skipTaskbar, true);
		assert.equal(revealPreloadedSplashWindow(), null);

		emitIpc('desktop-splash:ready', {sender: splash.webContents});
		assert.equal(splash.visible, false);

		assert.equal(revealPreloadedSplashWindow(), splash);
		assert.equal(splash.visible, true);
		assert.equal(splash.skipTaskbar, false);
		assert.equal(revealPreloadedSplashWindow(), null);
	});

	test('a preload that never reports ready still gets shown by the watchdog', async () => {
		const {openSplashWindow, SPLASH_READY_WATCHDOG_MS} = await loadSplashWindow('show-watchdog');

		const {result: window, timers, flush} = withPlatformTimers('darwin', () => openSplashWindow());

		assert.equal(window.visible, false);
		assert.equal(timers.length, 1);
		assert.equal(timers[0].delayMs, SPLASH_READY_WATCHDOG_MS);

		flush();

		assert.equal(window.visible, true);
		assert.equal(window.shownInactive, 1);
	});

	test('the ready handler disarms the watchdog so the window is never shown twice', async () => {
		const {openSplashWindow} = await loadSplashWindow('watchdog-disarm');

		const {
			result: window,
			timers,
			flush,
		} = withPlatformTimers('darwin', () => {
			const opened = openSplashWindow();
			emitIpc('desktop-splash:ready', {sender: opened.webContents});
			return opened;
		});

		assert.equal(timers[0].cleared, true);

		flush();

		assert.equal(window.shownInactive, 1);
	});

	test('reports a dead preload and a failed document load instead of leaving a black rectangle', async () => {
		const {openSplashWindow} = await loadSplashWindow('diagnostics');

		const window = withPlatformResult('darwin', () => openSplashWindow());

		assert.ok(window.webContents.listeners.has('did-fail-load'));
		assert.ok(window.webContents.listeners.has('preload-error'));
		assert.ok(window.webContents.listeners.has('console-message'));

		window.webContents.emit('did-fail-load', {}, -6, 'ERR_FILE_NOT_FOUND', 'file:///splash/index.html');
		window.webContents.emit('preload-error', {}, '/preload/splash.cjs', new Error('boom'));
		window.webContents.emit('console-message', {level: 'error', message: 'boom', lineNumber: 1});
	});
});

describe('splash window focus', () => {
	test('a launch forwarded while the splash is blocked restores, shows and raises it', async () => {
		const {focusSplashWindow, openSplashWindow} = await loadSplashWindow('focus-blocked');
		const window = withPlatformResult('darwin', () => openSplashWindow());
		window.visible = false;
		window.minimized = true;

		focusSplashWindow();

		assert.equal(window.minimized, false);
		assert.equal(window.visible, true);
		assert.equal(
			window.focusCount,
			1,
			'The blocked update paths never launch a main window, so raising the splash is the only answer a relaunch can get.',
		);
	});

	test('focusing without a splash window is a no operation', async () => {
		const {focusSplashWindow} = await loadSplashWindow('focus-none');
		assert.doesNotThrow(() => {
			focusSplashWindow();
		});
	});
});

describe('splash window teardown', () => {
	test('gives up the taskbar slot at once and only closes after the handoff gap', async () => {
		const {closeSplashWindow, openSplashWindow, SPLASH_CLOSE_DELAY_MS, markSplashLaunching} =
			await loadSplashWindow('teardown-order');

		const {
			result: window,
			timers,
			flush,
		} = withPlatformTimers('darwin', () => {
			const opened = openSplashWindow();
			markSplashLaunching();
			closeSplashWindow();
			return opened;
		});

		assert.equal(window.skipTaskbar, true);
		assert.equal(window.destroyed, false);
		assert.equal(timers.filter((timer) => !timer.cleared).length, 1);
		assert.equal(timers.at(-1).delayMs, SPLASH_CLOSE_DELAY_MS);

		flush();

		assert.equal(window.visible, false);
		assert.equal(window.destroyed, true);
	});

	test('a window that dies inside the handoff gap is never touched again', async () => {
		const {closeSplashWindow, openSplashWindow, markSplashLaunching} = await loadSplashWindow('teardown-race');

		const {result: window, flush} = withPlatformTimers('darwin', () => {
			const opened = openSplashWindow();
			markSplashLaunching();
			closeSplashWindow();
			return opened;
		});
		window.destroy();
		window.hide = () => {
			throw new Error('a destroyed window must never be hidden');
		};

		flush();
	});
});

describe('splash state serialisation', () => {
	test('truncates counters and clamps progress', async () => {
		const {serializeSplashState, SplashStatus} = await loadSplashWindow('serialise-clean');

		assert.deepEqual(
			serializeSplashState({
				status: SplashStatus.DOWNLOADING_UPDATES,
				current: 3.9,
				total: 7,
				progress: 42.5,
			}),
			{
				layout: 'splash',
				status: 'downloading-updates',
				requiredSecurityUpdate: false,
				current: 3,
				total: 7,
				progress: 42.5,
				seconds: null,
				receivedBytes: null,
				totalBytes: null,
				bytesPerSecond: null,
				action: null,
				message: null,
				versionLabel: null,
				options: null,
			},
		);
	});

	test('drops values that cannot cross the IPC boundary as numbers', async () => {
		const {serializeSplashState, SplashStatus} = await loadSplashWindow('serialise-junk');

		assert.deepEqual(
			serializeSplashState({
				status: SplashStatus.UPDATE_FAILURE,
				current: Number.NaN,
				total: Number.POSITIVE_INFINITY,
				progress: -12,
				seconds: -4,
			}),
			{
				layout: 'splash',
				status: 'update-failure',
				requiredSecurityUpdate: false,
				current: null,
				total: null,
				progress: 0,
				seconds: 0,
				receivedBytes: null,
				totalBytes: null,
				bytesPerSecond: null,
				action: null,
				message: null,
				versionLabel: null,
				options: null,
			},
		);
	});

	test('clamps progress to 100', async () => {
		const {serializeSplashState, SplashStatus} = await loadSplashWindow('serialise-clamp');

		assert.equal(serializeSplashState({status: SplashStatus.INSTALLING_UPDATES, progress: 480}).progress, 100);
	});

	test('falls back to checking for updates on an unknown status', async () => {
		const {serializeSplashState} = await loadSplashWindow('serialise-unknown');

		assert.equal(serializeSplashState({status: 'not-a-splash-status'}).status, 'checking-for-updates');
	});

	test('falls back to the splash layout on anything the renderer could not lay out', async () => {
		const {serializeSplashState, SplashLayout, SplashStatus} = await loadSplashWindow('serialise-layout');
		const layoutFor = (layout) => serializeSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, layout}).layout;

		assert.equal(layoutFor(SplashLayout.MANUAL_UPDATE), 'manual-update');
		assert.equal(layoutFor(SplashLayout.SPLASH), 'splash');
		assert.equal(layoutFor('dice-roll'), 'splash');
		assert.equal(layoutFor(undefined), 'splash');
		assert.equal(layoutFor(null), 'splash');
	});

	test('trims and caps the manual update copy', async () => {
		const {serializeSplashState, SplashStatus} = await loadSplashWindow('serialise-copy');
		const serialized = serializeSplashState({
			status: SplashStatus.BLOCKED_SHELL_UPDATE,
			message: `  ${'m'.repeat(400)}  `,
			versionLabel: `  ${'v'.repeat(200)}  `,
		});

		assert.equal(serialized.message.length, 160);
		assert.equal(serialized.versionLabel.length, 64);
		assert.equal(serializeSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, message: '   '}).message, null);
		assert.equal(serializeSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, versionLabel: 7}).versionLabel, null);
	});

	test('carries every shipped action through and drops one the renderer could not route', async () => {
		const {serializeSplashState, SplashAction, SplashStatus} = await loadSplashWindow('serialise-action');
		const actionFor = (action) => serializeSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, action}).action;

		assert.deepEqual(actionFor(SplashAction.RETRY), {kind: 'retry', label: 'Retry now'});
		assert.deepEqual(actionFor(SplashAction.DOWNLOAD), {kind: 'download', label: 'Download update'});
		assert.deepEqual(actionFor(SplashAction.QUIT), {kind: 'quit', label: 'Quit'});
		assert.equal(actionFor({kind: 'launch-anyway', label: 'Launch anyway'}), null);
		assert.equal(actionFor({kind: 'retry', label: '   '}), null);
		assert.equal(actionFor({kind: 'retry', label: 42}), null);
		assert.equal(actionFor(undefined), null);
		assert.equal(actionFor({kind: 'retry', label: 'l'.repeat(200)}).label.length, 48);
	});

	test('carries the whole picker through and drops entries the renderer could not route', async () => {
		const {serializeSplashState, SplashStatus} = await loadSplashWindow('serialise-options');
		const optionsFor = (options) => serializeSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, options}).options;

		assert.deepEqual(optionsFor(MANUAL_UPDATE_OPTIONS), MANUAL_UPDATE_OPTIONS);
		assert.equal(optionsFor(undefined), null);
		assert.equal(optionsFor('deb'), null);
		assert.equal(optionsFor([]), null);
		assert.equal(optionsFor([{value: 'deb', label: 'Debian', buttonLabel: 'Download', kind: 'launch-anyway'}]), null);
		assert.equal(optionsFor([{value: 'DEB', label: 'Debian', buttonLabel: 'Download', kind: 'download'}]), null);
		assert.equal(optionsFor([{value: '../etc', label: 'Debian', buttonLabel: 'Download', kind: 'download'}]), null);
		assert.equal(optionsFor([{value: 'deb', label: '  ', buttonLabel: 'Download', kind: 'download'}]), null);
		assert.equal(optionsFor([{value: 'deb', label: 'Debian', buttonLabel: '', kind: 'download'}]), null);
		assert.deepEqual(
			optionsFor([
				{value: 'deb', label: 'Debian', buttonLabel: 'Download', kind: 'download'},
				{value: 'not valid', label: 'Nope', buttonLabel: 'Nope', kind: 'download'},
			]),
			[{value: 'deb', label: 'Debian', buttonLabel: 'Download', kind: 'download'}],
		);
		assert.equal(
			optionsFor(
				Array.from({length: 12}, (_entry, index) => ({
					value: `format_${index}`,
					label: `Format ${index}`,
					buttonLabel: 'Download',
					kind: 'download',
				})),
			).length,
			8,
		);
		assert.equal(
			optionsFor([{value: 'deb', label: 'l'.repeat(200), buttonLabel: 'b'.repeat(200), kind: 'download'}])[0].label
				.length,
			48,
		);
		assert.equal(
			optionsFor([{value: 'deb', label: 'l'.repeat(200), buttonLabel: 'b'.repeat(200), kind: 'download'}])[0]
				.buttonLabel.length,
			24,
		);
	});

	test('only a state that carries a way out counts as an affordance', async () => {
		const {hasSplashAffordance, serializeSplashState, SplashAction, SplashStatus} =
			await loadSplashWindow('affordance-predicate');

		assert.equal(hasSplashAffordance(serializeSplashState({status: SplashStatus.CHECKING_FOR_UPDATES})), false);
		assert.equal(hasSplashAffordance(serializeSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE})), false);
		assert.equal(
			hasSplashAffordance(serializeSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, action: SplashAction.QUIT})),
			true,
		);
		assert.equal(
			hasSplashAffordance(
				serializeSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, options: MANUAL_UPDATE_OPTIONS}),
			),
			true,
		);
	});

	test('the manual layout can never be serialised without a way out of it', async () => {
		const {hasSplashAffordance, serializeSplashState, SplashLayout, SplashStatus} =
			await loadSplashWindow('affordance-manual');
		const manual = (patch) =>
			serializeSplashState({layout: SplashLayout.MANUAL_UPDATE, status: SplashStatus.BLOCKED_SHELL_UPDATE, ...patch});

		assert.deepEqual(manual({}).action, {kind: 'quit', label: 'Quit'});
		assert.deepEqual(manual({action: {kind: 'launch-anyway', label: 'Launch anyway'}, options: []}).action, {
			kind: 'quit',
			label: 'Quit',
		});
		assert.equal(hasSplashAffordance(manual({})), true);
		assert.equal(manual({options: MANUAL_UPDATE_OPTIONS}).action, null);
		assert.deepEqual(manual({options: MANUAL_UPDATE_OPTIONS}).options, MANUAL_UPDATE_OPTIONS);
		assert.equal(serializeSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE}).action, null);
	});

	test('only a token the main process could have minted survives the download payload check', async () => {
		const {toSplashDownloadValue} = await loadSplashWindow('download-payload');

		assert.equal(toSplashDownloadValue('deb'), 'deb');
		assert.equal(toSplashDownloadValue('tar_gz'), 'tar_gz');
		assert.equal(toSplashDownloadValue(undefined), null);
		assert.equal(toSplashDownloadValue(null), null);
		assert.equal(toSplashDownloadValue(42), null);
		assert.equal(toSplashDownloadValue('../../etc/passwd'), null);
		assert.equal(toSplashDownloadValue('https://evil.example/x'), null);
		assert.equal(toSplashDownloadValue('a'.repeat(33)), null);
		assert.equal(toSplashDownloadValue(''), null);
	});
});

describe('splash affordance latch', () => {
	test('a state with no way out can never erase one that has one', async () => {
		const {openSplashWindow, setSplashState, SplashAction, SplashStatus} = await loadSplashWindow('latch-affordance');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		setSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, action: SplashAction.DOWNLOAD});
		window.webContents.sent.length = 0;

		setSplashState({status: SplashStatus.CHECKING_FOR_UPDATES});
		setSplashState({status: SplashStatus.DOWNLOADING_UPDATES, current: 1, total: 2, progress: 10});

		assert.deepEqual(window.webContents.sent, []);
	});

	test('a state that carries its own way out replaces the armed one', async () => {
		const {openSplashWindow, setSplashState, SplashAction, SplashStatus} = await loadSplashWindow('latch-replace');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		setSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, action: SplashAction.DOWNLOAD});
		window.webContents.sent.length = 0;

		setSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE_MANAGED, action: SplashAction.QUIT});

		assert.equal(window.webContents.sent.length, 1);
		assert.deepEqual(window.webContents.sent[0].payload.action, {kind: 'quit', label: 'Quit'});
	});

	test('the shell self update paints its progress because the updater block carries no affordance', async () => {
		const {openSplashWindow, setSplashState, SplashStatus} = await loadSplashWindow('latch-self-update');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		setSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE});
		setSplashState({status: SplashStatus.SHELL_UPDATE_DOWNLOADING, progress: 42});
		setSplashState({status: SplashStatus.SHELL_UPDATE_RESTARTING});

		assert.deepEqual(
			window.webContents.sent.map((entry) => entry.payload.status),
			['blocked-shell-update', 'shell-update-downloading', 'shell-update-restarting'],
		);
	});

	test('every state that latches is also raised above the windows that would bury it', async () => {
		const {openSplashWindow, setSplashState, SplashAction, SplashStatus} = await loadSplashWindow('latch-on-top');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		setSplashState({status: SplashStatus.CHECKING_FOR_UPDATES});
		assert.equal(window.alwaysOnTop, false);

		setSplashState({status: SplashStatus.BLOCKED_UNSUPPORTED_BUILD, action: SplashAction.QUIT});
		assert.equal(window.alwaysOnTop, true);
	});

	test('releasing the latch lets progress paint again and lowers the window', async () => {
		const {openSplashWindow, releaseSplashAffordance, setSplashState, SplashAction, SplashStatus} =
			await loadSplashWindow('latch-released');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		setSplashState({status: SplashStatus.BLOCKED_UPDATE_UNREACHABLE, action: SplashAction.RETRY});
		setSplashState({status: SplashStatus.CHECKING_FOR_UPDATES});
		assert.equal(window.webContents.sent.at(-1).payload.status, 'blocked-update-unreachable');
		assert.equal(window.alwaysOnTop, true);

		releaseSplashAffordance();
		setSplashState({status: SplashStatus.DOWNLOADING_UPDATES, current: 1, total: 2, progress: 10});

		assert.equal(window.alwaysOnTop, false);
		assert.equal(window.webContents.sent.at(-1).payload.status, 'downloading-updates');
		assert.equal(window.webContents.sent.at(-1).payload.action, null);
	});

	test('closing the splash releases the latch so a re-opened splash is not born blocked', async () => {
		const {closeSplashWindow, openSplashWindow, setSplashState, SplashAction, SplashStatus} =
			await loadSplashWindow('latch-release');

		const {result: first, flush} = withPlatformTimers('darwin', () => {
			const opened = openSplashWindow();
			setSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, action: SplashAction.DOWNLOAD});
			closeSplashWindow();
			return opened;
		});
		flush();
		assert.equal(first.destroyed, true);

		const second = withPlatformResult('darwin', () => openSplashWindow());
		setSplashState({status: SplashStatus.CHECKING_FOR_UPDATES});

		assert.equal(second.webContents.sent.at(-1).payload.status, 'checking-for-updates');
	});
});

describe('splash IPC', () => {
	test('sends the serialised state on desktop-splash:state', async () => {
		const {openSplashWindow, setSplashState, SplashStatus} = await loadSplashWindow('ipc-state');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		setSplashState({status: SplashStatus.DOWNLOADING_UPDATES, current: 2, total: 5, progress: 40});

		assert.deepEqual(window.webContents.sent, [
			{
				channel: 'desktop-splash:state',
				payload: {
					layout: 'splash',
					status: 'downloading-updates',
					requiredSecurityUpdate: false,
					current: 2,
					total: 5,
					progress: 40,
					seconds: null,
					receivedBytes: null,
					totalBytes: null,
					bytesPerSecond: null,
					action: null,
					message: null,
					versionLabel: null,
					options: null,
				},
			},
		]);
	});

	test('replays the last state and notifies listeners when the splash reports ready', async () => {
		const {onSplashReady, openSplashWindow, setSplashState, SplashStatus} = await loadSplashWindow('ipc-ready');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		setSplashState({status: SplashStatus.VERIFYING});
		window.webContents.sent.length = 0;
		let readyCount = 0;
		onSplashReady(() => {
			readyCount += 1;
		});
		emitIpc('desktop-splash:ready', {sender: window.webContents});

		assert.equal(readyCount, 1);
		assert.deepEqual(window.webContents.sent, [
			{
				channel: 'desktop-splash:state',
				payload: {
					layout: 'splash',
					status: 'verifying',
					requiredSecurityUpdate: false,
					current: null,
					total: null,
					progress: null,
					seconds: null,
					receivedBytes: null,
					totalBytes: null,
					bytesPerSecond: null,
					action: null,
					message: null,
					versionLabel: null,
					options: null,
				},
			},
		]);
	});

	test('notifies retry and quit listeners only for the splash sender', async () => {
		const {onSplashQuit, onSplashRetry, openSplashWindow} = await loadSplashWindow('ipc-sender');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		let retryCount = 0;
		let quitCount = 0;
		onSplashRetry(() => {
			retryCount += 1;
		});
		onSplashQuit(() => {
			quitCount += 1;
		});
		emitIpc('desktop-splash:retry-now', {sender: new FakeWebContents()});
		emitIpc('desktop-splash:quit', {sender: new FakeWebContents()});

		assert.equal(retryCount, 0);
		assert.equal(quitCount, 0);

		emitIpc('desktop-splash:retry-now', {sender: window.webContents});
		emitIpc('desktop-splash:quit', {sender: window.webContents});

		assert.equal(retryCount, 1);
		assert.equal(quitCount, 1);
	});

	test('notifies network listeners when the splash sees the network return, and only for the splash sender', async () => {
		const {onSplashNetworkOnline, openSplashWindow} = await loadSplashWindow('ipc-network-online');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		let onlineCount = 0;
		onSplashNetworkOnline(() => {
			onlineCount += 1;
		});
		emitIpc('desktop-splash:network-online', {sender: new FakeWebContents()});

		assert.equal(onlineCount, 0);

		emitIpc('desktop-splash:network-online', {sender: window.webContents});

		assert.equal(onlineCount, 1);
	});

	test('hands the download listener the chosen option, and nothing at all for a bare click', async () => {
		const {onSplashOpenDownload, openSplashWindow} = await loadSplashWindow('ipc-open-download');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		const values = [];
		onSplashOpenDownload((value) => {
			values.push(value);
		});
		emitIpc('desktop-splash:open-download', {sender: new FakeWebContents()}, 'deb');

		assert.deepEqual(values, []);

		emitIpc('desktop-splash:open-download', {sender: window.webContents});
		emitIpc('desktop-splash:open-download', {sender: window.webContents}, 'tar_gz');
		emitIpc('desktop-splash:open-download', {sender: window.webContents}, 'https://evil.example');

		assert.deepEqual(values, [null, 'tar_gz', null]);
	});

	test('stops sending state once the splash is closed', async () => {
		const {closeSplashWindow, openSplashWindow, setSplashState, SplashStatus} = await loadSplashWindow('ipc-closed');

		const {result: window} = withPlatformTimers('darwin', () => {
			const opened = openSplashWindow();
			closeSplashWindow();
			return opened;
		});
		setSplashState({status: SplashStatus.LAUNCHING});

		assert.deepEqual(window.webContents.sent, []);
	});

	test('stops sending state to a window whose web contents died under it', async () => {
		const {openSplashWindow, setSplashState, SplashStatus} = await loadSplashWindow('ipc-dead-contents');

		const window = withPlatformResult('darwin', () => openSplashWindow());
		window.webContents.destroyed = true;
		setSplashState({status: SplashStatus.LAUNCHING});

		assert.deepEqual(window.webContents.sent, []);
	});
});

class StubClassList {
	constructor(node) {
		this.node = node;
	}

	contains(name) {
		return this.node.className
			.split(' ')
			.filter((part) => part.length > 0)
			.includes(name);
	}

	add(name) {
		if (this.contains(name)) return;
		this.node.className = `${this.node.className} ${name}`.trim();
	}

	remove(name) {
		this.node.className = this.node.className
			.split(' ')
			.filter((part) => part !== name && part.length > 0)
			.join(' ');
	}
}

class StubNode {
	constructor(tagName) {
		this.tagName = String(tagName).toLowerCase();
		this.className = '';
		this.id = '';
		this.value = '';
		this.style = {};
		this.attributes = new Map();
		this.children = [];
		this.parent = null;
		this.listeners = new Map();
		this.text = '';
		this.classList = new StubClassList(this);
		this.replaceChildrenCalls = 0;
		this.decodeCalls = 0;
	}

	decode() {
		this.decodeCalls += 1;
		return Promise.resolve();
	}

	get textContent() {
		return this.text + this.children.map((child) => child.textContent).join('');
	}

	set textContent(value) {
		for (const child of this.children) {
			child.parent = null;
		}
		this.children = [];
		this.text = String(value);
	}

	setAttribute(name, value) {
		this.attributes.set(name, String(value));
	}

	getAttribute(name) {
		return this.attributes.get(name) ?? null;
	}

	appendChild(node) {
		node.parent = this;
		this.children.push(node);
		return node;
	}

	replaceChildren(...nodes) {
		this.replaceChildrenCalls += 1;
		for (const child of this.children) {
			child.parent = null;
		}
		this.children = [];
		for (const node of nodes) {
			this.appendChild(node);
		}
	}

	replaceWith(node) {
		const parent = this.parent;
		if (parent == null) return;
		const index = parent.children.indexOf(this);
		if (index < 0) return;
		parent.children[index] = node;
		node.parent = parent;
		this.parent = null;
	}

	addEventListener(eventName, listener) {
		const existing = this.listeners.get(eventName) ?? [];
		existing.push(listener);
		this.listeners.set(eventName, existing);
	}

	dispatch(eventName) {
		for (const listener of this.listeners.get(eventName) ?? []) {
			listener();
		}
	}

	matches(selector) {
		return selector
			.split(',')
			.map((part) => part.trim())
			.filter((part) => part.length > 0)
			.some((part) => {
				if (part.startsWith('.')) return this.classList.contains(part.slice(1));
				if (part.startsWith('#')) return this.id === part.slice(1);
				return this.tagName === part.toLowerCase();
			});
	}

	descendants() {
		const found = [];
		for (const child of this.children) {
			found.push(child, ...child.descendants());
		}
		return found;
	}

	querySelector(selector) {
		return this.descendants().find((node) => node.matches(selector)) ?? null;
	}

	querySelectorAll(selector) {
		return this.descendants().filter((node) => node.matches(selector));
	}
}

function createPreloadHarness() {
	const sent = [];
	const ipcListeners = new Map();
	const windowListeners = new Map();
	const intervals = [];
	const frames = [];
	const timeouts = [];
	const fontLoads = [];
	const mount = new StubNode('div');
	mount.id = 'splash-mount';
	const document = {
		getElementById: (id) => (mount.id === id ? mount : (mount.querySelector(`#${id}`) ?? null)),
		createElement: (tagName) => new StubNode(tagName),
		fonts: {
			load: (face) => {
				fontLoads.push(face);
				return Promise.resolve([]);
			},
		},
	};
	const context = vm.createContext({
		document,
		require: (specifier) => {
			if (specifier !== 'electron') throw new Error(`The splash preload must not require ${specifier}`);
			return {
				ipcRenderer: {
					on: (channel, listener) => {
						ipcListeners.set(channel, listener);
					},
					send: (channel, payload) => {
						sent.push({channel, payload});
					},
				},
			};
		},
		setInterval: (callback, delayMs) => {
			const timer = {id: intervals.length + 1, callback, delayMs, cleared: false};
			intervals.push(timer);
			return timer.id;
		},
		clearInterval: (id) => {
			const timer = intervals.find((entry) => entry.id === id);
			if (timer != null) {
				timer.cleared = true;
			}
		},
		setTimeout: (callback, delayMs) => {
			timeouts.push({callback, delayMs});
			return timeouts.length;
		},
		clearTimeout: (id) => {
			const timer = timeouts[id - 1];
			if (timer != null) {
				timer.cleared = true;
			}
		},
		window: {
			addEventListener: (eventName, listener) => {
				windowListeners.set(eventName, listener);
			},
			requestAnimationFrame: (callback) => {
				frames.push(callback);
				return frames.length;
			},
		},
	});
	vm.runInContext(PRELOAD_SOURCE, context, {filename: PRELOAD_PATH});
	const query = (selector) => mount.querySelector(selector);
	return {
		context,
		intervals,
		liveIntervals: () => intervals.filter((timer) => !timer.cleared),
		mount,
		query,
		queryAll: (selector) => mount.querySelectorAll(selector),
		click: (selector) => query(selector).dispatch('click'),
		changeSelect: (value) => {
			const select = query('#dl-select-input');
			select.value = value;
			select.dispatch('change');
		},
		sendState: (payload) => ipcListeners.get('desktop-splash:state')({}, payload),
		sent,
		channels: () => sent.map((entry) => entry.channel),
		frames,
		timeouts,
		fontLoads,
		flushFrame: () => {
			for (const callback of frames.splice(0)) callback();
		},
		settle: () => new Promise((resolve) => setImmediate(resolve)),
		startUnpainted: () => windowListeners.get('DOMContentLoaded')(),
		start: async () => {
			windowListeners.get('DOMContentLoaded')();
			await new Promise((resolve) => setImmediate(resolve));
			for (const callback of frames.splice(0)) callback();
			for (const callback of frames.splice(0)) callback();
		},
		unload: () => windowListeners.get('beforeunload')(),
		networkOnline: () => windowListeners.get('online')(),
		statusText: () => query('.splash-status').textContent,
	};
}

function styleRule(selector) {
	const marker = `\n${selector} {`;
	const start = STYLESHEET_SOURCE.indexOf(marker);
	assert.notEqual(start, -1, `${selector} is missing from the splash stylesheet`);
	const end = STYLESHEET_SOURCE.indexOf('\n}', start);
	return STYLESHEET_SOURCE.slice(start + marker.length, end);
}

describe('splash preload rendering', () => {
	test('renders before it reports ready, so the first paint is never a blank frame', async () => {
		const harness = createPreloadHarness();

		await harness.start();

		assert.equal(harness.mount.replaceChildrenCalls, 1);
		assert.deepEqual(harness.sent, [{channel: 'desktop-splash:ready', payload: undefined}]);
		assert.equal(harness.statusText(), 'Checking for updates…');
		assert.equal(harness.query('.progress-placeholder').textContent, ' ');
		assert.equal(harness.query('.splash-mark').getAttribute('src'), './symbol.svg');
		assert.equal(harness.query('.splash-mark').getAttribute('width'), '88');
	});

	test('reports ready only once the mark is decoded, the fonts are in and two frames have painted', async () => {
		const harness = createPreloadHarness();

		harness.startUnpainted();
		assert.deepEqual(harness.sent, []);
		assert.equal(harness.query('.splash-mark').decodeCalls, 1);
		assert.deepEqual(harness.fontLoads, ['400 12px "Fluxer Sans"', '500 16px "Fluxer Sans"']);
		const brandingWaits = harness.timeouts.filter((timer) => timer.delayMs <= 500);
		assert.equal(brandingWaits.length, 1);

		await harness.settle();
		assert.deepEqual(harness.sent, []);
		harness.flushFrame();
		assert.deepEqual(harness.sent, []);
		harness.flushFrame();

		assert.deepEqual(harness.channels(), ['desktop-splash:ready']);
	});

	test('patches the status in place instead of rebuilding when the layout is unchanged', () => {
		const harness = createPreloadHarness();
		harness.start();

		harness.sendState({status: 'verifying'});

		assert.equal(harness.mount.replaceChildrenCalls, 1);
		assert.equal(harness.statusText(), 'Verifying files…');
	});

	test('never rebuilds when the bar appears, and keeps the same mark node throughout', () => {
		const harness = createPreloadHarness();
		harness.start();
		const mark = harness.query('.splash-mark');

		harness.sendState({status: 'downloading-updates', current: 3, total: 7, progress: 42});

		assert.equal(harness.mount.replaceChildrenCalls, 1);
		assert.equal(harness.query('.complete').style.width, '42%');
		assert.equal(harness.query('.splash-mark'), mark);

		const complete = harness.query('.complete');
		harness.sendState({status: 'downloading-updates', current: 4, total: 7, progress: 55});

		assert.equal(harness.mount.replaceChildrenCalls, 1);
		assert.equal(harness.statusText(), 'Downloading update 4 of 7…');
		assert.equal(harness.query('.complete'), complete);
		assert.equal(complete.style.width, '55%');
		assert.equal(harness.query('.splash-mark'), mark);
	});

	test('swaps the bar back to a placeholder once, then leaves it alone', () => {
		const harness = createPreloadHarness();
		harness.start();
		harness.sendState({status: 'downloading-updates', current: 1, total: 2, progress: 40});
		harness.sendState({status: 'verifying'});
		const placeholder = harness.query('.progress-placeholder');

		assert.equal(placeholder.textContent, ' ');
		assert.equal(harness.query('.progress'), null);

		harness.sendState({status: 'update-failure', seconds: 8});

		assert.equal(harness.query('.progress-placeholder'), placeholder);
	});

	test('keeps the placeholder whenever the bar is hidden', () => {
		const harness = createPreloadHarness();
		harness.start();

		for (const payload of [
			{status: 'downloading-updates', current: 1, total: 2},
			{status: 'verifying', progress: 50},
			{status: 'update-failure', seconds: 8, progress: 50},
			{status: 'shell-update-restarting'},
		]) {
			harness.sendState(payload);
			assert.notEqual(harness.query('.progress-placeholder'), null, `${payload.status} must keep the placeholder`);
			assert.equal(harness.query('.progress'), null, `${payload.status} must keep the placeholder`);
		}

		harness.sendState({status: 'installing-updates', current: 5, total: 7, progress: 78});

		assert.notEqual(harness.query('.progress-bar'), null);
	});

	test('shows the shell update bar only once velopack reports a number', () => {
		const harness = createPreloadHarness();
		harness.start();

		harness.sendState({status: 'shell-update-downloading'});

		assert.equal(harness.statusText(), 'Updating Fluxer…');
		assert.notEqual(harness.query('.progress-placeholder'), null);

		harness.sendState({status: 'shell-update-downloading', progress: 12});

		assert.equal(harness.query('.complete').style.width, '12%');
	});

	test('counts the retry down once a second and stops at zero', () => {
		const harness = createPreloadHarness();
		harness.start();

		harness.sendState({status: 'update-failure', seconds: 2});

		assert.equal(harness.liveIntervals().length, 1);
		assert.equal(harness.intervals[0].delayMs, 1000);
		assert.equal(harness.statusText(), 'Update failed. Retrying in 2 sec…');

		harness.intervals.at(-1).callback();
		assert.equal(harness.statusText(), 'Update failed. Retrying in 1 sec…');

		harness.intervals.at(-1).callback();
		assert.equal(harness.statusText(), 'Update failed. Retrying in 0 sec…');
		assert.equal(harness.liveIntervals().length, 0);

		harness.intervals.at(-1).callback();
		assert.equal(harness.statusText(), 'Update failed. Retrying in 0 sec…');
	});

	test('runs a timer only while a state carries a countdown, and stops on unload', () => {
		const harness = createPreloadHarness();
		harness.start();

		assert.equal(harness.intervals.length, 0);

		harness.sendState({status: 'verifying'});

		assert.equal(harness.intervals.length, 0);

		harness.sendState({status: 'update-failure', seconds: 3});

		assert.equal(harness.liveIntervals().length, 1);

		harness.sendState({status: 'launching'});

		assert.equal(harness.liveIntervals().length, 0);
		assert.equal(harness.intervals.length, 1);

		harness.sendState({status: 'update-failure', seconds: 3});
		assert.equal(harness.liveIntervals().length, 1);

		harness.unload();

		assert.equal(harness.liveIntervals().length, 0);
	});

	test('restarts the countdown on every state so the first tick is a full second away', () => {
		const harness = createPreloadHarness();
		harness.start();

		harness.sendState({status: 'update-failure', seconds: 10});
		harness.intervals.at(-1).callback();
		assert.equal(harness.statusText(), 'Update failed. Retrying in 9 sec…');

		harness.sendState({status: 'update-failure', seconds: 12});

		assert.equal(harness.intervals.length, 2);
		assert.equal(harness.intervals[0].cleared, true);
		assert.equal(harness.liveIntervals().length, 1);
		assert.equal(harness.statusText(), 'Update failed. Retrying in 12 sec…');
	});

	test('offers the retry action on blocked-update-required and sends retry-now on click', async () => {
		const {serializeSplashState, SplashAction} = await loadSplashWindow('preload-retry-action');
		const harness = createPreloadHarness();
		await harness.start();

		harness.sendState(serializeSplashState({status: 'blocked-update-required', action: SplashAction.RETRY}));

		assert.equal(harness.statusText(), 'Update required to continue');
		assert.equal(harness.query('.splash-action').textContent, 'Retry now');

		harness.click('.splash-action');

		assert.deepEqual(harness.channels(), ['desktop-splash:ready', 'desktop-splash:retry-now']);

		harness.sendState({status: 'launching'});

		assert.equal(harness.query('.splash-action'), null);
		assert.equal(harness.mount.replaceChildrenCalls, 1);
	});

	test('tells the main process when the network returns', async () => {
		const harness = createPreloadHarness();
		await harness.start();

		harness.networkOnline();

		assert.deepEqual(harness.channels(), ['desktop-splash:ready', 'desktop-splash:network-online']);
	});

	test('offers the download action on a required shell update and sends open-download with no token', async () => {
		const {serializeSplashState, SplashAction} = await loadSplashWindow('preload-download-action');
		const harness = createPreloadHarness();
		harness.start();

		harness.sendState(serializeSplashState({status: 'blocked-shell-update', action: SplashAction.DOWNLOAD}));

		assert.equal(harness.statusText(), 'A new version of Fluxer is required');
		assert.equal(harness.query('.splash-action').textContent, 'Download update');

		harness.click('.splash-action');

		assert.deepEqual(harness.sent.at(-1), {channel: 'desktop-splash:open-download', payload: undefined});
	});

	test('offers only a quit action on a package manager owned install', async () => {
		const {serializeSplashState, SplashAction} = await loadSplashWindow('preload-managed-action');
		const harness = createPreloadHarness();
		await harness.start();

		harness.sendState(serializeSplashState({status: 'blocked-shell-update-managed', action: SplashAction.QUIT}));

		assert.equal(harness.statusText(), 'Update Fluxer through your package manager');
		assert.equal(harness.query('.splash-action').textContent, 'Quit');

		harness.click('.splash-action');

		assert.deepEqual(harness.channels(), ['desktop-splash:ready', 'desktop-splash:quit']);
	});

	test('routes a click to the current action kind without rebuilding the button', async () => {
		const {serializeSplashState, SplashAction} = await loadSplashWindow('preload-action-swap');
		const harness = createPreloadHarness();
		await harness.start();

		harness.sendState(serializeSplashState({status: 'blocked-update-required', action: SplashAction.RETRY}));
		const button = harness.query('.splash-action');
		harness.sendState(serializeSplashState({status: 'blocked-shell-update', action: SplashAction.DOWNLOAD}));

		assert.equal(harness.query('.splash-action'), button);
		assert.equal(button.textContent, 'Download update');
		assert.equal(button.listeners.get('click').length, 1);

		harness.click('.splash-action');

		assert.deepEqual(harness.channels(), ['desktop-splash:ready', 'desktop-splash:open-download']);
	});

	test('renders no button for an action the preload cannot route', () => {
		const harness = createPreloadHarness();
		harness.start();

		for (const action of [
			{kind: 'launch-anyway', label: 'Launch anyway'},
			{kind: 'retry', label: '   '},
			{kind: 'retry'},
			'retry',
		]) {
			harness.sendState({status: 'blocked-shell-update', action});
			assert.equal(harness.query('.splash-action'), null, `${JSON.stringify(action)} must not render`);
		}
	});

	test('never hands a string to innerHTML, so no splash copy can ever be parsed as markup', () => {
		assert.doesNotMatch(PRELOAD_SOURCE, /innerHTML/);
	});
});

describe('splash preload manual update layout', () => {
	function manualState(patch = {}) {
		return {
			layout: 'manual-update',
			status: 'blocked-shell-update',
			current: null,
			total: null,
			progress: null,
			seconds: null,
			action: null,
			message: 'A new version of Fluxer is ready to install.',
			versionLabel: 'Version 2026.823.1 available',
			options: MANUAL_UPDATE_OPTIONS,
			...patch,
		};
	}

	test('renders the validated markup, every format and the opt-out', () => {
		const harness = createPreloadHarness();
		harness.start();

		harness.sendState(manualState());

		assert.equal(harness.mount.replaceChildrenCalls, 2);
		assert.equal(harness.query('.splash-inner-dl').children.length, 4);
		assert.equal(harness.query('.splash-mark-dl').querySelector('img').getAttribute('src'), './symbol.svg');
		assert.equal(harness.query('.dl-update-message').textContent, 'A new version of Fluxer is ready to install.');
		assert.equal(harness.query('.dl-version-message').textContent, 'Version 2026.823.1 available');
		assert.deepEqual(
			harness.queryAll('option').map((node) => [node.value, node.textContent]),
			[
				['deb', 'Debian (deb)'],
				['rpm', 'Fedora (rpm)'],
				['appimage', 'Linux (AppImage)'],
				['tar_gz', 'Linux (tar.gz)'],
				['nope', "I'll figure it out"],
			],
		);
		assert.equal(harness.query('#dl-select-input').value, 'deb');
		assert.equal(harness.query('#dl-button').textContent, 'Download');
		assert.equal(harness.query('#dl-button').tagName, 'button');
	});

	test('changing the select swaps only the button label', () => {
		const harness = createPreloadHarness();
		harness.start();
		harness.sendState(manualState());
		const button = harness.query('#dl-button');

		harness.changeSelect('nope');

		assert.equal(button.textContent, 'Okay');
		assert.equal(harness.mount.replaceChildrenCalls, 2);
		assert.equal(harness.query('#dl-button'), button);

		harness.changeSelect('rpm');

		assert.equal(button.textContent, 'Download');
		assert.equal(harness.mount.replaceChildrenCalls, 2);
	});

	test('clicking with a real format sends that format on the download channel', async () => {
		const harness = createPreloadHarness();
		await harness.start();
		harness.sendState(manualState());

		harness.click('#dl-button');
		harness.changeSelect('tar_gz');
		harness.click('#dl-button');

		assert.deepEqual(harness.sent.slice(1), [
			{channel: 'desktop-splash:open-download', payload: 'deb'},
			{channel: 'desktop-splash:open-download', payload: 'tar_gz'},
		]);
	});

	test('clicking the opt-out quits and opens nothing', async () => {
		const harness = createPreloadHarness();
		await harness.start();
		harness.sendState(manualState());

		harness.changeSelect('nope');
		harness.click('#dl-button');

		assert.deepEqual(harness.sent.slice(1), [{channel: 'desktop-splash:quit', payload: 'nope'}]);
	});

	test('a picker with nothing routable in it falls back to a quit control instead of a dead window', async () => {
		const harness = createPreloadHarness();
		await harness.start();

		harness.sendState(manualState({options: null}));

		assert.equal(harness.query('.dl-select-frame'), null);
		assert.equal(harness.query('#dl-button').textContent, 'Quit');
		assert.equal(harness.query('.dl-update-message').textContent, 'A new version of Fluxer is ready to install.');

		harness.click('#dl-button');

		assert.deepEqual(harness.channels(), ['desktop-splash:ready', 'desktop-splash:quit']);
	});

	test('a manual state that carries an action instead of a picker renders that action', () => {
		const harness = createPreloadHarness();
		harness.start();

		harness.sendState(manualState({options: null, action: {kind: 'download', label: 'Download update'}}));

		assert.equal(harness.query('.dl-select-frame'), null);
		assert.equal(harness.query('#dl-button').textContent, 'Download update');

		harness.click('#dl-button');

		assert.deepEqual(harness.sent.at(-1), {channel: 'desktop-splash:open-download', payload: undefined});
	});

	test('no manual update payload can normalise into a screen with nothing to click', () => {
		const {context} = createPreloadHarness();

		for (const patch of [
			{options: null, action: null},
			{options: [], action: {kind: 'launch-anyway', label: 'Launch anyway'}},
			{options: 'deb', action: {kind: 'retry', label: '  '}},
			{options: [{value: 'DEB', label: 'Debian', buttonLabel: 'Download', kind: 'download'}], action: undefined},
		]) {
			const state = context.normalizeState(manualState(patch));
			assert.notEqual(
				state.options ?? state.action,
				null,
				`${JSON.stringify(patch)} must still carry a control the preload can route`,
			);
		}
	});

	test('an unknown version leaves the bottom line empty rather than composing a string', () => {
		const harness = createPreloadHarness();
		harness.start();

		harness.sendState(manualState({versionLabel: null}));

		assert.equal(harness.query('.dl-version-message').textContent, '');
	});

	test('switching to the manual layout stops the countdown, so the terminal screen keeps no timer alive', () => {
		const harness = createPreloadHarness();
		harness.start();
		harness.sendState({status: 'update-failure', seconds: 5});

		assert.equal(harness.liveIntervals().length, 1);

		harness.sendState(manualState());

		assert.equal(harness.liveIntervals().length, 0);
		assert.equal(harness.intervals.length, 1);
		assert.equal(harness.mount.replaceChildrenCalls, 2);
		assert.equal(harness.query('#dl-button').textContent, 'Download');
	});

	test('a later state patches the terminal layout in place rather than rebuilding it', () => {
		const harness = createPreloadHarness();
		harness.start();
		harness.sendState(manualState());
		const button = harness.query('#dl-button');

		harness.sendState(manualState({versionLabel: 'Version 2026.900.0 available'}));

		assert.equal(harness.mount.replaceChildrenCalls, 2);
		assert.equal(harness.query('#dl-button'), button);
		assert.equal(harness.query('.dl-version-message').textContent, 'Version 2026.900.0 available');
	});
});

describe('splash preload contract', () => {
	test('renders the exact status text for every status the main process can send', async () => {
		const {SplashStatus} = await loadSplashWindow('preload-status-text');
		const {context} = createPreloadHarness();
		const textFor = (payload) => context.getStatusText(context.normalizeState(payload));

		assert.equal(textFor({status: SplashStatus.CHECKING_FOR_UPDATES}), 'Checking for updates…');
		assert.equal(
			textFor({status: SplashStatus.DOWNLOADING_UPDATES, current: 3, total: 7}),
			'Downloading update 3 of 7…',
		);
		assert.equal(textFor({status: SplashStatus.INSTALLING_UPDATES, current: 5, total: 7}), 'Installing update 5 of 7…');
		assert.equal(textFor({status: SplashStatus.VERIFYING}), 'Verifying files…');
		assert.equal(textFor({status: SplashStatus.UPDATE_FAILURE, seconds: 8}), 'Update failed. Retrying in 8 sec…');
		assert.equal(textFor({status: SplashStatus.SHELL_UPDATE_DOWNLOADING}), 'Updating Fluxer…');
		assert.equal(textFor({status: SplashStatus.SHELL_UPDATE_RESTARTING}), 'Restarting to finish the update…');
		assert.equal(textFor({status: SplashStatus.BLOCKED_UPDATE_REQUIRED}), 'Update required to continue');
		assert.equal(textFor({status: SplashStatus.BLOCKED_SHELL_UPDATE}), 'A new version of Fluxer is required');
		assert.equal(
			textFor({status: SplashStatus.BLOCKED_SHELL_UPDATE_MANAGED}),
			'Update Fluxer through your package manager',
		);
		assert.equal(
			textFor({status: SplashStatus.BLOCKED_UNSUPPORTED_BUILD}),
			'This installation is incomplete. Reinstall Fluxer',
		);
		assert.equal(textFor({status: SplashStatus.LAUNCHING}), 'Starting…');
		assert.equal(textFor({status: SplashStatus.UNREACHABLE_LAUNCH}), 'Starting offline…');
	});

	test('knows every status and layout the main process can send', async () => {
		const {SplashLayout, SplashStatus} = await loadSplashWindow('preload-status-parity');
		const {context} = createPreloadHarness();

		for (const status of Object.values(SplashStatus)) {
			assert.equal(context.normalizeState({status}).status, status, `${status} is unknown to the preload`);
		}
		for (const layout of Object.values(SplashLayout)) {
			assert.equal(context.normalizeState({layout}).layout, layout, `${layout} is unknown to the preload`);
		}
	});

	test('falls back to checking for updates on a status the preload does not know', () => {
		const {context} = createPreloadHarness();

		assert.equal(
			context.getStatusText(context.normalizeState({status: 'not-a-splash-status'})),
			'Checking for updates…',
		);
	});

	test('shows the bar only for a numeric progress on a status that carries one', () => {
		const {context} = createPreloadHarness();
		const shows = (payload) => context.shouldShowProgress(context.normalizeState(payload));

		assert.equal(shows({status: 'downloading-updates', progress: 0}), true);
		assert.equal(shows({status: 'installing-updates', progress: 100}), true);
		assert.equal(shows({status: 'shell-update-downloading', progress: 5}), true);
		assert.equal(shows({status: 'downloading-updates'}), false);
		assert.equal(shows({status: 'downloading-updates', progress: 'half'}), false);
		assert.equal(shows({status: 'shell-update-downloading'}), false);
		assert.equal(shows({status: 'shell-update-restarting', progress: 50}), false);
		assert.equal(shows({status: 'verifying', progress: 50}), false);
		assert.equal(shows({status: 'launching', progress: 50}), false);
	});

	test('normalises the picker the same way the main process serialises it', async () => {
		const {serializeSplashState} = await loadSplashWindow('preload-option-parity');
		const {context} = createPreloadHarness();
		const state = {status: 'blocked-shell-update', layout: 'manual-update', options: MANUAL_UPDATE_OPTIONS};

		assert.deepEqual(JSON.parse(JSON.stringify(context.normalizeState(serializeSplashState(state)))), {
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
			message: null,
			versionLabel: null,
			options: MANUAL_UPDATE_OPTIONS.map((option) => ({...option})),
		});
		assert.equal(
			context.normalizeState({options: [{value: 'DEB', label: 'x', buttonLabel: 'y', kind: 'download'}]}).options,
			null,
		);
		assert.equal(context.normalizeState({options: []}).options, null);
	});

	test('clamps the manual update message so a long one can never push the controls out of the window', () => {
		const message = styleRule('.dl-update-message');

		assert.match(message, /-webkit-line-clamp: \d+;/);
		assert.match(message, /overflow: hidden;/);
		assert.match(message, /min-height: 0;/);
		assert.match(styleRule('.splash-inner-dl'), /overflow: hidden;/);
	});

	test('keeps every manual control out of the shrink path and out of the drag region', () => {
		for (const selector of ['.dl-select-frame', '.dl-action-frame']) {
			assert.match(styleRule(selector), /flex: none;/, `${selector} must never shrink`);
			assert.match(styleRule(selector), /-webkit-app-region: no-drag;/, `${selector} must stay clickable`);
		}
		assert.match(styleRule('.dl-version-message'), /flex: none;/);
	});

	test('uses the same channel names as the main process', async () => {
		const module = await loadSplashWindow('preload-channels');

		for (const channel of [
			module.DESKTOP_SPLASH_STATE_CHANNEL,
			module.DESKTOP_SPLASH_READY_CHANNEL,
			module.DESKTOP_SPLASH_RETRY_NOW_CHANNEL,
			module.DESKTOP_SPLASH_QUIT_CHANNEL,
			module.DESKTOP_SPLASH_OPEN_DOWNLOAD_CHANNEL,
			module.DESKTOP_SPLASH_NETWORK_ONLINE_CHANNEL,
		]) {
			assert.ok(PRELOAD_SOURCE.includes(`'${channel}'`), `${channel} is missing from splash.cjs`);
		}
	});
});
