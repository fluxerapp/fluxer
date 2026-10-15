// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {describe, test} from 'node:test';
import {installElectronStub, installTestModuleStub} from './LocalAppTestSupport.test.mjs';
import {fakeNativeImageModule, pixel} from './TrayIconTestSupport.test.mjs';

const resourcesPath = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-desktop-tray-icon-test-'));
fs.mkdirSync(path.join(resourcesPath, 'icons'));
fs.writeFileSync(path.join(resourcesPath, 'icons', '16x16.png'), '');
Object.defineProperty(process, 'resourcesPath', {value: resourcesPath, configurable: true});
Object.defineProperty(process, 'platform', {value: 'win32', configurable: true});

const settings = {showTrayIcon: true, coloredTrayIcon: false};
globalThis.__fluxerDesktopTrayIconTestSettings__ = settings;

installTestModuleStub(
	'@electron/common/DesktopConfig',
	'export const getDesktopWindowBehaviorSettings = () => ({...globalThis.__fluxerDesktopTrayIconTestSettings__});',
);
installTestModuleStub(
	'@electron/main/DesktopUpdatePrompt',
	'export const checkForUpdatesFromShell = async () => {}; export const startDesktopUpdateFromShell = () => {};',
);
installTestModuleStub('@electron/main/LinuxLaunchPath', 'export const relaunchStableLaunchPath = () => {};');

const trays = [];

class FakeTray {
	constructor(image) {
		this.images = [image];
		this.toolTips = [];
		this.destroyed = false;
		trays.push(this);
	}

	get image() {
		return this.images.at(-1);
	}

	setImage(image) {
		this.images.push(image);
	}

	setToolTip(toolTip) {
		this.toolTips.push(toolTip);
	}

	setContextMenu() {}

	on() {}

	removeAllListeners() {}

	isDestroyed() {
		return this.destroyed;
	}

	destroy() {
		this.destroyed = true;
	}
}

const themeListeners = [];
const nativeTheme = {
	shouldUseDarkColorsForSystemIntegratedUI: true,
	on: (event, listener) => {
		if (event === 'updated') themeListeners.push(listener);
	},
};

installElectronStub({
	nativeImage: fakeNativeImageModule,
	nativeTheme,
	Tray: FakeTray,
	Menu: {buildFromTemplate: () => ({})},
	clipboard: {},
	app: {getPath: () => resourcesPath},
});

const {applyDesktopWindowBehaviorSettings, initializeDesktopTray, setDesktopTrayUnreadBadge} = await import(
	'./DesktopTray.ts'
);

const controller = {
	createWindow: () => null,
	getMainWindow: () => null,
	isMainWindowTakenOver: () => false,
	hideWindow: () => {},
	setQuitting: () => {},
	showWindow: () => {},
};

function topRight(image) {
	return pixel(image.toBitmap(), 16, 12, 3);
}

describe('desktop tray icon state', () => {
	test('follows the unread badge, theme and colored setting, and survives tray recreation', () => {
		initializeDesktopTray(controller);
		const [tray] = trays;
		const plainWhite = tray.image;
		assert.deepEqual(topRight(plainWhite), [0xff, 0xff, 0xff, 0xff], 'white glyph on a dark taskbar');
		assert.match(tray.toolTips.at(-1), /^Fluxer[^-]*$/);

		setDesktopTrayUnreadBadge(3);
		assert.equal(tray.images.length, 2);
		assert.deepEqual(topRight(tray.image), [0x00, 0x00, 0xdd, 0xff], 'red dot');
		assert.match(tray.toolTips.at(-1), / - Unread messages: 3$/);

		const dotted = tray.image;
		setDesktopTrayUnreadBadge(4);
		setDesktopTrayUnreadBadge(4);
		setDesktopTrayUnreadBadge(-1);
		assert.equal(tray.images.length, 2, 'the image is only set when the dot appears or disappears');
		assert.match(tray.toolTips.at(-1), / - Unread messages$/);

		setDesktopTrayUnreadBadge(0);
		assert.equal(tray.image, plainWhite, 'clearing reuses the cached plain image');
		setDesktopTrayUnreadBadge(2);
		assert.equal(tray.image, dotted, 'the dotted image is cached too');

		nativeTheme.shouldUseDarkColorsForSystemIntegratedUI = false;
		for (const listener of themeListeners) listener();
		assert.deepEqual(topRight(tray.image), [0x00, 0x00, 0xdd, 0xff]);
		assert.deepEqual(pixel(tray.image.toBitmap(), 16, 2, 8), [0x1c, 0x1c, 0x1c, 0xff], 'dark glyph on a light taskbar');

		settings.coloredTrayIcon = true;
		applyDesktopWindowBehaviorSettings();
		assert.deepEqual(pixel(tray.image.toBitmap(), 16, 2, 8), [0xd9, 0x41, 0x46, 0xff], 'colored icon');
		assert.deepEqual(topRight(tray.image), [0x00, 0x00, 0xdd, 0xff], 'the dot stays on the colored icon');

		settings.showTrayIcon = false;
		applyDesktopWindowBehaviorSettings();
		assert.equal(tray.destroyed, true);
		setDesktopTrayUnreadBadge(0);
		settings.showTrayIcon = true;
		applyDesktopWindowBehaviorSettings();
		assert.equal(trays.length, 2);
		assert.deepEqual(trays[1].images.length, 1);
		assert.deepEqual(pixel(trays[1].image.toBitmap(), 16, 2, 8), [0xd9, 0x41, 0x46, 0xff]);
		assert.notDeepEqual(topRight(trays[1].image), [0x00, 0x00, 0xdd, 0xff], 'a recreated tray shows the current state');
		setDesktopTrayUnreadBadge(1);
		assert.deepEqual(topRight(trays[1].image), [0x00, 0x00, 0xdd, 0xff]);
	});
});
