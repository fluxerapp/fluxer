// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {afterEach, describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

let wasOpenedAtLogin = false;
installElectronStub({app: {getLoginItemSettings: () => ({wasOpenedAtLogin})}});

const {loadDesktopConfig} = await import('@electron/common/DesktopConfig');
const {AUTOSTART_LAUNCH_ARG, isStartMinimizedLaunch} = await import('@electron/main/AutostartLaunch');

const originalArgv = process.argv;
const originalPlatform = Object.getOwnPropertyDescriptor(process, 'platform');
const userDataPaths = [];

function loadSettings(settings) {
	const userDataPath = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-autostart-launch-test-'));
	userDataPaths.push(userDataPath);
	fs.writeFileSync(path.join(userDataPath, 'settings.json'), JSON.stringify(settings), 'utf8');
	loadDesktopConfig(userDataPath);
}

function launch({argv = [], platform = 'linux', openedAtLogin = false} = {}) {
	process.argv = [originalArgv[0], originalArgv[1], ...argv];
	Object.defineProperty(process, 'platform', {...originalPlatform, value: platform});
	wasOpenedAtLogin = openedAtLogin;
}

afterEach(() => {
	process.argv = originalArgv;
	Object.defineProperty(process, 'platform', originalPlatform);
	wasOpenedAtLogin = false;
	for (const userDataPath of userDataPaths.splice(0)) {
		fs.rmSync(userDataPath, {recursive: true, force: true});
	}
});

describe('start minimized launch', () => {
	test('an autostart launch with start minimized enabled starts minimized', () => {
		loadSettings({window_behavior: {startMinimized: true, showTrayIcon: true}});
		launch({argv: [AUTOSTART_LAUNCH_ARG]});
		assert.equal(isStartMinimizedLaunch(), true);
	});

	test('a manual launch never starts minimized even with the setting on', () => {
		loadSettings({window_behavior: {startMinimized: true, showTrayIcon: true}});
		launch();
		assert.equal(isStartMinimizedLaunch(), false);
	});

	test('an autostart launch with the setting off shows the window', () => {
		loadSettings({window_behavior: {startMinimized: false, showTrayIcon: true}});
		launch({argv: [AUTOSTART_LAUNCH_ARG]});
		assert.equal(isStartMinimizedLaunch(), false);
	});

	test('start minimized is dropped when there is no tray icon to come back from', () => {
		loadSettings({window_behavior: {startMinimized: true, showTrayIcon: false}});
		launch({argv: [AUTOSTART_LAUNCH_ARG]});
		assert.equal(isStartMinimizedLaunch(), false);
	});

	test('a macOS login item launch counts as autostart without the argument', () => {
		loadSettings({window_behavior: {startMinimized: true, showTrayIcon: true}});
		launch({platform: 'darwin', openedAtLogin: true});
		assert.equal(isStartMinimizedLaunch(), true);
		launch({platform: 'linux', openedAtLogin: true});
		assert.equal(isStartMinimizedLaunch(), false);
	});
});
