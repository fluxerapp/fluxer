// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {getDesktopWindowBehaviorSettings, loadDesktopConfig, setDesktopWindowBehaviorSettings} = await import(
	'../common/DesktopConfig.ts'
);

function loadSettings(windowBehavior) {
	const userDataPath = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-desktop-tray-setting-test-'));
	fs.writeFileSync(path.join(userDataPath, 'settings.json'), JSON.stringify({window_behavior: windowBehavior}));
	loadDesktopConfig(userDataPath);
	return userDataPath;
}

function readPersisted(userDataPath) {
	return JSON.parse(fs.readFileSync(path.join(userDataPath, 'settings.json'), 'utf8')).window_behavior;
}

describe('colored tray icon setting', () => {
	test('defaults to the monochrome icon for existing and new configs', () => {
		loadSettings(undefined);
		assert.equal(getDesktopWindowBehaviorSettings().coloredTrayIcon, false);
		loadSettings({showTrayIcon: true});
		assert.equal(getDesktopWindowBehaviorSettings().coloredTrayIcon, false);
	});

	test('drops values that are not booleans', () => {
		for (const value of ['true', 1, null, {}]) {
			loadSettings({coloredTrayIcon: value});
			assert.equal(getDesktopWindowBehaviorSettings().coloredTrayIcon, false, String(value));
		}
	});

	test('persists and reloads the choice without touching the tray visibility', () => {
		const userDataPath = loadSettings({showTrayIcon: false});
		assert.equal(setDesktopWindowBehaviorSettings({coloredTrayIcon: true}).coloredTrayIcon, true);
		assert.equal(readPersisted(userDataPath).coloredTrayIcon, true);
		loadDesktopConfig(userDataPath);
		const settings = getDesktopWindowBehaviorSettings();
		assert.equal(settings.coloredTrayIcon, true);
		assert.equal(settings.showTrayIcon, false);
	});
});
