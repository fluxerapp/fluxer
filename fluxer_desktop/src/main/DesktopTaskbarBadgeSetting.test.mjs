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
	const userDataPath = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-desktop-taskbar-badge-setting-test-'));
	fs.writeFileSync(path.join(userDataPath, 'settings.json'), JSON.stringify({window_behavior: windowBehavior}));
	loadDesktopConfig(userDataPath);
	return userDataPath;
}

function readPersisted(userDataPath) {
	return JSON.parse(fs.readFileSync(path.join(userDataPath, 'settings.json'), 'utf8')).window_behavior;
}

describe('dark taskbar badge setting', () => {
	test('defaults to the red badge for existing and new configs', () => {
		loadSettings(undefined);
		assert.equal(getDesktopWindowBehaviorSettings().darkTaskbarBadge, false);
		loadSettings({showTrayIcon: true, coloredTrayIcon: true});
		assert.equal(getDesktopWindowBehaviorSettings().darkTaskbarBadge, false);
	});

	test('drops values that are not booleans', () => {
		for (const value of ['true', 1, null, {}]) {
			loadSettings({darkTaskbarBadge: value});
			assert.equal(getDesktopWindowBehaviorSettings().darkTaskbarBadge, false, String(value));
		}
	});

	test('persists and reloads the choice without touching the tray settings', () => {
		const userDataPath = loadSettings({showTrayIcon: false, coloredTrayIcon: true});
		assert.equal(setDesktopWindowBehaviorSettings({darkTaskbarBadge: true}).darkTaskbarBadge, true);
		assert.equal(readPersisted(userDataPath).darkTaskbarBadge, true);
		loadDesktopConfig(userDataPath);
		const settings = getDesktopWindowBehaviorSettings();
		assert.equal(settings.darkTaskbarBadge, true);
		assert.equal(settings.showTrayIcon, false);
		assert.equal(settings.coloredTrayIcon, true);
	});
});
