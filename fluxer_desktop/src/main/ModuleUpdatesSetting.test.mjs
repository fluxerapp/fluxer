// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {getDesktopTroubleshootingSettings, loadDesktopConfig, setDesktopTroubleshootingSettings} = await import(
	'../common/DesktopConfig.ts'
);
const {shouldDisableModuleUpdatesForLaunch} = await import('./LaunchOptions.ts');
const {isUpdateOnDemandLaunch, resolveEffectiveDesktopTroubleshootingSettings} = await import('./DesktopDebugInfo.ts');

function loadSettings(troubleshooting) {
	const userDataPath = fs.mkdtempSync(path.join(os.tmpdir(), 'fluxer-desktop-module-updates-setting-test-'));
	fs.writeFileSync(path.join(userDataPath, 'settings.json'), JSON.stringify({troubleshooting}));
	loadDesktopConfig(userDataPath);
	return userDataPath;
}

function readPersisted(userDataPath) {
	return JSON.parse(fs.readFileSync(path.join(userDataPath, 'settings.json'), 'utf8')).troubleshooting;
}

function withModuleUpdatesEnv(value, run) {
	const previous = process.env.FLUXER_DISABLE_MODULE_UPDATES;
	try {
		if (value === undefined) {
			delete process.env.FLUXER_DISABLE_MODULE_UPDATES;
		} else {
			process.env.FLUXER_DISABLE_MODULE_UPDATES = value;
		}
		run();
	} finally {
		if (previous === undefined) {
			delete process.env.FLUXER_DISABLE_MODULE_UPDATES;
		} else {
			process.env.FLUXER_DISABLE_MODULE_UPDATES = previous;
		}
	}
}

describe('disabling module updates', () => {
	test('reads the environment variable like the other FLUXER_DISABLE switches', () => {
		withModuleUpdatesEnv(undefined, () => assert.equal(shouldDisableModuleUpdatesForLaunch([]), false));
		for (const value of ['1', 'true', 'YES', 'on']) {
			withModuleUpdatesEnv(value, () => assert.equal(shouldDisableModuleUpdatesForLaunch([]), true, value));
		}
		withModuleUpdatesEnv('0', () => assert.equal(shouldDisableModuleUpdatesForLaunch([]), false));
	});

	test('honours the launch flag with or without the fluxer prefix', () => {
		withModuleUpdatesEnv(undefined, () => {
			for (const flag of [
				'--disable-module-updates',
				'--fluxer-disable-module-updates',
				'--disable-module-updates=1',
			]) {
				assert.equal(shouldDisableModuleUpdatesForLaunch([flag]), true, flag);
			}
			assert.equal(shouldDisableModuleUpdatesForLaunch(['--fluxer-update']), false);
		});
	});

	test('defaults to updating and drops values that are not booleans', () => {
		loadSettings(undefined);
		assert.equal(getDesktopTroubleshootingSettings().disableModuleUpdates, false);
		for (const value of ['true', 1, null, {}]) {
			loadSettings({disableModuleUpdates: value});
			assert.equal(getDesktopTroubleshootingSettings().disableModuleUpdates, false, String(value));
		}
	});

	test('the launch flag or the persisted setting disables updates for the launch', () => {
		withModuleUpdatesEnv(undefined, () => {
			const settings = {disableHardwareAcceleration: false, disableModuleUpdates: false};
			assert.equal(resolveEffectiveDesktopTroubleshootingSettings(settings, [], 'win32').disableModuleUpdates, false);
			assert.equal(
				resolveEffectiveDesktopTroubleshootingSettings(settings, ['--disable-module-updates'], 'darwin')
					.disableModuleUpdates,
				true,
			);
			assert.equal(
				resolveEffectiveDesktopTroubleshootingSettings({...settings, disableModuleUpdates: true}, [], 'linux')
					.disableModuleUpdates,
				true,
			);
		});
	});

	test('turning the setting back off restores updating without touching hardware acceleration', () => {
		const userDataPath = loadSettings({disableHardwareAcceleration: true, disableModuleUpdates: true});
		assert.equal(getDesktopTroubleshootingSettings().disableModuleUpdates, true);
		setDesktopTroubleshootingSettings({disableModuleUpdates: false});
		assert.deepEqual(readPersisted(userDataPath), {disableHardwareAcceleration: true, disableModuleUpdates: false});
		loadDesktopConfig(userDataPath);
		withModuleUpdatesEnv(undefined, () => {
			assert.equal(
				resolveEffectiveDesktopTroubleshootingSettings(getDesktopTroubleshootingSettings(), [], 'win32')
					.disableModuleUpdates,
				false,
			);
		});
	});

	test('keeps the launch answer when the persisted setting changes without a restart', () => {
		withModuleUpdatesEnv(undefined, () => {
			loadSettings({disableModuleUpdates: true});
			assert.equal(isUpdateOnDemandLaunch(), true);
			setDesktopTroubleshootingSettings({disableModuleUpdates: false});
			assert.equal(isUpdateOnDemandLaunch(), true);
		});
	});

	test('the persisted setting moves updates on demand and leaves the in-app update check on', () => {
		withModuleUpdatesEnv(undefined, () => {
			loadSettings({disableModuleUpdates: true});
			assert.equal(isUpdateOnDemandLaunch(), true);
			assert.equal(shouldDisableModuleUpdatesForLaunch([]), false);
		});
	});

	test('the environment variable turns updates off entirely, even with the setting on', () => {
		withModuleUpdatesEnv('1', () => {
			loadSettings({disableModuleUpdates: true});
			assert.equal(isUpdateOnDemandLaunch(), false);
			assert.equal(shouldDisableModuleUpdatesForLaunch([]), true);
		});
	});
});
