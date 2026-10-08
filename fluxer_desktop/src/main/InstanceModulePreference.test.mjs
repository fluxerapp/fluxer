// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {mkdirSync, mkdtempSync, readFileSync, writeFileSync} from 'node:fs';
import {rm} from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {after, describe, test} from 'node:test';
import {installElectronStub} from './LocalAppTestSupport.test.mjs';

installElectronStub();

const {
	INSTANCE_MODULE_PREFERENCE_FIELD,
	getInstanceModulePreferencePath,
	instanceTurnedModulesOff,
	recordInstanceModulePreference,
} = await import('./InstanceModulePreference.ts');
const {decideModuleSystemDisableRequest, resolveModuleSystemLaunch} = await import('@electron/common/ModuleSystem');
const {localAppRuntimePlanFromDiscovery} = await import('./LocalAppRuntimePlans.ts');
const {DEPLOYED_OFFICIAL_DOCUMENT} = await import(
	'../../../packages/instance_bootstrap/src/__tests__/DiscoveryFixtures.ts'
);

const temporaryRoots = [];

after(async () => {
	for (const root of temporaryRoots) {
		await rm(root, {recursive: true, force: true});
	}
});

function createUserData() {
	const root = mkdtempSync(path.join(os.tmpdir(), 'module-kill-switch-'));
	temporaryRoots.push(root);
	return root;
}

function writeRawSwitch(userData, body) {
	const target = getInstanceModulePreferencePath(userData);
	mkdirSync(path.dirname(target), {recursive: true});
	writeFileSync(target, body);
	return target;
}

function planWithFeature(value) {
	const features = {...DEPLOYED_OFFICIAL_DOCUMENT.features};
	if (value === undefined) {
		delete features.desktop_modules_enabled;
	} else {
		features.desktop_modules_enabled = value;
	}
	return localAppRuntimePlanFromDiscovery({...DEPLOYED_OFFICIAL_DOCUMENT, features});
}

describe('the instance desktop module system switch', () => {
	test('an install that never heard from an instance leaves the module system on', () => {
		assert.equal(instanceTurnedModulesOff(createUserData()), false);
	});

	test('an instance that turned modules off engages the switch', async () => {
		const userData = createUserData();
		await recordInstanceModulePreference(userData, false);
		assert.equal(instanceTurnedModulesOff(userData), true);
	});

	test('an instance that turned modules back on clears the switch', async () => {
		const userData = createUserData();
		await recordInstanceModulePreference(userData, false);
		await recordInstanceModulePreference(userData, true);
		assert.equal(instanceTurnedModulesOff(userData), false);
	});

	test('the switch is stored under the module store root, never in the app store database', async () => {
		const userData = createUserData();
		await recordInstanceModulePreference(userData, false);
		const stored = getInstanceModulePreferencePath(userData);
		assert.equal(stored, path.join(userData, 'modules', 'instance-switch.json'));
		assert.deepEqual(JSON.parse(readFileSync(stored, 'utf8')), {[INSTANCE_MODULE_PREFERENCE_FIELD]: false});
	});

	test('a corrupt, oversized or wrongly shaped file fails open rather than bricking the module system', () => {
		for (const body of ['', 'not json', '[]', 'null', '"false"', '{"desktop_modules_enabled":"false"}', '{}']) {
			const userData = createUserData();
			writeRawSwitch(userData, body);
			assert.equal(instanceTurnedModulesOff(userData), false, body);
		}
		const oversized = createUserData();
		writeRawSwitch(oversized, `{"desktop_modules_enabled":false,"pad":"${'x'.repeat(8192)}"}`);
		assert.equal(instanceTurnedModulesOff(oversized), false);
	});
});

describe('what the instance module preference can and cannot do to a boot', () => {
	test('it would turn the module system off on a build with an offline renderer, which no modules build has', () => {
		assert.deepEqual(decideModuleSystemDisableRequest(true, true), {kind: 'honoured'});
	});

	test('it cannot apply to a build with no offline renderer, the same rule the launch flag gets', () => {
		assert.deepEqual(decideModuleSystemDisableRequest(true, false), {kind: 'ignored-without-offline-renderer'});
	});

	test('it cannot turn the module system on, a build without the module system stays without it', () => {
		assert.equal(resolveModuleSystemLaunch({argv: [], env: {}, hasOfflineRenderer: true}).kind, 'disabled-by-build');
		assert.deepEqual(decideModuleSystemDisableRequest(false, true), {kind: 'not-requested'});
	});
});

describe('reading the module preference out of a discovery document', () => {
	test('an instance that says nothing leaves the preference untouched', () => {
		assert.equal(planWithFeature(undefined).desktopModulesEnabled, null);
	});

	test('an explicit false turns the module system off and an explicit true turns it back on', () => {
		assert.equal(planWithFeature(false).desktopModulesEnabled, false);
		assert.equal(planWithFeature(true).desktopModulesEnabled, true);
	});

	test('a non boolean is not a decision', () => {
		assert.equal(planWithFeature('false').desktopModulesEnabled, null);
		assert.equal(planWithFeature(0).desktopModulesEnabled, null);
	});
});
