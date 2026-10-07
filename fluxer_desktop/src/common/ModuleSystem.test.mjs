// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {mkdirSync, mkdtempSync, rmSync, writeFileSync} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {after, describe, test} from 'node:test';
import {pathToFileURL} from 'node:url';

const {
	decideModuleSystemDisableRequest,
	getOfflineRendererRoot,
	hasOfflineRenderer,
	readModuleSystemDisableRequest,
	resolveModuleSystemLaunch,
	MODULE_SYSTEM_BUILD_ENABLED,
} = await import('./ModuleSystem.ts');

function launch(module, argv, env, hasOfflineRenderer) {
	return module.resolveModuleSystemLaunch({argv, env, hasOfflineRenderer});
}

async function importWithBuildDefine(value, cacheKey) {
	const previous = process.env.FLUXER_MODULES;
	process.env.FLUXER_MODULES = value;
	try {
		return await import(`./ModuleSystem.ts?${cacheKey}`);
	} finally {
		if (previous === undefined) {
			delete process.env.FLUXER_MODULES;
		} else {
			process.env.FLUXER_MODULES = previous;
		}
	}
}

const modulesBuild = await importWithBuildDefine('1', 'build=on');

describe('the launch decision on a build with the module system flag off', () => {
	test('the build define defaults off', () => {
		assert.equal(MODULE_SYSTEM_BUILD_ENABLED, false);
	});

	test('a flag-off build stays off with no runtime flag', () => {
		assert.equal(readModuleSystemDisableRequest([], {}), false);
		assert.deepEqual(resolveModuleSystemLaunch({argv: [], env: {}, hasOfflineRenderer: true}), {
			kind: 'disabled-by-build',
		});
	});

	test('nothing at launch time can turn the module system on, only the build flag does', () => {
		for (const value of ['1', 'true', 'YES', ' on ']) {
			assert.equal(
				resolveModuleSystemLaunch({argv: [], env: {FLUXER_MODULE_SYSTEM: value}, hasOfflineRenderer: true}).kind,
				'disabled-by-build',
				value,
			);
		}
	});

	test('a flag-off build with no offline renderer is left alone, the disable override was never in play', () => {
		assert.deepEqual(
			resolveModuleSystemLaunch({argv: ['--fluxer-no-module-system'], env: {}, hasOfflineRenderer: false}),
			{kind: 'disabled-by-build'},
		);
	});

	test('an unrecognised environment value is not a disable request', () => {
		assert.equal(readModuleSystemDisableRequest([], {FLUXER_MODULE_SYSTEM: 'maybe'}), false);
		assert.equal(
			resolveModuleSystemLaunch({argv: [], env: {FLUXER_MODULE_SYSTEM: 'maybe'}, hasOfflineRenderer: true}).kind,
			'disabled-by-build',
		);
	});

	test('an empty environment value is not a disable request', () => {
		assert.equal(readModuleSystemDisableRequest([], {FLUXER_MODULE_SYSTEM: '   '}), false);
	});
});

describe('decideModuleSystemDisableRequest, a pure decision no shipped build can reach', () => {
	test('is honoured when an offline renderer can take over', () => {
		assert.deepEqual(decideModuleSystemDisableRequest(true, true), {kind: 'honoured'});
	});

	test('is ignored when there is no offline renderer to take over', () => {
		assert.deepEqual(decideModuleSystemDisableRequest(true, false), {kind: 'ignored-without-offline-renderer'});
	});

	test('that was never made is neither honoured nor ignored', () => {
		assert.deepEqual(decideModuleSystemDisableRequest(false, false), {kind: 'not-requested'});
		assert.deepEqual(decideModuleSystemDisableRequest(false, true), {kind: 'not-requested'});
	});
});

describe('the launch decision on a modules build, given an offline renderer no shipped modules build has', () => {
	test('the build define carries the module system on its own', () => {
		assert.equal(modulesBuild.MODULE_SYSTEM_BUILD_ENABLED, true);
		assert.equal(launch(modulesBuild, [], {}, true).kind, 'enabled');
	});

	test('the disable argument would turn the module system off', () => {
		assert.equal(launch(modulesBuild, ['fluxer', '--fluxer-no-module-system'], {}, true).kind, 'disabled-by-request');
	});

	test('the environment variable accepts the documented falsy spellings', () => {
		for (const value of ['0', 'false', 'NO', ' off ']) {
			assert.equal(launch(modulesBuild, [], {FLUXER_MODULE_SYSTEM: value}, true).kind, 'disabled-by-request', value);
		}
		for (const value of ['1', 'true', 'YES', ' on ']) {
			assert.equal(launch(modulesBuild, [], {FLUXER_MODULE_SYSTEM: value}, true).kind, 'enabled', value);
		}
	});
});

describe('the disable override on a modules build, the only shape that ships', () => {
	test('the disable argument is ignored instead of bricking the install', () => {
		assert.deepEqual(launch(modulesBuild, ['fluxer', '--fluxer-no-module-system'], {}, false), {
			kind: 'enabled-with-ignored-disable-request',
		});
	});

	test('the disable environment variable is ignored the same way', () => {
		for (const value of ['0', 'false', 'NO', ' off ']) {
			assert.deepEqual(
				launch(modulesBuild, [], {FLUXER_MODULE_SYSTEM: value}, false),
				{kind: 'enabled-with-ignored-disable-request'},
				value,
			);
		}
	});

	test('a boot without any disable override reports nothing ignored', () => {
		assert.deepEqual(launch(modulesBuild, [], {}, false), {kind: 'enabled'});
	});
});

const distRoots = [];

after(() => {
	for (const root of distRoots) {
		rmSync(root, {recursive: true, force: true});
	}
});

function createDistTree() {
	const root = mkdtempSync(path.join(os.tmpdir(), 'module-system-dist-'));
	distRoots.push(root);
	mkdirSync(path.join(root, 'main'), {recursive: true});
	return root;
}

function mainModuleUrl(root) {
	return pathToFileURL(path.join(root, 'main', 'index.js')).href;
}

describe('the offline renderer probe, which only an offline build can satisfy', () => {
	test('resolves dist/renderer as a sibling of the main bundle, the way the preloads resolve', () => {
		const root = createDistTree();
		assert.equal(getOfflineRendererRoot(mainModuleUrl(root)), path.join(root, 'main', '..', 'renderer'));
	});

	test('an asar with no renderer directory has no offline renderer', () => {
		assert.equal(hasOfflineRenderer(mainModuleUrl(createDistTree())), false);
	});

	test('an empty renderer directory has no offline renderer', () => {
		const root = createDistTree();
		mkdirSync(path.join(root, 'renderer', 'assets'), {recursive: true});
		assert.equal(hasOfflineRenderer(mainModuleUrl(root)), false);
	});

	test('a renderer directory carrying index.html is an offline renderer', () => {
		const root = createDistTree();
		mkdirSync(path.join(root, 'renderer'), {recursive: true});
		writeFileSync(path.join(root, 'renderer', 'index.html'), '<html lang="en"></html>');
		assert.equal(hasOfflineRenderer(mainModuleUrl(root)), true);
	});

	test('a directory named index.html is not an offline renderer', () => {
		const root = createDistTree();
		mkdirSync(path.join(root, 'renderer', 'index.html'), {recursive: true});
		assert.equal(hasOfflineRenderer(mainModuleUrl(root)), false);
	});
});
