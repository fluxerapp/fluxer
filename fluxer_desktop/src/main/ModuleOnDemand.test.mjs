// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {describe, test} from 'node:test';
import './LocalAppTestSupport.test.mjs';

const {setOnDemandModuleInstaller} = await import('@electron/main/ModuleBootHandoff');
const {createOnDemandModuleInstaller, ensureDesktopModule, UnknownDesktopModuleRequestError} = await import(
	'@electron/main/ModuleOnDemand'
);

function deferred() {
	let resolve;
	let reject;
	const promise = new Promise((resolvePromise, rejectPromise) => {
		resolve = resolvePromise;
		reject = rejectPromise;
	});
	return {promise, resolve, reject};
}

describe('createOnDemandModuleInstaller', () => {
	test('coalesces concurrent requests for the same module into one install', async () => {
		const gate = deferred();
		const requested = [];
		const refreshed = [];
		const install = createOnDemandModuleInstaller({
			ensure: async (moduleName) => {
				requested.push(moduleName);
				await gate.promise;
				return {module: moduleName, status: 'installed'};
			},
			refresh: async () => {
				refreshed.push(Date.now());
			},
		});

		const first = install('fluxer_grammars');
		const second = install('fluxer_grammars');
		const other = install('fluxer_fonts_sc');
		assert.equal(first, second);
		assert.notEqual(first, other);
		gate.resolve();

		assert.deepEqual(await first, {module: 'fluxer_grammars', status: 'installed'});
		assert.deepEqual(await second, {module: 'fluxer_grammars', status: 'installed'});
		await other;
		assert.deepEqual(requested, ['fluxer_grammars', 'fluxer_fonts_sc']);
		assert.equal(refreshed.length, 2);
	});

	test('a request that arrives after the install settles starts a new install', async () => {
		let calls = 0;
		const install = createOnDemandModuleInstaller({
			ensure: async (moduleName) => {
				calls += 1;
				return {module: moduleName, status: 'installed'};
			},
			refresh: async () => {},
		});

		await install('fluxer_grammars');
		await install('fluxer_grammars');

		assert.equal(calls, 2);
	});

	test('an already committed module never touches the served file index', async () => {
		let refreshes = 0;
		const install = createOnDemandModuleInstaller({
			ensure: async (moduleName) => ({module: moduleName, status: 'already-installed'}),
			refresh: async () => {
				refreshes += 1;
			},
		});

		assert.deepEqual(await install('fluxer_grammars'), {module: 'fluxer_grammars', status: 'already-installed'});
		assert.equal(refreshes, 0);
	});

	test('a failed install reports the module as unavailable instead of rejecting', async () => {
		const failures = [];
		const install = createOnDemandModuleInstaller({
			ensure: async () => {
				throw new Error('the manifest could not be reached');
			},
			refresh: async () => {},
			onFailure: (moduleName, error) => {
				failures.push([moduleName, error.message]);
			},
		});

		assert.deepEqual(await install('fluxer_grammars'), {module: 'fluxer_grammars', status: 'unavailable'});
		assert.deepEqual(failures, [['fluxer_grammars', 'the manifest could not be reached']]);
	});

	test('a module that installs but cannot be indexed is reported as unavailable', async () => {
		const install = createOnDemandModuleInstaller({
			ensure: async (moduleName) => ({module: moduleName, status: 'installed'}),
			refresh: async () => {
				throw new Error('the module manifest is missing');
			},
		});

		assert.deepEqual(await install('fluxer_grammars'), {module: 'fluxer_grammars', status: 'unavailable'});
	});
});

describe('ensureDesktopModule', () => {
	test('reports the module system as disabled when no installer is registered', async () => {
		setOnDemandModuleInstaller(null);
		assert.deepEqual(await ensureDesktopModule('fluxer_grammars'), {
			module: 'fluxer_grammars',
			status: 'disabled',
		});
	});

	test('rejects a module name the store could never hold', async () => {
		setOnDemandModuleInstaller(null);
		for (const candidate of ['', '../escape', 'Fluxer_Grammars', 'fluxer grammars', 42, null, 'a'.repeat(65)]) {
			await assert.rejects(() => ensureDesktopModule(candidate), UnknownDesktopModuleRequestError);
		}
	});

	test('hands a valid request to the registered installer', async () => {
		const seen = [];
		setOnDemandModuleInstaller(async (moduleName) => {
			seen.push(moduleName);
			return {module: moduleName, status: 'installed'};
		});

		assert.deepEqual(await ensureDesktopModule('fluxer_grammars'), {
			module: 'fluxer_grammars',
			status: 'installed',
		});
		assert.deepEqual(seen, ['fluxer_grammars']);
		setOnDemandModuleInstaller(null);
	});
});
