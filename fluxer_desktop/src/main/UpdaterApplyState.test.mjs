// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import path from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./UpdaterApplyState.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

const USER_DATA = 'C:\\Users\\csh\\AppData\\Roaming\\fluxercanary';
const STATE_PATH = path.win32.join(USER_DATA, 'update-apply-state.json');

function loadApplyState(initialFiles = []) {
	const files = new Map(initialFiles);
	const warnings = [];
	const fakeFs = {
		readFileSync: (target) => {
			if (!files.has(target)) throw new Error(`ENOENT ${target}`);
			return files.get(target);
		},
		writeFileSync: (target, contents) => {
			files.set(target, contents);
		},
		rmSync: (target) => {
			files.delete(target);
		},
	};

	const module = {exports: {}};
	const context = vm.createContext({
		module,
		exports: module.exports,
		console,
		Date,
		JSON,
		require: (specifier) => {
			if (specifier === 'node:fs') {
				return {default: fakeFs, ...fakeFs};
			}
			if (specifier === 'node:path') {
				return {default: path.win32, ...path.win32};
			}
			if (specifier === 'electron') {
				return {app: {getPath: () => USER_DATA}};
			}
			if (specifier === 'electron-log') {
				const log = {warn: (...args) => warnings.push(args)};
				return {default: log, ...log};
			}
			return require(specifier);
		},
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return {...module.exports, files, warnings};
}

describe('update apply attempt state', () => {
	test('records an attempt that reads back with its version', () => {
		const harness = loadApplyState();

		harness.recordVelopackApplyAttempt('2026.920.144552');

		const attempt = harness.readVelopackApplyAttempt();
		assert.equal(attempt.version, '2026.920.144552');
		assert.equal(typeof attempt.attemptedAt, 'number');
		assert.equal(harness.files.has(STATE_PATH), true);
	});

	test('reads nothing when no attempt was ever recorded', () => {
		const harness = loadApplyState();

		assert.equal(harness.readVelopackApplyAttempt(), null);
		assert.deepEqual(harness.warnings, []);
	});

	test('clearing an attempt removes the state file', () => {
		const harness = loadApplyState();
		harness.recordVelopackApplyAttempt('2026.920.144552');

		harness.clearVelopackApplyAttempt();

		assert.equal(harness.files.has(STATE_PATH), false);
		assert.equal(harness.readVelopackApplyAttempt(), null);
	});

	test('treats a corrupt state file as no attempt', () => {
		const harness = loadApplyState([[STATE_PATH, '{not json']]);

		assert.equal(harness.readVelopackApplyAttempt(), null);
		assert.equal(harness.warnings.length, 1);
	});

	test('treats a state file without a version as no attempt', () => {
		const harness = loadApplyState([[STATE_PATH, JSON.stringify({attemptedAt: 1})]]);

		assert.equal(harness.readVelopackApplyAttempt(), null);
	});

	test('defaults a missing timestamp so an old state file still blocks the loop', () => {
		const harness = loadApplyState([[STATE_PATH, JSON.stringify({version: '2026.920.144552'})]]);

		const attempt = harness.readVelopackApplyAttempt();
		assert.equal(attempt.version, '2026.920.144552');
		assert.equal(attempt.attemptedAt, 0);
	});
});
