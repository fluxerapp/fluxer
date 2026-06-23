// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import {promisify} from 'node:util';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const ts = require('typescript');

const sourcePath = fileURLToPath(new URL('./ActivityDetection.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = ts.transpileModule(source, {
	compilerOptions: {
		esModuleInterop: true,
		module: ts.ModuleKind.CommonJS,
		resolveJsonModule: true,
		target: ts.ScriptTarget.ES2022,
	},
}).outputText;

const detectablesCatalog = require('./fixtures/detectables.json');

function plain(value) {
	return JSON.parse(JSON.stringify(value));
}

function loadActivityDetection({platform = 'linux', execFile}) {
	const calls = [];
	const execFileStub = (file, args, options, callback) => {
		calls.push({file, args, options});
		execFile(file, args, options, callback);
	};
	execFileStub[promisify.custom] = (file, args, options) =>
		new Promise((resolve, reject) => {
			execFileStub(file, args, options, (error, stdout, stderr) => {
				if (error) {
					error.stdout = stdout;
					error.stderr = stderr;
					reject(error);
					return;
				}
				resolve({stdout, stderr});
			});
		});

	const module = {exports: {}};
	const context = vm.createContext({
		module,
		exports: module.exports,
		console,
		process: {platform},
		require: (specifier) => {
			if (specifier === 'node:child_process') {
				return {execFile: execFileStub};
			}
			if (specifier === './fixtures/detectables.json') {
				return detectablesCatalog;
			}
			return require(specifier);
		},
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return {...module.exports, calls};
}

describe('ActivityDetection', () => {
	test('detects POSIX runtime processes with required command-line arguments', async () => {
		const detector = loadActivityDetection({
			execFile: (_file, args, options, callback) => {
				callback(null, '  42 java net.minecraft.client.main.Main --username test\n  43 bash bash\n', '');
			},
		});

		const status = await detector.getActivityDetectionStatus();

		assert.deepEqual(plain(status), {
			detected: true,
			activities: [
				{
					id: 'minecraft',
					type: 'application',
					name: 'Minecraft',
					icon: 'minecraft.png',
					pid: 42,
					executable: 'java',
				},
			],
		});
		assert.deepEqual(plain(detector.calls.at(0).args), ['-axo', 'pid=,comm=,args=']);
		assert.equal(detector.calls.at(0).options.maxBuffer, 1024 * 1024);
	});

	test('deduplicates detected applications from Windows tasklist fallback', async () => {
		const detector = loadActivityDetection({
			platform: 'win32',
			execFile: (file, args, _options, callback) => {
				if (file === 'wmic') {
					callback(new Error('wmic unavailable'), '', '');
					return;
				}
				callback(
					null,
					[
						'"minecraft.windows.exe","100","Console","1","10,000 K"',
						'"osu!.exe","101","Console","1","10,000 K"',
						'"osu!.exe","102","Console","1","10,000 K"',
					].join('\r\n'),
					'',
				);
			},
		});

		const status = await detector.getActivityDetectionStatus();

		assert.deepEqual(
			plain(status.activities.map((activity) => ({id: activity.id, pid: activity.pid}))),
			[
				{id: 'minecraft', pid: 100},
				{id: 'osu', pid: 101},
			],
		);
		assert.equal(status.detected, true);
		assert.deepEqual(plain(detector.calls.at(-1).args), ['/fo', 'csv', '/nh']);
	});

	test('returns an empty status when process enumeration fails', async () => {
		const detector = loadActivityDetection({
			execFile: (_file, _args, _options, callback) => {
				callback(new Error('process list unavailable'), '', '');
			},
		});

		const status = await detector.getActivityDetectionStatus();

		assert.deepEqual(plain(status), {detected: false, activities: []});
	});
});
