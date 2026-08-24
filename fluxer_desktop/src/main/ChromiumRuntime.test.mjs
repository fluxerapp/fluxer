// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./ChromiumRuntime.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

const WGC_DISABLED_FEATURES = [
	'AllowWgcScreenCapturer',
	'AllowWgcWindowCapturer',
	'AllowWgcScreenZeroHz',
	'AllowWgcWindowZeroHz',
	'WebRtcWgcRequireBorder',
];

function loadChromiumRuntime(platform = 'win32') {
	const appendedSwitches = [];
	const app = {
		getVersion: () => '0.0.0-test',
		getGPUInfo: async () => ({}),
		commandLine: {
			appendSwitch(name, value) {
				appendedSwitches.push({name, value});
			},
			getSwitchValue: () => '',
		},
	};
	const log = {debug() {}, info() {}, warn() {}};

	function requireStub(specifier) {
		if (specifier === 'electron') return {app};
		if (specifier === 'electron-log') return log;
		return require(specifier);
	}

	const module = {exports: {}};
	const context = vm.createContext({
		require: requireStub,
		module,
		exports: module.exports,
		process: {
			...process,
			platform,
			execPath: '/tmp/fluxer-test',
			resourcesPath: '/tmp/fluxer-resources',
			versions: {...process.versions},
		},
		console,
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return {appendedSwitches, module: module.exports};
}

describe('ChromiumRuntime Windows capture policy', () => {
	test('adds all known WebRTC WGC capturer features to the Windows disable set', () => {
		const {module} = loadChromiumRuntime('win32');
		const features = new Set(['ExistingFeature']);

		module.addWindowsWebRtcWgcDisabledFeatures(features);

		for (const feature of WGC_DISABLED_FEATURES) {
			assert.equal(features.has(feature), true);
		}
		assert.equal(features.has('ExistingFeature'), true);
	});

	test('does not add WGC feature switches on non-Windows platforms', () => {
		const {module} = loadChromiumRuntime('linux');
		const features = new Set(['ExistingFeature']);

		module.addWindowsWebRtcWgcDisabledFeatures(features);

		assert.deepEqual([...features], ['ExistingFeature']);
	});
});

describe('ChromiumRuntime configured switch allowlist', () => {
	test('rejects settings-supplied switches that would disable or shrink the HTTP cache', () => {
		const {appendedSwitches, module} = loadChromiumRuntime('win32');

		module.appendConfiguredChromiumSwitches([
			'disable-http-cache',
			'disk-cache-size',
			'disk-cache-dir',
			'disable-gpu-shader-disk-cache',
			'disable_metal',
		]);

		assert.deepEqual(appendedSwitches, [{name: 'disable_metal', value: undefined}]);
	});
});
