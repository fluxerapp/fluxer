// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./PlatformInfo.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function loadPlatformInfo(platform = 'win32') {
	const app = {
		getVersion: () => '0.0.0-test',
		commandLine: {
			getSwitchValue: () => '',
			hasSwitch: () => true,
		},
	};
	const osModule = {
		arch: () => 'x64',
		release: () => '10.0.26100',
	};

	function requireStub(specifier) {
		if (specifier === 'node:module') {
			return {
				createRequire: () => (moduleSpecifier) => {
					throw new Error(`Unexpected native module require: ${moduleSpecifier}`);
				},
			};
		}
		if (specifier === 'node:os') return osModule;
		if (specifier === 'electron') return {app};
		if (specifier === '@electron/common/BuildChannel') return {BUILD_CHANNEL: 'stable'};
		if (specifier === '@electron/common/UserDataPath') return {isPortableMode: () => false};
		if (specifier === '@electron/main/LinuxSandbox') {
			return {getFlatpakAppId: () => null, isFlatpakRuntime: () => false};
		}
		throw new Error(`Unexpected import: ${specifier}`);
	}

	const module = {exports: {}};
	const context = vm.createContext({
		require: requireStub,
		module,
		exports: module.exports,
		process: {
			...process,
			platform,
			env: {},
			versions: {...process.versions},
			getSystemVersion: () => '10.0.26100',
		},
		console,
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return module.exports;
}

describe('PlatformInfo Chromium runtime diagnostics', () => {
	test('never reports Media Foundation H.264 switches that Chromium no longer has', async () => {
		const module = loadPlatformInfo('win32');

		const info = await module.getDesktopInfo({nativeProbes: false});

		const switches = [...info.chromiumRuntime.switches];

		assert.deepEqual(
			switches.filter((name) => name.startsWith('enable-h264-mf')),
			[],
		);
		assert.equal(switches.includes('enable-libopenh264'), true);
	});
});
