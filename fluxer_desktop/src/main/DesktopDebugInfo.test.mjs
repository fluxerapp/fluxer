// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./DesktopDebugInfo.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function loadDesktopDebugInfo(options = {}) {
	function requireStub(specifier) {
		if (specifier === 'electron') {
			return {
				app: {commandLine: {hasSwitch: () => false}, getPath: () => '', getLocale: () => 'en-US', isReady: () => true},
			};
		}
		if (specifier === 'electron-log') {
			return {info() {}, transports: {file: {getFile: () => null}}};
		}
		if (specifier === '@electron/common/DesktopConfig') {
			return {
				getAppUrl: () => '',
				getCustomAppUrl: () => null,
				getDesktopTroubleshootingSettings: () => options.troubleshooting ?? {disableHardwareAcceleration: false},
				getDesktopWindowBehaviorSettings: () => ({}),
			};
		}
		if (specifier === '@electron/common/UserDataPath') {
			return {isPortableMode: () => false};
		}
		if (specifier === '@electron/main/ChromiumRuntime') {
			return {hasEnabledBlinkFeature: () => false, MIDDLE_CLICK_AUTOSCROLL_BLINK_FEATURE: 'feature'};
		}
		if (specifier === '@electron/main/PlatformInfo') {
			return {getDesktopInfo: async () => ({})};
		}
		return require(specifier);
	}

	const module = {exports: {}};
	const context = vm.createContext({
		require: requireStub,
		module,
		exports: module.exports,
		process: {
			...process,
			env: {},
			argv: options.argv ?? [],
			platform: options.platform ?? process.platform,
		},
		console,
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});
	return module.exports;
}

describe('DesktopDebugInfo effective troubleshooting settings', () => {
	test('reports hardware acceleration disabled when a launch flag disabled it', () => {
		const {resolveEffectiveDesktopTroubleshootingSettings} = loadDesktopDebugInfo();

		for (const flag of ['--fluxer-disable-gpu', '--fluxer-disable-hardware-acceleration', '--fluxer-safe-mode']) {
			assert.equal(
				resolveEffectiveDesktopTroubleshootingSettings({disableHardwareAcceleration: false}, [flag], 'win32')
					.disableHardwareAcceleration,
				true,
				flag,
			);
			assert.equal(
				resolveEffectiveDesktopTroubleshootingSettings({disableHardwareAcceleration: false}, [flag], 'linux')
					.disableHardwareAcceleration,
				true,
				flag,
			);
		}
	});

	test('honours the persisted setting when no launch flag disabled it', () => {
		const {resolveEffectiveDesktopTroubleshootingSettings} = loadDesktopDebugInfo();

		assert.equal(
			resolveEffectiveDesktopTroubleshootingSettings({disableHardwareAcceleration: true}, [], 'win32')
				.disableHardwareAcceleration,
			true,
		);
		assert.equal(
			resolveEffectiveDesktopTroubleshootingSettings(
				{disableHardwareAcceleration: false},
				['--fluxer-devtools'],
				'linux',
			).disableHardwareAcceleration,
			false,
		);
	});

	test('reports hardware acceleration enabled on macOS', () => {
		const {resolveEffectiveDesktopTroubleshootingSettings} = loadDesktopDebugInfo();

		assert.equal(
			resolveEffectiveDesktopTroubleshootingSettings(
				{disableHardwareAcceleration: true},
				['--fluxer-safe-mode'],
				'darwin',
			).disableHardwareAcceleration,
			false,
		);
	});

	test('keeps the launch answer when the persisted setting changes without a restart', () => {
		const troubleshooting = {disableHardwareAcceleration: true};
		const {getLaunchDesktopTroubleshootingSettings} = loadDesktopDebugInfo({
			troubleshooting,
			argv: [],
			platform: 'win32',
		});

		assert.equal(getLaunchDesktopTroubleshootingSettings().disableHardwareAcceleration, true);
		troubleshooting.disableHardwareAcceleration = false;
		assert.equal(getLaunchDesktopTroubleshootingSettings().disableHardwareAcceleration, true);
	});

	test('answers the launch flag when nothing is persisted', () => {
		const {getLaunchDesktopTroubleshootingSettings} = loadDesktopDebugInfo({
			argv: ['--fluxer-disable-hardware-acceleration'],
			platform: 'linux',
		});

		assert.equal(getLaunchDesktopTroubleshootingSettings().disableHardwareAcceleration, true);
	});
});
