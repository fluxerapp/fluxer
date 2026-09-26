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

function loadPlatformInfo(platform = 'win32', {nativeGpu = null, electronGpu = null, gpuInfoRequests = []} = {}) {
	const app = {
		getVersion: () => '0.0.0-test',
		getGPUInfo: async (infoType) => {
			gpuInfoRequests.push(infoType);
			return {gpuDevice: electronGpu ?? []};
		},
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
					if (moduleSpecifier === '@fluxer/platform-info' && nativeGpu) {
						return {getGpuInfo: () => ({devices: nativeGpu, source: 'dxgi'}), loadError: null};
					}
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
		assert.equal(switches.includes('disable_accelerated_h264_encode'), true);
	});
});

describe('PlatformInfo dual-GPU merge', () => {
	const NVIDIA = {vendorId: 0x10de, deviceId: 0x2684, deviceString: 'NVIDIA GeForce RTX 4090 Laptop GPU'};
	const INTEL = {vendorId: 0x8086, deviceId: 0x9bc4, deviceString: 'Intel(R) UHD Graphics'};

	test('keeps the adapter Chromium reports active and drops the native probe active flag', async () => {
		const gpuInfoRequests = [];
		const module = loadPlatformInfo('win32', {
			gpuInfoRequests,
			nativeGpu: [
				{...NVIDIA, active: true, dedicatedVideoMemory: 16 * 1024 * 1024 * 1024, source: 'dxgi'},
				{...INTEL, active: false, dedicatedVideoMemory: 0, source: 'dxgi'},
			],
			electronGpu: [
				{...INTEL, active: true},
				{...NVIDIA, active: false},
			],
		});

		const info = await module.getGpuInfo();
		const byVendor = new Map(info.devices.map((device) => [device.vendorId, device]));

		assert.deepEqual(gpuInfoRequests, ['complete']);
		assert.equal(byVendor.get(INTEL.vendorId).active, true);
		assert.equal(byVendor.get(NVIDIA.vendorId).active, false);
		assert.equal(byVendor.get(NVIDIA.vendorId).dedicatedVideoMemory, 16 * 1024 * 1024 * 1024);
	});

	test('leaves a native adapter Chromium never lists inactive', async () => {
		const module = loadPlatformInfo('win32', {
			nativeGpu: [
				{...NVIDIA, active: true, dedicatedVideoMemory: 16 * 1024 * 1024 * 1024, source: 'dxgi'},
				{...INTEL, active: false, dedicatedVideoMemory: 0, source: 'dxgi'},
			],
			electronGpu: [{...INTEL, active: true}],
		});

		const info = await module.getGpuInfo();
		const byVendor = new Map(info.devices.map((device) => [device.vendorId, device]));

		assert.equal(byVendor.get(NVIDIA.vendorId).active, false);
		assert.equal(byVendor.get(INTEL.vendorId).active, true);
	});

	test('keeps the native details when Electron reports the dedicated adapter active', async () => {
		const module = loadPlatformInfo('win32', {
			nativeGpu: [
				{...NVIDIA, active: true, dedicatedVideoMemory: 16 * 1024 * 1024 * 1024, source: 'dxgi'},
				{...INTEL, active: false, dedicatedVideoMemory: 0, source: 'dxgi'},
			],
			electronGpu: [
				{...NVIDIA, active: true},
				{...INTEL, active: false},
			],
		});

		const info = await module.getGpuInfo();
		const byVendor = new Map(info.devices.map((device) => [device.vendorId, device]));

		assert.equal(byVendor.get(NVIDIA.vendorId).active, true);
		assert.equal(byVendor.get(INTEL.vendorId).active, false);
	});
});
