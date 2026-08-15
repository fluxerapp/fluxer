// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {EventEmitter} from 'node:events';
import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourcePath = fileURLToPath(new URL('./NativeScreenCapture.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function plain(value) {
	return structuredClone(value);
}

function makeSender() {
	const sent = [];
	const sender = new EventEmitter();
	sender.id = 100;
	sender.isDestroyed = () => false;
	sender.send = (...args) => sent.push(args);
	return {sender, sent};
}

function makeNativeAddon({
	sources = [],
	startResult = {width: 1280, height: 720, frameRate: 30, pixelFormat: 'nv12'},
	macAvailability = {sck: {supported: true, macosVersion: '14.6'}, screenPermission: 'authorized'},
	linuxAvailability = {
		available: true,
		backend: 'linux-pipewire-portal',
		detail: 'portal:5',
		capabilities: {process: true, system: true},
	},
} = {}) {
	const captures = [];
	class FakeCapture extends EventEmitter {
		constructor(options) {
			super();
			this.options = options;
			this.stopCount = 0;
			captures.push(this);
		}

		async start() {
			return startResult;
		}

		async stop() {
			this.stopCount += 1;
			this.emit('closed');
		}

		getDiagnostics() {
			return {
				addonDiagnostic: 'fake',
				width: startResult.width,
				height: startResult.height,
			};
		}
	}

	return {
		addon: {
			listSources: async () => sources,
			getBackendAvailability: async () => macAvailability,
			getAvailability: async () => linuxAvailability,
			getBackendInfo: () => ({
				backend: 'fake-screen-capture',
				supported: true,
			}),
			ScreenCapture: FakeCapture,
		},
		captures,
	};
}

function loadNativeScreenCapture({
	platform = 'linux',
	addon,
	tccStatus = 'not-determined',
	frameSinkHandle = null,
} = {}) {
	const handlers = new Map();
	const calls = {
		logs: {debug: [], warn: []},
		nativeModuleImports: [],
	};
	let uuidCounter = 0;

	function requireStub(specifier) {
		if (specifier === 'node:child_process') {
			return {
				execFile: (_file, _args, _options, callback) => {
					callback(null, '    HwSchMode    REG_DWORD    0x2\n', '');
				},
			};
		}
		if (specifier === 'node:crypto') {
			return {randomUUID: () => `capture-${++uuidCounter}`};
		}
		if (specifier === 'node:module') {
			return {
				createRequire: () => (moduleSpecifier) => {
					calls.nativeModuleImports.push(moduleSpecifier);
					if (
						moduleSpecifier === '@fluxer/linux-screen-capture' ||
						moduleSpecifier === '@fluxer/mac-screen-capture' ||
						moduleSpecifier === '@fluxer/win-game-capture'
					) {
						if (!addon) throw new Error(`No fake addon configured for ${moduleSpecifier}`);
						return addon;
					}
					throw new Error(`Unexpected createRequire import: ${moduleSpecifier}`);
				},
			};
		}
		if (specifier === '@electron/common/Logger') {
			return {
				createChildLogger: () => ({
					debug: (...args) => calls.logs.debug.push(args),
					warn: (...args) => calls.logs.warn.push(args),
				}),
			};
		}
		if (specifier === 'electron') {
			return {
				ipcMain: {
					handle(channel, handler) {
						handlers.set(channel, handler);
					},
					removeHandler(channel) {
						handlers.delete(channel);
					},
				},
			};
		}
		if (specifier === './MacTcc') {
			return {getTccStatus: () => tccStatus};
		}
		if (specifier === './NativeVoiceEngine') {
			return {
				createNativeVoiceEngineScreenFrameSinkHandle: (captureId) =>
					typeof frameSinkHandle === 'function' ? frameSinkHandle(captureId) : frameSinkHandle,
			};
		}
		if (specifier === './NativeScreenCaptureValidation') {
			return {
				isValidStartOptions: () => true,
				normalizeScreenCaptureDimension: (value) => value,
			};
		}
		throw new Error(`Unexpected import: ${specifier}`);
	}

	const module = {exports: {}};
	const context = vm.createContext({
		Buffer,
		ArrayBuffer,
		Error,
		console,
		clearTimeout,
		exports: module.exports,
		module,
		process: {env: {}, platform},
		require: requireStub,
		setTimeout,
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});

	return {calls, handlers, module: module.exports};
}

describe('NativeScreenCapture source identity and capability reporting', () => {
	test('normalizes display and window sources without changing source ids', async () => {
		const {addon} = makeNativeAddon({
			sources: [
				{kind: 'screen', id: 'display:69733632', name: 'Studio Display', width: 5120.9, height: 2880.2},
				{
					kind: 'window',
					id: 'window:4242',
					name: '',
					width: 1439.8,
					height: 899.9,
					appName: 'Fluxer',
					bundleId: 'app.fluxer.desktop',
					targetPid: 1234.9,
				},
				{kind: 'window', id: 'window:no-pid', name: 'No PID', width: 800, height: 600, targetPid: -1},
				{kind: 'screen', id: '', name: 'invalid', width: 1, height: 1},
				{kind: 'browser-tab', id: 'tab:1', name: 'invalid', width: 1, height: 1},
			],
		});
		const harness = loadNativeScreenCapture({platform: 'linux', addon});
		harness.module.registerNativeScreenCaptureHandlers();

		const sources = await harness.handlers.get('native-screen-capture:list-sources')();

		assert.deepEqual(plain(sources), [
			{
				kind: 'screen',
				id: 'display:69733632',
				name: 'Studio Display',
				width: 5120,
				height: 2880,
				appName: undefined,
				bundleId: undefined,
				targetPid: undefined,
			},
			{
				kind: 'window',
				id: 'window:4242',
				name: 'window window:4242',
				width: 1439,
				height: 899,
				appName: 'Fluxer',
				bundleId: 'app.fluxer.desktop',
				targetPid: 1234,
			},
			{
				kind: 'window',
				id: 'window:no-pid',
				name: 'No PID',
				width: 800,
				height: 600,
				appName: undefined,
				bundleId: undefined,
				targetPid: undefined,
			},
		]);
	});

	test('starts display and window captures with exact source id and kind and reports diagnostics', async () => {
		const frameSinkHandles = {
			'capture-1': {native: true, captureId: 'capture-1'},
			'capture-2': {native: true, captureId: 'capture-2'},
		};
		const {addon, captures} = makeNativeAddon();
		const harness = loadNativeScreenCapture({
			platform: 'linux',
			addon,
			frameSinkHandle: (captureId) => frameSinkHandles[captureId] ?? null,
		});
		const {sender, sent} = makeSender();
		harness.module.registerNativeScreenCaptureHandlers();

		const displayResult = await harness.handlers.get('native-screen-capture:start')(
			{sender},
			{
				sourceId: 'display:69733632',
				sourceKind: 'screen',
				width: 2560,
				height: 1440,
				frameRate: 60,
				colorRange: 'full',
				colorSpace: 'rec709',
				showCursorClicks: true,
				captureRect: {x: 10, y: 20, width: 300, height: 200},
				nativeFrameSinkRequired: true,
			},
		);
		const windowResult = await harness.handlers.get('native-screen-capture:start')(
			{sender},
			{
				sourceId: 'window:4242',
				sourceKind: 'window',
				width: 1280,
				height: 720,
				frameRate: 30,
				nativeFrameSinkRequired: true,
			},
		);

		assert.deepEqual(plain(captures.map((capture) => capture.options)), [
			{
				sourceId: 'display:69733632',
				sourceKind: 'screen',
				width: 2560,
				height: 1440,
				frameRate: 60,
				injectionMethod: undefined,
				captureId: 'capture-1',
				colorRange: 'full',
				colorSpace: 'rec709',
				showCursorClicks: true,
				captureRect: {x: 10, y: 20, width: 300, height: 200},
				nativeFrameSinkRequired: true,
				frameSinkHandle: frameSinkHandles['capture-1'],
			},
			{
				sourceId: 'window:4242',
				sourceKind: 'window',
				width: 1280,
				height: 720,
				frameRate: 30,
				injectionMethod: undefined,
				captureId: 'capture-2',
				colorRange: undefined,
				colorSpace: undefined,
				showCursorClicks: false,
				captureRect: undefined,
				nativeFrameSinkRequired: true,
				frameSinkHandle: frameSinkHandles['capture-2'],
			},
		]);
		assert.equal(displayResult.captureId, 'capture-1');
		assert.equal(windowResult.captureId, 'capture-2');
		assert.deepEqual(sent, []);

		const displayDiagnostics = await harness.handlers.get('native-screen-capture:get-diagnostics')(
			{sender},
			'capture-1',
		);
		const windowDiagnostics = await harness.handlers.get('native-screen-capture:get-diagnostics')(
			{sender},
			'capture-2',
		);

		assert.equal(displayDiagnostics.sourceId, 'display:69733632');
		assert.equal(displayDiagnostics.sourceKind, 'screen');
		assert.equal(displayDiagnostics.width, 1280);
		assert.equal(displayDiagnostics.height, 720);
		assert.equal(windowDiagnostics.sourceId, 'window:4242');
		assert.equal(windowDiagnostics.sourceKind, 'window');
		assert.equal(windowDiagnostics.width, 1280);
		assert.equal(windowDiagnostics.height, 720);

		await harness.handlers.get('native-screen-capture:stop')({sender}, 'capture-1');
		await harness.handlers.get('native-screen-capture:stop')({sender}, 'capture-2');
		assert.equal(captures[0].stopCount, 1);
		assert.equal(captures[1].stopCount, 1);
	});

	test('fails fast when native frame sink is required but unavailable', async () => {
		const {addon, captures} = makeNativeAddon();
		const harness = loadNativeScreenCapture({platform: 'linux', addon});
		const {sender} = makeSender();
		harness.module.registerNativeScreenCaptureHandlers();

		await assert.rejects(
			() =>
				harness.handlers.get('native-screen-capture:start')(
					{sender},
					{
						sourceId: 'window:4242',
						sourceKind: 'window',
						width: 1280,
						height: 720,
						captureId: 'preselected-capture-id',
						nativeFrameSinkRequired: true,
					},
				),
			/requires a native frame sink handle/,
		);
		assert.equal(captures.length, 0);
	});

	test('passes caller-provided capture id and native sink handle to the platform wrapper', async () => {
		const frameSinkHandle = {native: true};
		const {addon, captures} = makeNativeAddon();
		const harness = loadNativeScreenCapture({
			platform: 'linux',
			addon,
			frameSinkHandle: (captureId) => (captureId === 'preselected-capture-id' ? frameSinkHandle : null),
		});
		const {sender, sent} = makeSender();
		harness.module.registerNativeScreenCaptureHandlers();

		const result = await harness.handlers.get('native-screen-capture:start')(
			{sender},
			{
				sourceId: 'window:4242',
				sourceKind: 'window',
				width: 1280,
				height: 720,
				captureId: 'preselected-capture-id',
				nativeFrameSinkRequired: true,
			},
		);

		assert.equal(result.captureId, 'preselected-capture-id');
		assert.deepEqual(plain(captures[0].options), {
			sourceId: 'window:4242',
			sourceKind: 'window',
			width: 1280,
			height: 720,
			frameRate: 30,
			injectionMethod: undefined,
			captureId: 'preselected-capture-id',
			colorRange: undefined,
			colorSpace: undefined,
			showCursorClicks: false,
			captureRect: undefined,
			nativeFrameSinkRequired: true,
			frameSinkHandle,
		});
		assert.deepEqual(sent, []);
	});

	test('reports macOS and Linux display/window capture capabilities from platform backends', async () => {
		const macHarness = loadNativeScreenCapture({
			platform: 'darwin',
			addon: makeNativeAddon({
				macAvailability: {sck: {supported: true, macosVersion: '15.0'}, screenPermission: 'authorized'},
			}).addon,
			tccStatus: 'authorized',
		});
		macHarness.module.registerNativeScreenCaptureHandlers();
		const macAvailability = await macHarness.handlers.get('native-screen-capture:get-availability')();
		assert.deepEqual(plain(macAvailability), {
			available: true,
			backend: 'macos-sck',
			detail: '15.0',
			capabilities: {hidesCursor: true, screens: true, windows: true},
		});

		const linuxHarness = loadNativeScreenCapture({
			platform: 'linux',
			addon: makeNativeAddon({
				linuxAvailability: {
					available: true,
					backend: 'linux-pipewire-portal',
					detail: 'portal system capture disabled',
					capabilities: {process: true, system: false},
				},
			}).addon,
		});
		linuxHarness.module.registerNativeScreenCaptureHandlers();
		const linuxAvailability = await linuxHarness.handlers.get('native-screen-capture:get-availability')();
		assert.deepEqual(plain(linuxAvailability), {
			available: true,
			backend: 'linux-pipewire',
			detail: 'portal system capture disabled',
			capabilities: {hidesCursor: true, screens: false, windows: false},
		});
	});

	test('reports a Windows native game capture load failure when requiring the addon throws', async () => {
		const harness = loadNativeScreenCapture({platform: 'win32'});
		harness.module.registerNativeScreenCaptureHandlers();

		const availability = await harness.handlers.get('native-screen-capture:get-availability')();
		assert.deepEqual(plain(availability), {
			available: false,
			backend: 'windows-game-capture',
			reason: 'load-failed',
			detail: 'No fake addon configured for @fluxer/win-game-capture',
			windowsHagsState: 'enabled',
			windowsHagsDetail: 'HwSchMode=2',
		});
		assert.deepEqual(harness.calls.nativeModuleImports, ['@fluxer/win-game-capture']);
		assert.equal(harness.calls.logs.warn[0][0], 'Failed to load Windows native screen capture addon');
	});

	test('reports a Windows native game capture load failure surfaced by the addon loadError', async () => {
		const {addon} = makeNativeAddon();
		const loadError = new Error(
			'@fluxer/win-game-capture native module failed to load.\nmodule=@fluxer/win-game-capture\nreason=native binary not found',
		);
		loadError.name = 'NativeModuleLoadError';
		loadError.nativeDiagnostics = {
			schemaVersion: 1,
			moduleName: '@fluxer/win-game-capture',
			reason: 'native binary not found',
		};
		addon.loadError = loadError;
		const harness = loadNativeScreenCapture({platform: 'win32', addon});
		harness.module.registerNativeScreenCaptureHandlers();

		const availability = await harness.handlers.get('native-screen-capture:get-availability')();

		assert.deepEqual(plain(availability), {
			available: false,
			backend: 'windows-game-capture',
			reason: 'load-failed',
			detail:
				'@fluxer/win-game-capture native module failed to load.\nmodule=@fluxer/win-game-capture\nreason=native binary not found',
			windowsHagsState: 'enabled',
			windowsHagsDetail: 'HwSchMode=2',
		});
		assert.deepEqual(harness.calls.nativeModuleImports, ['@fluxer/win-game-capture']);
		assert.equal(harness.calls.logs.warn[0][0], 'Windows native screen capture addon reported load error');
		assert.equal(harness.calls.logs.warn[0][1], loadError);
	});

	test('loads Windows native game capture and reports its backend capabilities', async () => {
		const {addon} = makeNativeAddon();
		addon.getAvailability = () => ({available: true, backend: 'windows-game-capture'});
		const harness = loadNativeScreenCapture({platform: 'win32', addon});
		harness.module.registerNativeScreenCaptureHandlers();

		const availability = await harness.handlers.get('native-screen-capture:get-availability')();

		assert.deepEqual(plain(availability), {
			available: true,
			backend: 'windows-game-capture',
			windowsHagsState: 'enabled',
			windowsHagsDetail: 'HwSchMode=2',
			capabilities: {hidesCursor: false, screens: true, windows: true},
		});
		assert.deepEqual(harness.calls.nativeModuleImports, ['@fluxer/win-game-capture']);
	});

	test('routes Windows display sources through the native screen path without remapping to game', async () => {
		const {addon, captures} = makeNativeAddon();
		addon.getAvailability = () => ({available: true, backend: 'windows-game-capture'});
		const harness = loadNativeScreenCapture({
			platform: 'win32',
			addon,
			frameSinkHandle: (captureId) => ({native: true, captureId}),
		});
		harness.module.registerNativeScreenCaptureHandlers();
		const {sender} = makeSender();

		const result = await harness.handlers.get('native-screen-capture:start')(
			{sender},
			{
				sourceId: 'screen:0:0',
				sourceKind: 'screen',
				width: 2560,
				height: 1440,
				frameRate: 60,
				injectionMethod: 'set-windows-hook',
				nativeFrameSinkRequired: true,
			},
		);

		assert.equal(captures.length, 1);
		assert.equal(captures[0].options.sourceKind, 'screen');
		assert.equal(captures[0].options.injectionMethod, undefined);

		const diagnostics = await harness.handlers.get('native-screen-capture:get-diagnostics')({sender}, result.captureId);
		assert.equal(diagnostics.sourceKind, 'screen');
	});
});
