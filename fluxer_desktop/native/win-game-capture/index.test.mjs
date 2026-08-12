// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createRequire} from 'node:module';
import {afterEach, describe, test} from 'node:test';

const require = createRequire(import.meta.url);
const winGameCapture = require('./index.js');

const hasBindingHook = typeof winGameCapture.__setBindingForTests === 'function';
const injectionSkip = hasBindingHook ? false : 'no __setBindingForTests hook exported';

if (!hasBindingHook) {
	console.warn(
		'[index.test.mjs] __setBindingForTests not present on index.js; ' +
			'skipping binding-injection tests and running only the no-binding tests. ' +
			'This should only happen when testing an older or stripped loader.',
	);
}

function makeFakeBinding() {
	const calls = [];
	const frameSinkHandleCalls = [];
	const priorityCalls = [];
	const vulkanCalls = [];
	const hookAvailable = {value: false};
	const natives = [];
	const diagnostics = {
		state: 1,
		apiType: 5,
		transport: 0,
		fallbackReason: 0,
		captureFlags: 0,
		width: 1920,
		height: 1080,
		dxgiFormat: 87,
		frameCounter: 42,
		droppedFrameCounter: 0,
		lastPresentTimestampUs: 123456,
		lastError: 0,
		requestedInjectionMethod: 'auto',
		injectionMethod: 'remote-thread',
		activeStrategy: 'game-hook',
		lastFallbackReason: '',
		startOptions: {
			colorRange: 'full',
			colorSpace: 'rec709',
			showCursorClicks: true,
			captureRect: {x: 10, y: 20, width: 300, height: 200},
			unsupportedOptions: ['showCursorClicks', 'captureRect', 'colorRange', 'colorSpace'],
		},
		frameSinkAccepted: 0,
		frameSinkCoalesced: 0,
		frameSinkRejected: 0,
		mediaFramesDroppedWithoutSink: 0,
		cpuFallbackFramesDropped: 0,
	};
	const encoderDiagnostics = {
		attached: false,
		width: 0,
		height: 0,
		capacity: 0,
		framesSubmitted: 0,
		framesDropped: 0,
		ringFullEvents: 0,
		failedBlits: 0,
	};
	const frameSinkDiagnostics = {
		accepted: 0,
		coalesced: 0,
		rejected: 0,
		mediaFramesDroppedWithoutSink: 0,
		cpuFallbackFramesDropped: 0,
	};

	class FakeNative {
		constructor() {
			this.lifecycleCallback = undefined;
			this.stopCount = 0;
			natives.push(this);
		}

		setLifecycleCallback(callback) {
			this.lifecycleCallback = callback;
		}

		setFrameSinkHandle(handle) {
			frameSinkHandleCalls.push(handle);
		}

		start(
			sourceId,
			sourceKind,
			width,
			height,
			frameRate,
			hookDllPath,
			hookDllPathX86,
			injectionMethod,
			captureId,
			captureOptions,
		) {
			calls.push({
				sourceId,
				sourceKind,
				width,
				height,
				frameRate,
				hookDllPath,
				hookDllPathX86,
				injectionMethod,
				captureId,
				captureOptions,
			});
			return {
				width: width || 1920,
				height: height || 1080,
				frameRate: frameRate || 30,
				pixelFormat: 'bgra',
			};
		}

		stop() {
			this.stopCount += 1;
		}

		getDiagnostics() {
			return diagnostics;
		}

		attachEncoder(width, height) {
			encoderDiagnostics.attached = true;
			encoderDiagnostics.width = width;
			encoderDiagnostics.height = height;
			encoderDiagnostics.capacity = 8;
		}

		detachEncoder() {
			encoderDiagnostics.attached = false;
		}

		isEncoderAttached() {
			return encoderDiagnostics.attached;
		}

		encoderRingFullCount() {
			return encoderDiagnostics.ringFullEvents;
		}

		getEncoderAttachDiagnostics() {
			if (!encoderDiagnostics.attached) return null;
			return {...encoderDiagnostics};
		}

		getFrameSinkDiagnostics() {
			return {...frameSinkDiagnostics};
		}
	}

	return {
		binding: {
			ScreenCapture: FakeNative,
			isSupported: () => true,
			getAvailability: () => ({available: true, backend: 'windows-game-capture'}),
			listSources: () => [
				{
					kind: 'screen',
					id: 'screen:0:0',
					name: 'Display 1',
					width: 2560,
					height: 1440,
				},
				{
					kind: 'window',
					id: 'window:5050:0',
					name: 'Fixture',
					width: 1280,
					height: 720,
					targetPid: 4242,
				},
				{
					kind: 'browser',
					id: 'screen:1:0',
					name: 'unsupported shape',
				},
			],
			elevateGpuSchedulingPriority: (processId, priorityClass) => {
				priorityCalls.push({type: 'elevate', processId, priorityClass});
			},
			restoreGpuSchedulingPriority: (processId) => {
				priorityCalls.push({type: 'restore', processId});
			},
			isGameCaptureHookAvailable: () => hookAvailable.value,
			registerVulkanLayerManifest: (manifestPath) => {
				vulkanCalls.push({type: 'register', manifestPath});
			},
			unregisterVulkanLayerManifest: (manifestPath) => {
				vulkanCalls.push({type: 'unregister', manifestPath});
			},
		},
		calls,
		frameSinkHandleCalls,
		priorityCalls,
		vulkanCalls,
		hookAvailable,
		natives,
		diagnostics,
		encoderDiagnostics,
		frameSinkDiagnostics,
	};
}

afterEach(() => {
	if (hasBindingHook) {
		winGameCapture.__setBindingForTests(null);
	}
});

describe('win-game-capture loader wrapper -- binding-absent fallback path', () => {
	function forceNoBinding() {
		if (hasBindingHook) winGameCapture.__setBindingForTests(null);
	}

	test(
		'isSupported() is false when no binding is loaded',
		{skip: hasBindingHook ? false : 'no __setBindingForTests hook'},
		() => {
			forceNoBinding();
			assert.equal(winGameCapture.isSupported(), false);
		},
	);

	test(
		'getAvailability() returns the {available:false, ...} shape when no binding is loaded',
		{skip: hasBindingHook ? false : 'no __setBindingForTests hook'},
		() => {
			forceNoBinding();
			const availability = winGameCapture.getAvailability();
			assert.equal(availability.available, false);
			assert.equal(availability.backend, 'windows-game-capture');
			assert.equal(typeof availability.reason, 'string');
		},
	);

	test(
		'constructing ScreenCapture without a binding throws',
		{skip: hasBindingHook ? false : 'no __setBindingForTests hook'},
		() => {
			forceNoBinding();
			assert.throws(() => new winGameCapture.ScreenCapture({sourceId: '1'}));
			try {
				new winGameCapture.ScreenCapture({sourceId: '1'});
				assert.fail('expected ScreenCapture constructor to throw without a binding');
			} catch (error) {
				assert.ok(error instanceof Error);
			}
		},
	);
});

describe('win-game-capture loader wrapper -- arch path resolvers (platform-portable)', () => {
	test('resolveGameHookPath() is null or the host-arch hook path', () => {
		const r = winGameCapture.resolveGameHookPath();
		assert.ok(
			r === null || (typeof r === 'string' && /fluxer-game-hook\.win32-(x64|ia32|arm64)-msvc\.dll$/.test(r)),
			`unexpected resolveGameHookPath(): ${r}`,
		);
	});

	test('resolveGameHookPathX86() is null or the ia32 hook path', () => {
		const r = winGameCapture.resolveGameHookPathX86();
		assert.ok(
			r === null || (typeof r === 'string' && r.endsWith('fluxer-game-hook.win32-ia32-msvc.dll')),
			`unexpected resolveGameHookPathX86(): ${r}`,
		);
	});

	test('resolveVulkanLayerManifestPath() is null or the host-arch layer manifest path', () => {
		const r = winGameCapture.resolveVulkanLayerManifestPath();
		assert.ok(
			r === null || (typeof r === 'string' && /fluxer-vulkan-layer\.win32-(x64|ia32|arm64)-msvc\.json$/.test(r)),
			`unexpected resolveVulkanLayerManifestPath(): ${r}`,
		);
	});
});

describe('win-game-capture loader wrapper -- injected fake binding', () => {
	test('isGameCaptureHookAvailable() follows the native hook flag', {skip: injectionSkip}, () => {
		const {binding} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		assert.equal(winGameCapture.isGameCaptureHookAvailable(), false);
	});

	test(
		'isGameCaptureHookAvailable() is false when the native binding predates the hook flag',
		{skip: injectionSkip},
		() => {
			const {binding} = makeFakeBinding();
			binding.isGameCaptureHookAvailable = undefined;
			winGameCapture.__setBindingForTests(binding);
			assert.equal(winGameCapture.isGameCaptureHookAvailable(), false);
		},
	);

	test(
		'registerVulkanLayerManifest() never touches the registry while hook capture is unavailable',
		{skip: injectionSkip},
		() => {
			const {binding, vulkanCalls} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			assert.equal(winGameCapture.registerVulkanLayerManifest(), false);
			assert.deepEqual(vulkanCalls, [], 'the native registration entry point must not be called');
		},
	);

	test(
		'registerVulkanLayerManifest() still refuses when the hook DLL is missing for this host',
		{skip: injectionSkip || (winGameCapture.resolveGameHookPath() !== null && 'host ships a game capture hook DLL')},
		() => {
			const {binding, hookAvailable, vulkanCalls} = makeFakeBinding();
			hookAvailable.value = true;
			winGameCapture.__setBindingForTests(binding);
			assert.equal(winGameCapture.isGameCaptureHookAvailable(), false);
			assert.equal(winGameCapture.registerVulkanLayerManifest(), false);
			assert.deepEqual(vulkanCalls, []);
		},
	);

	test(
		'listSources() forwards sanitized screen/window sources from the native binding',
		{skip: injectionSkip},
		async () => {
			const {binding} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const sources = await winGameCapture.listSources();
			assert.deepEqual(sources, [
				{
					kind: 'screen',
					id: 'screen:0:0',
					name: 'Display 1',
					width: 2560,
					height: 1440,
					targetPid: undefined,
				},
				{
					kind: 'window',
					id: 'window:5050:0',
					name: 'Fixture',
					width: 1280,
					height: 720,
					targetPid: 4242,
				},
			]);
		},
	);

	test(
		'start() forwards sourceId/kind/dims/frameRate and BOTH hook paths (6th + 7th args)',
		{skip: injectionSkip},
		async () => {
			const {binding, calls} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const capture = new winGameCapture.ScreenCapture({
				sourceId: '987654',
				sourceKind: 'game',
				width: 1600,
				height: 900,
				frameRate: 60,
				hookDllPath: 'C:/hooks/fluxer-game-hook.win32-x64-msvc.dll',
				hookDllPathX86: 'C:/hooks/fluxer-game-hook.win32-ia32-msvc.dll',
				captureId: 'capture-1',
				colorRange: 'full',
				colorSpace: 'rec709',
				showCursorClicks: true,
				captureRect: {x: 10, y: 20, width: 300, height: 200},
			});
			capture.on('error', () => {});
			const result = await capture.start();
			assert.equal(calls.length, 1);
			assert.deepEqual(calls[0], {
				sourceId: '987654',
				sourceKind: 'game',
				width: 1600,
				height: 900,
				frameRate: 60,
				hookDllPath: 'C:/hooks/fluxer-game-hook.win32-x64-msvc.dll',
				hookDllPathX86: 'C:/hooks/fluxer-game-hook.win32-ia32-msvc.dll',
				injectionMethod: undefined,
				captureId: 'capture-1',
				captureOptions: {
					colorRange: 'full',
					colorSpace: 'rec709',
					showCursorClicks: true,
					captureRect: {x: 10, y: 20, width: 300, height: 200},
				},
			});
			assert.equal(result.pixelFormat, 'bgra');
			assert.equal(result.width, 1600);
			assert.equal(result.height, 900);
			assert.equal(result.frameRate, 60);
		},
	);

	test(
		'start() forwards the injectionMethod option as the 8th arg for game capture',
		{skip: injectionSkip},
		async () => {
			const {binding, calls} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const capture = new winGameCapture.ScreenCapture({
				sourceId: '555',
				sourceKind: 'game',
				hookDllPath: 'C:/hooks/fluxer-game-hook.win32-x64-msvc.dll',
				hookDllPathX86: 'C:/hooks/fluxer-game-hook.win32-ia32-msvc.dll',
				injectionMethod: 'set-windows-hook',
			});
			capture.on('error', () => {});
			await capture.start();
			assert.equal(calls.length, 1);
			assert.equal(calls[0].injectionMethod, 'set-windows-hook');
		},
	);

	test('window sourceKind does not forward the injectionMethod', {skip: injectionSkip}, async () => {
		const {binding, calls} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({
			sourceId: '42',
			sourceKind: 'window',
			injectionMethod: 'set-windows-hook',
		});
		capture.on('error', () => {});
		await capture.start();
		assert.equal(calls[0].injectionMethod, undefined);
	});

	test('window sourceKind does not forward hook paths', {skip: injectionSkip}, async () => {
		const {binding, calls} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({
			sourceId: '42',
			sourceKind: 'window',
			hookDllPath: 'C:/hooks/primary.dll',
			hookDllPathX86: 'C:/hooks/x86.dll',
		});
		capture.on('error', () => {});
		await capture.start();
		assert.equal(calls[0].sourceKind, 'window');
		assert.equal(calls[0].hookDllPath, undefined);
		assert.equal(calls[0].hookDllPathX86, undefined);
	});

	test('screen sourceKind does not forward hook paths or the injectionMethod', {skip: injectionSkip}, async () => {
		const {binding, calls} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({
			sourceId: 'screen:0:0',
			sourceKind: 'screen',
			hookDllPath: 'C:/hooks/primary.dll',
			hookDllPathX86: 'C:/hooks/x86.dll',
			injectionMethod: 'set-windows-hook',
		});
		capture.on('error', () => {});
		await capture.start();
		assert.equal(calls[0].sourceKind, 'screen');
		assert.equal(calls[0].hookDllPath, undefined);
		assert.equal(calls[0].hookDllPathX86, undefined);
		assert.equal(calls[0].injectionMethod, undefined);
	});

	test('getDiagnostics() exposes native start option state', {skip: injectionSkip}, () => {
		const {binding} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({sourceId: '42', sourceKind: 'window'});
		const diagnostics = capture.getDiagnostics();
		assert.deepEqual(diagnostics.startOptions, {
			colorRange: 'full',
			colorSpace: 'rec709',
			showCursorClicks: true,
			captureRect: {x: 10, y: 20, width: 300, height: 200},
			unsupportedOptions: ['showCursorClicks', 'captureRect', 'colorRange', 'colorSpace'],
		});
	});

	test('encoder attachment diagnostics are forwarded by the wrapper', {skip: injectionSkip}, () => {
		const {binding, encoderDiagnostics} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({sourceId: '42', sourceKind: 'window'});

		assert.equal(capture.isEncoderAttached(), false);
		assert.equal(capture.encoderRingFullCount(), 0);
		assert.equal(capture.getEncoderAttachDiagnostics(), null);

		capture.attachEncoder(1280, 720);
		encoderDiagnostics.framesSubmitted = 3;
		encoderDiagnostics.framesDropped = 1;
		encoderDiagnostics.ringFullEvents = 1;
		encoderDiagnostics.failedBlits = 0;

		assert.equal(capture.isEncoderAttached(), true);
		assert.equal(capture.encoderRingFullCount(), 1);
		assert.deepEqual(capture.getEncoderAttachDiagnostics(), {
			attached: true,
			width: 1280,
			height: 720,
			capacity: 8,
			framesSubmitted: 3,
			framesDropped: 1,
			ringFullEvents: 1,
			failedBlits: 0,
		});

		capture.detachEncoder();
		assert.equal(capture.isEncoderAttached(), false);
	});

	test('frame sink diagnostics are forwarded by the wrapper', {skip: injectionSkip}, () => {
		const {binding, frameSinkDiagnostics} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({sourceId: '42', sourceKind: 'window'});

		frameSinkDiagnostics.accepted = 7;
		frameSinkDiagnostics.coalesced = 2;
		frameSinkDiagnostics.rejected = 1;
		frameSinkDiagnostics.mediaFramesDroppedWithoutSink = 3;
		frameSinkDiagnostics.cpuFallbackFramesDropped = 4;

		assert.deepEqual(capture.getFrameSinkDiagnostics(), {
			accepted: 7,
			coalesced: 2,
			rejected: 1,
			mediaFramesDroppedWithoutSink: 3,
			cpuFallbackFramesDropped: 4,
		});
	});

	test('installs a native frame sink handle once before start', {skip: injectionSkip}, async () => {
		const {binding, calls, frameSinkHandleCalls} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const frameSinkHandle = {native: true};
		const capture = new winGameCapture.ScreenCapture({
			sourceId: '42',
			sourceKind: 'window',
			frameSinkHandle,
			nativeFrameSinkRequired: true,
		});
		capture.on('error', () => {});

		await capture.start();

		assert.deepEqual(frameSinkHandleCalls, [frameSinkHandle]);
		assert.equal(calls.length, 1);
	});

	test(
		'fails before native start when a native frame sink is required but missing',
		{skip: injectionSkip},
		async () => {
			const {binding, calls, frameSinkHandleCalls} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const capture = new winGameCapture.ScreenCapture({
				sourceId: '42',
				sourceKind: 'window',
				nativeFrameSinkRequired: true,
			});
			capture.on('error', () => {});

			await assert.rejects(() => capture.start(), /native frame sink handle is required/i);
			assert.deepEqual(frameSinkHandleCalls, []);
			assert.deepEqual(calls, []);
		},
	);

	test(
		'game sourceKind starts without a hook path because capture no longer injects',
		{skip: injectionSkip},
		async () => {
			const {binding, calls} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const capture = new winGameCapture.ScreenCapture({
				sourceId: '7',
				sourceKind: 'game',
				hookDllPath: '',
				hookDllPathX86: '',
			});
			capture.on('error', () => {});
			await capture.start();
			assert.equal(calls.length, 1, 'native start must be called for game capture without a hook');
			assert.equal(calls[0].sourceKind, 'game');
		},
	);

	test('lifecycle "error" emits an Error event', {skip: injectionSkip}, () => {
		const {binding, natives} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
		const errors = [];
		capture.on('error', (err) => errors.push(err));
		natives[0].lifecycleCallback('error', 'DXGI device removed');
		assert.equal(errors.length, 1);
		assert.ok(errors[0] instanceof Error);
		assert.equal(errors[0].message, 'DXGI device removed');
	});

	test('lifecycle "closed" stops the native capture and emits closed once', {skip: injectionSkip}, async () => {
		const {binding, natives} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
		let closed = 0;
		capture.on('closed', () => {
			closed += 1;
		});
		natives[0].lifecycleCallback('closed', '');
		await Promise.resolve();
		await capture.stop();
		assert.equal(closed, 1);
		assert.equal(natives[0].stopCount, 1);
	});

	test('non-fatal "stalled" lifecycle re-emits as a stalled event WITHOUT stopping', {skip: injectionSkip}, () => {
		const {binding, natives} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
		const stalls = [];
		let closed = 0;
		capture.on('stalled', (message) => stalls.push(message));
		capture.on('closed', () => {
			closed += 1;
		});
		natives[0].lifecycleCallback('stalled', 'frame counter frozen while focused');
		assert.deepEqual(stalls, ['frame counter frozen while focused']);
		assert.equal(natives[0].stopCount, 0);
		assert.equal(closed, 0);
	});

	test(
		'non-fatal "diagnostic" lifecycle re-emits as a diagnostic event WITHOUT stopping',
		{skip: injectionSkip},
		() => {
			const {binding, natives} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
			const diags = [];
			let closed = 0;
			capture.on('diagnostic', (message) => diags.push(message));
			capture.on('closed', () => {
				closed += 1;
			});
			natives[0].lifecycleCallback('diagnostic', 'fell back to CPU readback');
			assert.deepEqual(diags, ['fell back to CPU readback']);
			assert.equal(natives[0].stopCount, 0);
			assert.equal(closed, 0);
		},
	);

	test(
		'lifecycle "error" in the real napi [type, message] array shape emits an Error event',
		{skip: injectionSkip},
		() => {
			const {binding, natives} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
			const errors = [];
			capture.on('error', (err) => errors.push(err));
			natives[0].lifecycleCallback(['error', 'DXGI device removed']);
			assert.equal(errors.length, 1);
			assert.ok(errors[0] instanceof Error);
			assert.equal(errors[0].message, 'DXGI device removed');
		},
	);

	test(
		'lifecycle "closed" in the napi array shape stops the native capture and emits closed once',
		{skip: injectionSkip},
		async () => {
			const {binding, natives} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
			let closed = 0;
			capture.on('closed', () => {
				closed += 1;
			});
			natives[0].lifecycleCallback(['closed', '']);
			await Promise.resolve();
			await capture.stop();
			assert.equal(closed, 1);
			assert.equal(natives[0].stopCount, 1);
		},
	);

	test(
		'lifecycle "stalled" in the napi array shape re-emits as a stalled event WITHOUT stopping',
		{skip: injectionSkip},
		() => {
			const {binding, natives} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
			const stalls = [];
			let closed = 0;
			capture.on('stalled', (message) => stalls.push(message));
			capture.on('closed', () => {
				closed += 1;
			});
			natives[0].lifecycleCallback(['stalled', 'frame counter frozen while focused']);
			assert.deepEqual(stalls, ['frame counter frozen while focused']);
			assert.equal(natives[0].stopCount, 0);
			assert.equal(closed, 0);
		},
	);

	test(
		'lifecycle "diagnostic" (injected via <method>) in the napi array shape re-emits WITHOUT stopping',
		{skip: injectionSkip},
		() => {
			const {binding, natives} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
			const diags = [];
			let closed = 0;
			capture.on('diagnostic', (message) => diags.push(message));
			capture.on('closed', () => {
				closed += 1;
			});
			natives[0].lifecycleCallback(['diagnostic', 'game capture injected via set-windows-hook']);
			assert.deepEqual(diags, ['game capture injected via set-windows-hook']);
			assert.equal(natives[0].stopCount, 0);
			assert.equal(closed, 0);
		},
	);

	test('getDiagnostics() passes the native snapshot through', {skip: injectionSkip}, () => {
		const {binding, diagnostics} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
		const snapshot = capture.getDiagnostics();
		assert.deepEqual(snapshot, diagnostics);
	});

	test('getDiagnostics() surfaces the activeStrategy + lastFallbackReason fields', {skip: injectionSkip}, () => {
		const {binding, diagnostics} = makeFakeBinding();
		diagnostics.activeStrategy = 'dxgi-duplication';
		diagnostics.lastFallbackReason =
			'game-hook capture could not inject its capture hook; switching to dxgi-duplication capture';
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
		const snapshot = capture.getDiagnostics();
		assert.equal(snapshot.activeStrategy, 'dxgi-duplication');
		assert.match(snapshot.lastFallbackReason, /switching to dxgi-duplication/);
	});

	test('elevateGpuSchedulingPriority forwards the optional realtime priority class', {skip: injectionSkip}, () => {
		const {binding, priorityCalls} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		assert.equal(winGameCapture.elevateGpuSchedulingPriority(1234, 'realtime'), true);
		assert.deepEqual(priorityCalls, [{type: 'elevate', processId: 1234, priorityClass: 'realtime'}]);
	});

	test('elevateGpuSchedulingPriority canonicalizes the real-time priority alias', {skip: injectionSkip}, () => {
		const {binding, priorityCalls} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		assert.equal(winGameCapture.elevateGpuSchedulingPriority(1234, 'real-time'), true);
		assert.deepEqual(priorityCalls, [{type: 'elevate', processId: 1234, priorityClass: 'realtime'}]);
	});

	test('elevateGpuSchedulingPriority preserves the existing default priority class', {skip: injectionSkip}, () => {
		const {binding, priorityCalls} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		assert.equal(winGameCapture.elevateGpuSchedulingPriority(1234), true);
		assert.deepEqual(priorityCalls, [{type: 'elevate', processId: 1234, priorityClass: undefined}]);
	});

	test(
		'elevateGpuSchedulingPriority rejects unsupported priority classes before binding calls',
		{skip: injectionSkip},
		() => {
			const {binding, priorityCalls} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			assert.equal(winGameCapture.elevateGpuSchedulingPriority(1234, 'normal'), false);
			assert.deepEqual(priorityCalls, []);
		},
	);

	test('lifecycle fallback "error" (next-strategy=...) re-emits as a fatal error event', {skip: injectionSkip}, () => {
		const {binding, natives} = makeFakeBinding();
		winGameCapture.__setBindingForTests(binding);
		const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
		const errors = [];
		capture.on('error', (err) => errors.push(err));
		natives[0].lifecycleCallback([
			'error',
			'fallback: game-hook -> dxgi-duplication (game-hook capture could not inject its capture hook) [next-strategy=dxgi-duplication]',
		]);
		assert.equal(errors.length, 1);
		assert.ok(errors[0] instanceof Error);
		assert.equal(winGameCapture.parseFallbackRecommendation(errors[0].message), 'dxgi-duplication');
	});

	test(
		'lifecycle fallback "diagnostic" (upgrade, next-strategy=...) re-emits WITHOUT stopping',
		{skip: injectionSkip},
		() => {
			const {binding, natives} = makeFakeBinding();
			winGameCapture.__setBindingForTests(binding);
			const capture = new winGameCapture.ScreenCapture({sourceId: '1'});
			const diags = [];
			let closed = 0;
			capture.on('diagnostic', (message) => diags.push(message));
			capture.on('closed', () => {
				closed += 1;
			});
			natives[0].lifecycleCallback([
				'diagnostic',
				'upgrade: window-gdi -> dxgi-duplication (window-gdi has been stable) [next-strategy=dxgi-duplication]',
			]);
			assert.equal(diags.length, 1);
			assert.equal(winGameCapture.parseFallbackRecommendation(diags[0]), 'dxgi-duplication');
			assert.equal(natives[0].stopCount, 0);
			assert.equal(closed, 0);
		},
	);
});

describe('win-game-capture loader wrapper -- parseFallbackRecommendation', () => {
	test('extracts the recommended strategy from a transition error message', () => {
		assert.equal(
			winGameCapture.parseFallbackRecommendation(
				'fallback: game-hook -> window-gdi (reason) [next-strategy=window-gdi]',
			),
			'window-gdi',
		);
	});

	test('returns "none" for an exhausted give-up message', () => {
		assert.equal(
			winGameCapture.parseFallbackRecommendation(
				'fallback exhausted: window-gdi was the last resort [next-strategy=none]',
			),
			'none',
		);
	});

	test('returns null for an ordinary error with no recommendation', () => {
		assert.equal(winGameCapture.parseFallbackRecommendation('DXGI device removed'), null);
		assert.equal(winGameCapture.parseFallbackRecommendation(undefined), null);
		assert.equal(winGameCapture.parseFallbackRecommendation(''), null);
	});

	test('returns null for an unrecognised strategy token', () => {
		assert.equal(winGameCapture.parseFallbackRecommendation('[next-strategy=teleporter]'), null);
	});

	test('accepts the WGC fallback strategy token', () => {
		assert.equal(winGameCapture.parseFallbackRecommendation('[next-strategy=wgc]'), 'wgc');
		assert.equal(
			winGameCapture.parseFallbackRecommendation(
				'fallback: game-hook -> wgc (game-hook capture could not inject its capture hook) [next-strategy=wgc]',
			),
			'wgc',
		);
	});

	test('rejects browser fallback strategy tokens', () => {
		assert.equal(winGameCapture.parseFallbackRecommendation('[next-strategy=browser-display]'), null);
		assert.equal(winGameCapture.parseFallbackRecommendation('[next-strategy=desktop-capturer]'), null);
	});
});
