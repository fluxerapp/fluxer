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

const GPU_SCHEDULING_PRIORITY_ENV = 'FLUXER_STREAMING_GPU_SCHEDULING_PRIORITY';
const sourcePath = fileURLToPath(new URL('./StreamingPriority.ts', import.meta.url));
const source = readFileSync(sourcePath, 'utf8');
const transformedSource = esbuild.transformSync(source, {
	loader: 'ts',
	format: 'cjs',
	platform: 'node',
	target: 'node20',
}).code;

function makeWebContents(processId) {
	const webContents = new EventEmitter();
	webContents.isDestroyed = () => false;
	webContents.getOSProcessId = () => processId;
	webContents.backgroundThrottlingAllowed = [];
	webContents.setBackgroundThrottling = (allowed) => {
		webContents.backgroundThrottlingAllowed.push(allowed);
	};
	return webContents;
}

function normalize(value) {
	return JSON.parse(JSON.stringify(value));
}

function loadStreamingPriority({
	platform = 'win32',
	processId = 1000,
	envPriority,
	metrics = [],
	addonLoadError = null,
	currentPriority = 0,
	elevateResult = true,
	restoreResult = true,
} = {}) {
	const calls = {
		elevate: [],
		restore: [],
		intervals: [],
		clearedIntervals: [],
		powerStart: [],
		powerStop: [],
		guardRetain: 0,
		guardStop: [],
		setPriority: [],
		nativeModuleImports: [],
		logs: {debug: [], info: [], warn: []},
	};

	const addon = {
		loadError: addonLoadError,
		elevateGpuSchedulingPriority(pid, priorityClass) {
			calls.elevate.push({processId: pid, priorityClass});
			return typeof elevateResult === 'function' ? elevateResult(pid, priorityClass) : elevateResult;
		},
		restoreGpuSchedulingPriority(pid) {
			calls.restore.push({processId: pid});
			return typeof restoreResult === 'function' ? restoreResult(pid) : restoreResult;
		},
	};
	const app = {getAppMetrics: () => metrics};
	const powerSaveBlocker = {
		start(type) {
			calls.powerStart.push(type);
			return 7;
		},
		stop(id) {
			calls.powerStop.push(id);
		},
	};
	const osModule = {
		constants: {priority: {PRIORITY_ABOVE_NORMAL: -7}},
		getPriority: () => currentPriority,
		setPriority(priority) {
			calls.setPriority.push(priority);
		},
	};
	const log = {
		debug: (...args) => calls.logs.debug.push(args),
		info: (...args) => calls.logs.info.push(args),
		warn: (...args) => calls.logs.warn.push(args),
	};
	const guard = {
		retainWindowsScreenCaptureGuard() {
			calls.guardRetain += 1;
		},
		stopWindowsScreenCaptureGuard(reason) {
			calls.guardStop.push(reason);
		},
	};
	const env = envPriority === undefined ? {} : {[GPU_SCHEDULING_PRIORITY_ENV]: envPriority};

	function requireStub(specifier) {
		if (specifier === 'node:module') {
			return {
				createRequire: () => (moduleSpecifier) => {
					calls.nativeModuleImports.push(moduleSpecifier);
					if (moduleSpecifier === '@fluxer/win-game-capture') return addon;
					throw new Error(`Unexpected createRequire import: ${moduleSpecifier}`);
				},
			};
		}
		if (specifier === 'node:os') return osModule;
		if (specifier === 'electron') return {app, powerSaveBlocker};
		if (specifier === 'electron-log') return log;
		if (specifier === './WindowsScreenCaptureGuard') return guard;
		throw new Error(`Unexpected import: ${specifier}`);
	}

	const module = {exports: {}};
	const context = vm.createContext({
		require: requireStub,
		module,
		exports: module.exports,
		process: {env, pid: processId, platform},
		setInterval(callback, delay) {
			const timer = {callback, delay, unrefCalled: false};
			timer.unref = () => {
				timer.unrefCalled = true;
			};
			calls.intervals.push(timer);
			return timer;
		},
		clearInterval(timer) {
			calls.clearedIntervals.push(timer);
		},
		console,
	});
	vm.runInContext(transformedSource, context, {filename: sourcePath});

	return {calls, module: module.exports};
}

describe('StreamingPriority GPU scheduling priority', () => {
	test('elevates GPU scheduling priority on Windows', () => {
		const webContents = makeWebContents(2001);
		const {calls, module} = loadStreamingPriority({
			metrics: [
				{pid: 3001, type: 'GPU'},
				{pid: 3002, type: 'utility', name: 'Video Encode Service'},
				{pid: 3003, type: 'utility', serviceName: 'video_capture.mojom.VideoCaptureService'},
				{pid: 3004, type: 'utility', name: 'Network Service'},
				{pid: 3005, type: 'renderer'},
			],
		});

		module.acquireStreamingPriority(webContents);

		assert.deepEqual(calls.nativeModuleImports, ['@fluxer/win-game-capture']);
		assert.deepEqual(calls.elevate, [
			{processId: 1000, priorityClass: 'high'},
			{processId: 2001, priorityClass: 'high'},
			{processId: 3001, priorityClass: 'high'},
			{processId: 3002, priorityClass: 'high'},
			{processId: 3003, priorityClass: 'high'},
		]);
		assert.deepEqual(calls.setPriority, [-7]);
		assert.deepEqual(webContents.backgroundThrottlingAllowed, [false]);
		assert.equal(calls.intervals.length, 1);
		assert.equal(calls.intervals[0].delay, 20000);
		assert.equal(calls.intervals[0].unrefCalled, true);
		assert.deepEqual(normalize(module.getStreamingPriorityDiagnostics()), {
			refCount: 1,
			powerSaveBlocker: {active: true, id: 7},
			processPriority: {
				supported: true,
				streamingPriority: -7,
				savedPriority: 0,
				elevated: true,
			},
			gpuScheduling: {
				supported: true,
				priorityClass: 'high',
				env: GPU_SCHEDULING_PRIORITY_ENV,
				nativeModuleStatus: 'loaded',
				nativeModuleLoadErrorDetail: null,
				refreshActive: true,
				refreshIntervalMs: 20000,
				trackedWebContents: 1,
				elevatedProcesses: [
					{processId: 1000, priorityClass: 'high'},
					{processId: 2001, priorityClass: 'high'},
					{processId: 3001, priorityClass: 'high'},
					{processId: 3002, priorityClass: 'high'},
					{processId: 3003, priorityClass: 'high'},
				],
				lastAcquire: {
					status: 'succeeded',
					priorityClass: 'high',
					targets: [
						{processId: 1000, reasons: ['native-main-encoder-capture']},
						{processId: 2001, reasons: ['renderer']},
						{processId: 3001, reasons: ['chromium-gpu']},
						{processId: 3002, reasons: ['chromium-video-encode']},
						{processId: 3003, reasons: ['chromium-video-capture']},
					],
					elevatedProcessIds: [1000, 2001, 3001, 3002, 3003],
					skippedProcessIds: [],
					failedProcessIds: [],
				},
				lastRestore: null,
			},
		});

		module.acquireStreamingPriority(webContents);
		assert.equal(calls.elevate.length, 5);
		assert.deepEqual(
			normalize(module.getStreamingPriorityDiagnostics().gpuScheduling.lastAcquire.skippedProcessIds),
			[1000, 2001, 3001, 3002, 3003],
		);

		module.releaseStreamingPriority();
		assert.deepEqual(calls.restore, []);

		module.releaseStreamingPriority();
		assert.deepEqual(webContents.backgroundThrottlingAllowed, [false, false, true]);
		assert.deepEqual(calls.restore, [
			{processId: 1000},
			{processId: 2001},
			{processId: 3001},
			{processId: 3002},
			{processId: 3003},
		]);
		assert.deepEqual(calls.setPriority, [-7, 0]);
		assert.deepEqual(normalize(module.getStreamingPriorityDiagnostics().gpuScheduling.elevatedProcesses), []);
		assert.deepEqual(normalize(module.getStreamingPriorityDiagnostics().gpuScheduling.lastRestore), {
			status: 'succeeded',
			processIds: [1000, 2001, 3001, 3002, 3003],
			restoredProcessIds: [1000, 2001, 3001, 3002, 3003],
			failedProcessIds: [],
		});
		assert.deepEqual(calls.guardStop, ['streaming-priority-release']);
	});

	test('accepts realtime env overrides for OBS non-HAGS parity testing', () => {
		for (const envPriority of ['real-time', ' realtime ']) {
			const {calls, module} = loadStreamingPriority({envPriority});

			module.acquireStreamingPriority();

			assert.deepEqual(calls.elevate, [{processId: 1000, priorityClass: 'realtime'}]);
			assert.equal(module.getStreamingPriorityDiagnostics().gpuScheduling.priorityClass, 'realtime');
		}
	});

	test('loads the Windows game capture native module for GPU priority elevation', () => {
		const webContents = makeWebContents(2001);
		const {calls, module} = loadStreamingPriority({
			metrics: [{pid: 3001, type: 'GPU'}],
		});

		module.acquireStreamingPriority(webContents);

		assert.deepEqual(calls.nativeModuleImports, ['@fluxer/win-game-capture']);
		assert.deepEqual(
			calls.elevate.map((entry) => entry.processId),
			[1000, 2001, 3001],
		);
		assert.equal(module.getStreamingPriorityDiagnostics().gpuScheduling.nativeModuleStatus, 'loaded');
		assert.deepEqual(
			normalize(module.getStreamingPriorityDiagnostics().gpuScheduling.lastAcquire.elevatedProcessIds),
			[1000, 2001, 3001],
		);
	});

	test('allows the GPU scheduling priority workaround to be disabled by env override', () => {
		const webContents = makeWebContents(2001);
		const {calls, module} = loadStreamingPriority({
			envPriority: 'off',
			metrics: [{pid: 3001, type: 'GPU'}],
		});

		module.acquireStreamingPriority(webContents);

		assert.deepEqual(calls.elevate, []);
		assert.deepEqual(calls.intervals, []);
		assert.deepEqual(normalize(module.getStreamingPriorityDiagnostics().gpuScheduling), {
			supported: false,
			priorityClass: null,
			env: GPU_SCHEDULING_PRIORITY_ENV,
			nativeModuleStatus: 'disabled',
			nativeModuleLoadErrorDetail: null,
			refreshActive: false,
			refreshIntervalMs: 20000,
			trackedWebContents: 1,
			elevatedProcesses: [],
			lastAcquire: {
				status: 'disabled',
				priorityClass: null,
				targets: [],
				elevatedProcessIds: [],
				skippedProcessIds: [],
				failedProcessIds: [],
				detail: `${GPU_SCHEDULING_PRIORITY_ENV} disabled GPU scheduling priority`,
			},
			lastRestore: null,
		});
	});

	test('falls back to high priority and logs when the env override is invalid', () => {
		const {calls, module} = loadStreamingPriority({envPriority: 'normal'});

		module.acquireStreamingPriority();

		assert.deepEqual(calls.elevate, [{processId: 1000, priorityClass: 'high'}]);
		assert.equal(calls.logs.warn.length, 1);
		assert.equal(calls.logs.warn[0][0], '[StreamingPriority] Ignoring invalid GPU scheduling priority override');
	});

	test('does not lower process priority when the process is already above the OBS streaming class', () => {
		const {calls, module} = loadStreamingPriority({currentPriority: -10});

		module.acquireStreamingPriority();
		module.releaseStreamingPriority();

		assert.deepEqual(calls.setPriority, []);
		assert.deepEqual(calls.restore, [{processId: 1000}]);
		assert.equal(module.getStreamingPriorityDiagnostics().processPriority.elevated, false);
		assert.equal(module.getStreamingPriorityDiagnostics().processPriority.savedPriority, null);
	});

	test('records the real native module load failure detail', () => {
		const webContents = makeWebContents(2001);
		const {calls, module} = loadStreamingPriority({
			addonLoadError: new Error('native binary missing'),
			metrics: [{pid: 3001, type: 'GPU'}],
		});

		module.acquireStreamingPriority(webContents);

		assert.deepEqual(calls.elevate, []);
		assert.deepEqual(calls.nativeModuleImports, ['@fluxer/win-game-capture']);
		assert.equal(module.getStreamingPriorityDiagnostics().gpuScheduling.nativeModuleStatus, 'unavailable');
		assert.equal(
			module.getStreamingPriorityDiagnostics().gpuScheduling.nativeModuleLoadErrorDetail,
			'native binary missing',
		);
		assert.deepEqual(normalize(module.getStreamingPriorityDiagnostics().gpuScheduling.lastAcquire), {
			status: 'native-module-unavailable',
			priorityClass: 'high',
			targets: [
				{processId: 1000, reasons: ['native-main-encoder-capture']},
				{processId: 2001, reasons: ['renderer']},
				{processId: 3001, reasons: ['chromium-gpu']},
			],
			elevatedProcessIds: [],
			skippedProcessIds: [],
			failedProcessIds: [
				{processId: 1000, reason: 'native binary missing'},
				{processId: 2001, reason: 'native binary missing'},
				{processId: 3001, reason: 'native binary missing'},
			],
			detail: 'native binary missing',
		});
		assert.equal(calls.logs.debug[0][0], '[StreamingPriority] Windows GPU priority module reported load error');
		assert.equal(
			calls.logs.debug[1][0],
			'[StreamingPriority] Cannot elevate GPU scheduling priority; native module unavailable',
		);
	});

	test('reduces a multi-line native load error to its first line for per-process reasons', () => {
		const {calls, module} = loadStreamingPriority({
			addonLoadError: new Error(
				'@fluxer/win-game-capture native module failed to load.\nreason=native binary not found\nnativeRoot=C:/fluxer',
			),
		});

		module.acquireStreamingPriority();
		module.releaseStreamingPriority();

		assert.deepEqual(calls.elevate, []);
		const diagnostics = module.getStreamingPriorityDiagnostics();
		assert.equal(
			diagnostics.gpuScheduling.nativeModuleLoadErrorDetail,
			'@fluxer/win-game-capture native module failed to load.\nreason=native binary not found\nnativeRoot=C:/fluxer',
		);
		assert.deepEqual(normalize(diagnostics.gpuScheduling.lastAcquire.failedProcessIds), [
			{processId: 1000, reason: '@fluxer/win-game-capture native module failed to load.'},
		]);
		assert.equal(
			diagnostics.gpuScheduling.lastAcquire.detail,
			'@fluxer/win-game-capture native module failed to load.\nreason=native binary not found\nnativeRoot=C:/fluxer',
		);
	});
});
