// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createRequire} from 'node:module';
import {afterEach, describe, test} from 'node:test';

const require = createRequire(import.meta.url);
const macScreenCapture = require('./index.js');

function makeFakeBinding({sources = [], availability = {sck: {supported: true}, screenPermission: 'authorized'}} = {}) {
	const calls = [];
	const frameSinkHandleCalls = [];
	const natives = [];
	const frameSinkDiagnostics = {
		accepted: 0,
		coalesced: 0,
		rejected: 0,
		mediaFramesDroppedWithoutSink: 0,
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

		async start(sourceId, sourceKind, width, height, frameRate, captureId, captureOptions) {
			calls.push({sourceId, sourceKind, width, height, frameRate, captureId, captureOptions});
			return {width: width || 1920, height: height || 1080, frameRate: frameRate || 30, pixelFormat: 'nv12'};
		}

		async stop() {
			this.stopCount += 1;
		}

		getFrameSinkDiagnostics() {
			return {...frameSinkDiagnostics};
		}
	}
	return {
		binding: {
			ScreenCapture: FakeNative,
			listSources: async () => sources,
			getBackendAvailability: async () => availability,
			getBackendInfo: () => ({
				backend: 'mac-screen-capture',
				supported: true,
				reason: '',
				minMacosVersion: '12.3',
				detectedMacosVersion: '14.0',
				sckAvailable: true,
			}),
		},
		calls,
		frameSinkHandleCalls,
		frameSinkDiagnostics,
		natives,
	};
}

afterEach(() => {
	macScreenCapture.__setBindingForTests(null);
});

describe('mac-screen-capture loader wrapper', () => {
	test('forwards source id, kind, and dimensions to native binding', async () => {
		const {binding, calls} = makeFakeBinding();
		macScreenCapture.__setBindingForTests(binding);
		const capture = new macScreenCapture.ScreenCapture({
			sourceId: '12345',
			sourceKind: 'window',
			width: 1280,
			height: 720,
			frameRate: 30,
			colorRange: 'full',
			colorSpace: 'rec709',
			showCursorClicks: true,
			captureRect: {x: 10, y: 20, width: 300, height: 200},
		});
		capture.on('error', () => {});
		const result = await capture.start();
		assert.deepEqual(calls, [
			{
				sourceId: '12345',
				sourceKind: 'window',
				width: 1280,
				height: 720,
				frameRate: 30,
				captureId: undefined,
				captureOptions: {
					colorRange: 'full',
					colorSpace: 'rec709',
					showCursorClicks: true,
					captureRect: {x: 10, y: 20, width: 300, height: 200},
				},
			},
		]);
		assert.equal(result.pixelFormat, 'nv12');
		assert.equal(result.width, 1280);
		assert.equal(result.height, 720);
	});
	test('defaults sourceKind to screen and frameRate to 30', async () => {
		const {binding, calls} = makeFakeBinding();
		macScreenCapture.__setBindingForTests(binding);
		const capture = new macScreenCapture.ScreenCapture({sourceId: '1'});
		capture.on('error', () => {});
		await capture.start();
		assert.equal(calls[0].sourceKind, 'screen');
		assert.equal(calls[0].frameRate, 30);
	});
	test('forwards display and window sources from native binding without rewriting ids', async () => {
		const {binding} = makeFakeBinding({
			sources: [
				{kind: 'screen', id: 'display:69733632', name: 'Studio Display', width: 5120, height: 2880},
				{
					kind: 'window',
					id: 'window:4242',
					name: 'Fluxer',
					width: 1440,
					height: 900,
					appName: 'Fluxer',
					bundleId: 'app.fluxer.desktop',
					targetPid: 1234,
				},
			],
		});
		macScreenCapture.__setBindingForTests(binding);

		const sources = await macScreenCapture.listSources();

		assert.deepEqual(sources, [
			{kind: 'screen', id: 'display:69733632', name: 'Studio Display', width: 5120, height: 2880},
			{
				kind: 'window',
				id: 'window:4242',
				name: 'Fluxer',
				width: 1440,
				height: 900,
				appName: 'Fluxer',
				bundleId: 'app.fluxer.desktop',
				targetPid: 1234,
			},
		]);
	});
	test('reports ScreenCaptureKit support and permission from native binding', async () => {
		const {binding} = makeFakeBinding({
			availability: {
				sck: {supported: true, macosVersion: '15.0'},
				screenPermission: 'authorized',
			},
		});
		macScreenCapture.__setBindingForTests(binding);

		assert.deepEqual(await macScreenCapture.getBackendAvailability(), {
			sck: {supported: true, macosVersion: '15.0'},
			screenPermission: 'authorized',
		});
	});
	test('forwards the screen recording probe result from the native binding', async () => {
		const {binding} = makeFakeBinding();
		binding.probeScreenRecordingAccess = async () => 'granted';
		macScreenCapture.__setBindingForTests(binding);
		assert.equal(await macScreenCapture.probeScreenRecordingAccess(), 'granted');
	});
	test('reports the screen recording probe as unsupported without a binding that has it', async () => {
		macScreenCapture.__setBindingForTests(null);
		assert.equal(await macScreenCapture.probeScreenRecordingAccess(), 'unsupported');
		macScreenCapture.__setBindingForTests(makeFakeBinding().binding);
		assert.equal(await macScreenCapture.probeScreenRecordingAccess(), 'unsupported');
	});
	test('installs a native frame sink handle once before start', async () => {
		const {binding, calls, frameSinkHandleCalls} = makeFakeBinding();
		macScreenCapture.__setBindingForTests(binding);
		const frameSinkHandle = {native: true};
		const capture = new macScreenCapture.ScreenCapture({
			sourceId: '12345',
			sourceKind: 'window',
			frameSinkHandle,
			nativeFrameSinkRequired: true,
		});
		capture.on('error', () => {});

		await capture.start();

		assert.deepEqual(frameSinkHandleCalls, [frameSinkHandle]);
		assert.equal(calls.length, 1);
	});
	test('fails before native start when a native frame sink is required but missing', async () => {
		const {binding, calls, frameSinkHandleCalls} = makeFakeBinding();
		macScreenCapture.__setBindingForTests(binding);
		const capture = new macScreenCapture.ScreenCapture({
			sourceId: '12345',
			sourceKind: 'window',
			nativeFrameSinkRequired: true,
		});
		capture.on('error', () => {});

		await assert.rejects(() => capture.start(), /native frame sink handle is required/i);
		assert.deepEqual(frameSinkHandleCalls, []);
		assert.deepEqual(calls, []);
	});
	test('frame sink diagnostics are forwarded by the wrapper', () => {
		const {binding, frameSinkDiagnostics} = makeFakeBinding();
		macScreenCapture.__setBindingForTests(binding);
		const capture = new macScreenCapture.ScreenCapture({sourceId: '1'});

		frameSinkDiagnostics.accepted = 5;
		frameSinkDiagnostics.coalesced = 1;
		frameSinkDiagnostics.rejected = 2;
		frameSinkDiagnostics.mediaFramesDroppedWithoutSink = 3;

		assert.deepEqual(capture.getFrameSinkDiagnostics(), {
			accepted: 5,
			coalesced: 1,
			rejected: 2,
			mediaFramesDroppedWithoutSink: 3,
		});
	});
	test('emits closed once when native lifecycle closes and stop is called later', async () => {
		const {binding, natives} = makeFakeBinding();
		macScreenCapture.__setBindingForTests(binding);
		const capture = new macScreenCapture.ScreenCapture({sourceId: '1'});
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
	test('lifecycle error emits Error event', async () => {
		const {binding, natives} = makeFakeBinding();
		macScreenCapture.__setBindingForTests(binding);
		const capture = new macScreenCapture.ScreenCapture({sourceId: '1'});
		const errors = [];
		capture.on('error', (err) => errors.push(err));
		natives[0].lifecycleCallback('error', 'permission lost mid-stream');
		assert.equal(errors.length, 1);
		assert.equal(errors[0].message, 'permission lost mid-stream');
	});
	test('lifecycle diagnostic emits diagnostic event', () => {
		const {binding, natives} = makeFakeBinding();
		macScreenCapture.__setBindingForTests(binding);
		const capture = new macScreenCapture.ScreenCapture({sourceId: '1'});
		const diagnostics = [];
		capture.on('diagnostic', (message) => diagnostics.push(message));
		natives[0].lifecycleCallback('diagnostic', 'frame sink rejected a frame');
		assert.deepEqual(diagnostics, ['frame sink rejected a frame']);
	});
});
