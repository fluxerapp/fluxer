// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {afterEach, describe, it} from 'node:test';
import linuxScreenCapture from './index.js';

function makeFakeBinding({
	sources = [],
	availability = {
		available: true,
		backend: 'linux-pipewire-portal',
		detail: 'portal:5',
		capabilities: {process: true, system: true},
	},
} = {}) {
	const calls = [];
	const frameSinkHandleCalls = [];
	const natives = [];
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

		getDiagnostics() {
			return {
				portalSessionId: 'portal-session-1',
				width: 1280,
				height: 720,
			};
		}
	}
	return {
		binding: {
			ScreenCapture: FakeNative,
			listSources: async () => sources,
			getAvailability: async () => availability,
			getBackendInfo: () => ({
				backend: 'linux-pipewire-portal',
				supported: true,
				portalVersion: 5,
				pipewireReachable: true,
			}),
		},
		calls,
		frameSinkHandleCalls,
		natives,
	};
}

afterEach(() => {
	linuxScreenCapture.__setBindingForTests(null);
});

describe('linux-screen-capture package surface', () => {
	it('keeps obs-vkcapture runtime assets and license notes in the package surface', () => {
		const packageJson = JSON.parse(readFileSync(new URL('./package.json', import.meta.url), 'utf8'));
		assert.equal(packageJson.license, 'AGPL-3.0-or-later');
		assert(packageJson.files.includes('THIRD_PARTY_OBS_VKCAPTURE.md'));
		assert(packageJson.files.includes('obs-vkcapture/**/*'));

		const notice = readFileSync(new URL('./THIRD_PARTY_OBS_VKCAPTURE.md', import.meta.url), 'utf8');
		assert.match(notice, /separate third-party runtime component/);
		assert.match(notice, /exact upstream license text/);
		assert.match(notice, /corresponding source/);
	});
});

describe('linux-screen-capture loader wrapper', () => {
	it('forwards display and window sources from native binding without rewriting ids', async () => {
		const {binding} = makeFakeBinding({
			sources: [
				{kind: 'screen', id: 'pipewire:display:1', name: 'Display 1', width: 2560, height: 1440},
				{
					kind: 'window',
					id: 'pipewire:window:4242',
					name: 'Fluxer',
					width: 1280,
					height: 720,
					appName: 'Fluxer',
					targetPid: 1234,
				},
			],
		});
		linuxScreenCapture.__setBindingForTests(binding);

		assert.deepEqual(await linuxScreenCapture.listSources(), [
			{kind: 'screen', id: 'pipewire:display:1', name: 'Display 1', width: 2560, height: 1440},
			{
				kind: 'window',
				id: 'pipewire:window:4242',
				name: 'Fluxer',
				width: 1280,
				height: 720,
				appName: 'Fluxer',
				targetPid: 1234,
			},
		]);
	});

	it('forwards source id, kind, dimensions, and diagnostics to native binding', async () => {
		const {binding, calls} = makeFakeBinding();
		linuxScreenCapture.__setBindingForTests(binding);
		const displayCapture = new linuxScreenCapture.ScreenCapture({
			sourceId: 'pipewire:display:1',
			sourceKind: 'screen',
			width: 2560,
			height: 1440,
			frameRate: 60,
			captureId: 'capture-1',
			colorRange: 'full',
			colorSpace: 'rec709',
			showCursorClicks: true,
			captureRect: {x: 10, y: 20, width: 300, height: 200},
		});
		const windowCapture = new linuxScreenCapture.ScreenCapture({
			sourceId: 'pipewire:window:4242',
			sourceKind: 'window',
			width: 1280,
			height: 720,
			frameRate: 30,
		});

		await displayCapture.start();
		await windowCapture.start();

		assert.deepEqual(calls, [
			{
				sourceId: 'pipewire:display:1',
				sourceKind: 'screen',
				width: 2560,
				height: 1440,
				frameRate: 60,
				captureId: 'capture-1',
				captureOptions: {
					colorRange: 'full',
					colorSpace: 'rec709',
					showCursorClicks: true,
					captureRect: {x: 10, y: 20, width: 300, height: 200},
				},
			},
			{
				sourceId: 'pipewire:window:4242',
				sourceKind: 'window',
				width: 1280,
				height: 720,
				frameRate: 30,
				captureId: undefined,
				captureOptions: {
					colorRange: undefined,
					colorSpace: undefined,
					showCursorClicks: false,
					captureRect: undefined,
				},
			},
		]);
		assert.deepEqual(displayCapture.getDiagnostics(), {
			portalSessionId: 'portal-session-1',
			width: 1280,
			height: 720,
			sourceId: 'pipewire:display:1',
			sourceKind: 'screen',
		});
		assert.deepEqual(windowCapture.getDiagnostics(), {
			portalSessionId: 'portal-session-1',
			width: 1280,
			height: 720,
			sourceId: 'pipewire:window:4242',
			sourceKind: 'window',
		});
	});

	it('emits closed once for native closed-clean lifecycle events', async () => {
		const {binding, natives} = makeFakeBinding();
		linuxScreenCapture.__setBindingForTests(binding);
		const capture = new linuxScreenCapture.ScreenCapture({
			sourceId: 'pipewire:display:1',
			sourceKind: 'screen',
		});
		let closed = 0;
		capture.on('closed', () => {
			closed += 1;
		});

		await capture.start();
		natives[0].lifecycleCallback('closed-clean', 'capture stopped');
		await capture.stop();

		assert.equal(closed, 1);
		assert.equal(natives[0].stopCount, 1);
	});

	it('reports PipeWire portal capabilities from native binding', async () => {
		const {binding} = makeFakeBinding({
			availability: {
				available: true,
				backend: 'linux-pipewire-portal',
				detail: 'system capture disabled by portal',
				capabilities: {process: true, system: false},
			},
		});
		linuxScreenCapture.__setBindingForTests(binding);

		assert.deepEqual(await linuxScreenCapture.getAvailability(), {
			available: true,
			backend: 'linux-pipewire-portal',
			detail: 'system capture disabled by portal',
			capabilities: {process: true, system: false},
		});
	});

	it('installs a native frame sink handle once before start', async () => {
		const {binding, calls, frameSinkHandleCalls} = makeFakeBinding();
		linuxScreenCapture.__setBindingForTests(binding);
		const frameSinkHandle = {native: true};
		const capture = new linuxScreenCapture.ScreenCapture({
			sourceId: 'pipewire:display:1',
			sourceKind: 'screen',
			frameSinkHandle,
			nativeFrameSinkRequired: true,
		});

		await capture.start();

		assert.deepEqual(frameSinkHandleCalls, [frameSinkHandle]);
		assert.deepEqual(calls, [
			{
				sourceId: 'pipewire:display:1',
				sourceKind: 'screen',
				width: 0,
				height: 0,
				frameRate: 30,
				captureId: undefined,
				captureOptions: {
					colorRange: undefined,
					colorSpace: undefined,
					showCursorClicks: false,
					captureRect: undefined,
				},
			},
		]);
	});

	it('fails before native start when a native frame sink is required but missing', async () => {
		const {binding, calls, frameSinkHandleCalls} = makeFakeBinding();
		linuxScreenCapture.__setBindingForTests(binding);
		const capture = new linuxScreenCapture.ScreenCapture({
			sourceId: 'pipewire:display:1',
			sourceKind: 'screen',
			nativeFrameSinkRequired: true,
		});

		await assert.rejects(() => capture.start(), /native frame sink handle is required/);
		assert.deepEqual(frameSinkHandleCalls, []);
		assert.deepEqual(calls, []);
	});
});
