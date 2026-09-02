// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {selectVoiceEngineV2Model} from '../core';
import type {VoiceEngineV2Implementation} from '../implementations';
import {createVoiceEngineV2MemoryEventLogSpillSink, VoiceEngineV2Runtime} from '../runtime';
import type {FakeVoiceEngineV2Driver} from './FakeVoiceEngineV2Driver';

export interface VoiceEngineV2ConformanceSubject {
	implementation: VoiceEngineV2Implementation;
	driver: FakeVoiceEngineV2Driver;
}

export type VoiceEngineV2ConformanceSubjectFactory = () => VoiceEngineV2ConformanceSubject;

export function runVoiceEngineV2ConformanceSuite(
	label: string,
	createSubject: VoiceEngineV2ConformanceSubjectFactory,
): void {
	describe(`${label} VoiceEngineV2 implementation conformance`, () => {
		it('connects, publishes queued local media, and records deterministic calls', async () => {
			const {implementation, driver} = createSubject();
			const runtime = createConformanceRuntime(implementation);

			runtime.dispatch({type: 'microphone.publishRequested', options: {deviceId: 'default'}});
			runtime.dispatch({
				type: 'screen.publishRequested',
				options: {captureId: 'capture-1', width: 1920, height: 1080, codec: 'h264'},
			});
			runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}});
			await waitForRuntime();

			expect(selectVoiceEngineV2Model(runtime.snapshot)).toMatchObject({
				connection: {connected: true},
				media: {microphone: 'published', screen: 'published'},
			});
			expect(driver.calls).toEqual([
				{type: 'connect', options: {url: 'wss://voice', token: 'token'}},
				{type: 'publishMicrophone', options: {deviceId: 'default'}},
				{type: 'publishScreen', options: {captureId: 'capture-1', width: 1920, height: 1080, codec: 'h264'}},
			]);
		});

		it('ignores stale connect completion after a newer connect request', async () => {
			const {implementation} = createSubject();
			const runtime = createConformanceRuntime(implementation);

			const first = runtime.dispatch({
				type: 'connection.connectRequested',
				options: {url: 'wss://first', token: 'one'},
			});
			runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://second', token: 'two'}});
			runtime.dispatch({type: 'connection.connectSucceeded', operationId: first.commands[0]?.operationId ?? -1});

			expect(runtime.snapshot.connection.status).toBe('connecting');
			expect(runtime.snapshot.connection.active).toBeNull();
			expect(runtime.snapshot.connection.desired?.url).toBe('wss://second');
		});

		it('updates screen encoding without republishing the screen source', async () => {
			const {implementation, driver} = createSubject();
			const runtime = createConformanceRuntime(implementation);

			runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}});
			await waitForRuntime();
			runtime.dispatch({
				type: 'screen.publishRequested',
				options: {captureId: 'capture-1', width: 2560, height: 1440, codec: 'h264', maxFramerate: 60},
			});
			await waitForRuntime();
			runtime.dispatch({
				type: 'screen.updateEncodingRequested',
				options: {captureId: 'capture-1', width: 1280, height: 720, frameRate: 30, maxBitrateBps: 3_000_000},
			});
			await waitForRuntime();

			expect(driver.calls.map((call) => call.type)).toEqual(['connect', 'publishScreen', 'updateScreenEncoding']);
			expect(runtime.snapshot.screen.published).toMatchObject({
				captureId: 'capture-1',
				width: 1280,
				height: 720,
				maxFramerate: 30,
				maxBitrateBps: 3_000_000,
			});
		});

		it('adds and removes screen-share audio without republishing the screen video', async () => {
			const {implementation, driver} = createSubject();
			const runtime = createConformanceRuntime(implementation);

			runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}});
			await waitForRuntime();
			runtime.dispatch({
				type: 'screen.publishRequested',
				options: {captureId: 'capture-1', width: 1920, height: 1080, codec: 'h264', maxFramerate: 30},
			});
			await waitForRuntime();
			runtime.dispatch({type: 'screenAudio.publishRequested', options: {sampleRate: 48_000, numChannels: 2}});
			await waitForRuntime();
			runtime.dispatch({type: 'screenAudio.unpublishRequested'});
			await waitForRuntime();

			expect(driver.calls.map((call) => call.type)).toEqual([
				'connect',
				'publishScreen',
				'publishScreenAudio',
				'unpublishScreenAudio',
			]);
			expect(runtime.snapshot.screen.status).toBe('published');
			expect(runtime.snapshot.screen.published).toMatchObject({captureId: 'capture-1'});
			expect(runtime.snapshot.screenAudio.status).toBe('idle');
		});

		it('updates camera encoding in place without republishing the camera source', async () => {
			const {implementation, driver} = createSubject();
			const runtime = createConformanceRuntime(implementation);

			runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}});
			await waitForRuntime();
			runtime.dispatch({
				type: 'camera.publishRequested',
				options: {deviceId: 'camera-1', width: 1280, height: 720, mirror: false},
			});
			await waitForRuntime();
			runtime.dispatch({
				type: 'camera.updateEncodingRequested',
				options: {width: 1920, height: 1080, frameRate: 60, mirror: true, backgroundMode: 'blur'},
			});
			await waitForRuntime();

			expect(driver.calls.map((call) => call.type)).toEqual(['connect', 'publishCamera', 'updateCameraEncoding']);
			expect(runtime.snapshot.camera.published).toMatchObject({
				deviceId: 'camera-1',
				width: 1920,
				height: 1080,
				frameRate: 60,
				mirror: true,
				backgroundMode: 'blur',
			});
		});

		it('republishes the camera in place when the codec changes', async () => {
			const {implementation, driver} = createSubject();
			const runtime = createConformanceRuntime(implementation);

			runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}});
			await waitForRuntime();
			runtime.dispatch({type: 'camera.publishRequested', options: {deviceId: 'camera-1', codec: 'vp8'}});
			await waitForRuntime();
			runtime.dispatch({type: 'camera.updateEncodingRequested', options: {codec: 'vp9'}});
			await waitForRuntime();

			expect(driver.calls.map((call) => call.type)).toEqual(['connect', 'publishCamera', 'publishCamera']);
			expect(runtime.snapshot.camera.published).toMatchObject({deviceId: 'camera-1', codec: 'vp9'});
		});

		it('executes participant volume and remote track subscription commands deterministically', async () => {
			const {implementation, driver} = createSubject();
			const runtime = createConformanceRuntime(implementation);

			runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}});
			await waitForRuntime();
			runtime.dispatch({type: 'participantVolume.setRequested', options: {participantIdentity: 'alice', volume: 0.4}});
			await waitForRuntime();
			runtime.dispatch({
				type: 'remoteTrackSubscription.setRequested',
				options: {
					participantIdentity: 'alice',
					source: 'screen',
					subscribed: true,
					enabled: true,
					quality: 'high',
				},
			});
			await waitForRuntime();

			expect(driver.calls).toEqual([
				{type: 'connect', options: {url: 'wss://voice', token: 'token'}},
				{type: 'setParticipantVolume', options: {participantIdentity: 'alice', volume: 0.4}},
				{
					type: 'setRemoteTrackSubscription',
					options: {
						participantIdentity: 'alice',
						source: 'screen',
						subscribed: true,
						enabled: true,
						quality: 'high',
					},
				},
			]);
		});
	});
}

function createConformanceRuntime(implementation: VoiceEngineV2Implementation): VoiceEngineV2Runtime {
	return new VoiceEngineV2Runtime(implementation, {
		eventLogSpillSink: createVoiceEngineV2MemoryEventLogSpillSink(),
		verifyEventLogInvariantsOnDispatch: true,
	});
}

export function waitForRuntime(): Promise<void> {
	return flushRuntimeMicrotasks(32);
}

async function flushRuntimeMicrotasks(count: number): Promise<void> {
	for (let i = 0; i < count; i++) {
		await Promise.resolve();
	}
}
