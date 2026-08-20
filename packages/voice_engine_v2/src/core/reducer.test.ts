// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import type {VoiceEngineV2Event} from '../protocol/events';
import type {VoiceEngineV2DiagnosticEntry} from '../protocol/types';
import {transitionVoiceEngineV2} from './reducer';
import {VOICE_ENGINE_V2_TERMINAL_OPERATIONS_KEPT_MAX} from './reducers/_helpers';
import {selectVoiceEngineV2FailedSourceIds, selectVoiceEngineV2SourceLifecycle} from './selectors';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	unavailableVoiceEngineV2Capabilities,
	type VoiceEngineV2Snapshot,
} from './state';

function initialSnapshot(): VoiceEngineV2Snapshot {
	return createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
}

function applyEvents(snapshot: VoiceEngineV2Snapshot, events: Array<VoiceEngineV2Event>): VoiceEngineV2Snapshot {
	let next = snapshot;
	for (const event of events) {
		next = transitionVoiceEngineV2(next, event).snapshot;
	}
	return next;
}

describe('transitionVoiceEngineV2', () => {
	it('keeps business state explicit and starts from idle', () => {
		const snapshot = initialSnapshot();

		expect(snapshot.connection.status).toBe('idle');
		expect(snapshot.microphone.status).toBe('idle');
		expect(snapshot.camera.status).toBe('idle');
		expect(snapshot.screen.status).toBe('idle');
		expect(snapshot.screenAudio.status).toBe('idle');
		expect(snapshot.nextOperationId).toBe(1);
	});

	it('emits a single connect command for a connect request', () => {
		const transition = transitionVoiceEngineV2(initialSnapshot(), {
			type: 'connection.connectRequested',
			options: {url: 'wss://voice', token: 'token'},
		});

		expect(transition.snapshot.connection.status).toBe('connecting');
		expect(transition.snapshot.connection.operationId).toBe(1);
		expect(transition.commands).toEqual([
			{type: 'connection.connect', operationId: 1, options: {url: 'wss://voice', token: 'token'}},
		]);
	});

	it('ignores stale connect completions', () => {
		let snapshot = initialSnapshot();
		const first = transitionVoiceEngineV2(snapshot, {
			type: 'connection.connectRequested',
			options: {url: 'wss://first', token: 'one'},
		});
		snapshot = first.snapshot;
		const second = transitionVoiceEngineV2(snapshot, {
			type: 'connection.connectRequested',
			options: {url: 'wss://second', token: 'two'},
		});
		snapshot = second.snapshot;

		const stale = transitionVoiceEngineV2(snapshot, {
			type: 'connection.connectSucceeded',
			operationId: first.commands[0]?.operationId ?? -1,
		});

		expect(stale.snapshot.connection.status).toBe('connecting');
		expect(stale.snapshot.connection.active).toBeNull();
		expect(stale.snapshot.connection.desired?.url).toBe('wss://second');
		expect(stale.commands).toEqual([]);
	});

	it('publishes queued media after connection succeeds', () => {
		const queued = applyEvents(initialSnapshot(), [
			{type: 'microphone.publishRequested', options: {deviceId: 'default'}},
			{
				type: 'screen.publishRequested',
				options: {captureId: 'capture-1', width: 1920, height: 1080, codec: 'h264'},
			},
		]);
		const connecting = transitionVoiceEngineV2(queued, {
			type: 'connection.connectRequested',
			options: {url: 'wss://voice', token: 'token'},
		}).snapshot;

		const connected = transitionVoiceEngineV2(connecting, {type: 'connection.connectSucceeded', operationId: 1});

		expect(connected.snapshot.connection.status).toBe('connected');
		expect(connected.snapshot.microphone.status).toBe('publishing');
		expect(connected.snapshot.screen.status).toBe('publishing');
		expect(connected.commands).toEqual([
			{type: 'microphone.publish', operationId: 2, options: {deviceId: 'default'}},
			{
				type: 'screen.publish',
				operationId: 3,
				options: {captureId: 'capture-1', width: 1920, height: 1080, codec: 'h264'},
			},
		]);
	});

	it('marks the snapshot connected and publishes queued media on connection.externallyEstablished', () => {
		const queued = applyEvents(initialSnapshot(), [
			{type: 'microphone.publishRequested', options: {deviceId: 'default'}},
		]);

		const established = transitionVoiceEngineV2(queued, {
			type: 'connection.externallyEstablished',
			options: {url: 'wss://voice', token: ''},
		});

		expect(established.snapshot.connection.status).toBe('connected');
		expect(established.snapshot.connection.active?.url).toBe('wss://voice');
		expect(established.snapshot.connection.operationId).toBeNull();
		expect(established.snapshot.microphone.status).toBe('publishing');
		expect(established.commands).toEqual([
			{type: 'microphone.publish', operationId: 1, options: {deviceId: 'default'}},
		]);
	});

	it('treats a repeated connection.externallyEstablished for the same endpoint as a no-op', () => {
		const established = applyEvents(initialSnapshot(), [
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
		]);

		const repeated = transitionVoiceEngineV2(established, {
			type: 'connection.externallyEstablished',
			options: {url: 'wss://voice', token: ''},
		});

		expect(repeated.snapshot).toBe(established);
		expect(repeated.commands).toEqual([]);
	});

	it('preserves published camera when the same endpoint is re-established after a resume', () => {
		const published = applyEvents(initialSnapshot(), [
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
			{type: 'camera.publishRequested', options: {deviceId: 'camera-1'}},
			{type: 'camera.publishSucceeded', operationId: 1},
		]);

		const resumed = transitionVoiceEngineV2(published, {
			type: 'connection.externallyEstablished',
			options: {url: 'wss://voice', token: ''},
		});

		expect(resumed.snapshot).toBe(published);
		expect(resumed.commands).toEqual([]);
		expect(resumed.snapshot.camera.status).toBe('published');
		expect(resumed.snapshot.camera.published?.deviceId).toBe('camera-1');
	});

	it('does not replay already-published media when re-established on a new endpoint', () => {
		const published = applyEvents(initialSnapshot(), [
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
			{type: 'camera.publishRequested', options: {deviceId: 'camera-1'}},
			{type: 'camera.publishSucceeded', operationId: 1},
		]);

		const reestablished = transitionVoiceEngineV2(published, {
			type: 'connection.externallyEstablished',
			options: {url: 'wss://voice-2', token: ''},
		});

		expect(reestablished.snapshot.connection.status).toBe('connected');
		expect(reestablished.snapshot.connection.active?.url).toBe('wss://voice-2');
		expect(reestablished.snapshot.camera.status).toBe('published');
		expect(reestablished.commands).toEqual([]);
	});

	it('plans a real camera unpublish command after a resume re-establishment', () => {
		const resumed = applyEvents(initialSnapshot(), [
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
			{type: 'camera.publishRequested', options: {deviceId: 'camera-1'}},
			{type: 'camera.publishSucceeded', operationId: 1},
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
		]);

		const unpublish = transitionVoiceEngineV2(resumed, {type: 'camera.unpublishRequested'});

		expect(unpublish.snapshot.camera.status).toBe('unpublishing');
		expect(unpublish.snapshot.camera.desired).toBeNull();
		expect(unpublish.commands).toEqual([{type: 'camera.unpublish', operationId: 2, options: undefined}]);
	});

	it('still resets published camera on a true remote disconnect after a resume', () => {
		const resumed = applyEvents(initialSnapshot(), [
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
			{type: 'camera.publishRequested', options: {deviceId: 'camera-1'}},
			{type: 'camera.publishSucceeded', operationId: 1},
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
		]);

		const disconnected = transitionVoiceEngineV2(resumed, {type: 'connection.remoteDisconnected', reason: 'network'});

		expect(disconnected.snapshot.connection.status).toBe('idle');
		expect(disconnected.snapshot.camera.status).toBe('idle');
		expect(disconnected.snapshot.camera.published).toBeNull();
		expect(disconnected.commands).toEqual([]);
	});

	it('returns to idle and gates media planning again after connection.remoteDisconnected', () => {
		const established = applyEvents(initialSnapshot(), [
			{type: 'microphone.publishRequested', options: {deviceId: 'default'}},
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
			{type: 'microphone.publishSucceeded', operationId: 1},
		]);

		const disconnected = transitionVoiceEngineV2(established, {
			type: 'connection.remoteDisconnected',
			reason: 'network',
		});

		expect(disconnected.snapshot.connection.status).toBe('idle');
		expect(disconnected.snapshot.microphone.status).toBe('idle');
		expect(disconnected.commands).toEqual([]);

		const gated = transitionVoiceEngineV2(disconnected.snapshot, {
			type: 'camera.publishRequested',
			options: {},
		});

		expect(gated.commands).toEqual([]);
	});

	it('clears stale screen desired on a stop issued in failed status so re-establishment does not republish', () => {
		const failed = applyEvents(initialSnapshot(), [
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
			{type: 'screen.publishRequested', options: {captureId: 'capture-1', width: 1920, height: 1080}},
			{type: 'screen.publishFailed', operationId: 1, error: {code: 'implementationError', message: 'boom'}},
		]);
		expect(failed.screen.status).toBe('failed');
		expect(failed.screen.desired).not.toBeNull();

		const stopped = transitionVoiceEngineV2(failed, {type: 'screen.unpublishRequested'});
		expect(stopped.snapshot.screen.desired).toBeNull();
		expect(stopped.commands).toEqual([{type: 'screen.unpublish', operationId: 2}]);

		const idle = applyEvents(stopped.snapshot, [
			{type: 'screen.unpublishSucceeded', operationId: 2},
			{type: 'connection.remoteDisconnected', reason: 'network'},
		]);
		const reestablished = transitionVoiceEngineV2(idle, {
			type: 'connection.externallyEstablished',
			options: {url: 'wss://voice', token: ''},
		});
		expect(reestablished.commands.filter((command) => command.type === 'screen.publish')).toEqual([]);
	});

	it('replans a screen publish from stale desired on endpoint re-establishment when no stop intervenes', () => {
		const failed = applyEvents(initialSnapshot(), [
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
			{type: 'screen.publishRequested', options: {captureId: 'capture-1', width: 1920, height: 1080}},
			{type: 'screen.publishFailed', operationId: 1, error: {code: 'implementationError', message: 'boom'}},
		]);

		const reestablished = transitionVoiceEngineV2(failed, {
			type: 'connection.externallyEstablished',
			options: {url: 'wss://voice-2', token: ''},
		});

		expect(reestablished.commands.map((command) => command.type)).toContain('screen.publish');
	});

	it('does not replan a screen publish on endpoint re-establishment after a stop in failed status', () => {
		const stopped = applyEvents(initialSnapshot(), [
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
			{type: 'screen.publishRequested', options: {captureId: 'capture-1', width: 1920, height: 1080}},
			{type: 'screen.publishFailed', operationId: 1, error: {code: 'implementationError', message: 'boom'}},
			{type: 'screen.unpublishRequested'},
			{type: 'screen.unpublishSucceeded', operationId: 2},
		]);
		expect(stopped.screen.desired).toBeNull();

		const reestablished = transitionVoiceEngineV2(stopped, {
			type: 'connection.externallyEstablished',
			options: {url: 'wss://voice-2', token: ''},
		});

		expect(reestablished.commands.filter((command) => command.type === 'screen.publish')).toEqual([]);
	});

	it('does not persist a user mute when a microphone enable fails during a livekit reconnect', () => {
		const reconnecting = applyEvents(initialSnapshot(), [
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
			{type: 'microphone.publishRequested', options: {deviceId: 'default'}},
			{type: 'microphone.publishSucceeded', operationId: 1},
			{type: 'microphone.setEnabledRequested', enabled: true},
			{
				type: 'livekit.roomStateChanged',
				room: {connectionState: 'reconnecting', roomSid: null, roomName: null, serverRegion: null},
			},
		]);
		expect(reconnecting.microphone.setEnabledOperationId).toBe(2);

		const failed = transitionVoiceEngineV2(reconnecting, {
			type: 'microphone.setEnabledFailed',
			operationId: 2,
			error: {code: 'implementationError', message: 'engine resume in progress'},
		});

		expect(failed.snapshot.audioControls.hasUserSetMute).toBe(false);
		expect(failed.snapshot.audioControls.preferredLocallyMuted).toBe(false);
		expect(failed.snapshot.audioControls.locallyMuted).toBe(false);
		expect(failed.snapshot.microphone.failure?.message).toBe('engine resume in progress');
	});

	it('still persists the mute recovery when a microphone enable fails on a stable connection', () => {
		const stable = applyEvents(initialSnapshot(), [
			{type: 'connection.externallyEstablished', options: {url: 'wss://voice', token: ''}},
			{type: 'microphone.publishRequested', options: {deviceId: 'default'}},
			{type: 'microphone.publishSucceeded', operationId: 1},
			{type: 'microphone.setEnabledRequested', enabled: true},
		]);

		const failed = transitionVoiceEngineV2(stable, {
			type: 'microphone.setEnabledFailed',
			operationId: 2,
			error: {code: 'implementationError', message: 'device unavailable'},
		});

		expect(failed.snapshot.audioControls.hasUserSetMute).toBe(true);
		expect(failed.snapshot.audioControls.preferredLocallyMuted).toBe(true);
		expect(failed.snapshot.audioControls.locallyMuted).toBe(true);
	});

	it('does not emit duplicate microphone publish commands for an already-published config', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{type: 'microphone.publishRequested', options: {deviceId: 'default'}},
			{type: 'microphone.publishSucceeded', operationId: 2},
		]);

		const duplicate = transitionVoiceEngineV2(snapshot, {
			type: 'microphone.publishRequested',
			options: {deviceId: 'default'},
		});

		expect(duplicate.snapshot.microphone.status).toBe('published');
		expect(duplicate.commands).toEqual([]);
	});

	it('replans a microphone publish when only maxBitrateBps changes', () => {
		const published = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{type: 'microphone.publishRequested', options: {deviceId: 'default', maxBitrateBps: 32_000}},
			{type: 'microphone.publishSucceeded', operationId: 2},
		]);

		const replanned = transitionVoiceEngineV2(published, {
			type: 'microphone.publishRequested',
			options: {deviceId: 'default', maxBitrateBps: 64_000},
		});

		expect(replanned.snapshot.microphone.status).toBe('publishing');
		expect(replanned.commands).toEqual([
			{type: 'microphone.publish', operationId: 3, options: {deviceId: 'default', maxBitrateBps: 64_000}},
		]);

		const succeeded = transitionVoiceEngineV2(replanned.snapshot, {
			type: 'microphone.publishSucceeded',
			operationId: 3,
		});

		expect(succeeded.snapshot.microphone.status).toBe('published');
		expect(succeeded.snapshot.microphone.published?.maxBitrateBps).toBe(64_000);
	});

	it('does not supersede an in-flight camera publish for the same config', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{type: 'camera.publishRequested', options: {deviceId: 'camera-1'}},
		]);

		const duplicate = transitionVoiceEngineV2(snapshot, {
			type: 'camera.publishRequested',
			options: {deviceId: 'camera-1'},
		});

		expect(duplicate.snapshot.camera.status).toBe('publishing');
		expect(duplicate.snapshot.camera.operationId).toBe(2);
		expect(duplicate.commands).toEqual([]);
	});

	it('updates screen encoding in place for a matching capture id', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{
				type: 'screen.publishRequested',
				options: {captureId: 'capture-1', width: 2560, height: 1440, codec: 'h264', maxFramerate: 60},
			},
			{type: 'screen.publishSucceeded', operationId: 2},
		]);

		const updated = transitionVoiceEngineV2(snapshot, {
			type: 'screen.updateEncodingRequested',
			options: {captureId: 'capture-1', width: 1280, height: 720, frameRate: 30, maxBitrateBps: 3_000_000},
		});

		expect(updated.commands).toEqual([
			{
				type: 'screen.updateEncoding',
				operationId: 3,
				options: {captureId: 'capture-1', width: 1280, height: 720, frameRate: 30, maxBitrateBps: 3_000_000},
			},
		]);
		expect(updated.snapshot.screen.desired).toMatchObject({
			captureId: 'capture-1',
			width: 1280,
			height: 720,
			maxFramerate: 30,
			maxBitrateBps: 3_000_000,
		});
	});

	it('republishes screen share when codec or hardware encoder mode changes', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{
				type: 'screen.publishRequested',
				options: {
					captureId: 'capture-1',
					width: 1920,
					height: 1080,
					codec: 'h264',
					hardwareEncoding: true,
					zeroCopyRequired: true,
					maxFramerate: 60,
				},
			},
			{type: 'screen.publishSucceeded', operationId: 2},
		]);

		const updated = transitionVoiceEngineV2(snapshot, {
			type: 'screen.updateEncodingRequested',
			options: {
				captureId: 'capture-1',
				width: 1920,
				height: 1080,
				codec: 'h265',
				hardwareEncoding: true,
				zeroCopyRequired: true,
				frameRate: 60,
			},
		});

		expect(updated.commands).toEqual([
			{
				type: 'screen.publish',
				operationId: 3,
				options: {
					captureId: 'capture-1',
					width: 1920,
					height: 1080,
					codec: 'h265',
					hardwareEncoding: true,
					zeroCopyRequired: true,
					maxFramerate: 60,
				},
			},
		]);
	});

	it('adjusts screen-share audio inclusion without touching the screen video publication', () => {
		const withScreen = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{type: 'screen.publishRequested', options: {captureId: 'capture-1', width: 1920, height: 1080, codec: 'h264'}},
			{type: 'screen.publishSucceeded', operationId: 2},
		]);
		const screenOperationId = withScreen.screen.operationId;
		const screenPublished = withScreen.screen.published;

		const audioAdded = applyEvents(withScreen, [
			{type: 'screenAudio.publishRequested', options: {sampleRate: 48000, numChannels: 2}},
			{type: 'screenAudio.publishSucceeded', operationId: 3},
		]);
		expect(audioAdded.screenAudio.status).toBe('published');
		expect(audioAdded.screen.status).toBe('published');
		expect(audioAdded.screen.published).toBe(screenPublished);
		expect(audioAdded.screen.operationId).toBe(screenOperationId);

		const audioRemoved = applyEvents(audioAdded, [
			{type: 'screenAudio.unpublishRequested'},
			{type: 'screenAudio.unpublishSucceeded', operationId: 4},
		]);
		expect(audioRemoved.screenAudio.status).toBe('idle');
		expect(audioRemoved.screen.status).toBe('published');
		expect(audioRemoved.screen.published).toBe(screenPublished);
		expect(audioRemoved.screen.operationId).toBe(screenOperationId);
	});

	it('changes screen frame rate in place without republishing the screen source', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{
				type: 'screen.publishRequested',
				options: {captureId: 'capture-1', width: 1920, height: 1080, codec: 'h264', maxFramerate: 30},
			},
			{type: 'screen.publishSucceeded', operationId: 2},
		]);

		const updated = transitionVoiceEngineV2(snapshot, {
			type: 'screen.updateEncodingRequested',
			options: {captureId: 'capture-1', width: 1920, height: 1080, frameRate: 60},
		});

		expect(updated.commands).toEqual([
			{
				type: 'screen.updateEncoding',
				operationId: 3,
				options: {captureId: 'capture-1', width: 1920, height: 1080, frameRate: 60},
			},
		]);
		expect(updated.snapshot.screen.desired?.maxFramerate).toBe(60);
	});

	it('updates camera encoding in place for mirror, background, resolution, and frame rate', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{
				type: 'camera.publishRequested',
				options: {deviceId: 'camera-1', width: 1280, height: 720, frameRate: 30, mirror: false},
			},
			{type: 'camera.publishSucceeded', operationId: 2},
		]);

		const updated = transitionVoiceEngineV2(snapshot, {
			type: 'camera.updateEncodingRequested',
			options: {
				width: 1920,
				height: 1080,
				frameRate: 60,
				mirror: true,
				backgroundMode: 'blur',
				backgroundBlurStrength: 80,
			},
		});

		expect(updated.commands).toEqual([
			{
				type: 'camera.updateEncoding',
				operationId: 3,
				options: {
					width: 1920,
					height: 1080,
					frameRate: 60,
					mirror: true,
					backgroundMode: 'blur',
					backgroundBlurStrength: 80,
				},
			},
		]);
		expect(updated.snapshot.camera.status).toBe('publishing');
		expect(updated.snapshot.camera.operationId).toBe(3);
		expect(updated.snapshot.camera.desired).toMatchObject({
			deviceId: 'camera-1',
			width: 1920,
			height: 1080,
			frameRate: 60,
			mirror: true,
			backgroundMode: 'blur',
			backgroundBlurStrength: 80,
		});
	});

	it('keeps the same camera publication identity when updating encoding in place', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{type: 'camera.publishRequested', options: {deviceId: 'camera-1', width: 1280, height: 720, mirror: false}},
			{type: 'camera.publishSucceeded', operationId: 2},
		]);

		const updated = transitionVoiceEngineV2(snapshot, {
			type: 'camera.updateEncodingRequested',
			options: {mirror: true},
		});
		const succeeded = transitionVoiceEngineV2(updated.snapshot, {
			type: 'camera.updateEncodingSucceeded',
			operationId: 3,
		});

		expect(updated.commands).toEqual([{type: 'camera.updateEncoding', operationId: 3, options: {mirror: true}}]);
		expect(succeeded.snapshot.camera.status).toBe('published');
		expect(succeeded.snapshot.camera.published).toMatchObject({deviceId: 'camera-1', mirror: true});
	});

	it('republishes the camera when the device id or codec changes', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{type: 'camera.publishRequested', options: {deviceId: 'camera-1', width: 1280, height: 720, codec: 'vp8'}},
			{type: 'camera.publishSucceeded', operationId: 2},
		]);

		const deviceChange = transitionVoiceEngineV2(snapshot, {
			type: 'camera.updateEncodingRequested',
			options: {deviceId: 'camera-2'},
		});
		expect(deviceChange.commands).toEqual([
			{
				type: 'camera.publish',
				operationId: 3,
				options: {deviceId: 'camera-2', width: 1280, height: 720, codec: 'vp8'},
			},
		]);

		const codecChange = transitionVoiceEngineV2(snapshot, {
			type: 'camera.updateEncodingRequested',
			options: {codec: 'vp9'},
		});
		expect(codecChange.commands).toEqual([
			{
				type: 'camera.publish',
				operationId: 3,
				options: {deviceId: 'camera-1', width: 1280, height: 720, codec: 'vp9'},
			},
		]);
	});

	it('rejects a camera encoding update with no published camera and emits no command', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
		]);

		const updated = transitionVoiceEngineV2(snapshot, {
			type: 'camera.updateEncodingRequested',
			options: {mirror: true},
		});

		expect(updated.commands).toEqual([]);
		expect(updated.snapshot.camera.failure?.code).toBe('invalidArgument');
		expect(updated.snapshot.camera.status).toBe('idle');
	});

	it('treats a no-op camera encoding update as desired-only with no command', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{type: 'camera.publishRequested', options: {deviceId: 'camera-1', width: 1280, height: 720, mirror: true}},
			{type: 'camera.publishSucceeded', operationId: 2},
		]);

		const updated = transitionVoiceEngineV2(snapshot, {
			type: 'camera.updateEncodingRequested',
			options: {width: 1280, height: 720, mirror: true},
		});

		expect(updated.commands).toEqual([]);
		expect(updated.snapshot.camera.status).toBe('published');
	});

	it('replays desired session configuration after connect succeeds', () => {
		const queued = applyEvents(initialSnapshot(), [
			{type: 'outputDevice.setRequested', options: {deviceId: 'speaker-1'}},
			{type: 'participantVolume.setRequested', options: {participantIdentity: 'user-b', volume: 0.4}},
			{type: 'participantVolume.setRequested', options: {participantIdentity: 'user-a', volume: 0.8}},
			{
				type: 'remoteTrackSubscription.setRequested',
				options: {participantIdentity: 'user-b', source: 'screen', subscribed: true, quality: 'high'},
			},
		]);
		const connecting = transitionVoiceEngineV2(queued, {
			type: 'connection.connectRequested',
			options: {url: 'wss://voice', token: 'token'},
		}).snapshot;

		const connected = transitionVoiceEngineV2(connecting, {type: 'connection.connectSucceeded', operationId: 1});

		expect(connected.commands).toEqual([
			{type: 'outputDevice.set', operationId: 2, options: {deviceId: 'speaker-1'}},
			{type: 'participantVolume.set', operationId: 3, options: {participantIdentity: 'user-a', volume: 0.8}},
			{type: 'participantVolume.set', operationId: 4, options: {participantIdentity: 'user-b', volume: 0.4}},
			{
				type: 'remoteTrackSubscription.set',
				operationId: 5,
				options: {participantIdentity: 'user-b', source: 'screen', subscribed: true, quality: 'high'},
			},
		]);
	});

	it('rejects native capture requests that do not require zero-copy transport', () => {
		const transition = transitionVoiceEngineV2(initialSnapshot(), {
			type: 'nativeCapture.startRequested',
			options: {
				captureId: 'capture-1',
				source: {id: 'display-1', kind: 'screen', title: 'Display 1'},
				width: 1920,
				height: 1080,
				frameRate: 60,
				includeCursor: true,
				includeAudio: false,
				zeroCopyRequired: false,
			},
		} as unknown as VoiceEngineV2Event);

		expect(transition.commands).toEqual([]);
		expect(transition.snapshot.nativeCapture.failure).toMatchObject({
			code: 'implementationError',
			capability: 'zeroCopyScreenTransport',
		});
	});

	it('rejects native capture requests when zero-copy transport is unavailable', () => {
		const transition = transitionVoiceEngineV2(
			createVoiceEngineV2InitialSnapshot(unavailableVoiceEngineV2Capabilities()),
			{
				type: 'nativeCapture.startRequested',
				options: {
					captureId: 'capture-1',
					source: {id: 'display-1', kind: 'screen', title: 'Display 1'},
					width: 1920,
					height: 1080,
					frameRate: 60,
					includeCursor: true,
					includeAudio: false,
					zeroCopyRequired: true,
				},
			},
		);

		expect(transition.commands).toEqual([]);
		expect(transition.snapshot.nativeCapture.failure).toMatchObject({
			code: 'unsupportedCapability',
			capability: 'zeroCopyScreenTransport',
		});
	});

	it('rejects native frame sinks that do not require zero-copy transport', () => {
		const transition = transitionVoiceEngineV2(initialSnapshot(), {
			type: 'nativeFrameSink.attachRequested',
			options: {
				sinkId: 'sink-1',
				captureId: 'capture-1',
				trackSid: 'TR_screen',
				zeroCopyRequired: false,
			},
		} as unknown as VoiceEngineV2Event);

		expect(transition.commands).toEqual([]);
		expect(transition.snapshot.nativeFrameSink.failure).toMatchObject({
			code: 'implementationError',
			capability: 'zeroCopyScreenTransport',
		});
	});

	it('rejects non-zero-copy native frame events', () => {
		const transition = transitionVoiceEngineV2(initialSnapshot(), {
			type: 'nativeCapture.frame',
			frame: {
				captureId: 'capture-1',
				frameId: 'frame-1',
				width: 1920,
				height: 1080,
				timestampMs: 123,
				format: 'native',
				zeroCopy: false,
			},
		} as unknown as VoiceEngineV2Event);

		expect(transition.commands).toEqual([]);
		expect(transition.snapshot.nativeCapture.failure).toMatchObject({
			code: 'implementationError',
			capability: 'zeroCopyScreenTransport',
		});
	});

	it('rejects native hardware encoder usage without explicit zero-copy transport', () => {
		const transition = transitionVoiceEngineV2(initialSnapshot(), {
			type: 'screen.publishRequested',
			options: {
				captureId: 'capture-1',
				width: 1920,
				height: 1080,
				codec: 'h264',
				hardwareEncoding: true,
			},
		});

		expect(transition.commands).toEqual([]);
		expect(transition.snapshot.screen.status).toBe('failed');
		expect(transition.snapshot.screen.failure).toMatchObject({
			code: 'implementationError',
			capability: 'zeroCopyScreenTransport',
		});
	});

	it('sourceLifecycle.transitioned from active to reconnecting populates the sourceLifecycles slot', () => {
		const transition = transitionVoiceEngineV2(initialSnapshot(), {
			type: 'sourceLifecycle.transitioned',
			sourceId: 'source-4',
			kind: 'reconnecting',
			since: 1234n,
			attempts: 2,
			fault: 'networkError',
			atMs: 100,
		});

		expect(transition.commands).toEqual([]);
		const state = selectVoiceEngineV2SourceLifecycle(transition.snapshot, 'source-4');
		expect(state).not.toBeNull();
		expect(state?.kind).toBe('reconnecting');
		if (state?.kind === 'reconnecting') {
			expect(state.attempts).toBe(2);
			expect(state.lastFault).toBe('networkError');
			expect(state.since).toBe(1234n);
		}
	});

	it('sourceLifecycle.transitioned to failed emits a diagnostics.log command', () => {
		const transition = transitionVoiceEngineV2(initialSnapshot(), {
			type: 'sourceLifecycle.transitioned',
			sourceId: 'source-5',
			kind: 'failed',
			since: 5000n,
			attempts: 8,
			fault: 'encoderError',
			atMs: 250,
		});

		expect(transition.commands).toHaveLength(1);
		expect(transition.commands[0]?.type).toBe('diagnostics.log');
		const command = transition.commands[0];
		if (command?.type === 'diagnostics.log') {
			expect(command.entry.code).toBe('sourceFailed');
			expect(command.entry.level).toBe('error');
			expect(command.entry.atMs).toBe(250);
		}
		const state = selectVoiceEngineV2SourceLifecycle(transition.snapshot, 'source-5');
		expect(state?.kind).toBe('failed');
	});

	it('selectors return null for unknown sources', () => {
		const snapshot = initialSnapshot();
		expect(selectVoiceEngineV2SourceLifecycle(snapshot, 'unknown')).toBeNull();
	});

	it('selectVoiceEngineV2FailedSourceIds returns only failed lifecycle ids', () => {
		const snapshot = applyEvents(initialSnapshot(), [
			{
				type: 'sourceLifecycle.transitioned',
				sourceId: 'source-a',
				kind: 'active',
				since: 1n,
				attempts: 0,
				fault: null,
				atMs: 1,
			},
			{
				type: 'sourceLifecycle.transitioned',
				sourceId: 'source-b',
				kind: 'failed',
				since: 2n,
				attempts: 8,
				fault: 'gpuDeviceLost',
				atMs: 2,
			},
			{
				type: 'sourceLifecycle.transitioned',
				sourceId: 'source-c',
				kind: 'failed',
				since: 3n,
				attempts: 8,
				fault: 'captureDeviceLost',
				atMs: 3,
			},
		]);

		const failed = selectVoiceEngineV2FailedSourceIds(snapshot);
		expect([...failed]).toEqual(['source-b', 'source-c']);
	});

	it('replaying the same source lifecycle event sequence yields the same snapshot hash', () => {
		const events: Array<VoiceEngineV2Event> = [
			{
				type: 'sourceLifecycle.transitioned',
				sourceId: 'source-d',
				kind: 'active',
				since: 4n,
				attempts: 0,
				fault: null,
				atMs: 1,
			},
			{
				type: 'sourceLifecycle.transitioned',
				sourceId: 'source-d',
				kind: 'reconnecting',
				since: 7n,
				attempts: 1,
				fault: 'networkError',
				atMs: 5,
			},
			{
				type: 'sourceLifecycle.transitioned',
				sourceId: 'source-d',
				kind: 'failed',
				since: 12n,
				attempts: 8,
				fault: 'encoderError',
				atMs: 9,
			},
		];

		const first = applyEvents(initialSnapshot(), events);
		const second = applyEvents(initialSnapshot(), events);
		const replacer = (_key: string, value: unknown): unknown =>
			typeof value === 'bigint' ? `bigint:${value.toString()}` : value;

		expect(JSON.stringify(second, replacer)).toBe(JSON.stringify(first, replacer));
	});
});

describe('terminal operation pruning', () => {
	function diagnosticEntry(index: number): VoiceEngineV2DiagnosticEntry {
		return {
			id: `diagnostic-${index}`,
			atMs: index,
			level: 'info',
			code: 'test.diagnostic',
			message: `diagnostic ${index}`,
		};
	}

	function completeDiagnosticCycles(snapshot: VoiceEngineV2Snapshot, total: number): VoiceEngineV2Snapshot {
		let next = snapshot;
		for (let index = 0; index < total; index += 1) {
			const requested = transitionVoiceEngineV2(next, {
				type: 'diagnostics.logRequested',
				entry: diagnosticEntry(index),
			});
			next = requested.snapshot;
			const command = requested.commands[0];
			if (command === undefined) throw new Error('expected a queued diagnostics command');
			next = transitionVoiceEngineV2(next, {
				type: 'command.succeeded',
				operationId: command.operationId,
				commandType: command.type,
			}).snapshot;
		}
		return next;
	}

	it('keeps at most the named cap of terminal operations, dropping the oldest by operation id', () => {
		const overflow = 10;
		const total = VOICE_ENGINE_V2_TERMINAL_OPERATIONS_KEPT_MAX + overflow;
		const snapshot = completeDiagnosticCycles(initialSnapshot(), total);

		const operations = Object.values(snapshot.operations);
		expect(operations).toHaveLength(VOICE_ENGINE_V2_TERMINAL_OPERATIONS_KEPT_MAX);
		const operationIds = operations.map((operation) => operation.operationId).sort((a, b) => a - b);
		expect(operationIds[0]).toBe(overflow + 1);
		expect(operationIds[operationIds.length - 1]).toBe(total);
		for (const operation of operations) {
			expect(operation.status).toBe('succeeded');
		}
	});

	it('never prunes pending operations even when the terminal cap is exceeded', () => {
		let snapshot = completeDiagnosticCycles(initialSnapshot(), VOICE_ENGINE_V2_TERMINAL_OPERATIONS_KEPT_MAX + 5);
		const pendingTotal = 10;
		const pendingOperationIds: Array<number> = [];
		for (let index = 0; index < pendingTotal; index += 1) {
			const requested = transitionVoiceEngineV2(snapshot, {
				type: 'diagnostics.logRequested',
				entry: diagnosticEntry(1_000 + index),
			});
			snapshot = requested.snapshot;
			const command = requested.commands[0];
			if (command === undefined) throw new Error('expected a queued diagnostics command');
			pendingOperationIds.push(command.operationId);
		}

		for (const operationId of pendingOperationIds) {
			expect(snapshot.operations[String(operationId)]?.status).toBe('queued');
		}
		const terminalCount = Object.values(snapshot.operations).filter(
			(operation) => operation.status === 'succeeded',
		).length;
		expect(terminalCount).toBe(VOICE_ENGINE_V2_TERMINAL_OPERATIONS_KEPT_MAX);
	});

	it('prunes deterministically across replays', () => {
		const total = VOICE_ENGINE_V2_TERMINAL_OPERATIONS_KEPT_MAX * 2;
		const first = completeDiagnosticCycles(initialSnapshot(), total);
		const second = completeDiagnosticCycles(initialSnapshot(), total);
		expect(second).toEqual(first);
	});
});
