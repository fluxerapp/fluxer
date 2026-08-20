// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	availableVoiceEngineV2Capabilities,
	type NativeMediaPort,
	type StatsPort,
	type SubscriptionPort,
	type VoiceEngineV2CameraOptions,
	type VoiceEngineV2Event,
	type VoiceEngineV2MicrophoneOptions,
	type VoiceEngineV2Stats,
} from '@fluxer/voice_engine_v2';
import {createVoiceEngineV2DeterministicClockPort} from '@fluxer/voice_engine_v2/runtime';
import {waitForRuntime} from '@fluxer/voice_engine_v2/testing';
import type {Room} from 'livekit-client';
import {describe, expect, it} from 'vitest';
import {createVoiceEngineV2AppTestControllerHost} from './VoiceEngineV2AppControllerHostTestUtils';
import {createVoiceEngineV2AppIngestionPort} from './VoiceEngineV2AppHostPorts';
import {createVoiceEngineV2AppLifecycleAdapter} from './VoiceEngineV2AppLifecycleAdapter';
import {createVoiceEngineV2AppProductionHostPorts} from './VoiceEngineV2AppProductionHostPorts';
import type {VoiceEngineV2AppTimerScheduler} from './VoiceEngineV2AppTimerAdapter';
import {createVoiceEngineV2ShadowHostPorts, type VoiceEngineV2ShadowHostPortCall} from './VoiceEngineV2ShadowHostPorts';

type ProductionCall =
	| {type: 'connect'; guildId: string | null; channelId: string}
	| {type: 'enableMicrophone'; channelId: string | null; options?: VoiceEngineV2MicrophoneOptions}
	| {type: 'setCameraEnabled'; enabled: boolean; options?: VoiceEngineV2CameraOptions}
	| {type: 'setOutputDevice'; deviceId: string};
type NativeMediaCall =
	| {type: 'startCapture'; captureId: string; zeroCopyRequired: boolean}
	| {type: 'attachFrameSink'; captureId: string; sinkId: string; zeroCopyRequired: boolean};

function createStatsPort(): StatsPort {
	return {
		async collectStats(): Promise<VoiceEngineV2Stats> {
			return {rttMs: null, outbound: [], inbound: []};
		},
	};
}

function createSubscriptionPort(): SubscriptionPort {
	return {
		async setParticipantVolume(): Promise<void> {},
		async setRemoteTrackSubscription(): Promise<void> {},
	};
}

function createNativeMediaPort(calls: Array<NativeMediaCall>): NativeMediaPort {
	return {
		async startCapture(options): Promise<void> {
			calls.push({
				type: 'startCapture',
				captureId: options.captureId,
				zeroCopyRequired: options.zeroCopyRequired,
			});
		},
		async updateCapture(): Promise<void> {},
		async stopCapture(): Promise<void> {},
		async startAudioTap(): Promise<void> {},
		async stopAudioTap(): Promise<void> {},
		async attachFrameSink(options): Promise<void> {
			calls.push({
				type: 'attachFrameSink',
				captureId: options.captureId,
				sinkId: options.sinkId,
				zeroCopyRequired: options.zeroCopyRequired,
			});
		},
		async detachFrameSink(): Promise<void> {},
	};
}

function createLogger() {
	return {
		trace(): void {},
		debug(): void {},
		info(): void {},
		warn(): void {},
		error(): void {},
	};
}

function createBaseProductionOptions() {
	return {
		gateway: {
			async writeVoiceState(): Promise<void> {},
			async clearVoiceState(): Promise<void> {},
		},
		connection: {
			startConnection(): boolean {
				return true;
			},
		},
		media: {
			async enableMicrophone(): Promise<void> {},
			async disableMicrophone(): Promise<void> {},
			async setMicrophoneEnabled(): Promise<void> {},
			async setCameraEnabled(): Promise<'applied'> {
				return 'applied';
			},
			async updateCameraEncoding(): Promise<void> {},
		},
		screenShare: {
			async publishControllerScreenViaLiveKitFlows(): Promise<void> {},
			async unpublishControllerScreenViaLiveKitFlows(): Promise<void> {},
			async updateActiveScreenShareSettings(): Promise<boolean> {
				return true;
			},
			setScreenShareAudioMuted(): void {},
		},
		getRoom: () => ({}) as Room,
		getActiveGuildId: () => 'guild-1',
		getActiveChannelId: () => 'channel-1',
		stats: createStatsPort(),
		subscriptions: createSubscriptionPort(),
		audioOutputStore: {
			async setOutputDevice(): Promise<void> {},
		},
		logger: createLogger(),
	};
}

describe('VoiceEngineV2AppProductionHostPorts', () => {
	it('routes media commands through real production delegates instead of shadow ports', async () => {
		const productionCalls: Array<ProductionCall> = [];
		const shadowCalls: Array<VoiceEngineV2ShadowHostPortCall> = [];
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2AppProductionHostPorts({
				gateway: {
					async writeVoiceState(): Promise<void> {},
					async clearVoiceState(): Promise<void> {},
				},
				connection: {
					startConnection(guildId, channelId): boolean {
						productionCalls.push({type: 'connect', guildId, channelId});
						return true;
					},
				},
				media: {
					async enableMicrophone(_room, channelId, options): Promise<void> {
						productionCalls.push({type: 'enableMicrophone', channelId, options});
					},
					async disableMicrophone(): Promise<void> {},
					async setMicrophoneEnabled(): Promise<void> {},
					async setCameraEnabled(enabled, options): Promise<'applied'> {
						productionCalls.push({type: 'setCameraEnabled', enabled, options});
						return 'applied';
					},
					async updateCameraEncoding(): Promise<void> {},
				},
				screenShare: {
					async publishControllerScreenViaLiveKitFlows(): Promise<void> {},
					async unpublishControllerScreenViaLiveKitFlows(): Promise<void> {},
					async updateActiveScreenShareSettings(): Promise<boolean> {
						return true;
					},
					setScreenShareAudioMuted(): void {},
				},
				getRoom: () => ({}) as Room,
				getActiveGuildId: () => 'guild-1',
				getActiveChannelId: () => 'channel-1',
				stats: createStatsPort(),
				subscriptions: createSubscriptionPort(),
				audioOutputStore: {
					async setOutputDevice(deviceId): Promise<void> {
						productionCalls.push({type: 'setOutputDevice', deviceId});
					},
				},
				logger: createLogger(),
				passthrough: createVoiceEngineV2ShadowHostPorts({
					record(call): void {
						shadowCalls.push(call);
					},
				}),
			}),
		});

		host.controller.connect({url: 'wss://voice.example.test', token: 'token'});
		await waitForRuntime();
		host.controller.publishMicrophone({
			deviceId: 'mic-1',
			echoCancellation: false,
			noiseSuppression: true,
			autoGainControl: false,
		});
		await waitForRuntime();
		host.controller.setOutputDevice({deviceId: 'speaker-1'});
		await waitForRuntime();
		host.controller.publishCamera({deviceId: 'cam-1'});
		await waitForRuntime();
		host.controller.unpublishCamera();
		await waitForRuntime();

		expect(productionCalls).toEqual([
			{type: 'connect', guildId: 'guild-1', channelId: 'channel-1'},
			{
				type: 'enableMicrophone',
				channelId: 'channel-1',
				options: {
					deviceId: 'mic-1',
					echoCancellation: false,
					noiseSuppression: true,
					autoGainControl: false,
				},
			},
			{type: 'setOutputDevice', deviceId: 'speaker-1'},
			{type: 'setCameraEnabled', enabled: true, options: {deviceId: 'cam-1'}},
			{type: 'setCameraEnabled', enabled: false, options: undefined},
		]);
		expect(shadowCalls).toEqual([]);
		host.dispose();
	});

	it('routes native capture and frame sinks through production native-media port', async () => {
		const nativeMediaCalls: Array<NativeMediaCall> = [];
		const shadowCalls: Array<VoiceEngineV2ShadowHostPortCall> = [];
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2AppProductionHostPorts({
				gateway: {
					async writeVoiceState(): Promise<void> {},
					async clearVoiceState(): Promise<void> {},
				},
				connection: {
					startConnection(): boolean {
						return true;
					},
				},
				media: {
					async enableMicrophone(): Promise<void> {},
					async disableMicrophone(): Promise<void> {},
					async setMicrophoneEnabled(): Promise<void> {},
					async setCameraEnabled(): Promise<'applied'> {
						return 'applied';
					},
					async updateCameraEncoding(): Promise<void> {},
				},
				screenShare: {
					async publishControllerScreenViaLiveKitFlows(): Promise<void> {},
					async unpublishControllerScreenViaLiveKitFlows(): Promise<void> {},
					async updateActiveScreenShareSettings(): Promise<boolean> {
						return true;
					},
					setScreenShareAudioMuted(): void {},
				},
				getRoom: () => ({}) as Room,
				getActiveGuildId: () => 'guild-1',
				getActiveChannelId: () => 'channel-1',
				stats: createStatsPort(),
				subscriptions: createSubscriptionPort(),
				audioOutputStore: {
					async setOutputDevice(): Promise<void> {},
				},
				nativeMedia: createNativeMediaPort(nativeMediaCalls),
				logger: createLogger(),
				passthrough: createVoiceEngineV2ShadowHostPorts({
					record(call): void {
						shadowCalls.push(call);
					},
				}),
			}),
		});

		host.dispatch({type: 'capabilities.changed', capabilities: availableVoiceEngineV2Capabilities()});
		host.controller.startNativeCapture({
			captureId: 'screen-1',
			source: {kind: 'screen', id: 'display-1', title: 'Display 1'},
			width: 1920,
			height: 1080,
			frameRate: 60,
			includeCursor: true,
			includeAudio: true,
			zeroCopyRequired: true,
		});
		host.controller.attachNativeFrameSink({
			captureId: 'screen-1',
			sinkId: 'sink-1',
			trackSid: 'TR_screen',
			zeroCopyRequired: true,
		});
		await waitForRuntime();

		expect(nativeMediaCalls).toEqual([
			{type: 'startCapture', captureId: 'screen-1', zeroCopyRequired: true},
			{type: 'attachFrameSink', captureId: 'screen-1', sinkId: 'sink-1', zeroCopyRequired: true},
		]);
		expect(shadowCalls).toEqual([]);
		host.dispose();
	});

	it('fails missing production native-media ports instead of falling through to shadow', async () => {
		const shadowCalls: Array<VoiceEngineV2ShadowHostPortCall> = [];
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2AppProductionHostPorts({
				gateway: {
					async writeVoiceState(): Promise<void> {},
					async clearVoiceState(): Promise<void> {},
				},
				connection: {
					startConnection(): boolean {
						return true;
					},
				},
				media: {
					async enableMicrophone(): Promise<void> {},
					async disableMicrophone(): Promise<void> {},
					async setMicrophoneEnabled(): Promise<void> {},
					async setCameraEnabled(): Promise<'applied'> {
						return 'applied';
					},
					async updateCameraEncoding(): Promise<void> {},
				},
				screenShare: {
					async publishControllerScreenViaLiveKitFlows(): Promise<void> {},
					async unpublishControllerScreenViaLiveKitFlows(): Promise<void> {},
					async updateActiveScreenShareSettings(): Promise<boolean> {
						return true;
					},
					setScreenShareAudioMuted(): void {},
				},
				getRoom: () => ({}) as Room,
				getActiveGuildId: () => 'guild-1',
				getActiveChannelId: () => 'channel-1',
				stats: createStatsPort(),
				subscriptions: createSubscriptionPort(),
				audioOutputStore: {
					async setOutputDevice(): Promise<void> {},
				},
				logger: createLogger(),
			}),
		});

		host.dispatch({type: 'capabilities.changed', capabilities: availableVoiceEngineV2Capabilities()});
		host.controller.startNativeCapture({
			captureId: 'screen-1',
			source: {kind: 'screen', id: 'display-1', title: 'Display 1'},
			width: 1920,
			height: 1080,
			frameRate: 60,
			includeCursor: true,
			includeAudio: true,
			zeroCopyRequired: true,
		});
		await waitForRuntime();

		expect(host.snapshot.nativeCapture.failure).toMatchObject({
			code: 'unsupportedCapability',
			capability: 'nativeMedia',
		});
		expect(shadowCalls).toEqual([]);
		host.dispose();
	});

	it('dispatches timer.fired into the runtime when a scheduled timer fires', async () => {
		const firePending: Array<() => void> = [];
		const scheduler: VoiceEngineV2AppTimerScheduler = {
			setTimeout(callback): unknown {
				firePending.push(callback);
				return firePending.length;
			},
			clearTimeout(): void {},
		};
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2AppProductionHostPorts({
				...createBaseProductionOptions(),
				timerScheduler: scheduler,
			}),
		});
		const seenEvents: Array<VoiceEngineV2Event> = [];
		const unsubscribe = host.subscribe(({event}) => {
			seenEvents.push(event);
		});

		host.dispatch({type: 'timer.scheduleRequested', options: {timerId: 'reconnect-backoff', delayMs: 250}});
		await waitForRuntime();
		expect(firePending).toHaveLength(1);

		firePending[0]?.();
		await waitForRuntime();

		expect(seenEvents).toContainEqual({type: 'timer.fired', timerId: 'reconnect-backoff', operationId: null});
		unsubscribe();
		host.dispose();
	});

	it('wires the production participant projection ingestion source into the runtime', () => {
		const ingestion = createVoiceEngineV2AppIngestionPort();
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2AppProductionHostPorts({
				...createBaseProductionOptions(),
				ingestion,
			}),
		});

		ingestion.ingest({
			type: 'room.participantJoined',
			participant: {sid: 'participant-1', identity: 'user-1', name: 'Ada'},
		});

		expect(host.model.participants).toEqual([{sid: 'participant-1', identity: 'user-1', name: 'Ada'}]);
		host.dispose();
	});

	it('tears down production lifecycle disposables when lifecycle teardown is requested', async () => {
		const disposed: Array<string> = [];
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2AppProductionHostPorts({
				...createBaseProductionOptions(),
				lifecycleDisposables: [
					{
						name: 'native-bridge',
						async dispose(): Promise<void> {
							disposed.push('native-bridge');
						},
					},
				],
			}),
		});

		host.dispatch({type: 'lifecycle.teardownRequested', reason: 'appShutdown'});
		await waitForRuntime();

		expect(disposed).toEqual(['native-bridge']);
		expect(host.snapshot.lifecycle.tearingDown).toBe(false);
		expect(host.snapshot.lifecycle.failure).toBeNull();
		host.dispose();
	});

	it('cancels registered operations through an injected production lifecycle adapter', async () => {
		const lifecycle = createVoiceEngineV2AppLifecycleAdapter({
			disposables: [],
			logger: createLogger(),
			clock: createVoiceEngineV2DeterministicClockPort(),
		});
		const controller = new AbortController();
		lifecycle.register(7, controller, 'test-adapter');
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2AppProductionHostPorts({
				...createBaseProductionOptions(),
				lifecycle,
			}),
		});

		host.dispatch({
			type: 'operation.cancelRequested',
			operationId: 7,
			resourceKey: 'connection',
			reason: 'user-cancelled',
		});
		await waitForRuntime();

		expect(controller.signal.aborted).toBe(true);
		expect(lifecycle.registrySize).toBe(0);
		host.dispose();
	});
});
