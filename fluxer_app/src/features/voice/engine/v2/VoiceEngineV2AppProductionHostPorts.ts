// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	DevicePort,
	GatewayPort,
	NativeMediaPort,
	StatsPort,
	SubscriptionPort,
	VoiceEngineV2HostPorts,
} from '@fluxer/voice_engine_v2';
import {createVoiceEngineV2SystemClockPort, type VoiceEngineV2ClockPort} from '@fluxer/voice_engine_v2/runtime';
import type {Room} from 'livekit-client';
import {createVoiceEngineV2AppDevicesAdapter} from './VoiceEngineV2AppDevicesAdapter';
import {createVoiceEngineV2AppDiagnosticsAdapter} from './VoiceEngineV2AppDiagnosticsAdapter';
import {
	createVoiceEngineV2AppHostPorts,
	createVoiceEngineV2AppIngestionPort,
	type VoiceEngineV2AppIngestionPort,
} from './VoiceEngineV2AppHostPorts';
import {
	createVoiceEngineV2AppLifecycleAdapter,
	type VoiceEngineV2AppLifecycleAdapter,
	type VoiceEngineV2AppLifecycleDisposable,
} from './VoiceEngineV2AppLifecycleAdapter';
import {
	type VoiceEngineV2AppLiveKitAudioOutputStore,
	type VoiceEngineV2AppLiveKitConnectionDelegate,
	VoiceEngineV2AppLiveKitExecutionAdapter,
	type VoiceEngineV2AppLiveKitMediaDelegate,
	type VoiceEngineV2AppLiveKitScreenShareDelegate,
} from './VoiceEngineV2AppLiveKitExecutionAdapter';
import {createVoiceEngineV2AppTimerAdapter, type VoiceEngineV2AppTimerScheduler} from './VoiceEngineV2AppTimerAdapter';

export interface VoiceEngineV2AppProductionHostPortsLogger {
	trace(...args: Array<unknown>): void;
	debug(...args: Array<unknown>): void;
	info(...args: Array<unknown>): void;
	warn(...args: Array<unknown>): void;
	error(...args: Array<unknown>): void;
}

export interface VoiceEngineV2AppProductionHostPortsOptions {
	gateway: GatewayPort;
	connection: VoiceEngineV2AppLiveKitConnectionDelegate;
	media: VoiceEngineV2AppLiveKitMediaDelegate;
	screenShare: VoiceEngineV2AppLiveKitScreenShareDelegate;
	getRoom: () => Room | null;
	getActiveGuildId: () => string | null;
	getActiveChannelId: () => string | null;
	stats: StatsPort;
	subscriptions: SubscriptionPort;
	audioOutputStore: VoiceEngineV2AppLiveKitAudioOutputStore;
	logger: VoiceEngineV2AppProductionHostPortsLogger;
	clock?: VoiceEngineV2ClockPort;
	devices?: DevicePort;
	nativeMedia?: NativeMediaPort;
	ingestion?: VoiceEngineV2AppIngestionPort;
	passthrough?: VoiceEngineV2HostPorts;
	lifecycle?: VoiceEngineV2AppLifecycleAdapter;
	lifecycleDisposables?: ReadonlyArray<VoiceEngineV2AppLifecycleDisposable>;
	timerScheduler?: VoiceEngineV2AppTimerScheduler;
}

export function createVoiceEngineV2AppProductionHostPorts(
	options: VoiceEngineV2AppProductionHostPortsOptions,
): VoiceEngineV2HostPorts {
	const clock = options.clock ?? createVoiceEngineV2SystemClockPort();
	const liveKit = new VoiceEngineV2AppLiveKitExecutionAdapter({
		media: options.media,
		connection: options.connection,
		screenShare: options.screenShare,
		getRoom: options.getRoom,
		audioOutputStore: options.audioOutputStore,
		subscriptions: options.subscriptions,
		stats: options.stats,
		logger: options.logger,
		getActiveGuildId: options.getActiveGuildId,
		getActiveChannelId: options.getActiveChannelId,
	});
	const media = liveKit;
	const ingestion = options.ingestion ?? createVoiceEngineV2AppIngestionPort();
	const lifecycle =
		options.lifecycle ??
		createVoiceEngineV2AppLifecycleAdapter({
			disposables: options.lifecycleDisposables ?? [],
			logger: options.logger,
			clock,
		});
	return createVoiceEngineV2AppHostPorts({
		gateway: options.gateway,
		voiceState: ingestion,
		participantProjection: ingestion,
		liveKit,
		media,
		nativeMedia: options.nativeMedia,
		subscriptions: options.subscriptions,
		stats: options.stats,
		devices: options.devices ?? createVoiceEngineV2AppDevicesAdapter(),
		diagnostics: createVoiceEngineV2AppDiagnosticsAdapter({logger: options.logger}),
		timers: createVoiceEngineV2AppTimerAdapter({
			clock,
			scheduler: options.timerScheduler,
			onFire: (event) => {
				ingestion.ingest({type: 'timer.fired', timerId: event.timerId, operationId: null});
			},
			onError: (operation, timerId, error) => {
				options.logger.error('voice-engine-v2 timer adapter callback failed', {operation, timerId, error});
			},
		}),
		lifecycle,
		passthrough: options.passthrough,
	});
}
