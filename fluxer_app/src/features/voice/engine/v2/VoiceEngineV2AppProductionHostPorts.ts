// SPDX-License-Identifier: AGPL-3.0-or-later

import {createVoiceEngineV2AppDevicesAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppDevicesAdapter';
import {createVoiceEngineV2AppDiagnosticsAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppDiagnosticsAdapter';
import {
	createVoiceEngineV2AppLifecycleAdapter,
	type VoiceEngineV2AppLifecycleDisposable,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppLifecycleAdapter';
import {
	type VoiceEngineV2AppLiveKitAudioOutputStore,
	type VoiceEngineV2AppLiveKitConnectionDelegate,
	VoiceEngineV2AppLiveKitExecutionAdapter,
	type VoiceEngineV2AppLiveKitMediaDelegate,
	type VoiceEngineV2AppLiveKitScreenShareDelegate,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppLiveKitExecutionAdapter';
import {createVoiceEngineV2AppTimerAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppTimerAdapter';
import type {
	GatewayPort,
	ParticipantProjectionIngestionPort,
	StatsPort,
	SubscriptionPort,
	VoiceEngineV2HostEventListener,
	VoiceEngineV2HostPorts,
	VoiceStateIngestionPort,
} from '@fluxer/voice_engine_v2/src/ports';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import {createVoiceEngineV2SystemClockPort} from '@fluxer/voice_engine_v2/src/runtime/platformPort';
import type {Room} from 'livekit-client';

interface VoiceEngineV2AppProductionHostPortsLogger {
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
	ingestion?: VoiceEngineV2AppIngestionPort;
	lifecycleDisposables?: ReadonlyArray<VoiceEngineV2AppLifecycleDisposable>;
}

export function createVoiceEngineV2AppProductionHostPorts(
	options: VoiceEngineV2AppProductionHostPortsOptions,
): VoiceEngineV2HostPorts {
	const clock = createVoiceEngineV2SystemClockPort();
	const media = new VoiceEngineV2AppLiveKitExecutionAdapter({
		media: options.media,
		connection: options.connection,
		screenShare: options.screenShare,
		getRoom: options.getRoom,
		audioOutputStore: options.audioOutputStore,
		logger: options.logger,
		getActiveGuildId: options.getActiveGuildId,
		getActiveChannelId: options.getActiveChannelId,
	});
	const ingestion = options.ingestion ?? createVoiceEngineV2AppIngestionPort();
	const lifecycle = createVoiceEngineV2AppLifecycleAdapter({
		disposables: options.lifecycleDisposables ?? [],
		logger: options.logger,
		clock,
	});
	return {
		gateway: options.gateway,
		voiceState: ingestion,
		participantProjection: ingestion,
		media,
		subscriptions: options.subscriptions,
		stats: options.stats,
		devices: createVoiceEngineV2AppDevicesAdapter(),
		diagnostics: createVoiceEngineV2AppDiagnosticsAdapter({logger: options.logger}),
		timers: createVoiceEngineV2AppTimerAdapter({
			clock,
			onFire: (event) => {
				ingestion.ingest({type: 'timer.fired', timerId: event.timerId, operationId: null});
			},
			onError: (operation, timerId, error) => {
				options.logger.error('voice-engine-v2 timer adapter callback failed', {operation, timerId, error});
			},
		}),
		cancelOperation: (operationId, reason) => lifecycle.cancelOperation(operationId, reason),
		teardown: () => lifecycle.teardown(),
	};
}

export interface VoiceEngineV2AppIngestionPort extends VoiceStateIngestionPort, ParticipantProjectionIngestionPort {
	ingest(event: VoiceEngineV2Event): void;
}

export function createVoiceEngineV2AppIngestionPort(): VoiceEngineV2AppIngestionPort {
	const listeners = new Set<VoiceEngineV2HostEventListener>();
	return {
		subscribe(listener): () => void {
			listeners.add(listener);
			return () => {
				listeners.delete(listener);
			};
		},
		ingest(event): void {
			for (const listener of listeners) {
				listener(event);
			}
		},
	};
}
