// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	VoiceEngineV2AppCapabilitiesAdapter,
	type VoiceEngineV2NativeCapabilitiesBinding,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppCapabilitiesAdapter';
import type {VoiceEngineV2AppLifecycleAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppLifecycleAdapter';
import type {VoiceEngineV2AppLiveKitExecutionAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppLiveKitExecutionAdapter';
import {createVoiceEngineV2AppSystemPermissionAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppSystemPermissionAdapter';
import type {
	CapabilitiesPort,
	DevicePort,
	DiagnosticsPort,
	GatewayPort,
	LiveKitMediaPort,
	LiveKitPort,
	NativeMediaPort,
	ParticipantProjectionIngestionPort,
	PermissionPort,
	StatsPort,
	SubscriptionPort,
	TimerPort,
	VoiceEngineV2Event,
	VoiceEngineV2HostEventListener,
	VoiceEngineV2HostPorts,
	VoiceStateIngestionPort,
} from '@fluxer/voice_engine_v2';

export type VoiceEngineV2AppGatewayVoiceStateAdapter = GatewayPort;
export type VoiceEngineV2AppVoiceStateIngestionAdapter = VoiceStateIngestionPort;
export type VoiceEngineV2AppLiveKitMediaExecutionAdapter = LiveKitMediaPort;
export type VoiceEngineV2AppParticipantProjectionIngestionAdapter = ParticipantProjectionIngestionPort;
export type VoiceEngineV2AppSubscriptionExecutionAdapter = SubscriptionPort;
export type VoiceEngineV2AppStatsPollingAdapter = StatsPort;
export type VoiceEngineV2AppPermissionPortAdapter = PermissionPort;
export type VoiceEngineV2AppDiagnosticsExecutionAdapter = DiagnosticsPort;
export type VoiceEngineV2AppDevicesExecutionAdapter = DevicePort;
export type VoiceEngineV2AppTimerAdapter = TimerPort;

export interface VoiceEngineV2AppHostPortAdapters {
	gateway?: VoiceEngineV2AppGatewayVoiceStateAdapter;
	voiceState?: VoiceEngineV2AppVoiceStateIngestionAdapter;
	liveKit?: VoiceEngineV2AppLiveKitExecutionAdapter;
	media?: VoiceEngineV2AppLiveKitMediaExecutionAdapter;
	nativeMedia?: NativeMediaPort;
	participantProjection?: VoiceEngineV2AppParticipantProjectionIngestionAdapter;
	subscriptions?: VoiceEngineV2AppSubscriptionExecutionAdapter;
	stats?: VoiceEngineV2AppStatsPollingAdapter;
	capabilities?: CapabilitiesPort;
	capabilitiesBinding?: VoiceEngineV2NativeCapabilitiesBinding | null;
	permissions?: VoiceEngineV2AppPermissionPortAdapter;
	diagnostics?: VoiceEngineV2AppDiagnosticsExecutionAdapter;
	devices?: VoiceEngineV2AppDevicesExecutionAdapter;
	timers?: VoiceEngineV2AppTimerAdapter;
	lifecycle?: VoiceEngineV2AppLifecycleAdapter;
	passthrough?: VoiceEngineV2HostPorts;
}

export interface VoiceEngineV2AppIngestionPort
	extends VoiceEngineV2AppVoiceStateIngestionAdapter,
		VoiceEngineV2AppParticipantProjectionIngestionAdapter {
	ingest(event: VoiceEngineV2Event): void;
}

export function createVoiceEngineV2AppHostPorts(adapters: VoiceEngineV2AppHostPortAdapters): VoiceEngineV2HostPorts {
	const passthrough = adapters.passthrough ?? {};
	const capabilities =
		adapters.capabilities ?? new VoiceEngineV2AppCapabilitiesAdapter({binding: adapters.capabilitiesBinding ?? null});
	const permissions =
		adapters.permissions ?? passthrough.permissions ?? createVoiceEngineV2AppSystemPermissionAdapter();
	const liveKit: LiveKitPort | undefined = adapters.liveKit ?? passthrough.liveKit;
	const media: LiveKitMediaPort | undefined = adapters.media ?? adapters.liveKit ?? passthrough.media;
	const lifecycle = adapters.lifecycle;
	const cancelOperation = lifecycle
		? (operationId: number, reason: string): Promise<void> => lifecycle.cancelOperation(operationId, reason)
		: passthrough.cancelOperation;
	const teardown = lifecycle ? (): Promise<void> => lifecycle.teardown() : passthrough.teardown;
	return {
		...passthrough,
		gateway: adapters.gateway ?? passthrough.gateway,
		voiceState: adapters.voiceState ?? passthrough.voiceState,
		liveKit,
		media,
		nativeMedia: adapters.nativeMedia ?? passthrough.nativeMedia,
		participantProjection: adapters.participantProjection ?? passthrough.participantProjection,
		subscriptions: adapters.subscriptions ?? passthrough.subscriptions,
		stats: adapters.stats ?? passthrough.stats,
		capabilities,
		permissions,
		diagnostics: adapters.diagnostics ?? passthrough.diagnostics,
		devices: adapters.devices ?? passthrough.devices,
		timers: adapters.timers ?? passthrough.timers,
		cancelOperation,
		teardown,
	};
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
