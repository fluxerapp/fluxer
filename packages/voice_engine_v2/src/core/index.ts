// SPDX-License-Identifier: AGPL-3.0-or-later

export {isVoiceEngineV2CommandCompletionStale, transitionVoiceEngineV2} from './reducer';
export {shouldApplyGatewayVoiceStateEcho} from './reducers/gateway';
export type {
	VoiceEngineV2CapabilitiesProjection,
	VoiceEngineV2DeviceProjection,
	VoiceEngineV2DiagnosticsProjection,
	VoiceEngineV2ParticipantProjection,
	VoiceEngineV2StatsPresentationNetworkSummary,
	VoiceEngineV2StatsPresentationProjection,
	VoiceEngineV2StatsProjection,
} from './selectors';
export {
	selectVoiceEngineV2CapabilitiesProjection,
	selectVoiceEngineV2ConnectionProjection,
	selectVoiceEngineV2DeviceProjection,
	selectVoiceEngineV2DiagnosticsProjection,
	selectVoiceEngineV2E2eeProjection,
	selectVoiceEngineV2FailedSourceIds,
	selectVoiceEngineV2MediaProjection,
	selectVoiceEngineV2Model,
	selectVoiceEngineV2ParticipantProjection,
	selectVoiceEngineV2SourceLifecycle,
	selectVoiceEngineV2StatsPresentationProjection,
	selectVoiceEngineV2StatsProjection,
	selectVoiceEngineV2StatsSummary,
	selectVoiceEngineV2WatchedStreams,
} from './selectors';
export type {
	SourceLifecycleState,
	VoiceEngineV2ConnectionState,
	VoiceEngineV2DeviceState,
	VoiceEngineV2E2eeLifecycleState,
	VoiceEngineV2GatewayState,
	VoiceEngineV2HardwareEncoderState,
	VoiceEngineV2InboundVideoState,
	VoiceEngineV2LifecycleState,
	VoiceEngineV2LiveKitState,
	VoiceEngineV2LocalMediaState,
	VoiceEngineV2MicrophoneState,
	VoiceEngineV2NativeAudioTapState,
	VoiceEngineV2NativeCaptureState,
	VoiceEngineV2NativeFrameSinkState,
	VoiceEngineV2OperationState,
	VoiceEngineV2OperationStatus,
	VoiceEngineV2OutputDeviceState,
	VoiceEngineV2PermissionState,
	VoiceEngineV2RoomState,
	VoiceEngineV2Snapshot,
	VoiceEngineV2Transition,
} from './state';
export {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	createVoiceEngineV2PermissionResult,
	emptyVoiceEngineV2DeviceInventory,
	unavailableVoiceEngineV2Capabilities,
} from './state';
