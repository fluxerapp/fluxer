// SPDX-License-Identifier: AGPL-3.0-or-later

export type {
	VoiceEngineV2CameraEncodingPlan,
	VoiceEngineV2CameraEncodingPlanAction,
	VoiceEngineV2CameraEncodingPlanInput,
	VoiceEngineV2CameraEncodingPlanReason,
} from './cameraShare';
export {applyVoiceEngineV2CameraEncodingOptions, planVoiceEngineV2CameraEncodingChange} from './cameraShare';
export type {
	VoiceEngineV2CodecViewer,
	VoiceEngineV2NegotiatedCodecPlan,
	VoiceEngineV2NegotiatedCodecReason,
} from './codecNegotiation';
export {
	isMoreEfficientVoiceEngineV2VideoCodec,
	maxDecodableVoiceEngineV2VideoCodec,
	planVoiceEngineV2NegotiatedVideoCodec,
	VOICE_ENGINE_V2_VIDEO_CODEC_FLOOR,
	VOICE_ENGINE_V2_VIDEO_CODEC_PREFERENCE,
	voiceEngineV2VideoCodecRank,
	worseVoiceEngineV2VideoCodec,
} from './codecNegotiation';
export {
	hasVoiceEngineV2NativeHardwareEncoder,
	hasVoiceEngineV2NativeNvencEncoder,
	hasVoiceEngineV2ZeroCopyNativeInput,
	normalizeVoiceEngineV2HardwareEncoderCapabilities,
	unavailableVoiceEngineV2HardwareEncoderCapabilities,
} from './hardwareEncoderCapabilities';
export type {
	VoiceEngineV2MicrophoneFailureContext,
	VoiceEngineV2MicrophoneOperationFailureAction,
	VoiceEngineV2OperationFailureLike,
	VoiceEngineV2OperationResultLike,
} from './microphoneFailureAction';
export {getVoiceEngineV2MicrophoneOperationFailureAction} from './microphoneFailureAction';
export type {
	VoiceEngineV2ScreenEncodingPlan,
	VoiceEngineV2ScreenEncodingPlanAction,
	VoiceEngineV2ScreenEncodingPlanInput,
	VoiceEngineV2ScreenEncodingPlanReason,
} from './screenShare';
export {applyVoiceEngineV2ScreenEncodingOptions, planVoiceEngineV2ScreenEncodingChange} from './screenShare';
export type {
	VoiceEngineV2StatsNetworkSummary,
	VoiceEngineV2StatsSummary,
	VoiceEngineV2StatsTrackClassificationInput,
	VoiceEngineV2StatsTrackPublicationIds,
	VoiceEngineV2StatsTrackRoleCandidate,
	VoiceEngineV2StatsTrackRoleSelection,
	VoiceEngineV2StatsTrackSummary,
	VoiceStatsNetworkSummary,
	VoiceStatsSummary,
	VoiceStatsTrackClassificationInput,
	VoiceStatsTrackPublicationIds,
	VoiceStatsTrackRoleCandidate,
	VoiceStatsTrackRoleSelection,
	VoiceStatsTrackSummary,
} from './voiceStats';
export {
	asVoiceEngineV2StatsTrackSource,
	classifyVoiceEngineV2TrackStats,
	coalesceVoiceEngineV2OutboundStats,
	coerceVoiceEngineV2Stats,
	summarizeVoiceEngineV2Stats,
	VoiceEngineV2StatsTrackSource,
} from './voiceStats';
