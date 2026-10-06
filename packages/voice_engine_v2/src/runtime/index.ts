// SPDX-License-Identifier: AGPL-3.0-or-later

export type {VoiceEngineV2MemoryEventLogSpillSink} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
export {
	assertEventLogRingInvariants,
	createVoiceEngineV2MemoryEventLogSpillSink,
	VOICE_ENGINE_V2_EVENT_LOG_CAP,
	VOICE_ENGINE_V2_MEMORY_EVENT_LOG_SPILL_SINK_CAP,
	VoiceEngineV2EventLogRing,
} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
export type {VoiceEngineV2FrameReceivedEvent} from '@fluxer/voice_engine_v2/src/runtime/frameCoalescing';
export {
	canCoalesceVoiceEngineV2Events,
	coalesceVoiceEngineV2EventSequence,
	isVoiceEngineV2FrameReceivedEvent,
	VOICE_ENGINE_V2_COALESCED_TRACKS_CAP,
} from '@fluxer/voice_engine_v2/src/runtime/frameCoalescing';
export type {
	VoiceEngineV2ClockPort,
	VoiceEngineV2EntropySource,
	VoiceEngineV2PlatformPort,
	VoiceEngineV2RandomPort,
	VoiceEngineV2SystemClockPort,
	VoiceEngineV2SystemRandomPort,
	VoiceEngineV2WallClockSource,
} from '@fluxer/voice_engine_v2/src/runtime/platformPort';
export {
	createVoiceEngineV2DeterministicClockPort,
	createVoiceEngineV2DeterministicPlatformPort,
	createVoiceEngineV2SeededRandomPort,
	createVoiceEngineV2SystemClockPort,
	createVoiceEngineV2SystemPlatformPort,
	createVoiceEngineV2SystemRandomPort,
} from '@fluxer/voice_engine_v2/src/runtime/platformPort';
export {VoiceEngineV2Controller} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Controller';
export type {
	VoiceEngineV2EventLogEntry,
	VoiceEngineV2EventLogSpillSink,
	VoiceEngineV2RuntimeClock,
	VoiceEngineV2RuntimeDiagnostic,
	VoiceEngineV2RuntimeDiagnosticListener,
	VoiceEngineV2RuntimeListener,
	VoiceEngineV2RuntimeListenerPayload,
	VoiceEngineV2RuntimeOptions,
	VoiceEngineV2RuntimeQueueKind,
} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
export {
	assertEventLogInvariants,
	commandResultToEvent,
	isVoiceEngineV2ProgrammerError,
	VOICE_ENGINE_V2_CANCELLED_OPERATIONS_CAP,
	VOICE_ENGINE_V2_DIAGNOSTIC_LISTENERS_CAP,
	VOICE_ENGINE_V2_LISTENERS_CAP,
	VOICE_ENGINE_V2_QUEUED_COMMANDS_CAP,
	VOICE_ENGINE_V2_RESOURCE_QUEUES_CAP,
	VoiceEngineV2Runtime,
} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
