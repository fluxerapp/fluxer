// SPDX-License-Identifier: AGPL-3.0-or-later

export type {VoiceEngineV2MemoryEventLogSpillSink} from './eventLogRing';
export {
	assertEventLogRingInvariants,
	createVoiceEngineV2MemoryEventLogSpillSink,
	VOICE_ENGINE_V2_EVENT_LOG_CAP,
	VOICE_ENGINE_V2_MEMORY_EVENT_LOG_SPILL_SINK_CAP,
	VoiceEngineV2EventLogRing,
} from './eventLogRing';
export type {VoiceEngineV2FrameReceivedEvent} from './frameCoalescing';
export {
	canCoalesceVoiceEngineV2Events,
	coalesceVoiceEngineV2EventSequence,
	isVoiceEngineV2FrameReceivedEvent,
	VOICE_ENGINE_V2_COALESCED_TRACKS_CAP,
} from './frameCoalescing';
export type {
	VoiceEngineV2ClockPort,
	VoiceEngineV2EntropySource,
	VoiceEngineV2PlatformPort,
	VoiceEngineV2RandomPort,
	VoiceEngineV2SystemClockPort,
	VoiceEngineV2SystemRandomPort,
	VoiceEngineV2WallClockSource,
} from './platformPort';
export {
	createVoiceEngineV2DeterministicClockPort,
	createVoiceEngineV2DeterministicPlatformPort,
	createVoiceEngineV2SeededRandomPort,
	createVoiceEngineV2SystemClockPort,
	createVoiceEngineV2SystemPlatformPort,
	createVoiceEngineV2SystemRandomPort,
} from './platformPort';
export {VoiceEngineV2Controller} from './VoiceEngineV2Controller';
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
} from './VoiceEngineV2Runtime';
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
} from './VoiceEngineV2Runtime';
